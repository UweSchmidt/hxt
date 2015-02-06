{- |
   Module     : Text.XML.HXT.XMLSchema.Validation
   Copyright  : Copyright (C) 2005-2012 Thorben Guelck, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Contains functions to perform validation following a given schema definition.
-}

module Text.XML.HXT.XMLSchema.ValidationCore
--  ( testRoot
--  )

where

import           Control.Monad                          (liftM2)
import           Control.Monad.Reader                   (ask, asks, local)

import           Data.List                              (isPrefixOf, partition)
import           Data.Map                               (Map, fromList, keys,
                                                         lookup, toList)

import           Prelude                                hiding (lookup)

import           Text.XML.HXT.Core                      (QName, XmlTree,
                                                         XmlTrees,
                                                         isNameSpaceName,
                                                         localPart, namePrefix,
                                                         namespaceUri,
                                                         qualifiedName,
                                                         universalName)
{-
import Text.XML.HXT.Arrow.XmlRegex           ( XmlRegex
                                             , matchXmlRegex
                                             -- , mkZero
                                             -- , mkUnit
                                             -- , mkPrim'
                                             -- , mkAlts
                                             -- , mkStar
                                             -- , mkSeq
                                             -- , mkSeqs
                                             -- , mkRep
                                             -- , mkRng
                                             -- , mkPerms
                                             -- , mkMerge
                                             )
-- -}
import           Text.XML.HXT.XMLSchema.Loader
import           Text.XML.HXT.XMLSchema.Regex           (ShowSym (..),
                                                         matchRegex, mkPerm,
                                                         mkPrim, mkStar)
import           Text.XML.HXT.XMLSchema.ValidationTypes
import           Text.XML.HXT.XMLSchema.XmlUtils

-- ----------------------------------------
{-
-- | A table for counting element occurrences
type CountingTable = Map QName Int
-- -}
-- | A table for namespace prefixes and URIs
type PrefixMap     = Map String String

-- ----------------------------------------

-- | Creates a regex which matches on an element with a given name
mkElemNameRE :: QName -> XmlRegex'
mkElemNameRE qn
    = mkElemRE (== qn) testElem ("<" ++ universalName qn ++ ">")

-- | Creates a regex which matches on an element with a given namespace predicate
mkElemRE :: (QName -> Bool) -> (XmlTree -> SVal Bool) -> String -> XmlRegex'
mkElemRE p checkCont exrep
    = mkPrim
      (p . getElemName)
      (\ t -> (.&&. checkCont t))
      exrep

-- | Creates a regex which matches on text nodes
mkTextRE :: XmlRegex'
mkTextRE
    = mkStar $
      mkPrim isText (const id) "{plain text}"

mkSimpleContentRE :: (String -> SVal Bool) -> XmlRegex'
mkSimpleContentRE sp
    = mkPrim isText (\ t -> (.&&. (sp . getText $ t))) "{simple type}"

mkMixedContentRE :: XmlRegex' -> XmlRegex'
mkMixedContentRE re
    = mkPerm re $ mkStar mkTextRE

mkWildcardRE :: Wildcard -> XmlRegex'
mkWildcardRE (WC p cl)
    = mkPrim
      (\ t -> isElem t && p (getElemName t))
      (\ t -> (.&&. testWildcard cl t))
      "{any element}"

mkErrorRE :: String -> XmlRegex'
mkErrorRE s
    = mkPrim (const False) (const id) $ "{" ++ s ++ "}"

-- ----------------------------------------

-- | Computes a list of required attributes
getReqAttrNames :: AttrMap -> [QName]
getReqAttrNames m
    = map fst $ filter (fst . snd) $ toList m

-- | Checks list inclusion with a list of required attributes for a list of attributes
hasReqAttrs :: [QName] -> [QName] -> SVal Bool
hasReqAttrs [] _
  = return True

hasReqAttrs (x:xs) attrs
    | x `notElem` attrs
        = mkErrorSTTF''
              (++ ("/@" ++ qualifiedName x))
              "required attribute is missing."
          >>
          hasReqAttrs xs attrs
          >>
          return False

    | otherwise
        = hasReqAttrs xs attrs

-- | Checks whether a list of attributes is allowed for an element and checks each attribute's value
checkAllowedAttrs :: [(QName, String)] -> SVal Bool
checkAllowedAttrs as
  = foldr (.&&.) (return True) . map checkAllowedAttr $ as

checkAllowedAttr :: (QName, String) -> SVal Bool
checkAllowedAttr (n, val)
  = do
    env <- ask
    let (am, wp) = attrDesc $ elemDesc env
    res <- case lookup n am of
             Nothing
                 -> if isNameSpaceName n
                    then return True  -- namespace declarations may occure everywhere
                    else
                    if namespaceUri n == nsUriXMLSchemaInstance
                       &&
                       localPart n `elem` localNamesXMLSchemaInstance
                    then return True  -- predefined XML schema instance names
                    else
                    if illegalNsUri `isPrefixOf` namespaceUri n -- TODO: this is a hack
                    then mkErrorSTTF''
                             (++ ("/@" ++ qualifiedName n))
                             (namespaceUri n)
                    else
                    if or $ map (\ f -> f n) wp
                    then mkWarnSTTF''
                             (++ ("/@" ++ qualifiedName n))
                             "no check implemented for attribute wildcard's content."
                             -- TODO: check attribute wildcard's content?
                    else mkErrorSTTF''
                             (++ ("/@" ++ qualifiedName n))
                             "attribute not allowed here."
             Just (_, tf)
                 -> local (appendXPath ("/@" ++ qualifiedName n)) $ tf val
    return  res

-- | Performs the combined attribute list check for a given element
testAttrs :: XmlTree -> SVal Bool
testAttrs t
    = do ed <- asks elemDesc
         checkAllowedAttrs attrl
           .&&. (hasReqAttrs (getReqAttrNames . fst $ attrDesc ed) $ map fst attrl)
    where
      attrl = getElemAttrs t

-- | Checks the content model of a given element using regular expression derivation
{- old stuff
testContentModel :: XmlRegex -> XmlTrees -> SVal Bool
testContentModel re t
    = case matchXmlRegex re t of
        Nothing
            -> return True
        Just msg
            -> mkErrorSTTF''
               (++ "/*")
               ("content does not match content model.\n" ++ msg) -- ++ "\ncontent model: " ++ show re)
-- -}

testContentModel' :: XmlRegex' -> XmlTrees -> SVal Bool
testContentModel' re ts
    = case matchRegex ts re (return True) of
        Right checkElems
            -> checkElems
        Left msg
            -> mkErrorSTTF''
               (++ "/*")
               ("content does not match content model.\n" ++ msg ++ "\ncontent model: " ++ show re)

-- | Extends the validation environment's XPath
appendXPath :: String -> SValEnv -> SValEnv
appendXPath s env
    = env {xpath = xpath env ++ s}

-- | Exchanges the validation environment's element description
setDesc :: ElemDesc -> SValEnv -> SValEnv
setDesc d env
    = env {elemDesc = d}

{- old stuff

-- | Recursively invokes checks on all subelems of a given element
--   Also constructs an absolute XPath for each subelem for accurate error reporting
testElemChildren :: CountingTable -> XmlTrees -> SVal Bool
testElemChildren _ []
  = return True

testElemChildren t (x:xs)
  = do
    env <- ask
    let n = getElemName x
    let c = maybe 1 (+1) $ lookup n t
    let elemXPath = "/" ++ (qualifiedName n) ++ "[" ++ (show c) ++ "]"
    res <- case lookup n $ subElemDesc $ elemDesc env of
             Nothing
                 -> mkWarnSTTF''
                        (++ elemXPath)
                        ("no check implemented for element wildcard's content." ++ show x)
                        -- TODO: check element wildcard's content?
             Just d
                 -> local (appendXPath elemXPath . setDesc d) $ testElem x
    rest <- testElemChildren (insert n c t) xs
    return $ res && rest

-- | Checks the element's text content
testElemText :: XmlTrees -> SVal Bool
testElemText t
  = do
    env <- ask
    local (appendXPath "/child::text()") $ (sttf $ elemDesc env) $ getCombinedText t

-- | Performs the combined checks for a given element
testElem :: XmlTree -> SVal Bool
testElem e
    = do ed <- asks elemDesc
         case (errmsg ed) of
           Just msg -> mkErrorSTTF' msg
           Nothing  -> do
                  attrRes <- testAttrs e
                  let content = getElemChildren e
                  let (tags, text) = extractElems content
                  contModelRes <- if mixedContent ed
                                    then testContentModel (contentModel ed) tags
                                    else if (length tags > 0 && length text > 0)
                                           then mkErrorSTTF'' (++ "/*") "no mixed content allowed here."
                                           else testContentModel (contentModel ed) content
                  contRes <- if contModelRes
                               then do
                                    textRes <- testElemText text
                                    tagsRes <- testElemChildren empty tags
                                    return $ textRes && tagsRes
                               else return False
                  return $ attrRes && contRes
-- -}
{-
testCont :: CountingTable -> XmlTree  -> SVal (CountingTable, Bool)
testCont t e
    | isElem e
        = let n        = getElemName e in
          let (xp, t') = path n in
          do ed <- asks elemDesc
             res <- case lookup n $ subElemDesc ed of
                      Nothing
                          -> mkWarnSTTF'' (++ xp)
                             ("no check implemented for element wildcard's content.") -- ++ show e)
                             -- TODO: check element wildcard's content?
                             -- testCont must be further parameterized with wildcard spec
                             -- for variants: skip, lax, strict
                             -- if skip or lax are given this case is ok
                             -- else it's an error
                             -- the subElemDesc must be set to allElemDesc when transforming a wildcard type
                      Just d
                          -> local (appendXPath xp . setDesc d) $ testElem' e

             return (t', res)

    | otherwise
        = return (t, True)
    where
      path qn = (xp, insert qn c t)
          where
            n  = qualifiedName qn
            c  = maybe 1 (+1) $ lookup qn t
            xp = "/" ++ n ++ "[" ++ (show c) ++ "]"

testContent :: CountingTable -> XmlTrees -> SVal Bool
testContent _ []
    = return True
testContent t (x : xs)
    = do (t1, res1) <- testCont t x
         res2       <- testContent t1 xs
         return     $  res1 && res2
-- -}
-- ----------------------------------------

testElem :: XmlTree -> SVal Bool
testElem
    = localPath testElem''
    where
      testElem'' :: XmlTree -> SVal Bool
      testElem'' t
          = do ed <- asks elemDesc
               case lookupQN t $ subElemDesc ed of
                 Nothing
                     -> do xp <- asks xpath
                           mkErrorSTTF'' (const xp) "element is not defined in schema"
                 Just ed'
                     -> localED ed' testElem' t

testElem' :: XmlTree -> SVal Bool
testElem' e
    = do ed <- asks elemDesc
         logg ["testElem'", showSym e, "elemDesc =", showElemDesc ed]
         case (errmsg ed) of
           Just msg -> mkErrorSTTF' msg
           Nothing  -> testElem'' ed
    where
      testElem'' ed
          = testAttrs e
            .&&.
            testElems (mixedContent ed) (sttf ed)
          where
            testElems True _
                = testContentModel' (mkMixedContentRE $ contentModel ed) content
            testElems _    Nothing
                = testContentModel' (                   contentModel ed) content
            testElems _    (Just sf)
                | allText
                     = local (appendXPath "/child::text()") $ sf $ getCombinedText content
                | otherwise
                    = mkErrorSTTF'' (++ "/*") $ "no mixed content allowed here." ++ show content

      content       = filter isElemOrText $ getElemChildren e
      allText       = all isText content

-- ----------------------------------------

testWildcard :: WildcardClass -> XmlTree -> SVal Bool
testWildcard wc t
    = localPath (testWildcard' wc) t

testWildcard' :: WildcardClass -> XmlTree -> SVal Bool
testWildcard' wc t
    = do allEds <- asks allElemDesc
         logg ["testWildcard", showSym t]
         let ed = lookupQN t allEds
         case wc of
           Skip   ->        return True
           Lax    -> maybe (return True) test ed
           Strict -> maybe errMsg        test ed
    where
      test ed' = localED ed' testElem' t
      -- qn = qualifiedName . getElemName $ t
      un = getElemName $ t
      errMsg
          = do xp <- asks xpath
               alluns <- asks (keys . allElemDesc)
               mkErrorSTTF'' (const xp) $
                             unwords [ "undefined element:"
                                     , show un -- qn
                                     , ", found in wildcard contents (processContents=\"strict\")"
                                     , "\nelements defined:", show alluns
                                     , "\ninput", show t
                                     ]

localPath :: (XmlTree -> SVal a) -> XmlTree -> SVal a
localPath fct t
    = local (appendXPath xp) $ fct t
    where
      xp | isElem t  = "/" ++ qualifiedName (getElemName t)
         | otherwise = "/child::text()"

localED :: ElemDesc -> (XmlTree -> SVal a) -> XmlTree -> SVal a
localED ed fct
    = local (setDesc ed) . fct

lookupQN :: XmlTree -> SubElemDesc -> Maybe ElemDesc
lookupQN = lookup . getElemName

-- ----------------------------------------
--
-- | Extracts a namespace prefix table from a given element's attribute list

extractPrefixMap :: XmlTree -> (XmlTree, PrefixMap)
extractPrefixMap el
  = (el', prefixMap)
    where
    el'             = setAttrList rest el
    prefixMap       = fromList $ map (\ x -> (localPart $ getAttrName x, getAttrValue x)) nsAttrs
    (nsAttrs, rest) = partition ((== "xmlns") . namePrefix . getAttrName) $ getAttrList el

-- | Entry point for the instance document's validation
testRoot :: XmlTree -> SVal Bool
testRoot r
  = do
    let (el, _) = extractPrefixMap r -- TODO: apply namespaces
    testElem' $ mkRoot [] [el]
    -- testWildcard Strict el -- $ mkRoot [] [el]

(.&&.) :: SVal Bool -> SVal Bool -> SVal Bool
(.&&.) = liftM2 (&&)

(.&&&.) :: SVal Bool -> SVal Bool -> SVal Bool
x1 .&&&. x2
    = do r1 <- x1
         if r1
           then x2
           else return r1

-- ----------------------------------------
