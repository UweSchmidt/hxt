{- |
   Module     : Text.XML.HXT.XMLSchema.Validation
   Copyright  : Copyright (C) 2005-2012 Thorben Guelck, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Contains functions to perform validation following a given schema definition.
-}

module Text.XML.HXT.XMLSchema.Validation
  ( validateDocumentWithXmlSchema

    -- these are only used in test suite
  , validateWithXmlSchema'
  , SValResult
  , SValEnv(..)
  , XmlSchema
  , runSVal
  , testRoot
  , createRootDesc
  )

where

import Control.Monad                         ( liftM2
                                             )
import Control.Monad.Reader                  ( ask
                                             , asks
                                             , local
                                             )

import Data.List                             ( isPrefixOf
                                             , partition
                                             )
import Data.Map                              ( Map
                                             , empty
                                             , lookup
                                             , insert
                                             , fromList
                                             , toList
                                             )

import Prelude                        hiding ( lookup )

import Text.XML.HXT.Arrow.XmlRegex           ( matchXmlRegex )

import Text.XML.HXT.Core                     ( QName
                                             , XmlTree
                                             , XmlTrees

                                             , isNameSpaceName	-- QName stuff
                                             , qualifiedName
                                             , namespaceUri
                                             , localPart
                                             , namePrefix

                                             , arr		-- arrow stuff
                                             , arrL
                                             , constA
                                             , isA
                                             , none
                                             , this
                                             , traceMsg
                                             , perform
                                             , (>>>)
                                             , ($<)
                                             , orElse
                                             , when
                                             , guards
                                             , setDocumentStatusFromSystemState
                                             , documentStatusOk
                                             , issueWarn
                                             , issueErr
                                             , issueFatal
                                             , c_err
                                             )
import qualified Text.XML.HXT.Core           as C
import Text.XML.HXT.Arrow.XmlRegex           ( XmlRegex
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
import Text.XML.HXT.Arrow.XmlState.TypeDefs  ( SysConfigList
                                             , IOStateArrow
                                             , IOSArrow
                                             , setS
                                             , theXmlSchemaValidate
                                             , withoutUserState
                                             , localSysEnv
                                             , configSysVars
                                             )

import Text.XML.HXT.XMLSchema.AbstractSyntax ( XmlSchema )
import Text.XML.HXT.XMLSchema.Loader
import Text.XML.HXT.XMLSchema.Transformation ( createRootDesc
                                             -- , mkTextRE
                                             )
import Text.XML.HXT.XMLSchema.ValidationTypes
import Text.XML.HXT.XMLSchema.XmlUtils

-- ----------------------------------------

-- | A table for counting element occurrences
type CountingTable = Map QName Int

-- | A table for namespace prefixes and URIs
type PrefixMap     = Map String String

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
checkAllowedAttrs []
  = return True

checkAllowedAttrs ((n, val):xs)
  = do
    env <- ask
    let (am, wp) = attrDesc $ elemDesc env
    res <- case lookup n am of
             Nothing
                 -> if isNameSpaceName n
                    then return True  -- namespace declarations may occure everywhere  
                    else
                    if illegalNsUri `isPrefixOf` namespaceUri n
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
    rest <- checkAllowedAttrs xs
    return $ res && rest

-- | Performs the combined attribute list check for a given element
testAttrs :: XmlTree -> SVal Bool
testAttrs e
    = do ed              <- asks elemDesc
         allowedAttrsRes <- checkAllowedAttrs attrl
         reqAttrsRes     <- hasReqAttrs (getReqAttrNames $ fst $ attrDesc ed) $ map fst attrl
         return          $  allowedAttrsRes && reqAttrsRes
    where
      attrl = getElemAttrs e
      
-- | Checks the content model of a given element using regular expression derivation

testContentModel :: XmlRegex -> XmlTrees -> SVal Bool
testContentModel re t
    = case matchXmlRegex re t of
        Nothing
            -> return True
        Just msg
            -> mkErrorSTTF''
               (++ "/*")
               ("content does not match content model.\n" ++ msg) -- ++ "\ncontent model: " ++ show re)

-- | Extends the validation environment's XPath
appendXPath :: String -> SValEnv -> SValEnv
appendXPath s env
    = env {xpath = xpath env ++ s}

-- | Exchanges the validation environment's element description
newDesc :: ElemDesc -> SValEnv -> SValEnv
newDesc d env
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
                 -> local (appendXPath elemXPath . newDesc d) $ testElem x
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
                          -> local (appendXPath xp . newDesc d) $ testElem' e
             
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

testElem' :: XmlTree -> SVal Bool
testElem' e
    = do ed <- asks elemDesc
         case (errmsg ed) of
           Just msg -> mkErrorSTTF' msg
           Nothing  -> testAttrs e
                       .&&.
                       if mixedContent ed
                       then testContentModel (contentModel ed) contentElems
                            .&&&.
                            testContent empty                  contentElems
                       else
                       if allText || allElem
                       then testContentModel (contentModel ed) content
                            .&&&.
                            if all isText content
                               then local (appendXPath "/child::text()")
                                        $ (sttf ed)
                                        $ getCombinedText content
                               else testContent empty content
                       else mkErrorSTTF'' (++ "/*") "no mixed content allowed here."
    where
      content       = filter isElemOrText $ getElemChildren e
      contentElems  = filter isElem       $ content
      allText       = null contentElems
      allElem       = length content == length contentElems

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

(.&&.) :: SVal Bool -> SVal Bool -> SVal Bool
(.&&.) = liftM2 (&&)

(.&&&.) :: SVal Bool -> SVal Bool -> SVal Bool
x1 .&&&. x2
    = do r1 <- x1
         if r1
           then x2
           else return r1

-- ----------------------------------------

validateWithXmlSchema' :: XmlSchema -> XmlTree -> SValResult
validateWithXmlSchema' schema doc
    = runSVal (SValEnv { xpath = ""
                       , elemDesc        = rootElemDesc
                       , allElemDesc     = allElems
                       , allContentModel = allCont
                       }
              ) $ testRoot doc
    where
      (rootElemDesc, allElems, allCont) = createRootDesc schema

-- ----------------------------------------

validateDocumentWithXmlSchema :: SysConfigList -> String -> IOStateArrow s XmlTree XmlTree
validateDocumentWithXmlSchema config schemaUri
    = ( withoutUserState
        $
        localSysEnv
        $
        configSysVars (config ++ [setS theXmlSchemaValidate False])
        >>>
        traceMsg 1 ( "validating document with XML schema: " ++ show schemaUri )
        >>>
        ( ( validateWithSchema schemaUri
            >>>
            traceMsg 1 "document validation done, no errors found"
          )
          `orElse`
          ( traceMsg 1 "document not valid, errors found"
            >>>
            setDocumentStatusFromSystemState "validating with XML schema"
          )
        )
      )
      `when`
      documentStatusOk                                 -- only do something when document status is ok

validateWithSchema :: String -> IOSArrow XmlTree XmlTree
validateWithSchema uri
    = validate $< ( ( constA uri
                      >>>
                      traceMsg 1 ("reading XML schema definition from " ++ show uri)
                      >>>
                      loadDefinition []
                      >>>
                      traceMsg 1 ("XML schema definition read from " ++ show uri)
                    )
                    `orElse`
                    ( issueFatal ("Could not read XML Schema definition from " ++ show uri)
                      >>>
                      none
                    )
                  )
    where
      issueMsg (lev, xp, e)
          = (if lev < c_err then issueWarn else issueErr) $
            "At XPath " ++ xp ++ ": " ++ e

      validate schema
          = ( C.getChildren >>> C.isElem	-- select document root element
              >>>
              traceMsg 1 "document validation process started"
              >>>
              arr (validateWithXmlSchema' schema)
              >>>
              perform ( arrL snd
                        >>>
                        ( issueMsg $< this )
                      )
              >>>
              isA ((== True) . fst)
            )
            `guards` this

-- ----------------------------------------

