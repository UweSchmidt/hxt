{- |
   Module     : Text.XML.HXT.XMLSchema.Validation
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Contains functions to perform validation following a given schema definition.
-}

module Text.XML.HXT.XMLSchema.Validation

  ( validateWithSchema
  , SValResult
  , printSValResult
  )

where

import Text.XML.HXT.XMLSchema.XmlUtils
import Text.XML.HXT.XMLSchema.ValidationTypes

import Text.XML.HXT.XMLSchema.Transformation ( createRootDesc )

import Text.XML.HXT.XMLSchema.Loader         ( loadDefinition
                                             , loadInstance
                                             )

import Text.XML.HXT.Core                     ( QName
                                             , localPart
                                             , namePrefix
                                             , qualifiedName
                                             , XmlTree
                                             , XmlTrees
                                             )

import Text.XML.HXT.Arrow.XmlRegex           ( matchXmlRegex )

import Control.Monad.Reader                  ( ask
                                             , local
                                             )

import Control.Monad.Writer                  ( tell )

import Data.List                             ( partition )

import Data.Map                              ( Map
                                             , empty
                                             , lookup
                                             , insert
                                             , fromList
                                             , toList
                                             )

import Prelude hiding (lookup)

-- ----------------------------------------

-- | A table for counting element occurrences
type CountingTable = Map QName Int

-- | A table for namespace prefixes and URIs
type PrefixMap     = Map String String

-- ----------------------------------------

-- | Computes a list of required attributes
getReqAttrNames :: AttrMap -> [QName]
getReqAttrNames m = map (\ (n, _) -> n) $ filter (\ (_, (req, _)) -> req) (toList m)

-- | Checks list inclusion with a list of required attributes for a list of attributes
hasReqAttrs :: [QName] -> [QName] -> SVal Bool
hasReqAttrs [] _
  = return True
hasReqAttrs (x:xs) attrs
  = do
    env <- ask
    if x `notElem` attrs
      then do
           tell [((xpath env) ++ "/@" ++ (qualifiedName x), "required attribute is missing.")]
           res <- hasReqAttrs xs attrs
           return (res && False)
      else hasReqAttrs xs attrs

-- | Checks whether a list of attributes is allowed for an element and checks each attribute's value
checkAllowedAttrs :: [(QName, String)] -> SVal Bool
checkAllowedAttrs []
  = return True
checkAllowedAttrs ((n, val):xs)
  = do
    env <- ask
    let ad = attrDesc $ elemDesc env
    res <- case lookup n $ fst ad of
             Nothing      -> if foldr (||) False $ map (\ f -> f n) $ snd ad
                               then do
                                    tell [((xpath env) ++ "/@" ++ (qualifiedName n),
                                           "no check implemented for attribute wildcard's content.")]
                                           -- TODO: check attribute wildcard's content?
                                    return True
                               else do
                                    tell [((xpath env) ++ "/@" ++ (qualifiedName n), "attribute not allowed here.")]
                                    return False
             Just (_, tf) -> local (const (appendXPath ("/@" ++ (qualifiedName n)) env)) (tf val)
    rest <- checkAllowedAttrs xs
    return (res && rest)

-- | Performs the combined attribute list check for a given element
testAttrs :: XmlTree -> SVal Bool
testAttrs e
  = do
    env <- ask
    let attrl = getElemAttrs e
    allowedAttrsRes <- checkAllowedAttrs attrl
    reqAttrsRes <- hasReqAttrs (getReqAttrNames (fst $ attrDesc $ elemDesc env)) (map fst attrl)
    return (allowedAttrsRes && reqAttrsRes)

-- | Checks the content model of a given element using regular expression derivation
testContentModel :: XmlTrees -> SVal Bool
testContentModel t
  = do
    env <- ask
    case matchXmlRegex (contentModel $ elemDesc env) t of
      Nothing -> return True
      Just msg-> do
                 tell [(xpath env ++ "/*", "content does not match content model.\n" ++ msg)]
                 return False

-- | Extends the validation environment's XPath
appendXPath :: String -> SValEnv -> SValEnv
appendXPath s env
  = SValEnv ((xpath env) ++ s) $ elemDesc env

-- | Exchanges the validation environment's element description
newDesc :: ElemDesc -> SValEnv -> SValEnv
newDesc d env
  = SValEnv (xpath env) d

-- | Recursively invokes checks on all subelems of a given element
--   Also constructs an absolute XPath for each subelem for accurate error reporting
testElemChildren :: CountingTable -> XmlTrees -> SVal Bool
testElemChildren _ []
  = return True
testElemChildren t (x:xs)
  = do
    env <- ask
    let n = getElemName x
    let c = case lookup n t of
              Nothing -> 1
              Just v  -> v+1
    let elemXPath = "/" ++ (qualifiedName n) ++ "[" ++ (show c) ++ "]"
    res <- case lookup n $ subElemDesc $ elemDesc env of
             Nothing -> do
                        tell [((xpath env) ++ elemXPath,
                               "no check implemented for element wildcard's content.")]
                               -- TODO: check element wildcard's content?
                        return True
             Just d  -> local (const (appendXPath elemXPath (newDesc d env))) (testElem x)
    rest <- testElemChildren (insert n c t) xs
    return (res && rest)

-- | Checks the element's text content
testElemText :: XmlTrees -> SVal Bool
testElemText t
  = do
    env <- ask
    local (const (appendXPath "/child::text()" env)) $ (sttf $ elemDesc env) $ getCombinedText t

-- | Performs the combined checks for a given element
testElem :: XmlTree -> SVal Bool
testElem e
  = do
    env <- ask
    case (errmsg $ elemDesc env) of
      Just msg -> do
                  tell [(xpath env, msg)]
                  return False
      Nothing  -> do
                  attrRes <- testAttrs e
                  let content = getElemChildren e
                  contModelRes <- testContentModel content
                  contRes <- if contModelRes
                               then do
                                    let (tags, text) = extractElems content
                                    textRes <- testElemText text
                                    tagsRes <- testElemChildren empty tags
                                    return (textRes && tagsRes)
                               else return False
                  return (attrRes && contRes)

-- | Extracts a namespace prefix table from a given element's attribute list
extractPrefixMap :: XmlTree -> (XmlTree, PrefixMap)
extractPrefixMap el
  = (el', prefixMap)
    where
    el' = setAttrList rest el
    prefixMap = fromList $ map (\ x -> (localPart $ getAttrName x, getAttrValue x)) nsAttrs
    (nsAttrs, rest) = partition (\ x -> "xmlns" == (namePrefix $ getAttrName x)) $ getAttrList el

-- | Entry point for the instance document's validation
testRoot :: XmlTree -> SVal Bool
testRoot r
  = do
    let (el, _) = extractPrefixMap r -- TODO: apply namespaces
    testElem $ mkRoot [] [el]

-- ----------------------------------------

-- | Loads the schema definition and instance documents and invokes validation
validateWithSchema :: String -> String -> IO SValResult
validateWithSchema defUri instUri
  = do
    def <- loadDefinition defUri
    case def of
      Nothing -> return (False, [("/", "Could not process definition file.")])
      Just d  -> do
                 inst <- loadInstance instUri
                 case inst of
                   Nothing -> return (False, [("/", "Could not process instance file.")])
                   Just i  -> return $ runSVal (SValEnv "" $ createRootDesc d) (testRoot i)

-- | Writes validation results to stdout
printSValResult :: SValResult -> IO ()
printSValResult (status, l)
  = do
    if status
      then putStrLn "\nok.\n"
      else putStrLn "\nerrors occurred:\n"
    mapM_ (\ (a, b) -> putStrLn $ a ++ "\n" ++ b ++ "\n") l
    return ()

