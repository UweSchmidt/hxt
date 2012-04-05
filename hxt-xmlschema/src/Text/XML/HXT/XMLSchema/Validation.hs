{- |
   Module     : Text.XML.HXT.XMLSchema.Validation
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

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

import Text.XML.HXT.XMLSchema.Loader         ( loadDescription
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

-- | ...
type PrefixMap     = Map String String

type CountingTable = Map QName Int

-- ----------------------------------------

-- Validation

getReqAttrNames :: AttrMap -> [QName]
getReqAttrNames m = map (\ (n, _) -> n) $ filter (\ (_, (req, _)) -> req) (toList m)

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
                                    tell [((xpath env) ++ "/@" ++ (qualifiedName n), "no further check implemented for attribute wildcards.")] -- TODO: check attribute wildcards
                                    return True
                               else do
                                    tell [((xpath env) ++ "/@" ++ (qualifiedName n), "attribute not allowed here.")]
                                    return False
             Just (_, tf) -> local (const (appendXPath ("/@" ++ (qualifiedName n)) env)) (tf val)
    rest <- checkAllowedAttrs xs
    return (res && rest)

testAttrs :: XmlTree -> SVal Bool
testAttrs e
  = do
    env <- ask
    let attrl = getElemAttrs e
    allowedAttrsRes <- checkAllowedAttrs attrl
    reqAttrsRes <- hasReqAttrs (getReqAttrNames (fst $ attrDesc $ elemDesc env)) (map fst attrl)
    return (allowedAttrsRes && reqAttrsRes)

testContentModel :: XmlTrees -> SVal Bool
testContentModel t
  = do
    env <- ask
    case matchXmlRegex (contentModel $ elemDesc env) t of
      Nothing -> return True
      Just msg-> do
                 tell [(xpath env ++ "/*", "content does not match content model.\n" ++ msg)]
                 return False

appendXPath :: String -> SValEnv -> SValEnv
appendXPath s env
  = SValEnv ((xpath env) ++ s) $ elemDesc env

newDesc :: ElemDesc -> SValEnv -> SValEnv
newDesc d env
  = SValEnv (xpath env) d

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
                        tell [((xpath env) ++ elemXPath, "no further check implemented for element wildcards.")] -- TODO: check element wildcards
                        return True
             Just d  -> local (const (appendXPath elemXPath (newDesc d env))) (testElem x)
    rest <- testElemChildren (insert n c t) xs
    return (res && rest)

testElemText :: XmlTrees -> SVal Bool
testElemText t
  = do
    env <- ask
    local (const (appendXPath "/child::text()" env)) $ (sttf $ elemDesc env) $ getCombinedText t

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

extractPrefixMap :: XmlTree -> (XmlTree, PrefixMap)
extractPrefixMap el
  = (el', prefixMap)
    where
    el' = setAttrList rest el
    prefixMap = fromList $ map (\ x -> (localPart $ getAttrName x, getAttrValue' x)) nsAttrs
    (nsAttrs, rest) = partition (\ x -> "xmlns" == (namePrefix $ getAttrName x)) $ getAttrList el

testRoot :: XmlTree -> SVal Bool
testRoot r
  = do
    let (el, _) = extractPrefixMap r -- TODO: apply namespaces
    testElem $ mkRoot [] [el]

-- ----------------------------------------

validateWithSchema :: String -> String -> IO SValResult
validateWithSchema descUri instUri
  = do
    desc <- loadDescription descUri
    case desc of
      Nothing -> return (False, [("/", "Could not process description file.")])
      Just d  -> do
                 inst <- loadInstance instUri
                 case inst of
                   Nothing -> return (False, [("/", "Could not process instance file.")])
                   Just i  -> return $ runSVal (SValEnv "" $ createRootDesc d) (testRoot i)

printSValResult :: SValResult -> IO ()
printSValResult (status, l)
  = do
    if status
      then putStrLn "\nok.\n"
      else putStrLn "\nerrors occurred:\n"
    mapM_ (\ (a, b) -> putStrLn $ a ++ "\n" ++ b ++ "\n") l
    return ()

