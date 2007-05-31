-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XSLT.Application
   Copyright  : Copyright (C) 2006 Tim Walkenhorst, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Common.hs,v 1.5 2007/05/02 06:41:05 hxml Exp $

   Common imports and functions for HXSLT

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XSLT.Common
    ( module Control.Arrow
    , module Text.XML.HXT.Arrow.XmlNode
    , module Text.XML.HXT.DOM.XmlKeywords
    , module Text.XML.HXT.DOM.XmlTreeTypes
    , module Text.XML.HXT.DOM.XmlTreeFunctions
    , module Text.XML.HXT.DOM.FormatXmlTree
    , module Text.XML.HXT.XPath.XPathDataTypes
    , module Text.XML.HXT.XPath.XPathParser
    , module Text.XML.HXT.XPath.XPathEval
    , module Text.XML.HXT.XPath.XPathFct
    , module Text.XML.HXT.XPath.XPathToString
    , module Data.NavTree
    , module Data.Tree.Class

    -- Tree Functions
    , filterTree                 -- Tree t => (a -> Bool) -> t a -> Maybe (t a)
    , mapTreeCtx                 -- Tree t => (c -> a -> (c, b)) -> c -> t a -> t b
    , filterTreeCtx              -- Tree t => (c -> a -> (c, Bool)) -> c -> t a -> Maybe (t a)
    , zipTreeWith                -- Tree t => (a -> b -> c) -> t a -> t b -> t c
    , zipTree                    -- Tree t => t a -> t b -> t (a,b)           (zipTreeWith (,))
    , unzipTree                  -- Tree t => t (a,b) -> (t a, t b)           (mapTree fst &&& mapTree snd)

    -- XML functions
    , isRoot                     -- XmlTree -> Bool
    , isElemType                 -- XmlNode n => QName -> n -> Bool
    , isAttrType                 -- XmlNode n => QName -> n -> Bool
    , isWhitespaceNode           -- XmlTree -> Bool
    , collectTextnodes           -- [XmlTree] -> String
    , tryFetchAttribute          -- XmlNode n => n -> QName -> Maybe String
    , fetchAttributeWDefault     -- XmlNode n => n -> QName -> String -> String
    , fetchAttribute             -- XmlNode n => n -> QName -> String
    , hasAttribute               -- XmlNode n => n -> QName -> Bool
    , setAttribute               -- XmlNode n => QName -> String -> n -> n

    -- Namespace functions
    , mkQName                    -- String (prefix) -> String (local) -> String (uri) -> QName
    , ExName(ExName)             -- String (local) -> String (uri) -> ExName
    , mkExName                   -- QName -> ExName
    , exLocal		         -- ExName -> String
    , exUri		         -- ExName -> String
    , parseExName                -- UriMapping -> String -> ExName
    , UriMapping                 -- Map String String
    , getUriMap                  -- XmlNode n => n -> UriMapping (extract an Ns-Uri Map from an Element node)
    , setUriMap                  -- XmlNode n => UriMap -> n -> n
    , uriMap2Attrs               -- UriMapping -> [XmlTree]      (create xmlns:* Attribute nodes for Uri-Mapping)
    , expandNSDecls              -- XmlTree -> XmlTree
    , lookupPrefix               -- UriMapping -> String -> String
    , isNsAttr                   -- XmlTree -> Bool

    -- additions to XPATH:
    , mkLiteralExpr              -- String -> Expr
    , mkStringExpr               -- Expr -> Expr
    , mkBoolExpr                 -- Expr -> Expr
    , mkTrueExpr                 -- Expr
    , concatExpr                 -- [Expr] -> Expr
    , splitExpr                  -- Expr -> [Expr]
    , unionExpr                  -- [Expr] -> Expr
    , splitMatchByPrio           -- Expr -> [(Float, Expr)]
    , computePriority            -- Expr -> Float
    , computeNTestPriority       -- NodeTest -> Float
    , isMatchExpr                -- Expr -> Bool

    -- Misc.:
    , fromJustErr                -- String -> Maybe a -> a, fromJust with error message
    , readWDefault               -- Read a => a -> String -> a
    )
where

import Control.Arrow

import Text.XML.HXT.Arrow.XmlNode
    ( XmlNode (..)
    , mkElement
    , mkAttr
    , mergeAttrl
    )
import Text.XML.HXT.DOM.XmlKeywords

import Text.XML.HXT.DOM.XmlTreeTypes
    ( XmlTree
    , XNode(XTag, XAttr)
    , QName(QN, namePrefix, localPart, namespaceUri)
    , equivQName
    )
import Text.XML.HXT.DOM.XmlTreeFunctions
    ( mkRootTree
    , xshow
    , isRootNode
    , mkXPiTree
    )
import Text.XML.HXT.DOM.FormatXmlTree
    ( formatXmlTree )

import Text.XML.HXT.XPath.XPathDataTypes
    ( Expr(LiteralExpr, FctExpr, GenExpr, PathExpr)
    , Op(Union)
    , LocationPath (LocPath)
    , Path (Rel)
    , XStep (Step)
    , NodeTest (NameTest, PI, TypeTest)
    , NavXmlTree
    , XPathValue(XPVNode, XPVBool, XPVString, XPVError)
    )

import Text.XML.HXT.XPath.XPathParser
    ( parseXPath )

import Text.XML.HXT.XPath.XPathEval
    ( evalExpr )

import Text.XML.HXT.XPath.XPathFct
    ( isNotInNodeList )

import Text.XML.HXT.XPath.XPathToString
    ( xPValue2XmlTrees )

import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)

import Data.Tree.Class 

import Data.NavTree
    ( NavTree
    , ntree
    , subtreeNT
    , upNT
    , downNT
    , rightNT
    , leftNT
    , getChildrenNT
    )

import Data.Maybe
import Data.List
import Data.Char

--------------------------- 
-- Tree functions

-- mapTree :: Functor t => (a -> b) -> t a -> t b
-- mapTree = fmap

-- "map" on a tree with a context. 
-- Contextual information from the ancestors of the current node can be collected in the context

mapTreeCtx :: Tree t => (c -> a -> (c, b)) -> c -> t a -> t b
mapTreeCtx f c tree = 
    mkTree b $ map (mapTreeCtx f cN) $ getChildren tree
  where 
    (cN, b) = f c $ getNode tree

filterTree :: Tree t => (a -> Bool) -> t a -> Maybe (t a)
filterTree p tree = if p node
                      then Just $ mkTree node $ mapMaybe (filterTree p) $ getChildren tree
                      else Nothing
                    where node = getNode tree

-- "filter" on a tree with a context. 
-- Contextual information from the ancestors of the current node can be collected in the context
filterTreeCtx :: Tree t => (c -> a -> (c, Bool)) -> c -> t a -> Maybe (t a)
filterTreeCtx p c tree =
  if b 
    then Just $ mkTree node $ mapMaybe (filterTreeCtx p cN) $ getChildren tree
    else Nothing
  where 
    (cN, b) = p c node
    node    = getNode tree

zipTreeWith   :: Tree t => (a -> b -> c) -> t a -> t b -> t c
zipTreeWith f a b = mkTree (f (getNode a) (getNode b)) 
                       $ zipWith (zipTreeWith f) (getChildren a) $ getChildren b

zipTree     :: Tree t => t a -> t b -> t (a,b)
zipTree      = zipTreeWith (,)

unzipTree  :: Functor t => t (a,b) -> (t a, t b)
unzipTree   = fmap fst &&& fmap snd

--------------------------- 
-- Xml Functions

isRoot :: XmlTree -> Bool
isRoot = isRootNode . getNode

collectTextnodes :: [XmlTree] -> String
collectTextnodes = concat . mapMaybe getText

isElemType :: XmlNode n => QName -> n -> Bool
isElemType qn node = isElem node && equivQName qn (fromJust $ getElemName node)

isAttrType :: XmlNode n => QName -> n -> Bool
isAttrType qname node = isAttr node && equivQName qname (fromJust $ getAttrName node)

tryFetchAttribute :: XmlNode n => n -> QName -> Maybe String
tryFetchAttribute node qn
  | isElem node =

      if null candidates 
      then Nothing

      else if length candidates > 1
      then error ("More than one attribute " ++ show qn)

      else Just $ collectTextnodes $ getChildren $ head candidates

  | otherwise = Nothing    
  where 
    candidates = filter (isAttrType qn) $ fromJust $ getAttrl node

fetchAttributeWDefault ::  XmlNode n => n -> QName -> String -> String
fetchAttributeWDefault node name def = maybe def id (tryFetchAttribute node name)

fetchAttribute ::  XmlNode n => n -> QName -> String
fetchAttribute node name = fetchAttributeWDefault node name $ error ("Element " ++ show (getElemName node) ++ " has no attribute: " ++ show name)

hasAttribute ::  XmlNode n => n -> QName -> Bool
hasAttribute node = isJust . tryFetchAttribute node

setAttribute :: XmlNode n => QName -> String -> n -> n
setAttribute qn val node
  | isElem node = setElemAttrl (newA : attrs) node
  | otherwise   = error $ "setAttribute on none-element node"             -- how print an XmlNode...
  where 
    attrs = filter (not . isAttrType qn) $ fromJust $ getAttrl node
    newA  = mkTree (XAttr qn) [mkText val]

isWhitespaceNode :: (XmlNode n) => n -> Bool
isWhitespaceNode = maybe False (all isSpace) . getText

--------------------------- 
-- Namespace Functions

mkQName :: String -> String -> String -> QName
mkQName p l u = QN {namePrefix=p, localPart=l, namespaceUri=u}

-- Expanded name, is unique can therefore be used as a key (unlike QName)
data ExName = ExName String String
  deriving (Show, Eq, Ord)

mkExName :: QName -> ExName
mkExName qn = ExName (localPart qn) (namespaceUri qn)

exLocal, exUri :: ExName -> String
exLocal (ExName l _) = l
exUri   (ExName _ u) = u

parseExName :: UriMapping -> String -> ExName
parseExName uris str =

    if noPrefix 
    then ExName str ""

    else ExName loc $ lookupPrefix uris prefix

  where
    noPrefix       = null loc 
    loc            = drop 1 loc'                
    (prefix, loc') = span (/= ':') str

-- Mapping from namespace-Prefixes to namespace-URIs
type UriMapping = Map String String

getUriMap :: XmlNode n => n -> UriMapping
getUriMap = uriMappingsFromNsAttrs . filter isNsAttr . maybe err id . getAttrl
  where err = error "Internal error: getUriMap on none-element node"

setUriMap :: XmlNode n => UriMapping -> n -> n
setUriMap nsMap node = setElemAttrl (mergeAttrl (maybe [] id $ getAttrl node) $ uriMap2Attrs nsMap) node

uriMap2Attrs :: UriMapping -> [XmlTree]
uriMap2Attrs  = map joinNsAttr . Map.toAscList

lookupPrefix :: UriMapping -> String -> String
lookupPrefix uris prefix = fromJustErr ("No namespace-Uri bound to prefix: "++prefix) $ Map.lookup prefix uris

expandNSDecls :: XmlTree -> XmlTree
expandNSDecls = mapTreeCtx (expandNSElem) $ Map.fromAscList [("xml", xmlNamespace), ("xmlns", xmlnsNamespace)]

expandNSElem :: UriMapping -> XNode -> (UriMapping, XNode)
expandNSElem umap node
  | isElem node = (umapNew, nodeNew)
  | otherwise   = (umap, node)
  where
    nodeNew          = XTag (fromJust $ getElemName node) attrNew
    attrNew          = attrs ++ map joinNsAttr (Map.toAscList umapNew)
    umapNew          = uriMappingsFromNsAttrs nsAttrs `Map.union` umap
    (nsAttrs, attrs) = partition isNsAttr $ fromJust $ getAttrl node

uriMappingsFromNsAttrs :: [XmlTree] -> UriMapping
uriMappingsFromNsAttrs = Map.fromList . map splitNsAttr

isNsAttr :: XmlTree -> Bool
isNsAttr = maybe False ((==) xmlnsNamespace . namespaceUri) . getAttrName

splitNsAttr :: XmlTree -> (String, String)
splitNsAttr node = (localPart $ fromJust $ getAttrName node, collectTextnodes $ getChildren node)

joinNsAttr :: (String, String) -> XmlTree
joinNsAttr (prefix, uri) = mkAttr (mkQName "xmlns" prefix xmlnsNamespace) [mkText uri]

-------------------------
-- additions to XPATH:

mkLiteralExpr :: String -> Expr
mkLiteralExpr = LiteralExpr

mkStringExpr :: Expr -> Expr
mkStringExpr = FctExpr "string" . return

mkBoolExpr :: Expr -> Expr
mkBoolExpr = FctExpr "boolean" . return

mkTrueExpr :: Expr
mkTrueExpr = FctExpr "true" []

concatExpr :: [Expr] -> Expr
concatExpr []                    = LiteralExpr ""
concatExpr [lit@(LiteralExpr _)] = lit
concatExpr xs1@[_]               = FctExpr "string" xs1
concatExpr xs                    = FctExpr "concat" xs

splitExpr :: Expr -> [Expr]
splitExpr (GenExpr Union expr) = expr
splitExpr rest                 = [rest]

unionExpr :: [Expr] -> Expr
unionExpr [e] = e
unionExpr es  = GenExpr Union es

-- Intelligent splitting: Split an expression into subexpressions with equal priority
-- for example: "a|c/d|e|f/g"  => [(0.0, "a|e"), (0.5, "c/d|f/g")]
splitMatchByPrio :: Expr -> [(Float, Expr)]
splitMatchByPrio = 
    map compress . groupBy eq . sortBy cmp . map (computePriority &&& id) . splitExpr
  where
    eq  x y  = fst x == fst y
    cmp x y  = compare (fst x) (fst y)
    compress = (head *** unionExpr) . unzip

computePriority :: Expr -> Float
computePriority (PathExpr Nothing (Just (LocPath Rel [Step _ ntest []]))) = computeNTestPriority ntest
computePriority _ = 0.5

computeNTestPriority :: NodeTest -> Float
computeNTestPriority (PI _)        =  0.0
computeNTestPriority (TypeTest _)  = -0.5
computeNTestPriority (NameTest nt) 
  | namePrefix nt /= ""
    && localPart nt == "*"        = -0.25
  | localPart nt == "*"           = -0.5
  | otherwise                     =  0.0

isMatchExpr :: Expr -> Bool
isMatchExpr (GenExpr Union exprs)                          = all isMatchExpr exprs
isMatchExpr (PathExpr _ _)                                 = True
isMatchExpr (FctExpr "id" [LiteralExpr _])                 = True
isMatchExpr (FctExpr "key" [LiteralExpr _, LiteralExpr _]) = True
isMatchExpr _                                              = False

--------------------------- 
-- Misc:

fromJustErr :: String -> Maybe a -> a
fromJustErr msg = maybe (error msg) id 

readWDefault :: Read a => a -> String -> a
readWDefault a str = fst $ head $ reads str ++ [(a, "")]

--------------------------- 
