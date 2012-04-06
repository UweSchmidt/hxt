{- |
   Module     : Text.XML.HXT.XMLSchema.XmlUtils
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Contains helper functions to work with the XmlTree datatype.
-}

module Text.XML.HXT.XMLSchema.XmlUtils

where

import qualified Text.XML.HXT.DOM.XmlNode as XN

import Text.XML.HXT.Core ( QName
                         , mkName
                         , XmlTree
                         , XmlTrees
                         )

import Data.Maybe        ( fromMaybe )

import Data.List         ( partition )

import Data.Tree.NTree.TypeDefs

-- ----------------------------------------

-- | Create a root XmlTree
mkRoot :: XmlTrees -> XmlTrees -> XmlTree
mkRoot = XN.mkRoot

-- | Retrieve an element's name
getElemName :: XmlTree -> QName
getElemName = (fromMaybe (mkName "")) . XN.getElemName

-- | Retrieve an element's attributes
getAttrList :: XmlTree -> [XmlTree]
getAttrList = (fromMaybe []) . XN.getAttrl

-- | Redefine an element's attributes
setAttrList :: XmlTrees -> XmlTree -> XmlTree
setAttrList = XN.setElemAttrl

-- | Retrieve an element's attributes as a list of key-value pairs
getElemAttrs :: XmlTree -> [(QName, String)]
getElemAttrs = (map (\ x -> (getAttrName x, getAttrValue x))) . getAttrList

-- | Retrieve a node's children
getChildren :: XmlTree -> XmlTrees
getChildren (NTree _ c) = c

-- | Retrieve an element's children which are relevant for validation
getElemChildren :: XmlTree -> XmlTrees
getElemChildren = (filter isRelevant) . getChildren

-- | Split a list of trees into a list of elements and a list of the rest
extractElems :: XmlTrees -> (XmlTrees, XmlTrees)
extractElems = partition isElem

-- | Tests whether a tree is an element
isElem :: XmlTree -> Bool
isElem = XN.isElem

-- | Tests whether a tree is a text node
isText :: XmlTree -> Bool
isText = XN.isText

-- | Tests whether a tree is relevant for validation
isRelevant :: XmlTree -> Bool
isRelevant t = (isElem t) || (isText t)

-- | Retrieve an attribute's name
getAttrName :: XmlTree -> QName
getAttrName = (fromMaybe (mkName "")) .  XN.getAttrName

-- | Retrieve an attribute's value
getAttrValue :: XmlTree -> String
getAttrValue = getCombinedText . getChildren

-- | Retrieve the concatenated text inside a node
getCombinedText :: XmlTrees -> String
getCombinedText = concat . getTexts

-- | Retrieve a list of strings from a list of nodes
getTexts :: XmlTrees -> [String]
getTexts = map getText

-- | Retrieve the text inside a node
getText :: XmlTree -> String
getText = (fromMaybe "") . XN.getText

