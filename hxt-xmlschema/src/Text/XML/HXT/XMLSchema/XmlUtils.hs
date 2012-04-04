{- |
   Module     : Text.XML.HXT.XMLSchema.XmlUtils
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

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

-- | ...
mkRoot :: XmlTrees -> XmlTrees -> XmlTree
mkRoot = XN.mkRoot

getElemName :: XmlTree -> QName
getElemName t = fromMaybe (mkName "") $ XN.getElemName t

getAttrList :: XmlTree -> [XmlTree]
getAttrList t = fromMaybe [] $ XN.getAttrl t

setAttrList :: XmlTrees -> XmlTree -> XmlTree
setAttrList = XN.setElemAttrl

getElemAttrs :: XmlTree -> [(QName, String)]
getElemAttrs t = map (\ x -> (getAttrName x, getAttrValue' x)) $ getAttrList t

getChildren' :: XmlTree -> XmlTrees
getChildren' (NTree _ c) = c

getElemChildren :: XmlTree -> XmlTrees
getElemChildren t = filter isRelevant $ getChildren' t

extractElems :: XmlTrees -> (XmlTrees, XmlTrees)
extractElems = partition isElem

isElem :: XmlTree -> Bool
isElem = XN.isElem

isText :: XmlTree -> Bool
isText = XN.isText

isRelevant :: XmlTree -> Bool
isRelevant t = (isElem t) || (isText t)

getAttrName :: XmlTree -> QName
getAttrName t = fromMaybe (mkName "") $  XN.getAttrName t

getAttrValue' :: XmlTree -> String
getAttrValue' t = getCombinedText $ getChildren' t

getCombinedText :: XmlTrees -> String
getCombinedText t = concat $ getTexts t

getTexts :: XmlTrees -> [String]
getTexts = map getText

getText :: XmlTree -> String
getText t = fromMaybe "" $ XN.getText t

