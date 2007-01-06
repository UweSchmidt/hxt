-- |
-- Navigable tree structure which allow a program to traverse
-- for XPath expressions
-- copied and modified from HXML (<http://www.flightlab.com/~joe/hxml/>)
--

module Text.XML.HXT.XPath.NavTree
    ( module Text.XML.HXT.XPath.NavTree
    , module Data.NavTree
    )
where

import Data.NavTree

import Text.XML.HXT.DOM.XmlTreeTypes
    ( XNode(..) ) --  QName(..) not yet used

import Text.XML.HXT.DOM.XmlTreeFunctions
    ( isRootNode, namespaceOf )

import Text.XML.HXT.DOM.XmlKeywords
   ( xmlnsNamespace )

import Data.Maybe
	
-- -----------------------------------------------------------------------------
-- functions for representing XPath axes. All axes except the namespace-axis are supported


parentAxis		:: NavTree a -> [NavTree a]
parentAxis		= maybeToList . upNT

ancestorAxis		:: NavTree a -> [NavTree a]
ancestorAxis 		= \(NT _ u _ _) -> u		-- or: maybePlus upNT

ancestorOrSelfAxis	:: NavTree a -> [NavTree a]
ancestorOrSelfAxis	= \t@(NT _ u _ _) -> t:u	-- or: maybeStar upNT

childAxis		:: NavTree a -> [NavTree a]
childAxis		= maybe [] (maybeStar rightNT) . downNT

descendantAxis		:: NavTree a -> [NavTree a]
descendantAxis		= tail . preorderNT -- concatMap preorderNT . childAxis

descendantOrSelfAxis		:: NavTree a -> [NavTree a]
descendantOrSelfAxis	= preorderNT

followingSiblingAxis	:: NavTree a -> [NavTree a]
followingSiblingAxis	= maybePlus rightNT

precedingSiblingAxis	:: NavTree a -> [NavTree a]
precedingSiblingAxis	= maybePlus leftNT

selfAxis		:: NavTree a -> [NavTree a]
selfAxis		= wrap  where wrap x = [x]

followingAxis		:: NavTree a -> [NavTree a]
followingAxis		= preorderNT     `o'` followingSiblingAxis `o'` ancestorOrSelfAxis

precedingAxis		:: NavTree a -> [NavTree a]
precedingAxis		= revPreorderNT  `o'` precedingSiblingAxis `o'` ancestorOrSelfAxis


attributeAxis :: NavTree XNode -> [NavTree XNode]
attributeAxis t@(NT (NTree x@(XTag _ alFull) _) a _ _)
    | not . isRootNode $ x
	= foldr (\ attr -> ((NT attr (t:a) [] []):)) [] al
    | otherwise
        = []
    where al = filter ((/= xmlnsNamespace) . namespaceOf) alFull

attributeAxis _ = []
