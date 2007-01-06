-- |
-- Convert an XPath result set into a node set
--


module Text.XML.HXT.XPath.XPathToNodeSet
    ( xPValue2NodeSet
    , emptyNodeSet
    )
where

import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.XPath.XPathDataTypes

import Data.Maybe

-- -----------------------------------------------------------------------------
-- |
-- Convert a a XPath-value into a XmlNodeSet represented by a tree structure
--
-- The XmlNodeSet can be used to traverse a tree an process all
-- marked nodes.

xPValue2NodeSet		:: XPathValue -> XmlNodeSet
xPValue2NodeSet (XPVNode ns)
    = toNodeSet ns

xPValue2NodeSet _
    = emptyNodeSet

emptyNodeSet		:: XmlNodeSet
emptyNodeSet		= XNS False [] []

leafNodeSet		:: XmlNodeSet
leafNodeSet		= XNS True [] []

toNodeSet		:: NodeSet -> XmlNodeSet
toNodeSet
    = pathListToNodeSet . map toPath

toPath			:: NavXmlTree -> XmlNodeSet
toPath
    = upTree leafNodeSet


upTree			:: XmlNodeSet -> NavXmlTree -> XmlNodeSet
upTree ps t@(NT (NTree n _) _par left _right)
    = up (upNT t)
    where
    up (Just pt)
	| isJust . upNT $ pt		-- pt is a "real" node
	    = upTree (pix n) pt
	| otherwise			-- pt is the added root node, stop recursion
	    = ps
    up Nothing				-- never used recursion should stop earler
	= ps

    pix (XAttr qn)	= XNS False [qn] []
    pix _		= XNS False []   [(length left, ps)]

pathListToNodeSet	::[XmlNodeSet] -> XmlNodeSet
pathListToNodeSet
    = foldr mergePaths emptyNodeSet
    where
    mergePaths (XNS p1 al1 cl1) (XNS p2 al2 cl2)
	= XNS (p1 || p2) (al1 ++ al2) (mergeSubPaths cl1 cl2)

    mergeSubPaths []       sp2 = sp2
    mergeSubPaths (s1:sp1) sp2 = mergeSubPath s1 (mergeSubPaths sp1 sp2)

    mergeSubPath s1 []
	= [s1]
    mergeSubPath s1@(ix1,p1) sl@(s2@(ix2, p2) : sl')
	| ix1 < ix2	= s1 : sl
	| ix1 > ix2	= s2 : mergeSubPath s1 sl'		-- ordered insert of s1 
	| otherwise	= (ix1, mergePaths p1 p2) : sl'		-- same ix merge subpaths

-- -----------------------------------------------------------------------------
