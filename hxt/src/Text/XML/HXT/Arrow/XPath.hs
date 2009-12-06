-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XPath
   Copyright  : Copyright (C) 2006 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Arrows for working with XPath and XmlNodeSets.

   Most of the XPath arrows come in two versions,
   one without dealing with namespaces, element and attribute names
   in XPath expressions are taken as they ar ignoring any prefix:localname structure.

   The second variant uses a namespace environment for associating the right
   namespace for the appropriate prefix. an entry for the empty prefix
   defines the default namespace for the expression.

   The second variant should be used, when in the application namespaces
   are significant, that means when namespace propagation is done for
   the documents to be processed.

   NodeSets are sets of \"pointer\" into an XML tree to reference
   subnodes. These nodesets can be computed by XPath expressions or
   normal arrows. The nodesets can then be used later for selecting
   or modifying subtrees of this tree in an efficient way.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XPath
    ( getXPathTreesInDoc
    , getXPathTreesInDocWithNsEnv
    , getXPathTrees
    , getXPathTreesWithNsEnv
    , getElemNodeSet
    , getElemAndAttrNodeSet
    , getXPathNodeSet
    , getFromNodeSet
    , processXPathTrees
    , processXPathTreesWithNsEnv
    , processFromNodeSet
    )
where

import qualified Text.XML.HXT.XPath as PT
    ( getXPathSubTreesWithNsEnv
    , getXPathNodeSetWithNsEnv
    )

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow

import Text.XML.HXT.Arrow.Edit
    ( canonicalizeForXPath
    )

-- ------------------------------------------------------------

-- |
-- Select parts of a whole XML document with root node by a XPath expression.
--
-- The main filter for selecting parts of a document via XPath.
--
-- The string argument must be a XPath expression with an absolute location path,
-- the argument tree must be a complete document tree.
--
-- Before evaluating the xpath query, the document is canonicalized
-- with 'Text.XML.HXT.Arrow.Edit.canonicalizeForXPath'
--
-- Result is a possibly empty list of XmlTrees forming the set of selected XPath values.
-- XPath values other than XmlTrees (numbers, attributes, tagnames, ...)
-- are convertet to text nodes.

getXPathTreesInDoc			:: ArrowXml a => String -> a XmlTree XmlTree
getXPathTreesInDoc			= getXPathTreesInDocWithNsEnv []

-- | Same as 'getXPathTreesInDoc' but with namespace environment for the XPath names

getXPathTreesInDocWithNsEnv		:: ArrowXml a => Attributes -> String -> a XmlTree XmlTree
getXPathTreesInDocWithNsEnv env query	= canonicalizeForXPath
					  >>>
					  arrL (PT.getXPathSubTreesWithNsEnv env query)

-- |
-- Select parts of an arbitrary XML tree by a XPath expression.
--
-- The main filter for selecting parts of an arbitrary XML tree via XPath.
-- The string argument must be a XPath expression with an absolute location path,
-- There are no restrictions on the argument tree.
--
-- No canonicalization is performed before evaluating the query
--
-- Result is a possibly empty list of XmlTrees forming the set of selected XPath values.
-- XPath values other than XmlTrees (numbers, attributes, tagnames, ...)
-- are convertet to text nodes.

getXPathTrees				:: ArrowXml a => String -> a XmlTree XmlTree
getXPathTrees				= getXPathTreesWithNsEnv []

-- | Same as 'getXPathTrees' but with namespace environment for the XPath names

getXPathTreesWithNsEnv			:: ArrowXml a => Attributes -> String -> a XmlTree XmlTree
getXPathTreesWithNsEnv env query	= arrL (PT.getXPathSubTreesWithNsEnv env query)

-- | Select a set of nodes via an XPath expression from an arbitray XML tree
--
-- The result is a set of \"pointers\" to nodes. This set can be used to
-- access or modify the values of the subnodes in subsequent calls to 'getFromNodeSet' or 'processFromNodeSet'.
--
-- This function enables for parsing an XPath expressions and traversing the tree for node selection once
-- and reuse this result possibly many times for later selection and modification operations.

getXPathNodeSet				:: ArrowXml a => String -> a XmlTree XmlNodeSet
getXPathNodeSet				= getXPathNodeSetWithNsEnv []

-- | Same as 'getXPathNodeSet' but with namespace environment for the XPath names

getXPathNodeSetWithNsEnv		:: ArrowXml a => Attributes -> String -> a XmlTree XmlNodeSet
getXPathNodeSetWithNsEnv nsEnv query	= arr (PT.getXPathNodeSetWithNsEnv nsEnv query)

-- ------------------------------------------------------------

getNodeSet	:: ArrowXml a => a XmlTree QName -> a XmlTree XmlTree -> a XmlTree XmlNodeSet
getNodeSet af f
    = ( ( listA ( getChildren
		  >>>
		  getNodeSet af f
		)
	  >>>
	  arr filterNodeSet
	)
	&&&
	listA af
	&&&
	listA f
      )
      >>^ (\ ~(cl, (al, n)) -> XNS (not . null $ n) al cl)
    where
    filterNodeSet	:: [XmlNodeSet] -> ChildNodes
    filterNodeSet
	= concat . zipWith filterIx [0..]

    filterIx	:: Int -> XmlNodeSet -> ChildNodes
    filterIx _ix (XNS False [] [])
	= []
    filterIx ix ps
	= [(ix, ps)]

-- |
-- compute a node set from a tree, containing all nodes selected by the predicate arrow
--
-- computation of the set of element nodes with name \"a\" is done with
--
-- > getElemNodeSet (hasName "a")

getElemNodeSet		:: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlNodeSet
getElemNodeSet f
    = getNodeSet none f

-- |
-- compute a node set from a tree, containing all nodes including attribute nodes
-- elected by the predicate arrow

getElemAndAttrNodeSet	:: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlNodeSet
getElemAndAttrNodeSet f
    = getNodeSet ( getAttrl
		   >>>
		   ( f `guards` getAttrName )
		 ) f

-- ------------------------------------------------------------

-- |
-- select all subtrees specified by a previously computed node set
--
-- the following law holds:
--
-- > getFromNodeSet $< getElemNodeSet f == multi f

getFromNodeSet		:: ArrowXml a => XmlNodeSet -> a XmlTree XmlTree
getFromNodeSet (XNS t al cl)
    = fromLA $
      ( if t then this else none )
      <+>
      ( getAttrl >>> getFromAttrl al )
      <+>
      ( getFromChildren (0-1) cl $< listA getChildren )
    where

    getFromAttrl	:: [QName] -> LA XmlTree XmlTree
    getFromAttrl l
	= ( catA . map hasQName $ l)
	  `guards`
	  this

    getFromChildren	:: Int -> ChildNodes -> XmlTrees -> LA XmlTree XmlTree
    getFromChildren _ [] _
	= none

    getFromChildren i' ((i, sp) : sps) ts
	= ( arrL (const t') >>> getFromNodeSet sp )
	  <+>
	  getFromChildren i sps ts'
	  where
	  (t', ts') = splitAt 1 . drop (i-i'-1) $ ts

-- ------------------------------------------------------------

-- |
-- process all subtrees selected by an XPath expression
--
-- the following law holds:
--
-- > processXPathTrees p xpathExpr == processFromNodeSet p $< getXPathNodeSet xpathExpr


processXPathTrees		:: ArrowXml a => a XmlTree XmlTree  -> String -> a XmlTree XmlTree
processXPathTrees f		= processXPathTreesWithNsEnv f []

-- | Same as 'processXPathTrees' but with namespace environment for the XPath names

processXPathTreesWithNsEnv	:: ArrowXml a => a XmlTree XmlTree  -> Attributes -> String -> a XmlTree XmlTree
processXPathTreesWithNsEnv f nsEnv query
    = choiceA
      [ isRoot :-> processChildren pns
      , this   :-> pns
      ]
    where
    pns = processFromNodeSet f $< getXPathNodeSetWithNsEnv nsEnv query

-- |
-- process all subtrees specified by a previously computed node set in bottom up manner
--
-- the follwoing law holds:
--
-- > processFromNodeSet g $< getElemNodeSet f == processBottomUp (g `when` f)
--
-- when attributes are contained in the node set (see 'getElemAndAttrNodeSet'), these are processed
-- after the children and before the node itself
--
-- the advantage of processFromNodeSet is the separation of the selection of set of nodes to be processed (e.g. modified)
-- from the real proccessing. The selection sometimes can be done once, the processing possibly many times.

processFromNodeSet	:: ArrowXml a => a XmlTree XmlTree  -> XmlNodeSet -> a XmlTree XmlTree
processFromNodeSet f (XNS t al cl)
    = ( if null cl
	then this
	else replaceChildren ( processC (0-1) cl $< listA getChildren )
      )
      >>>
      ( if null al
	then this
	else processAttrl (processA al)
      )
      >>>
      ( if not t
	then this
	else f
      )
    where

    -- processA		:: ChildNodes -> a XmlTree XmlTree
    processA l
	= f `when` ( catA . map hasQName $ l)

    -- processC		:: ChildNodes -> XmlTrees -> a XmlTree XmlTree
    processC _ [] ts
	= arrL (const ts)

    processC i' ((i, sp) : sps) ts
	= arrL (const ts1)
	  <+>
	  ( arrL (const ti) >>> processFromNodeSet f sp)
	  <+>
	  processC i sps ts21
	  where
	  (ts1, ts2) = splitAt (i-i'-1) ts
	  (ti, ts21) = splitAt 1 ts2

-- ------------------------------------------------------------
