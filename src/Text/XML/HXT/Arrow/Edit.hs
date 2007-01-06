-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.Edit
   Copyright  : Copyright (C) 2006 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Edit.hs,v 1.8 2006/11/12 14:52:59 hxml Exp $

   common edit arrows

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.Edit
    ( canonicalizeAllNodes
    , canonicalizeForXPath
    , canonicalizeContents
    , collapseAllXText
    , collapseXText

    , escapeXmlDoc
    , escapeHtmlDoc

    , haskellRepOfXmlDoc
    , treeRepOfXmlDoc
    , addHeadlineToXmlDoc

    , indentDoc
    , numberLinesInXmlDoc
 
    , removeComment
    , removeAllComment
    , removeWhiteSpace
    , removeAllWhiteSpace
    , removeDocWhiteSpace

    , transfCdata
    , transfAllCdata
    , transfCharRef
    , transfAllCharRef

    , hasXmlPi
    , addXmlPi
    , addXmlPiEncoding

    , addDoctypeDecl
    , addXHtmlDoctypeStrict
    , addXHtmlDoctypeTransitional
    , addXHtmlDoctypeFrameset
    )
where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import Control.Arrow.ListArrow

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow

import qualified Text.XML.HXT.Arrow.XmlNode as XN

import Text.XML.HXT.DOM.FormatXmlTree
    ( formatXmlTree )

import qualified Text.XML.HXT.DOM.EditFilters as EF
    ( indentDoc
    , removeDocWhiteSpace
    )

import Text.XML.HXT.Parser.XmlParsec
    ( xmlEntities )

import Text.XML.HXT.Parser.HtmlParsec
    ( xhtmlEntities )

import Data.Maybe

import qualified Data.Map as M

-- ------------------------------------------------------------

-- |
-- Applies some "Canonical XML" rules to a document tree.
--
-- The rule differ slightly for canonical XML and XPath in handling of comments
--
-- Note: This is not the whole canonicalization as it is specified by the W3C
-- Recommendation. Adding attribute defaults or sorting attributes in lexicographic
-- order is done by the @transform@ function of module @Text.XML.HXT.Validator.Validation@.
-- Replacing entities or line feed normalization is done by the parser.
--
--
-- Not implemented yet:
--
--  - Whitespace within start and end tags is normalized
--
--  - Special characters in attribute values and character content are replaced by character references
--
-- see 'canonicalizeAllNodes' and 'canonicalizeForXPath'

canonicalizeTree'	:: LA XmlTree XmlTree -> LA XmlTree XmlTree
canonicalizeTree' toBeRemoved
    = processChildren (none `when` isText)
      >>>
      processBottomUp canonicalize1Node
      where
      canonicalize1Node	:: LA XmlTree XmlTree
      canonicalize1Node
	  = (deep isPi `when` isDTD)		-- remove DTD parts, except PIs
	    >>>
	    (none `when` toBeRemoved)		-- remove unintersting nodes
	    >>>
	    ( processAttrl ( processChildren transfCharRef
			     >>>
			     collapseXText
			   )
	      `when` isElem
	    )
	    >>>
	    transfCdata				-- CDATA -> text
	    >>>
	    transfCharRef			-- Char refs -> text
	    >>>
	    collapseXText			-- combine text


-- |
-- Applies some "Canonical XML" rules to a document tree.
--
-- The rule differ slightly for canonical XML and XPath in handling of comments
--
-- Note: This is not the whole canonicalization as it is specified by the W3C
-- Recommendation. Adding attribute defaults or sorting attributes in lexicographic
-- order is done by the @transform@ function of module @Text.XML.HXT.Validator.Validation@.
-- Replacing entities or line feed normalization is done by the parser.
--
-- Rules: remove DTD parts, processing instructions, comments and substitute char refs in attribute
-- values and text
--
-- Not implemented yet:
--
--  - Whitespace within start and end tags is normalized
--
--  - Special characters in attribute values and character content are replaced by character references

canonicalizeAllNodes	:: ArrowList a => a XmlTree XmlTree
canonicalizeAllNodes
    = fromLA $
      canonicalizeTree' ( isCmt		-- remove comment
			  <+>
			  isXmlPi	-- remove xml declaration
			)

-- |
-- Canonicalize a tree for XPath
-- Like 'canonicalizeAllNodes' but comment nodes are not removed
--
-- see 'canonicalizeAllNodes'

canonicalizeForXPath	:: ArrowList a => a XmlTree XmlTree
canonicalizeForXPath
    = fromLA $
      canonicalizeTree' isXmlPi

-- |
-- Canonicalize the contents of a document
--
-- substitutes all char refs in text and attribute values,
-- removes CDATA section and combines all sequences of resulting text
-- nodes into a single text node
--
-- see 'canonicalizeAllNodes'

canonicalizeContents	:: ArrowList a => a XmlTree XmlTree
canonicalizeContents
    = fromLA $
      processBottomUp canonicalize1Node
      where
      canonicalize1Node	:: LA XmlTree XmlTree
      canonicalize1Node
	  = ( processAttrl ( processChildren transfCharRef
			     >>>
			     collapseXText
			   )
	      `when` isElem
	    )
	    >>>
	    transfCdata				-- CDATA -> text
	    >>>
	    transfCharRef			-- Char refs -> text
	    >>>
	    collapseXText			-- combine text

-- ------------------------------------------------------------

collapseXText'		:: LA XmlTree XmlTree
collapseXText'
    = replaceChildren ( listA getChildren >>> arrL (foldr mergeText' []) )
    where
    mergeText'	:: XmlTree -> XmlTrees -> XmlTrees
    mergeText' t1 (t2 : ts2)
	| XN.isText t1 && XN.isText t2
	    = let
	      s1 = fromJust . XN.getText $ t1
	      s2 = fromJust . XN.getText $ t2
	      t  = XN.mkText (s1 ++ s2)
	      in
	      t : ts2
    mergeText' t1 ts
	= t1 : ts

-- |
-- Collects sequences of text nodes in the list of children of a node into one single text node.
-- This is useful, e.g. after char and entity reference substitution

collapseXText		:: ArrowList a => a XmlTree XmlTree
collapseXText
    = fromLA $
      collapseXText'

-- |
-- Applies collapseXText recursively.
--
--
-- see also : 'collapseXText'

collapseAllXText	:: ArrowList a => a XmlTree XmlTree
collapseAllXText
    = fromLA $
      processBottomUp collapseXText'

-- ------------------------------------------------------------

-- |
-- escape XmlText,
-- transform all special XML chars into char- or enitity- refs

type EntityRefTable	= M.Map Int String

xmlEntityRefTable
 , xhtmlEntityRefTable	:: EntityRefTable

xmlEntityRefTable   = buildEntityRefTable $ xmlEntities
xhtmlEntityRefTable = buildEntityRefTable $ xhtmlEntities

buildEntityRefTable	:: [(String, Int)] -> EntityRefTable
buildEntityRefTable	= M.fromList . map (\ (x,y) -> (y,x) )

escapeText''	:: (Char -> XmlTree) -> (Char -> Bool) -> XmlTree -> XmlTrees
escapeText'' escChar isEsc t
    = maybe [t] escape . XN.getText $ t
    where
    escape []
	= []
    escape (c:s1)
	| isEsc c
	    = escChar c : escape s1
    escape s
	= XN.mkText s1 : escape s2
	where
	(s1, s2) = break isEsc s

{-
escapeCharRef	:: Char -> XmlTree
escapeCharRef	= XN.mkCharRef . fromEnum
-}

escapeEntityRef	:: EntityRefTable -> Char -> XmlTree
escapeEntityRef entityTable c
    = maybe (XN.mkCharRef c') XN.mkEntityRef . M.lookup c' $ entityTable
    where
    c' = fromEnum c

escapeXmlEntityRef	:: Char -> XmlTree
escapeXmlEntityRef	= escapeEntityRef xmlEntityRefTable

escapeHtmlEntityRef	:: Char -> XmlTree
escapeHtmlEntityRef	= escapeEntityRef xhtmlEntityRefTable

-- |
-- escape all special XML chars into XML entity references or char references
--
-- convert the special XML chars \< and & in text nodes into prefefiened XML entity references,
-- in attribute values also \', \", \>, \\n, \\r and \\t are converted into entity or char references,
-- in comments nothing is converted (see XML standard 2.4, useful e.g. for JavaScript).

escapeXmlDoc		:: ArrowList a => a XmlTree XmlTree
escapeXmlDoc
    = fromLA $ escapeDoc escXmlText escXmlAttrValue
    where
    escXmlText
	= arrL $ escapeText'' escapeXmlEntityRef (`elem` "<&")		-- no escape for ", ' and > required: XML standard 2.4
    escXmlAttrValue
        = arrL $ escapeText'' escapeXmlEntityRef (`elem` "<>\"\'&\n\r\t")


-- |
-- escape all special HTML chars into XHTML entity references or char references
--
-- convert the special XML chars \< and & and all none ASCII chars in text nodes into prefefiened XML or XHTML entity references,
-- in attribute values also \', \", \>, \\n, \\r and \\t are converted into entity or char references,
-- in comments nothing is converted

escapeHtmlDoc		:: ArrowList a => a XmlTree XmlTree
escapeHtmlDoc
    = fromLA $ escapeDoc escHtmlText escHtmlAttrValue
    where
    escHtmlText
	= arrL $ escapeText'' escapeHtmlEntityRef isHtmlTextEsc
    escHtmlAttrValue
        = arrL $ escapeText'' escapeHtmlEntityRef isHtmlAttrEsc

    isHtmlTextEsc c
	= c >= toEnum(128) || ( c `elem` "<&" )
    isHtmlAttrEsc c
	= c >= toEnum(128) || ( c `elem` "<>\"\'&\n\r\t" )

escapeDoc		:: LA XmlTree XmlTree -> LA XmlTree XmlTree -> LA XmlTree XmlTree
escapeDoc escText escAttr
    = escape
    where
    escape
	= choiceA
	  [ isElem  :-> ( processChildren escape
			  >>>
			  processAttrl escVal
			)
	  , isText  :-> escText
	  -- , isCmt   :-> escCmt
	  , isDTD   :-> processTopDown escDTD
	  , this    :-> this
	  ]
    escVal   = processChildren escAttr
    escDTD   = escVal `when` ( isDTDEntity <+> isDTDPEntity )

-- ------------------------------------------------------------

-- |
-- convert a document into a Haskell representation (with show).
--
-- Useful for debugging and trace output.
-- see also : 'treeRepOfXmlDoc', 'numberLinesInXmlDoc'

haskellRepOfXmlDoc	:: ArrowList a => a XmlTree XmlTree
haskellRepOfXmlDoc
    = fromLA $
      root [getAttrl] [show ^>> mkText]

-- |
-- convert a document into a text and add line numbers to the text representation.
-- 
-- Result is a root node with a single text node as child.
-- Useful for debugging and trace output.
-- see also : 'haskellRepOfXmlDoc', 'treeRepOfXmlDoc'

numberLinesInXmlDoc	:: ArrowList a => a XmlTree XmlTree
numberLinesInXmlDoc
    = fromLA $
      processChildren (changeText numberLines)
    where
    numberLines	:: String -> String
    numberLines str
	= concat $
	  zipWith (\ n l -> lineNr n ++ l ++ "\n") [1..] (lines str)
	where
	lineNr	 :: Int -> String
	lineNr n = (reverse (take 6 (reverse (show n) ++ replicate 6 ' '))) ++ "  "

-- |
-- convert a document into a text representation in tree form.
--
-- Useful for debugging and trace output.
-- see also : 'haskellRepOfXmlDoc', 'numberLinesInXmlDoc'

treeRepOfXmlDoc	:: ArrowList a => a XmlTree XmlTree
treeRepOfXmlDoc
    = fromLA $
      root [getAttrl] [formatXmlTree ^>> mkText]

addHeadlineToXmlDoc	:: ArrowXml a => a XmlTree XmlTree
addHeadlineToXmlDoc
    = fromLA $ ( addTitle $< (getAttrValue a_source >>^ formatTitle) )
    where
    addTitle str
	= replaceChildren ( txt str <+> getChildren <+> txt "\n" )
    formatTitle str
	= "\n" ++ headline ++ "\n" ++ underline ++ "\n\n"
	where
	headline  = "content of: " ++ str
        underline = map (const '=') headline

-- ------------------------------------------------------------

removeComment'		:: LA XmlTree XmlTree
removeComment'		= none `when` isCmt

-- |
-- remove Comments: @none `when` isCmt@

removeComment		:: ArrowXml a => a XmlTree XmlTree
removeComment		= fromLA $ removeComment'

-- |
-- remove all comments recursively

removeAllComment	:: ArrowXml a => a XmlTree XmlTree
removeAllComment	= fromLA $ processBottomUp removeComment'

-- ----------

removeWhiteSpace'	:: LA XmlTree XmlTree
removeWhiteSpace'	= none `when` hasText (all (`elem` " \t\n"))

-- |
-- simple filter for removing whitespace.
--
-- no check on sigificant whitespace, e.g. in HTML \<pre\>-elements, is done.
--
--
-- see also : 'removeAllWhiteSpace', 'removeDocWhiteSpace'

removeWhiteSpace	:: ArrowXml a => a XmlTree XmlTree
removeWhiteSpace	= fromLA $ removeWhiteSpace'

-- |
-- simple recursive filter for removing all whitespace.
--
-- removes all text nodes in a tree that consist only of whitespace.
--
--
-- see also : 'removeWhiteSpace', 'removeDocWhiteSpace'

removeAllWhiteSpace	:: ArrowXml a => a XmlTree XmlTree
removeAllWhiteSpace	= fromLA $ processBottomUp removeWhiteSpace'

-- |
-- filter for removing all not significant whitespace.
--
-- the tree traversed for removing whitespace between elements,
-- that was inserted for indentation and readability.
-- whitespace is only removed at places, where it's not significat
-- preserving whitespace may be controlled in a document tree
-- by a tag attribute @xml:space@
--
-- allowed values for this attribute are @default | preserve@
--
-- input is root node of the document to be cleaned up,
-- output the semantically equivalent simplified tree
--
--
-- see also : 'indentDoc', 'removeAllWhiteSpace'

removeDocWhiteSpace	:: ArrowXml a => a XmlTree XmlTree
removeDocWhiteSpace	= arrL EF.removeDocWhiteSpace

-- |
-- filter for indenting a document tree for pretty printing.
--
-- the tree is traversed for inserting whitespace for tag indentation.
--
-- whitespace is only inserted or changed at places, where it isn't significant,
-- is's not inserted between tags and text containing non whitespace chars.
--
-- whitespace is only inserted or changed at places, where it's not significant.
-- preserving whitespace may be controlled in a document tree
-- by a tag attribute @xml:space@
--
-- allowed values for this attribute are @default | preserve@.
--
-- input is a complete document tree.
-- result the semantically equivalent formatted tree.
--
--
-- see also : 'removeDocWhiteSpace'

indentDoc		:: ArrowList a => a XmlTree XmlTree
indentDoc		= arrL EF.indentDoc

-- ------------------------------------------------------------

transfCdata'		:: LA XmlTree XmlTree
transfCdata'		= (getCdata >>> mkText) `when` isCdata

-- |
-- converts a CDATA section node into a normal text node

transfCdata		:: ArrowXml a => a XmlTree XmlTree
transfCdata		= fromLA $
			  transfCdata'

-- |
-- converts CDATA sections in whole document tree into normal text nodes

transfAllCdata		:: ArrowXml a => a XmlTree XmlTree
transfAllCdata		= fromLA $
			  processBottomUp transfCdata'
			  
--

transfCharRef'		:: LA XmlTree XmlTree
transfCharRef'		= ( getCharRef >>> arr (\ i -> [toEnum i]) >>> mkText )
		          `when`
			  isCharRef

-- |
-- converts character references to normal text

transfCharRef		:: ArrowXml a => a XmlTree XmlTree
transfCharRef		= fromLA $
			  transfCharRef'

-- |
-- recursively converts all character references to normal text

transfAllCharRef	:: ArrowXml a => a XmlTree XmlTree
transfAllCharRef	= fromLA $
			  processBottomUp transfCharRef'

-- ------------------------------------------------------------

hasXmlPi		:: ArrowXml a => a XmlTree XmlTree
hasXmlPi
    = fromLA
      ( getChildren
	>>>
	isPi
	>>>
	hasName t_xml
      )

-- | add an \<?xml version=\"1.0\"?\> processing instruction
-- if it's not already there

addXmlPi		:: ArrowXml a => a XmlTree XmlTree
addXmlPi
    = fromLA
      ( insertChildrenAt 0 ( ( mkPi (mkSNsName t_xml) none
			       >>>
			       addAttr a_version "1.0"
			     )
			     <+>
			     txt "\n"
			   )
	`whenNot`
	hasXmlPi
      )

-- | add an encoding spec to the \<?xml version=\"1.0\"?\> processing instruction

addXmlPiEncoding	:: ArrowXml a => String -> a XmlTree XmlTree
addXmlPiEncoding enc
    = fromLA $
      processChildren ( addAttr a_encoding enc
		        `when`
			( isPi >>> hasName t_xml )
		      )

-- | add an XHTML strict doctype declaration to a document

addXHtmlDoctypeStrict
  , addXHtmlDoctypeTransitional
  , addXHtmlDoctypeFrameset	:: ArrowXml a => a XmlTree XmlTree

-- | add an XHTML strict doctype declaration to a document

addXHtmlDoctypeStrict
    = addDoctypeDecl "html" "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"

-- | add an XHTML transitional doctype declaration to a document

addXHtmlDoctypeTransitional
    = addDoctypeDecl "html" "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"

-- | add an XHTML frameset doctype declaration to a document

addXHtmlDoctypeFrameset
    = addDoctypeDecl "html" "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"

-- | add a doctype declaration to a document
--
-- The arguments are the root element name, the PUBLIC id and the SYSTEM id

addDoctypeDecl	:: ArrowXml a => String -> String -> String -> a XmlTree XmlTree
addDoctypeDecl rootElem public system
    = fromLA $
      replaceChildren
      ( mkDTDDoctype [ (a_name, rootElem)
		     , (k_public, public)
		     , (k_system, system)
		     ] none
	<+>
	getChildren
      )

-- ------------------------------------------------------------
