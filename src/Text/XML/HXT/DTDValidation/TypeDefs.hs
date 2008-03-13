-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DTDValidation.TypeDefs
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   This module provides all datatypes for DTD validation

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DTDValidation.TypeDefs
    ( module Text.XML.HXT.DTDValidation.TypeDefs
    , module Text.XML.HXT.Arrow.DOMInterface
    , module Text.XML.HXT.Arrow.XmlArrow
    , module Control.Arrow
    , module Control.Arrow.ArrowList
    , module Control.Arrow.ArrowIf
    , module Control.Arrow.ArrowState
    , module Control.Arrow.ArrowTree
    , module Control.Arrow.ListArrow
    , module Control.Arrow.StateListArrow
    )
where

import Control.Arrow			-- classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowState
import Control.Arrow.ArrowTree

import Control.Arrow.ListArrow		-- arrow types
import Control.Arrow.StateListArrow

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow

import qualified
       Text.XML.HXT.Arrow.XmlNode as XN

-- ------------------------------------------------------------

infixr 0 $$

type XmlArrow	= LA XmlTree XmlTree
type XmlArrowS	= LA XmlTree XmlTrees

-- ------------------------------------------------------------

dtd_name
 , dtd_value
 , dtd_type
 , dtd_kind
 , dtd_modifier
 , dtd_default	:: Attributes -> String

dtd_name	= lookup1 a_name
dtd_value	= lookup1 a_value
dtd_type	= lookup1 a_type
dtd_kind	= lookup1 a_kind
dtd_modifier	= lookup1 a_modifier
dtd_default	= lookup1 a_default

-- ------------------------------------------------------------

isUnparsedEntity	:: ArrowDTD a => a XmlTree XmlTree
isUnparsedEntity	= filterA $
			  getDTDAttrl >>> isA (hasEntry k_ndata)

hasDTDAttrValue		:: ArrowDTD a => String -> (String -> Bool) -> a XmlTree XmlTree
hasDTDAttrValue	an p	= filterA $
			  getDTDAttrl >>> isA (p . lookup1 an)

isRequiredAttrKind	:: ArrowDTD a => a XmlTree XmlTree
isRequiredAttrKind	= hasDTDAttrValue a_kind (== k_required)

isDefaultAttrKind	:: ArrowDTD a => a XmlTree XmlTree
isDefaultAttrKind	= hasDTDAttrValue a_kind (== k_default)

isFixedAttrKind		:: ArrowDTD a => a XmlTree XmlTree
isFixedAttrKind		= hasDTDAttrValue a_kind (== k_fixed)

isMixedContentElement	:: ArrowDTD a => a XmlTree XmlTree
isMixedContentElement	= hasDTDAttrValue a_type (== v_mixed)

isEmptyElement		:: ArrowDTD a => a XmlTree XmlTree
isEmptyElement		= hasDTDAttrValue a_type (== k_empty)

isEnumAttrType		:: ArrowDTD a => a XmlTree XmlTree
isEnumAttrType		= hasDTDAttrValue a_type (== k_enumeration)

isIdAttrType		:: ArrowDTD a => a XmlTree XmlTree
isIdAttrType		= hasDTDAttrValue a_type (== k_id)

isIdRefAttrType		:: ArrowDTD a => a XmlTree XmlTree
isIdRefAttrType		= hasDTDAttrValue a_type (`elem` [k_idref, k_idrefs])

isNotationAttrType	:: ArrowDTD a => a XmlTree XmlTree
isNotationAttrType	= hasDTDAttrValue a_type (== k_notation)

isAttlistOfElement	:: ArrowDTD a => String -> a XmlTree XmlTree
isAttlistOfElement el	= isDTDAttlist
			  >>>
			  hasDTDAttrValue a_name (== el)

valueOfDTD		:: String -> XmlTree -> String
valueOfDTD n		= concat . runLA ( getDTDAttrl >>^ lookup1 n )

valueOf			:: String -> XmlTree -> String
valueOf n		= concat . runLA ( getAttrValue n )

getDTDAttributes	:: XmlTree -> Attributes
getDTDAttributes	= concat . runLA getDTDAttrl

isDTDDoctypeNode	:: XmlTree -> Bool
isDTDDoctypeNode	= not . null . runLA isDTDDoctype

isDTDElementNode	:: XmlTree -> Bool
isDTDElementNode	= not . null . runLA isDTDElement

isDTDAttlistNode	:: XmlTree -> Bool
isDTDAttlistNode	= not . null . runLA isDTDAttlist

isDTDContentNode	:: XmlTree -> Bool
isDTDContentNode	= not . null . runLA isDTDContent

isDTDNameNode		:: XmlTree -> Bool
isDTDNameNode		= not . null . runLA isDTDName

isElemNode		:: XmlTree -> Bool
isElemNode		= not . null . runLA isElem

nameOfAttr		:: XmlTree -> String
nameOfAttr		= concat . runLA (getAttrName >>^ qualifiedName)

nameOfElem		:: XmlTree -> String
nameOfElem		= concat . runLA (getElemName >>^ qualifiedName)

-- |
-- infix operator for applying an arrow to a list of trees
--
--    * 1.parameter f :  the arrow
--
--    - 2.parameter ts :  the list of trees
--
--    - returns : list of results

($$)		:: XmlArrow -> XmlTrees -> XmlTrees
f $$ l		= runLA (unlistA >>> f) l

-- | create an error message

msgToErr	:: (String -> String) -> LA String XmlTree
msgToErr f	= mkErr $< this
		  where
		  mkErr "" = none
		  mkErr s  = err (f s)


-- ------------------------------------------------------------
