-- ------------------------------------------------------------

module Text.XML.HXT.Monad.DTDValidation.TypeDefs
    ( module Text.XML.HXT.Monad.DTDValidation.TypeDefs
    , module Text.XML.HXT.DOM.Interface
    , module Text.XML.HXT.Monad.ArrowXml
    , module Control.Monad.Arrow
    , module Data.Sequence.ArrowTypes
    )
where

import           Control.Monad.Arrow
import           Data.Sequence.ArrowTypes
import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.Monad.ArrowXml

-- ------------------------------------------------------------

infixr 0 $$

type XmlArrow   = LA XmlTree XmlTree
type XmlArrowS  = LA XmlTree XmlTrees

-- ------------------------------------------------------------

dtd_name
 , dtd_value
 , dtd_type
 , dtd_kind
 , dtd_modifier
 , dtd_default  :: Attributes -> String

dtd_name        = lookup1 a_name
dtd_value       = lookup1 a_value
dtd_type        = lookup1 a_type
dtd_kind        = lookup1 a_kind
dtd_modifier    = lookup1 a_modifier
dtd_default     = lookup1 a_default

-- ------------------------------------------------------------

isUnparsedEntity        :: MonadSeq m => XmlTree -> m XmlTree
isUnparsedEntity        = filterA $
                          getDTDAttrl >=> isA (hasEntry k_ndata)

hasDTDAttrValue         :: MonadSeq m => String -> (String -> Bool) -> XmlTree -> m XmlTree
hasDTDAttrValue an p    = filterA $
                          getDTDAttrl >=> isA (p . lookup1 an)

isRequiredAttrKind      :: MonadSeq m => XmlTree -> m XmlTree
isRequiredAttrKind      = hasDTDAttrValue a_kind (== k_required)

isDefaultAttrKind       :: MonadSeq m => XmlTree -> m XmlTree
isDefaultAttrKind       = hasDTDAttrValue a_kind (== k_default)

isFixedAttrKind         :: MonadSeq m => XmlTree -> m XmlTree
isFixedAttrKind         = hasDTDAttrValue a_kind (== k_fixed)

isMixedContentElement   :: MonadSeq m => XmlTree -> m XmlTree
isMixedContentElement   = hasDTDAttrValue a_type (== v_mixed)

isEmptyElement          :: MonadSeq m => XmlTree -> m XmlTree
isEmptyElement          = hasDTDAttrValue a_type (== k_empty)

isEnumAttrType          :: MonadSeq m => XmlTree -> m XmlTree
isEnumAttrType          = hasDTDAttrValue a_type (== k_enumeration)

isIdAttrType            :: MonadSeq m => XmlTree -> m XmlTree
isIdAttrType            = hasDTDAttrValue a_type (== k_id)

isIdRefAttrType         :: MonadSeq m => XmlTree -> m XmlTree
isIdRefAttrType         = hasDTDAttrValue a_type (`elem` [k_idref, k_idrefs])

isNotationAttrType      :: MonadSeq m => XmlTree -> m XmlTree
isNotationAttrType      = hasDTDAttrValue a_type (== k_notation)

isAttlistOfElement      :: MonadSeq m => String -> XmlTree -> m XmlTree
isAttlistOfElement el   = isDTDAttlist
                          >=>
                          hasDTDAttrValue a_name (== el)

valueOfDTD              :: String -> XmlTree -> String
valueOfDTD n            = concat . runLA ( getDTDAttrl >=^ lookup1 n )

valueOf                 :: String -> XmlTree -> String
valueOf n               = concat . runLA ( getAttrValue n )

getDTDAttributes        :: XmlTree -> Attributes
getDTDAttributes        = concat . runLA getDTDAttrl

isDTDDoctypeNode        :: XmlTree -> Bool
isDTDDoctypeNode        = not . null . runLA isDTDDoctype

isDTDElementNode        :: XmlTree -> Bool
isDTDElementNode        = not . null . runLA isDTDElement

isDTDAttlistNode        :: XmlTree -> Bool
isDTDAttlistNode        = not . null . runLA isDTDAttlist

isDTDContentNode        :: XmlTree -> Bool
isDTDContentNode        = not . null . runLA isDTDContent

isDTDNameNode           :: XmlTree -> Bool
isDTDNameNode           = not . null . runLA isDTDName

isElemNode              :: XmlTree -> Bool
isElemNode              = not . null . runLA isElem

nameOfAttr              :: XmlTree -> String
nameOfAttr              = concat . runLA (getAttrName >=^ qualifiedName)

nameOfElem              :: XmlTree -> String
nameOfElem              = concat . runLA (getElemName >=^ qualifiedName)

-- |
-- infix operator for applying an arrow to a list of trees
--
--    * 1.parameter f :  the arrow
--
--    - 2.parameter ts :  the list of trees
--
--    - returns : list of results

($$)            :: XmlArrow -> XmlTrees -> XmlTrees
f $$ l          = runLA (unlistA >=> f) l

-- | create an error message

msgToErr        :: (String -> String) -> LA String XmlTree
msgToErr f      = mkErr $< this
                  where
                  mkErr "" = none
                  mkErr s  = err (f s)


-- ------------------------------------------------------------
