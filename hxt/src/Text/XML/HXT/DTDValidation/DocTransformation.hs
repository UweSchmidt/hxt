-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DTDValidation.DocTransformation
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   This module provides functions for transforming XML documents represented as
   XmlTree with respect to its DTD.

   Transforming an XML document with respect to its DTD means:

    - add all attributes with default values

    - normalize all attribute values

    - sort all attributes in lexical order

   Note: Transformation should be started after validation.

   Before the document is validated, a lookup-table is build on the basis of
   the DTD which maps element names to their transformation functions.
   After this initialization phase the whole document is traversed in preorder
   and every element is transformed by the XmlFilter from the lookup-table.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DTDValidation.DocTransformation
    ( transform
    )
where

import Text.XML.HXT.DTDValidation.TypeDefs
import Text.XML.HXT.DTDValidation.AttributeValueValidation

import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Map as M

-- ------------------------------------------------------------

-- |
-- Lookup-table which maps element names to their transformation functions. The
-- transformation functions are XmlArrows.

type TransEnvTable      = M.Map ElemName TransFct
type ElemName           = String
type TransFct           = XmlArrow


-- ------------------------------------------------------------

-- |
-- filter for transforming the document.
--
--    * 1.parameter dtdPart :  the DTD subset (Node @DOCTYPE@) of the XmlTree
--
--    - 2.parameter doc :  the document subset of the XmlTree
--
--    - returns : a list of errors

transform :: XmlTree -> XmlArrow
transform dtdPart
    = traverseTree transTable
    where
    transTable = buildAllTransformationFunctions (runLA getChildren dtdPart)

-- |
-- Traverse the XmlTree in preorder.
--
--    * 1.parameter transEnv :  lookup-table which maps element names to their transformation functions
--
--    - returns : the whole transformed document

traverseTree :: TransEnvTable -> XmlArrow
traverseTree transEnv
    = processTopDown ( (transFct $< getName)
                       `when`
                       isElem
                     )
    where
    transFct            :: String -> XmlArrow
    transFct name       = fromMaybe this . M.lookup name $ transEnv


-- |
-- Build all transformation functions.
--
--    * 1.parameter dtdPart :  the DTD subset, root node should be of type @DOCTYPE@
--
--    - returns : lookup-table which maps element names to their transformation functions

buildAllTransformationFunctions :: XmlTrees -> TransEnvTable
buildAllTransformationFunctions dtdNodes
    = M.fromList $
      (t_root, this)
      :
      concatMap (buildTransformationFunctions dtdNodes) dtdNodes

-- |
-- Build transformation functions for an element.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    * 1.parameter nd :  element declaration for which the transformation functions are
--                    created
--
--    - returns : entry for the lookup-table

buildTransformationFunctions :: XmlTrees -> XmlTree -> [(ElemName, TransFct)]

buildTransformationFunctions dtdPart dn
    | isDTDElementNode dn       = [(name, transFct)]
    | otherwise                 = []
    where
    al          = getDTDAttributes dn
    name        = dtd_name al
    transFct    = setDefaultAttributeValues dtdPart dn
                  >>>
                  normalizeAttributeValues dtdPart dn
                  >>>
                  lexicographicAttributeOrder

-- ------------------------------------------------------------

-- |
-- Sort the attributes of an element in lexicographic order.
--
--    * returns : a function which takes an element (XTag), sorts its
--                  attributes in lexicographic order and returns the changed element

lexicographicAttributeOrder :: XmlArrow
lexicographicAttributeOrder
    = setAttrl (getAttrl >>. sortAttrl)
      where
      sortAttrl         :: XmlTrees -> XmlTrees
      sortAttrl         = sortBy (comparing nameOfAttr)

-- |
-- Normalize attribute values.
--
--    * returns : a function which takes an element (XTag), normalizes its
--                  attribute values and returns the changed element

normalizeAttributeValues :: XmlTrees -> XmlTree -> XmlArrow
normalizeAttributeValues dtdPart dn
    | isDTDElementNode dn       = processAttrl (normalizeAttr $< getName)
    | otherwise                 = this
    where
    al           = getDTDAttributes dn
    elemName     = dtd_name al
    declaredAtts = isAttlistOfElement elemName $$ dtdPart

    normalizeAttr :: String -> XmlArrow
    normalizeAttr nameOfAtt
        = normalizeAttrValue ( if null attDescr
                               then Nothing
                               else Just (head attDescr)
                             )
          where
          attDescr = filter ((== nameOfAtt) . valueOfDTD a_value) declaredAtts

    normalizeAttrValue :: Maybe XmlTree -> XmlArrow
    normalizeAttrValue descr
        = replaceChildren ((xshow getChildren >>^ normalizeAttributeValue descr) >>> mkText)

-- |
-- Set default attribute values if they are not set.
--
--    * returns : a function which takes an element (XTag), adds missing attribute
--                  defaults and returns the changed element

setDefaultAttributeValues :: XmlTrees -> XmlTree -> XmlArrow
setDefaultAttributeValues dtdPart dn
    | isDTDElementNode dn       = seqA (map setDefault defaultAtts)
    | otherwise                 = this
    where
    elemName    = dtd_name . getDTDAttributes $ dn
    defaultAtts = ( isAttlistOfElement elemName
                    >>>
                    ( isFixedAttrKind           -- select attributes with default values
                      `orElse`
                      isDefaultAttrKind
                    )
                  ) $$ dtdPart

    setDefault  :: XmlTree -> XmlArrow
    setDefault attrDescr                        -- add the default attributes
          = ( addAttr attName defaultValue      -- to tag nodes with missing attributes
              `whenNot`
              hasAttr attName
            )
            `when`
            isElem
        where
        al              = getDTDAttributes attrDescr
        attName         = dtd_value   al
        defaultValue    = dtd_default al

-- ------------------------------------------------------------

