-- |
-- This module provides functions for transforming XML documents represented as
-- XmlTree with respect to its DTD.

-- Transforming an XML document with respect to its DTD means:
--
--  - add all attributes with default values
--
--  - normalize all attribute values
--
--  - sort all attributes in lexical order
--


-- Note: Transformation should be started after validation.


-- Before the document is validated, a lookup-table is build on the basis of
-- the DTD which maps element names to their transformation functions.
-- After this initialization phase the whole document is traversed in preorder
-- and every element is transformed by the XmlFilter from the lookup-table.

-- Special namings in source code:
--
--  - nd - XDTD node
--
--  - n  - XTag node
--
-- Author : .\\artin Schmidt

module Text.XML.HXT.Validator.DocTransformation
    ( transform
    )
where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.Validator.AttributeValueValidation

import Data.List

-- |
-- Lookup-table which maps element names to their transformation functions. The
-- transformation functions are XmlFilters.

type TransEnvTable      = [TransEnv]
type TransEnv           = (ElemName, TransFct)
type ElemName           = String
type TransFct           = XmlFilter


-- ------------------------------------------------------------


-- |
-- filter for transforming the document.
--
--    * 1.parameter dtdPart :  the DTD subset (Node @DOCTYPE@) of the XmlTree
--
--    - 2.parameter doc :  the document subset of the XmlTree
--
--    - returns : a list of errors

transform :: XmlTree -> XmlTree -> XmlTrees
transform dtdPart dom
    = traverseTree transTable dom
    where
    transTable = {-# SCC "buildAllTransFcts" #-} buildAllTransformationFunctions dtdPart



-- |
-- Traverse the XmlTree in preorder.
--
--    * 1.parameter transEnv :  lookup-table which maps element names to their transformation functions
--
--    - returns : the whole transformed document

traverseTree :: TransEnvTable -> XmlFilter
traverseTree transEnv n@(NTree (XTag name _) cs)
    = replaceChildren (concatMap (traverseTree transEnv) cs) (head (transFct n))
      where
      transFct = case (lookup (qualifiedName name) transEnv) of
          Nothing -> this   -- element not in DTD, can't be transformed
          Just f  -> f

traverseTree _ n = [n]


-- |
-- Build all transformation functions.
--
--    * 1.parameter dtdPart :  the DTD subset, root node should be of type @DOCTYPE@
--
--    - returns : lookup-table which maps element names to their transformation functions

buildAllTransformationFunctions :: XmlTree -> TransEnvTable
buildAllTransformationFunctions dtdPart
    = buildTransRoot
      :
      -- construct the list of filters
      map (buildTransformationFunctions dtdNodes) (isElement $$ dtdNodes)
      where
      dtdNodes = getChildren dtdPart


-- |
-- Build a transformation function for the document root. By root @\/@
-- is meant, which is the topmost dummy created by the parser. This function is only a
-- dummy, too.
--
--    * returns : entry for the lookup-table

buildTransRoot :: TransEnv
buildTransRoot = (t_root, this)



-- |
-- Build transformation functions for an element.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    * 1.parameter nd :  element declaration for which the transformation functions are
--                    created
--
--    - returns : entry for the lookup-table

buildTransformationFunctions :: XmlTrees -> XmlTree -> TransEnv

buildTransformationFunctions dtdPart n@(NTree (XDTD ELEMENT al) _)
    = (name, transFct)
    where
    name = lookup1 a_name al
    transFct = lexicographicAttributeOrder
               `o`
               normalizeAttributeValues n dtdPart
               `o`
               setDefaultAttributeValues n dtdPart

buildTransformationFunctions _ n
    = error ("buildTransformationFunctions: illegeal parameter:\n" ++ show n)



-- ------------------------------------------------------------


-- |
-- Sort the attributes of an element in lexicographic order.
--
--    * returns : a function which takes an element (XTag), sorts its
--                  attributes in lexicographic order and returns the changed element

lexicographicAttributeOrder :: XmlFilter
lexicographicAttributeOrder
    = processAttrl sortAttrl
      where
      sortAttrl al
          = map (al !!) ixs
            where
            ns  = map nameOf al
            ixs = map snd . sort . zip ns $ [(0::Int)..]

-- |
-- Normalize attribute values.
--
--    * returns : a function which takes an element (XTag), normalizes its
--                  attribute values and returns the changed element

normalizeAttributeValues :: XmlTree -> XmlTrees -> XmlFilter
normalizeAttributeValues elemDescr@(NTree (XDTD ELEMENT _) _) dtdPart
    = processAttr normalizeAttr
      where
      elemName     = valueOfDTD a_name elemDescr
      declaredAtts = isAttlistOfElement elemName $$ dtdPart

      normalizeAttr :: XmlFilter
      normalizeAttr att
          = normalizeAttrValue (if null attDescr
                                then Nothing
                                else Just (head attDescr)) att
            where
            attDescr = filter (\ x -> (valueOfDTD a_value x) == nameOf att) declaredAtts

      normalizeAttrValue :: Maybe XmlTree -> XmlFilter
      normalizeAttrValue descr
          = modifyChildren ((modifyText (normalizeAttributeValue descr) $$) . xmlTreesToText)

normalizeAttributeValues n _
    = error ("normalizeAttributeValues: illegeal parameter:\n" ++ show n)



-- |
-- Set default attribute values if they are not set.
--
--    * returns : a function which takes an element (XTag), adds missing attribute
--                  defaults and returns the changed element

setDefaultAttributeValues :: XmlTree -> XmlTrees -> XmlFilter
setDefaultAttributeValues elemDescr@(NTree (XDTD ELEMENT _) _) dtdPart
    = seqF (map setDefault defaultAtts)
      where
                                                -- select the element name from the dtd
      elemName = valueOfDTD a_name elemDescr

      defaultAtts = ( isFixedAttrKind           -- select attributes with default values
                      `orElse`
                      isDefaultAttrKind
                    )
                    $$
                    (isAttlistOfElement elemName $$ dtdPart)

      setDefault        :: XmlTree -> XmlFilter
      setDefault attrDescr                      -- add the default attributes
          = ( addAttr attName defaultValue      -- to tag nodes with missing attributes
              `whenNot`
              hasAttr attName
            )
            `when`
            isXTag
            where
            attName      = valueOfDTD a_value   attrDescr
            defaultValue = valueOfDTD a_default attrDescr


setDefaultAttributeValues n _
    = error ("setDefaultAttributeValues: illegeal parameter:\n" ++ show n)


