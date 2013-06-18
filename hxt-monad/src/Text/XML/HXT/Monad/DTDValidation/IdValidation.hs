-- ------------------------------------------------------------

{- |
   This module provides functions for checking special ID/IDREF/IDREFS constraints.

   Checking special ID\/IDREF\/IDREFS constraints means:

    - checking that all ID values are unique.

    - checking that all IDREF\/IDREFS values match the value of some ID attribute

   ID-Validation should be started before or after validating the document.

   First all nodes with ID attributes are collected from the document, then
   it is validated that values of ID attributes do not occure more than once.
   During a second iteration over the document it is validated that there exists
   an ID attribute value for IDREF\/IDREFS attribute values.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Monad.DTDValidation.IdValidation
    ( validateIds
    )
where

import           Data.Maybe

import           Text.XML.HXT.Monad.DTDValidation.AttributeValueValidation
import           Text.XML.HXT.Monad.DTDValidation.TypeDefs

-- ------------------------------------------------------------

-- |
-- Lookup-table which maps element names to their validation functions. The
-- validation functions are XmlFilters.

type IdEnvTable         = [IdEnv]
type IdEnv              = (ElemName, IdFct)
type ElemName           = String
type IdFct              = XmlArrow

-- ------------------------------------------------------------

-- |
-- Perform the validation of the ID/IDREF/IDREFS constraints.
--
--    * 1.parameter dtdPart :  the DTD subset (Node @DOCTYPE@) of the XmlTree
--
--    - 2.parameter doc :  the document subset of the XmlTree
--
--    - returns : a list of errors

validateIds :: XmlTree -> XmlArrow
validateIds dtdPart
    = validateIds' $< listA (traverseTree idEnv)
      where
      idAttrTypes = runLA (getChildren >=> isIdAttrType) dtdPart
      elements    = runLA (getChildren >=> isDTDElement) dtdPart
      atts        = runLA (getChildren >=> isDTDAttlist) dtdPart
      idEnv       = buildIdCollectorFcts idAttrTypes

      validateIds'      :: XmlTrees -> XmlArrow
      validateIds' idNodeList
          = ( constA idNodeList >=> checkForUniqueIds idAttrTypes )
            <++>
            checkIdReferences idRefEnv
          where
          idRefEnv   = buildIdrefValidationFcts idAttrTypes elements atts idNodeList



-- |
-- Traverse the XmlTree in preorder.
--
--    * 1.parameter idEnv :  lookup-table which maps element names to their validation functions
--
--    - returns : list of errors

traverseTree :: IdEnvTable -> XmlArrow
traverseTree idEnv
    = multi (isElem `guards` (idFct $< getName))
      where
      idFct             :: String -> XmlArrow
      idFct name        = fromMaybe none . lookup name $ idEnv

-- |
-- Returns the value of an element's ID attribute. The attribute name has to be
-- retrieved first from the DTD.
--
--    * 1.parameter dtdPart :  list of ID attribute definitions from the DTD
--
--    - 2.parameter n :  element which ID attribute value should be returned
--
--    - returns : normalized value of the ID attribute

getIdValue      :: XmlTrees -> XmlTree -> String
getIdValue dns
    = concat . runLA (single getIdValue')
    where
    getIdValue' :: LA XmlTree String
    getIdValue'
        = isElem `guards` catA (map getIdVal dns)
        where
        getIdVal dn
            | isDTDAttlistNode dn       = hasName elemName
                                          `guards`
                                          ( getAttrValue0 attrName
                                            >=>
                                            return . normalizeAttributeValue (Just dn)
                                          )
            | otherwise                 = none
            where
            al       = getDTDAttributes dn
            elemName = dtd_name  al
            attrName = dtd_value al

-- ------------------------------------------------------------


-- |
-- Build collector functions which return XTag nodes with ID attributes from
-- a document.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - returns : lookup-table which maps element names to their collector function

buildIdCollectorFcts :: XmlTrees -> IdEnvTable
buildIdCollectorFcts idAttrTypes
    = concatMap buildIdCollectorFct idAttrTypes
      where
      buildIdCollectorFct :: XmlTree -> [IdEnv]
      buildIdCollectorFct dn
          | isDTDAttlistNode dn = [(elemName, hasAttr attrName)]
          | otherwise           = []
          where
          al       = getDTDAttributes dn
          elemName = dtd_name  al
          attrName = dtd_value al

-- |
-- Build validation functions for checking if IDREF\/IDREFS values match a value
-- of some ID attributes.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter idNodeList :  list of all XTag nodes with ID attributes
--
--    - returns : lookup-table which maps element names to their validation function

buildIdrefValidationFcts :: XmlTrees -> XmlTrees -> XmlTrees -> XmlTrees -> IdEnvTable
buildIdrefValidationFcts idAttrTypes elements atts idNodeList
    = concatMap buildElemValidationFct elements
      where
      idValueList = map (getIdValue idAttrTypes) idNodeList

      buildElemValidationFct :: XmlTree -> [IdEnv]
      buildElemValidationFct dn
          | isDTDElementNode dn = [(elemName, buildIdrefValidationFct idRefAttrTypes)]
          | otherwise           = []
          where
          al             = getDTDAttributes dn
          elemName       = dtd_name al
          idRefAttrTypes = (isAttlistOfElement elemName >=> isIdRefAttrType) $$ atts

      buildIdrefValidationFct :: XmlTrees -> XmlArrow
      buildIdrefValidationFct
          = catA . map buildIdref

      buildIdref        :: XmlTree -> XmlArrow
      buildIdref dn
          | isDTDAttlistNode dn = isElem >=> (checkIdref $< getName)
          | otherwise           = none
          where
          al             = getDTDAttributes dn
          attrName = dtd_value al
          attrType = dtd_type  al

          checkIdref :: String -> XmlArrow
          checkIdref name
              = hasAttr attrName
                `guards`
                ( checkIdVal $< getAttrValue attrName )
              where
              checkIdVal        :: String -> XmlArrow
              checkIdVal av
                  | attrType == k_idref
                      = checkValueDeclared attrValue
                  | null valueList
                      = err ( "Attribute " ++ show attrName ++
                              " of Element " ++ show name ++
                              " must have at least one name."
                            )
                  | otherwise
                      = catA . map checkValueDeclared $ valueList
                  where
                  valueList = words attrValue
                  attrValue = normalizeAttributeValue (Just dn) av

          checkValueDeclared :: String -> XmlArrow
          checkValueDeclared  attrValue
              = if attrValue `elem` idValueList
                then none
                else err ( "An Element with identifier " ++ show attrValue ++
                           " must appear in the document."
                         )

-- ------------------------------------------------------------


-- |
-- Validate that all ID values are unique within a document.
-- Validity constraint: ID (3.3.1 \/p. 25 in Spec)
--
--    * 1.parameter idNodeList :  list of all XTag nodes with ID attributes
--
--    - 2.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - returns : a list of errors

checkForUniqueIds :: XmlTrees -> LA XmlTrees XmlTree
checkForUniqueIds idAttrTypes            -- idNodeList
    = fromSLA [] ( unlistA
                   >=>
                   isElem
                   >=>
                   (checkForUniqueId $<< getName &=& this)
                 )
      where
      checkForUniqueId :: String -> XmlTree -> SLA [String] XmlTree XmlTree
      checkForUniqueId name x
          = ifA ( getState
                  >=>
                  isA (attrValue `elem`)
                )
            (err ( "Attribute value " ++ show attrValue ++ " of type ID for element " ++
                   show name ++ " must be unique within the document." ))
            (nextState (attrValue:) >=> none)
          where
          attrValue = getIdValue (isAttlistOfElement name $$ idAttrTypes) x

-- |
-- Validate that all IDREF\/IDREFS values match the value of some ID attribute.
-- Validity constraint: IDREF (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter idRefEnv :  lookup-table which maps element names to their validation function
--
--    - 2.parameter doc :  the document to validate
--
--    - returns : a list of errors

checkIdReferences :: IdEnvTable -> LA XmlTree XmlTree
checkIdReferences idRefEnv
    = traverseTree idRefEnv

-- ------------------------------------------------------------
