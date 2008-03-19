-- |
-- This module provides functions for checking special ID/IDREF/IDREFS constraints.

-- Checking special ID/IDREF/IDREFS constraints means:
--
--  - checking that all ID values are unique.
--
--  - checking that all IDREF/IDREFS values match the value of some ID attribute
--


-- ID-Validation should be started before or after validating the document.


-- First all nodes with ID attributes are collected from the document, then
-- it is validated that values of ID attributes do not occure more than once.
-- During a second iteration over the document it is validated that there exists
-- an ID attribute value for IDREF/IDREFS attribute values.

-- Special namings in source code:
--
--  - nd - XDTD node
--
--  - n  - XTag node
--
-- Author : .\\artin Schmidt
-- Version : $Id: IdValidation.hs,v 1.1 2004/09/02 19:12:03 hxml Exp $

module Text.XML.HXT.Validator.IdValidation
    ( validateIds
    )
where


import Text.XML.HXT.DOM.XmlTree
import Text.XML.HXT.Validator.AttributeValueValidation



-- |
-- Lookup-table which maps element names to their validation functions. The
-- validation functions are XmlFilters.

type IdEnvTable		= [IdEnv]
type IdEnv 		= (ElemName, IdFct)
type ElemName		= String
type IdFct		= XmlFilter



-- |
-- Perform the validation of the ID/IDREF/IDREFS constraints.
--
--    * 1.parameter dtdPart :  the DTD subset (Node @DOCTYPE@) of the XmlTree
--
--    - 2.parameter doc :  the document subset of the XmlTree
--
--    - returns : a list of errors

validateIds :: XmlTree -> XmlTree -> XmlTrees
validateIds dtdPart doc
    = {-# SCC "checkForUniqueIds" #-} checkForUniqueIds idNodeList dtdNodes
      ++
      {-# SCC "checkIdReferences" #-} checkIdReferences idRefEnv doc
      where
      idEnv      = buildIdCollectorFcts dtdNodes
      idRefEnv   = buildIdrefValidationFcts dtdNodes idNodeList
      idNodeList = traverseTree idEnv doc
      dtdNodes   = getChildren dtdPart


-- |
-- Traverse the XmlTree in preorder.
--
--    * 1.parameter idEnv :  lookup-table which maps element names to their validation functions
--
--    - returns : list of errors

traverseTree :: IdEnvTable -> XmlFilter
traverseTree idEnv n@(NTree (XTag name _) cs)
    = (idFct n) ++ concatMap (traverseTree idEnv) cs
      where
      idFct :: XmlFilter
      idFct
          = case (lookup (qualifiedName name) idEnv) of
	    Nothing -> none
	    Just f  -> f

traverseTree _ _ = []



-- |
-- Returns the value of an element's ID attribute. The attribute name has to be
-- retrieved first from the DTD.
--
--    * 1.parameter dtdPart :  list of ID attribute definitions from the DTD
--
--    - 2.parameter n :  element which ID attribute value should be returned
--
--    - returns : normalized value of the ID attribute

getIdValue :: XmlTrees -> XmlTree -> String
getIdValue (x@(NTree (XDTD ATTLIST al) _):xs) n@(NTree (XTag name _al') _)
    = if (qualifiedName name) == elemName
      then attrValue
      else getIdValue xs n
      where
      elemName  = lookup1 a_name al
      attrName  = lookup1 a_value al
      attrValue = normalizeAttributeValue (Just x) (valueOf attrName n)

getIdValue _ _ = ""



-- ------------------------------------------------------------


-- |
-- Build collector functions which return XTag nodes with ID attributes from
-- a document.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - returns : lookup-table which maps element names to their collector function

buildIdCollectorFcts :: XmlTrees -> IdEnvTable
buildIdCollectorFcts dtdPart
    = map (buildIdCollectorFct) (isIdAttrType $$ dtdPart)
      where
      buildIdCollectorFct :: XmlTree -> IdEnv
      buildIdCollectorFct (NTree (XDTD ATTLIST al) _)
          = (elemName, hasAttr attrName)
            where
            elemName = lookup1 a_name al
            attrName = lookup1 a_value al

      buildIdCollectorFct nd
          = error ("buildIdCollectorFct: illegeal parameter:\n" ++ show nd)


-- |
-- Build validation functions for checking if IDREF\/IDREFS values match a value
-- of some ID attributes.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter idNodeList :  list of all XTag nodes with ID attributes
--
--    - returns : lookup-table which maps element names to their validation function

buildIdrefValidationFcts :: XmlTrees -> XmlTrees -> IdEnvTable
buildIdrefValidationFcts dtdPart idNodeList
    = map buildElemValidationFct elements
      where
      elements    = isElement $$ dtdPart
      idValueList = getIdValues (isIdAttrType $$ dtdPart)

      getIdValues :: XmlTrees -> [String]
      getIdValues idAttrTypes
          = map (getIdValue idAttrTypes) idNodeList


      buildElemValidationFct :: XmlTree -> IdEnv
      buildElemValidationFct (NTree (XDTD ELEMENT al) _)
          = (elemName, buildIdrefValidationFct idRefAttrTypes)
	  where
	  elemName = lookup1 a_name al
	  idRefAttrTypes = isIdRefAttrType $$ (isAttlistOfElement elemName) $$ dtdPart

      buildElemValidationFct nd
          = error ("buildIdrefValidationFct: illegeal parameter:\n" ++ show nd)


      buildIdrefValidationFct :: XmlTrees -> XmlFilter
      buildIdrefValidationFct (nd@(NTree (XDTD ATTLIST al) _):xs)
          = checkIdref +++ buildIdrefValidationFct xs
            where
            attrName = lookup1 a_value al
	    attrType = lookup1 a_type al

            checkIdref :: XmlFilter
            checkIdref n@(NTree (XTag name _) _)
                = if satisfies (hasAttr attrName) n
	          then if attrType == k_idref
		       then checkValueDeclared n attrValue
		       else let valueList = words attrValue
		            in if null valueList
		               then err ("Attribute " ++ show attrName ++
					 " of Element " ++ show (qualifiedName name) ++
					 " must have at least one name.") n
		               else concatMap (checkValueDeclared n) (words attrValue)
	          else []
		  where
		  attrValue = normalizeAttributeValue (Just nd) (valueOf attrName n)

	    checkIdref _ = []


	    checkValueDeclared :: XmlTree -> String -> XmlTrees
	    checkValueDeclared n@(NTree (XTag _ _) _) attrValue
	        = if attrValue `elem` idValueList
		  then []
		  else err ("An Element with identifier " ++ show attrValue ++
		            " must appear in the document.") n

            checkValueDeclared _ _ = []

      buildIdrefValidationFct []
          = none

      buildIdrefValidationFct nd
          = error ("buildIdCollectorFct: illegeal parameter:\n" ++ show nd)


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

checkForUniqueIds :: XmlTrees -> XmlTrees -> XmlTrees
checkForUniqueIds idNodeList dtdPart
    = checkForUniqueId idNodeList []
      where
      idAttrTypes = isIdAttrType $$ dtdPart

      checkForUniqueId :: XmlTrees -> [String] -> XmlTrees
      checkForUniqueId (x@(NTree (XTag name _) _):xs) used
          = if attrValue `elem` used
	    then err ("Attribute value " ++ show attrValue ++ " of type ID for element " ++
	              show (qualifiedName name) ++ " must be unique within the document.") x
		 ++
		 checkForUniqueId xs used

	    else checkForUniqueId xs (attrValue : used)
	    where
	    attrValue = getIdValue (isAttlistOfElement (qualifiedName name) $$ idAttrTypes) x

      checkForUniqueId _ _ = []



-- |
-- Validate that all IDREF\/IDREFS values match the value of some ID attribute.
-- Validity constraint: IDREF (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter idRefEnv :  lookup-table which maps element names to their validation function
--
--    - 2.parameter doc :  the document to validate
--
--    - returns : a list of errors

checkIdReferences :: IdEnvTable -> XmlTree -> XmlTrees
checkIdReferences idRefEnv doc
    = traverseTree idRefEnv doc





