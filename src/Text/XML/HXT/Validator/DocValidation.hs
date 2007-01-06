-- |
-- This module provides functions for validating XML Documents represented as
-- XmlTree.

-- Unlike other popular XML validation tools the validation process returns
-- a list of errors instead of aborting after the first error was found.


-- Before the document is validated, a lookup-table is build on the basis of
-- the DTD which maps element names to their validation functions.
-- After this initialization phase the whole document is traversed in preorder
-- and every element is validated by the XmlFilter from the lookup-table.

-- Special namings in source code:
--
--  - nd - XDTD node
--
--  - n  - XTag node
--
-- Author : .\\artin Schmidt
-- Version : $Id: DocValidation.hs,v 1.1 2004/09/02 19:12:03 hxml Exp $

module Text.XML.HXT.Validator.DocValidation
    ( validateDoc
    )
where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.Validator.AttributeValueValidation
import Text.XML.HXT.Validator.XmlRE

import Data.Maybe

-- |
-- Lookup-table which maps element names to their validation functions. The
-- validation functions are XmlFilters.

type ValiEnvTable	= [ValiEnv]
type ValiEnv 		= (ElemName, ValFct)
type ElemName		= String
type ValFct		= XmlFilter



-- ------------------------------------------------------------


-- |
-- Validate a document.
--
--    * 1.parameter dtdPart :  the DTD subset (Node @DOCTYPE@) of the XmlTree
--
--    - 2.parameter doc :  the document subset of the XmlTree
--
--    - returns : a list of errors

validateDoc :: XmlTree -> XmlTree -> XmlTrees
validateDoc dtdPart doc
    = traverseTree valTable doc
    where
    valTable = {-# SCC "buildAllValFcts" #-} buildAllValidationFunctions dtdPart


-- |
-- Traverse the XmlTree in preorder.
--
--    * 1.parameter valiEnv :  lookup-table which maps element names to their validation functions
--
--    - returns : list of errors

traverseTree :: ValiEnvTable -> XmlFilter
traverseTree valiEnv n@(NTree (XTag name _) cs)
    = (valFct n) ++ concatMap (traverseTree valiEnv) cs
      where
      valFct :: XmlFilter
      valFct
          = case (lookup (tName name) valiEnv) of
	    Nothing -> err ("Element " ++ show (tName name) ++ " not declared in DTD.")
	    Just f  -> f

traverseTree _ _ = []



-- ------------------------------------------------------------


-- |
-- Build all validation functions.
--
--    * 1.parameter dtdPart :  DTD subset, root node should be of type @DOCTYPE@
--
--    - returns : lookup-table which maps element names to their validation functions

buildAllValidationFunctions :: XmlTree -> ValiEnvTable
buildAllValidationFunctions dtdPart
    = buildValidateRoot dtdPart
      :
      -- construct a list of validation filters for all element declarations
      map (buildValidateFunctions dtdNodes) (isElement $$ dtdNodes)
      where
      dtdNodes = getChildren dtdPart


-- |
-- Build a validation function for the document root. By root node @\/@
-- is meant, which is the topmost dummy created by the parser.
--
--    * 1.parameter dtdPart :  DTD subset, root node should be of type @DOCTYPE@
--
--    - returns : entry for the lookup-table

buildValidateRoot :: XmlTree -> ValiEnv
buildValidateRoot (NTree (XDTD DOCTYPE al) _)
    = (t_root, valFct)
      where
      name = lookup1 a_name al

      valFct :: XmlFilter
      valFct nd@(NTree (XTag _ _) cs)
          = if msg == ""
            then []
            else err ("Root Element must be " ++ show name ++ ". " ++ msg) nd
	    where
	    re = re_sym (name)
	    msg = checkRE (matches re cs)

      valFct n = error ("buildValidateRoot: illegeal parameter:\n" ++ show n)

buildValidateRoot nd
    = error ("buildValidateRoot: illegeal parameter:\n" ++ show nd)


-- |
-- Build validation functions for an element.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter nd :  element declaration for which the validation functions are
--                   created
--
--    - returns : entry for the lookup-table

buildValidateFunctions :: XmlTrees -> XmlTree -> ValiEnv

buildValidateFunctions dtdPart nd@(NTree (XDTD ELEMENT al) _)
    = (elemName, valFct)
      where
      elemName = lookup1 a_name al

      valFct :: XmlFilter
      valFct = buildContentValidation nd
               +++
	       buildAttributeValidation dtdPart nd

buildValidateFunctions _ nd
    = error ("buildValidateFunctions: illegeal parameter:\n" ++ show nd)


-- ------------------------------------------------------------


-- |
-- Build validation functions for the content model of an element.
-- Validity constraint: Element Valid (3 \/ p.18 in Spec)
--
--    * 1.parameter nd :  element declaration for which the content validation functions
--                  are built
--
--    - returns : a function which takes an element (XTag), checks if its
--                  children match its content model and returns a list of errors

buildContentValidation :: XmlTree -> XmlFilter
buildContentValidation nd@(NTree (XDTD ELEMENT al) _)
    = contentValidation attrType nd
      where
      attrType = lookup1 a_type al


      -- Delegates construction of the validation function on the basis of the
      -- content model type
      contentValidation :: String -> XmlTree -> XmlFilter
      contentValidation typ (NTree (XDTD ELEMENT _) cs)
          | typ == k_pcdata   = contentValidationPcdata
          | typ == k_empty    = contentValidationEmpty
          | typ == k_any      = contentValidationAny
          | typ == v_children = contentValidationChildren cs
          | typ == v_mixed    = contentValidationMixed cs
          | otherwise         = error ("contentValidation: unknown type: " ++ show typ)

      contentValidation _ nd'
          = error ("contentValidation: illegeal parameter:\n" ++ show nd')


      -- Checks #PCDATA content models
      contentValidationPcdata :: XmlFilter
      contentValidationPcdata n@(NTree (XTag name _) cs)
          = if msg == ""
            then []
            else err ("The content of element "++ show (tName name) ++
	              " must match (#PCDATA). "++ msg) n
	    where
	    re = re_rep (re_sym k_pcdata)
	    msg = checkRE (matches re cs)

      contentValidationPcdata n
          = error ("contentValidationPcdata: illegeal parameter:\n" ++ show n)


      -- Checks EMPTY content models
      contentValidationEmpty :: XmlFilter
      contentValidationEmpty n@(NTree (XTag name _) cs)
          = if msg == ""
            then []
            else err ("The content of element " ++ show (tName name) ++
	              " must match EMPTY. " ++ msg) n
	    where
	    re = re_unit
	    msg = checkRE (matches re cs)

      contentValidationEmpty n
          = error ("contentValidationEmpty: illegeal parameter:\n" ++ show n)


      -- Checks ANY content models
      contentValidationAny :: XmlFilter
      contentValidationAny n@(NTree (XTag name _) cs)
          = if msg == ""
            then []
            else err ("The content of element " ++ show (tName name) ++
	              " must match ANY. " ++ msg) n
	    where
	    re = re_rep (re_dot)
	    msg = checkRE (matches re cs)

      contentValidationAny n
          = error ("contentValidationAny: illegeal parameter:\n" ++ show n)


      -- Checks "children" content models
      contentValidationChildren :: XmlTrees -> XmlFilter
      contentValidationChildren cm n@(NTree (XTag name _) cs)
          = if msg == ""
            then []
            else err ("The content of element " ++ show (tName name) ++
	              " must match " ++ printRE re ++ ". " ++ msg) n
	    where
	    re = createRE (head cm)
	    msg = checkRE (matches re cs)

      contentValidationChildren _ n
          = error ("contentValidationChildren: illegeal parameter:\n" ++ show n)


      -- Checks "mixed content" content models
      contentValidationMixed :: XmlTrees -> XmlFilter
      contentValidationMixed cm n@(NTree (XTag name _) cs)
          = if msg == ""
            then []
            else err ("The content of element "++ show (tName name) ++
	              " must match " ++ printRE re ++ ". " ++ msg) n
	    where
	    re = re_rep (re_alt (re_sym k_pcdata) (createRE (head cm)))
	    msg = checkRE (matches re cs)

      contentValidationMixed _ n
          = error ("contentValidationMixed: illegeal parameter:\n" ++ show n)


buildContentValidation nd
    = error ("buildContentValidation: illegeal parameter:\n" ++ show nd)



-- |
-- Build a regular expression from the content model. The regular expression
-- is provided by the module XmlRE.
--
--    * 1.parameter nd :  node of the content model. Expected: @CONTENT@ or
--              @NAME@
--
--    - returns : regular expression of the content model

createRE ::  XmlTree -> RE String
createRE (NTree (XDTD CONTENT al) cs)
    = processModifier modifier
      where
      modifier = lookup1 a_modifier al
      kind     = lookup1 a_kind al

      processModifier :: String -> RE String
      processModifier m
          | m == v_plus	  = re_plus (processKind kind)
	  | m == v_star	  = re_rep  (processKind kind)
	  | m == v_option = re_opt  (processKind kind)
	  | m == v_null	  = processKind kind
	  | otherwise     = error ("Unknown modifier: " ++ show m)

      processKind :: String -> RE String
      processKind k
          | k == v_seq	  = makeSequence cs
	  | k == v_choice = makeChoice cs
	  | otherwise	  = error ("Unknown kind: " ++ show k)

      makeSequence :: XmlTrees -> RE String
      makeSequence []     = re_unit
      makeSequence (x:xs) = re_seq (createRE x) (makeSequence xs)

      makeChoice :: XmlTrees -> RE String
      makeChoice []       = re_zero ""
      makeChoice (x:xs)   = re_alt (createRE x) (makeChoice xs)

createRE (NTree (XDTD NAME al) _)
    = re_sym (lookup1 a_name al)

createRE nd
    = error ("createRE: illegeal parameter:\n" ++ show nd)



-- ------------------------------------------------------------



-- |
-- Build validation functions for the attributes of an element.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter nd :  element declaration for which the attribute validation functions
--                  are created
--
--    - returns : a function which takes an element (XTag), checks if its
--                  attributes are valid and returns a list of errors

buildAttributeValidation :: XmlTrees -> XmlTree -> XmlFilter
buildAttributeValidation dtdPart nd =
    noDoublicateAttributes
    +++
    checkNotDeclardAttributes attrDecls nd
    +++
    checkRequiredAttributes attrDecls nd
    +++
    checkFixedAttributes attrDecls nd
    +++
    checkValuesOfAttributes attrDecls dtdPart nd
    where
    attrDecls = isAttlist $$ dtdPart


-- |
-- Validate that all attributes of an element are unique.
-- Well-formdness constraint: Unique AttSpec (3.1 \/ p.19 in Spec)
--
--    - returns : a function which takes an element (XTag), checks if its
--                  attributes are unique and returns a list of errors

noDoublicateAttributes :: XmlFilter
noDoublicateAttributes n@(NTree (XTag _ _) _)
    = doubles . reverse $ names
      where
      tagname = nameOf n
      names   = map nameOf . getAttrl $ n

      doubles :: [String] -> XmlTrees
      doubles []
	  = []
      doubles (n1:ns)
	  = ( if n1 `elem` ns
	      then err ("Attribute " ++ show n1 ++ " was already specified for element " ++ show tagname ++ ".") n
	      else []
	    ) ++ doubles ns

noDoublicateAttributes n
    = error ("noDoublicateAttributes: illegeal parameter:\n" ++ show n)


-- |
-- Validate that all \#REQUIRED attributes are provided.
-- Validity constraint: Required Attributes (3.3.2 \/ p.28 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter nd :  element declaration which attributes have to be checked
--
--    - returns : a function which takes an element (XTag), checks if all
--                  required attributes are provided and returns a list of errors

checkRequiredAttributes :: XmlTrees -> XmlTree -> XmlFilter
checkRequiredAttributes attrDecls (NTree (XDTD ELEMENT al) _)
    = checkRequired requiredAtts
      where
      elemName     = lookup1 a_name al
      requiredAtts = isRequiredAttrKind $$ (isAttlistOfElement elemName $$ attrDecls)


      checkRequired :: XmlTrees -> XmlFilter
      checkRequired ((NTree (XDTD ATTLIST al') _):xs) n@(NTree (XTag name _) _)
          = if satisfies (hasAttr attName) n
	    then checkRequired xs n
	    else err ("Attribute " ++ show attName ++ " must be declared for element type " ++
	             show (tName name) ++ ".") n
	         ++
		 checkRequired xs n
	    where
	    attName = lookup1 a_value al'

      checkRequired [] _ = []

      checkRequired nd n
          = error ("checkRequired: illegeal parameter:\n" ++ show nd ++ show n)


checkRequiredAttributes _ nd
    = error ("checkRequiredAttributes: illegeal parameter:\n" ++ show nd)



-- |
-- Validate that \#FIXED attributes match the default value.
-- Validity constraint: Fixed Attribute Default (3.3.2 \/ p.28 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter nd :  element declaration which attributes have to be checked
--
--    - returns : a function which takes an element (XTag), checks if all
--                  fixed attributes match the default value and returns a list of errors

checkFixedAttributes :: XmlTrees -> XmlTree -> XmlFilter
checkFixedAttributes attrDecls (NTree (XDTD ELEMENT al) _)
    = checkFixed fixedAtts
      where
      elemName  = lookup1 a_name al
      fixedAtts = isFixedAttrKind $$ (isAttlistOfElement elemName $$ attrDecls)


      checkFixed :: XmlTrees -> XmlFilter
      checkFixed (x@(NTree (XDTD ATTLIST al') _):xs) n@(NTree (XTag name _) _)
          = if satisfies (hasAttr attName) n
	    then if attValue == fixedValue
	         then checkFixed xs n
	         else err ("Attribute " ++ show attName ++ " of element " ++ show (tName name) ++
		           " with value " ++ show attValue ++ " must have a value of " ++
			   show fixedValue ++ ".") n
	              ++
		      checkFixed xs n
	    else checkFixed xs n
	    where
	    attName    = lookup1 a_value al'
	    fixedValue = normalizeAttributeValue (Just x) (lookup1 a_default al')
	    attValue   = normalizeAttributeValue (Just x) (valueOf attName n)

      checkFixed [] _ = []

      checkFixed nd n
          = error ("checkFixed: illegeal parameter:\n" ++ show nd ++ show n)


checkFixedAttributes _ nd
    = error ("checkFixedAttributes: illegeal parameter:\n" ++ show nd)


-- |
-- Validate that an element has no attributes which are not declared.
-- Validity constraint: Attribute Value Type (3.1 \/ p.19 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter nd :  element declaration which attributes have to be checked
--
--    - returns : a function which takes an element (XTag), checks if all
--                  attributes are declared and returns a list of errors

checkNotDeclardAttributes :: XmlTrees -> XmlTree -> XmlFilter
checkNotDeclardAttributes attrDecls elemDescr
    = checkNotDeclared
      where
      elemName = valueOfDTD a_name elemDescr
      decls    = isAttlistOfElement elemName $$ attrDecls

      checkNotDeclared :: XmlFilter
      checkNotDeclared n
	  = ( isXTag
	      `guards`
	      cat (map (searchForDeclaredAtt elemName decls) (getAttrl n))
	    ) n

      searchForDeclaredAtt :: String -> XmlTrees -> XmlTree -> XmlFilter
      searchForDeclaredAtt name ((NTree (XDTD ATTLIST al') _):xs) att
          = if (lookup1 a_value al') == nameOf att
            then none
	    else searchForDeclaredAtt name xs att

      searchForDeclaredAtt name [] (NTree (XAttr attrName) _)
          = err ("Attribute " ++ show (aName attrName) ++ " of element " ++ show name ++ " is not declared in DTD.")

      searchForDeclaredAtt _ nd a
          = error ("searchForDeclaredAtt: illegeal paramter:\n" ++ show nd ++ show a)

-- |
-- Validate that the attribute value meets the lexical constraints of its type.
-- Validity constaint: Attribute Value Type (3.1 \/ p.19 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter nd :  element declaration which attributes have to be checked
--
--    - returns : a function which takes an element (XTag), checks if all
--                  attributes meet the lexical constraints and returns a list of errors

checkValuesOfAttributes :: XmlTrees -> XmlTrees -> XmlTree -> XmlFilter
checkValuesOfAttributes attrDecls dtdPart elemDescr
    = checkValues
      where
      elemName = valueOfDTD a_name elemDescr
      decls    = isAttlistOfElement elemName $$ attrDecls

      checkValues :: XmlFilter
      checkValues n
          = ( isXTag
	      `guards`
	      cat (map (checkValue decls) (getAttrl n))
	    ) n

      checkValue :: XmlTrees -> XmlTree -> XmlFilter
      checkValue (attrDecl@(NTree (XDTD ATTLIST al') _):xs) att
          = if (lookup1 a_value al') == nameOf att
            then checkAttributeValue dtdPart attrDecl
	    else checkValue xs att

      checkValue [] _
          = none  -- undeclared attribute, reported by separate function

      checkValue n _
          = error ("checkValue: illegeal parameter:\n" ++ show n)

