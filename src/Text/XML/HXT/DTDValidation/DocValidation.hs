-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DTDValidation.TypeDefs
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   This module provides functions for validating XML Documents represented as
   XmlTree.

   Unlike other popular XML validation tools the validation process returns
   a list of errors instead of aborting after the first error was found.

   Before the document is validated, a lookup-table is build on the basis of
   the DTD which maps element names to their validation functions.
   After this initialization phase the whole document is traversed in preorder
   and every element is validated by the XmlFilter from the lookup-table.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DTDValidation.DocValidation
    ( validateDoc
    )
where

import Text.XML.HXT.DTDValidation.TypeDefs

import Text.XML.HXT.DTDValidation.AttributeValueValidation
import Text.XML.HXT.DTDValidation.XmlRE

import Data.Maybe

-- ------------------------------------------------------------

-- |
-- Lookup-table which maps element names to their validation functions. The
-- validation functions are XmlArrows.

type ValiEnvTable	= [ValiEnv]
type ValiEnv 		= (ElemName, ValFct)
type ElemName		= String
type ValFct		= XmlArrow


-- ------------------------------------------------------------

-- |
-- Validate a document.
--
--    * 1.parameter dtdPart :  the DTD subset (Node @DOCTYPE@) of the XmlTree
--
--    - 2.parameter doc :  the document subset of the XmlTree
--
--    - returns : a list of errors

validateDoc	:: XmlTree -> XmlArrow
validateDoc dtdPart
    = traverseTree valTable
    where
    valTable = buildAllValidationFunctions dtdPart


-- |
-- Traverse the XmlTree in preorder.
--
--    * 1.parameter valiEnv :  lookup-table which maps element names to their validation functions
--
--    - returns : list of errors

traverseTree	:: ValiEnvTable -> XmlArrow
traverseTree valiEnv
    = choiceA [ isElem	:-> (valFct $< getQName)
	      , this	:-> none
	      ]
      <+>
      ( getChildren >>> traverseTree valiEnv )
    where
    valFct	:: QName -> XmlArrow
    valFct name	= case (lookup (qualifiedName name) valiEnv) of
		  Nothing -> err ("Element " ++ show (qualifiedName name) ++ " not declared in DTD.")
		  Just f  -> f

-- ------------------------------------------------------------

-- |
-- Build all validation functions.
--
--    * 1.parameter dtdPart :  DTD subset, root node should be of type @DOCTYPE@
--
--    - returns : lookup-table which maps element names to their validation functions

buildAllValidationFunctions :: XmlTree -> ValiEnvTable
buildAllValidationFunctions dtdPart
    = concat $
      buildValidateRoot dtdPart :	      -- construct a list of validation filters for all element declarations
      map (buildValidateFunctions dtdNodes) dtdNodes
      where
      dtdNodes = runLA getChildren dtdPart

-- |
-- Build a validation function for the document root. By root node @\/@
-- is meant, which is the topmost dummy created by the parser.
--
--    * 1.parameter dtdPart :  DTD subset, root node should be of type @DOCTYPE@
--
--    - returns : entry for the lookup-table

buildValidateRoot :: XmlTree -> [ValiEnv]
buildValidateRoot dn
    | isDTDDoctypeNode dn	= [(t_root, valFct)]
    | otherwise			= []
      where
      name	= dtd_name . getDTDAttributes $ dn

      valFct	:: XmlArrow
      valFct	= isElem
		  `guards`
		  ( checkRegex (re_sym name)
		    >>>
		    msgToErr (("Root Element must be " ++ show name ++ ". ") ++)
		  )

checkRegex	:: RE String -> LA XmlTree String
checkRegex re	= listA getChildren
		  >>> arr (\ cs -> checkRE (matches re cs))

-- |
-- Build validation functions for an element.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter nd :  element declaration for which the validation functions are
--                   created
--
--    - returns : entry for the lookup-table

buildValidateFunctions :: XmlTrees -> XmlTree -> [ValiEnv]

buildValidateFunctions dtdPart dn
    | isDTDElementNode dn	= [(elemName, valFct)]
    | otherwise			= []
      where
      elemName = dtd_name . getDTDAttributes $ dn

      valFct :: XmlArrow
      valFct = buildContentValidation dn
               <+>
	       buildAttributeValidation dtdPart dn

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

buildContentValidation :: XmlTree -> XmlArrow
buildContentValidation nd
    = contentValidation attrType nd
      where
      attrType = dtd_type . getDTDAttributes $ nd


      -- Delegates construction of the validation function on the basis of the
      -- content model type
      contentValidation :: String -> XmlTree -> XmlArrow
      contentValidation typ dn
          | typ == k_pcdata   = contentValidationPcdata
          | typ == k_empty    = contentValidationEmpty
          | typ == k_any      = contentValidationAny
          | typ == v_children = contentValidationChildren cs
          | typ == v_mixed    = contentValidationMixed cs
          | otherwise         = none
	  where
	  cs = runLA getChildren dn

      -- Checks #PCDATA content models
      contentValidationPcdata :: XmlArrow
      contentValidationPcdata
	  = isElem `guards` (contentVal $< getQName)
	    where
	    contentVal name
		= checkRegex (re_rep (re_sym k_pcdata))
		  >>>
		  msgToErr ( ( "The content of element " ++
			       show (qualifiedName name) ++
			       " must match (#PCDATA). "
			     ) ++
			   )

      -- Checks EMPTY content models
      contentValidationEmpty :: XmlArrow
      contentValidationEmpty
	  = isElem `guards` (contentVal $< getQName)
	    where
	    contentVal name
		= checkRegex re_unit
		  >>>
		  msgToErr ( ( "The content of element " ++
				 show (qualifiedName name) ++
				 " must match EMPTY. "
			     ) ++
			   )

      -- Checks ANY content models
      contentValidationAny :: XmlArrow
      contentValidationAny
	  = isElem `guards` (contentVal $< getName)
	    where
	    contentVal name
		= checkRegex (re_rep (re_dot))
		  >>>
		  msgToErr ( ( "The content of element " ++
			       show name ++
			       " must match ANY. "
			     ) ++
			   )

      -- Checks "children" content models
      contentValidationChildren :: XmlTrees -> XmlArrow
      contentValidationChildren cm
	  = isElem `guards` (contentVal $< getName)
	    where
	    contentVal name
		= checkRegex re
		  >>>
		  msgToErr ( ( "The content of element " ++
			       show name ++
			       " must match " ++ printRE re ++ ". "
			     ) ++
			   )
	    re = createRE (head cm)

      -- Checks "mixed content" content models
      contentValidationMixed :: XmlTrees -> XmlArrow
      contentValidationMixed cm
	  = isElem `guards` (contentVal $< getName)
	    where
	    contentVal name
		= checkRegex re
		  >>>
		  msgToErr ( ( "The content of element " ++
			       show name ++
			       " must match " ++ printRE re ++ ". "
			     ) ++
			   )
	    re = re_rep (re_alt (re_sym k_pcdata) (createRE (head cm)))

-- |
-- Build a regular expression from the content model. The regular expression
-- is provided by the module XmlRE.
--
--    * 1.parameter nd :  node of the content model. Expected: @CONTENT@ or
--              @NAME@
--
--    - returns : regular expression of the content model

createRE	::  XmlTree -> RE String
createRE dn
    | isDTDContentNode dn
	= processModifier modifier
    | isDTDNameNode dn
	= re_sym name
    | otherwise
	= error ("createRE: illegeal parameter:\n" ++ show dn)
    where
    al		= getDTDAttributes dn
    name	= dtd_name     al
    modifier 	= dtd_modifier al
    kind     	= dtd_kind     al
    cs		= runLA getChildren dn

    processModifier :: String -> RE String
    processModifier m
        | m == v_plus	  = re_plus (processKind kind)
	| m == v_star	  = re_rep  (processKind kind)
	| m == v_option	  = re_opt  (processKind kind)
	| m == v_null	  = processKind kind
	| otherwise       = error ("Unknown modifier: " ++ show m)

    processKind :: String -> RE String
    processKind k
        | k == v_seq	  = makeSequence cs
	| k == v_choice	  = makeChoice cs
	| otherwise	  = error ("Unknown kind: " ++ show k)

    makeSequence :: XmlTrees -> RE String
    makeSequence []     = re_unit
    makeSequence (x:xs) = re_seq (createRE x) (makeSequence xs)

    makeChoice :: XmlTrees -> RE String
    makeChoice []       = re_zero ""
    makeChoice (x:xs)   = re_alt (createRE x) (makeChoice xs)

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

buildAttributeValidation :: XmlTrees -> XmlTree -> XmlArrow
buildAttributeValidation dtdPart nd =
    noDoublicateAttributes
    <+>
    checkNotDeclardAttributes attrDecls nd
    <+>
    checkRequiredAttributes attrDecls nd
    <+>
    checkFixedAttributes attrDecls nd
    <+>
    checkValuesOfAttributes attrDecls dtdPart nd
    where
    attrDecls = isDTDAttlist $$ dtdPart


-- |
-- Validate that all attributes of an element are unique.
-- Well-formdness constraint: Unique AttSpec (3.1 \/ p.19 in Spec)
--
--    - returns : a function which takes an element (XTag), checks if its
--                  attributes are unique and returns a list of errors

noDoublicateAttributes	:: XmlArrow
noDoublicateAttributes
    = isElem
      `guards`
      ( noDoubles' $< getName )
    where
    noDoubles' elemName
	= listA (getAttrl >>> getName)
	  >>> applyA (arr (catA . map toErr . doubles . reverse))
	where
	toErr n1 = err ( "Attribute " ++ show n1 ++
			 " was already specified for element " ++
			 show elemName ++ "."
		       )

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

checkRequiredAttributes	:: XmlTrees -> XmlTree -> XmlArrow
checkRequiredAttributes attrDecls dn
    | isDTDElementNode dn
	= isElem
	  `guards`
	  ( checkRequired $< getName )
    | otherwise
	= none
      where
      elemName     = dtd_name . getDTDAttributes $ dn
      requiredAtts = (isAttlistOfElement elemName >>> isRequiredAttrKind) $$ attrDecls

      checkRequired :: String -> XmlArrow
      checkRequired name
	  = catA . map checkReq $ requiredAtts
	  where
	  checkReq	:: XmlTree -> XmlArrow
	  checkReq attrDecl
	      = neg (hasAttr attName)
		`guards`
		err ( "Attribute " ++ show attName ++ " must be declared for element type " ++
		      show name ++ "." )
	      where
	      attName = dtd_value . getDTDAttributes $ attrDecl

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

checkFixedAttributes :: XmlTrees -> XmlTree -> XmlArrow
checkFixedAttributes attrDecls dn
    | isDTDElementNode dn
	= isElem
	  `guards`
	  ( checkFixed $< getName )
    | otherwise
	= none
      where
      elemName  = dtd_name . getDTDAttributes $ dn
      fixedAtts = (isAttlistOfElement elemName >>> isFixedAttrKind) $$ attrDecls

      checkFixed :: String -> XmlArrow
      checkFixed name
	  = catA . map checkFix $ fixedAtts
	  where
	  checkFix	:: XmlTree -> XmlArrow
	  checkFix an
	      |  isDTDAttlistNode an
		  = checkFixedVal $< getAttrValue attName
	      | otherwise
		  = none
	      where
	      al'	= getDTDAttributes an
	      attName   = dtd_value   al'
	      defa	= dtd_default al'
	      fixedValue = normalizeAttributeValue (Just an) defa

              checkFixedVal	:: String -> XmlArrow
	      checkFixedVal val
		  = ( ( hasAttr attName
			>>>
			isA (const (attValue /= fixedValue))
		      )
		      `guards`
		      err ( "Attribute " ++ show attName ++ " of element " ++ show name ++
			    " with value " ++ show attValue ++ " must have a value of " ++
			    show fixedValue ++ "." )
		    )
		  where
		  attValue   = normalizeAttributeValue (Just an) val

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

checkNotDeclardAttributes :: XmlTrees -> XmlTree -> XmlArrow
checkNotDeclardAttributes attrDecls elemDescr
    = checkNotDeclared
      where
      elemName = valueOfDTD a_name elemDescr
      decls    = isAttlistOfElement elemName $$ attrDecls

      checkNotDeclared :: XmlArrow
      checkNotDeclared
	  = isElem
	    `guards`
	    ( getAttrl >>> searchForDeclaredAtt elemName decls )

      searchForDeclaredAtt :: String -> XmlTrees -> XmlArrow
      searchForDeclaredAtt name (dn : xs)
	  | isDTDAttlistNode dn
	      = ( getName >>> isA ( (dtd_value . getDTDAttributes $ dn) /= ) )
		`guards`
		searchForDeclaredAtt name xs
	  | otherwise
	      = searchForDeclaredAtt name xs

      searchForDeclaredAtt name []
	  = mkErr $< getName
	    where
	    mkErr n = err ( "Attribute " ++ show n ++ " of element " ++
			    show name ++ " is not declared in DTD." )

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

checkValuesOfAttributes :: XmlTrees -> XmlTrees -> XmlTree -> XmlArrow
checkValuesOfAttributes attrDecls dtdPart elemDescr
    = checkValues
      where
      elemName	= dtd_name . getDTDAttributes $ elemDescr
      decls     = isAttlistOfElement elemName $$ attrDecls

      checkValues :: XmlArrow
      checkValues
	  = isElem
	    `guards`
	    ( checkValue $< getAttrl )

      checkValue att
	  = catA . map checkVal $ decls
	    where
	    checkVal :: XmlTree -> XmlArrow
	    checkVal attrDecl
		| isDTDAttlistNode attrDecl
		  &&
		  nameOfAttr att == dtd_value al'
		      = checkAttributeValue dtdPart attrDecl
		| otherwise
		    = none
		where
		al' = getDTDAttributes attrDecl

-- ------------------------------------------------------------