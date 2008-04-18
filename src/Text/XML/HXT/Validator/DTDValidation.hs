-- |
-- This module provides functions for validating the DTD of XML documents
-- represented as XmlTree.

-- Unlike other popular XML validation tools the validation process returns
-- a list of errors instead of aborting after the first error was found.


-- Unlike validation of the document, the DTD branch is traversed four times:
--
--  - Validation of Notations
--
--  - Validation of Unparsed Entities
--
--  - Validation of Element declarations
--
--  - Validation of Attribute declarations

-- Special namings in source code:
--
--  - nd - XDTD node
--
-- Author : .\\artin Schmidt

module Text.XML.HXT.Validator.DTDValidation
    ( removeDoublicateDefs
    , validateDTD
    )
where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.Validator.AttributeValueValidation



-- |
-- Validate a DTD.
--
--    - returns : a functions which takes the DTD subset of the XmlTree, checks
--                  if the DTD is valid and returns a list of errors

validateDTD :: XmlFilter
validateDTD dtdPart
    = {-# SCC "validateNotations" #-} validateNotations dtdPart'
      ++
      {-# SCC "validateEntities" #-} validateEntities dtdPart' notationNames
      ++
      {-# SCC "validateElements" #-} validateElements dtdPart' elemNames
      ++
      {-# SCC "validateAttributes" #-} validateAttributes dtdPart' elemNames notationNames

      where
      dtdPart' = getChildren dtdPart

      notationNames :: [String]
      notationNames
          = getXTextValues (isNotation .> getDTDValue a_name $$ dtdPart')

      elemNames :: [String]
      elemNames
          = getXTextValues (isElement  .> getDTDValue a_name $$ dtdPart')




-- ------------------------------------------------------------

-- |
-- Returns a list of XText values.
--
--    * 1.parameter xtextList :  a list of XText nodes
--
--    - returns : the values of the XText nodes

getXTextValues :: XmlTrees -> [String]
getXTextValues
  = concatMap showT
  where
    showT (NTree n _)
	| isXTextNode n = [textOfXNode n]
    showT _             = []


-- ------------------------------------------------------------


-- |
-- Validation of Notations, checks if all notation names are unique.
-- Validity constraint: Unique Notation Name (4.7 \/ p.44 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - returns : a list of errors

validateNotations :: XmlTrees -> XmlTrees
validateNotations dtdPart
    = checkForUniqueNotation notations []
      where
      notations = isNotation $$ dtdPart

      checkForUniqueNotation :: XmlTrees -> [String] -> XmlTrees
      checkForUniqueNotation (x@(NTree (XDTD NOTATION al) _):xs) used
          = if name `elem` used
	    then err ("Notation "++ show name ++ " was already specified.") x
		 ++
		 checkForUniqueNotation xs used

	    else checkForUniqueNotation xs (name : used)
	    where
	    name = lookup1 a_name al

      checkForUniqueNotation [] _ = []

      checkForUniqueNotation nd _
          = error ("checkForUniqueNotation: illegeal parameter:\n" ++ show nd)


-- |
-- Validation of Entities.
--
-- 1. Issues a warning if entities are declared multiple times.
--
--    Optional warning: (4.2 \/ p.35 in Spec) 
--
--
-- 2. Validates that a notation is declared for an unparsed entity.
--
--    Validity constraint: Notation Declared (4.2.2 \/ p.36 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter notationNames :  list of all notation names declared in the DTD
--
--    - returns : a list of errors

validateEntities :: XmlTrees -> [String] -> XmlTrees
validateEntities dtdPart notationNames
    = checkForUniqueEntity entities []
      ++
      checkNotationDecls notationNames upEntities

      where
      entities   = isEntity $$ dtdPart
      upEntities = isUnparsedEntity $$ dtdPart

      -- Check if entities are declared multiple times
      checkForUniqueEntity :: XmlTrees -> [String] -> XmlTrees
      checkForUniqueEntity (x@(NTree (XDTD ENTITY al) _):xs) used
          = if name `elem` used
	    then warn ("Entity "++ show name ++ " was already specified. " ++
	               "First declaration will be used.") x
		 ++
		 checkForUniqueEntity xs used

	    else checkForUniqueEntity xs (name : used)
	    where
	    name = lookup1 a_name al

      checkForUniqueEntity [] _ = []

      checkForUniqueEntity nd _
          = error ("checkForUniqueEntity: illegeal parameter:\n" ++ show nd)


      -- Find unparsed entities for which no notation is specified
      checkNotationDecls :: [String] -> XmlTrees -> XmlTrees
      checkNotationDecls notationNames' upEntities'
	  = concatMap (checkNotationDecl) upEntities'
          where
          checkNotationDecl :: XmlTree -> XmlTrees
          checkNotationDecl n@(NTree (XDTD ENTITY al) _)
              = if (notationName al) `elem` notationNames'
	        then []
                else err ("The notation " ++ show (notationName al) ++ " must be declared " ++
	                  "when referenced in the unparsed entity declaration for " ++
		          show (upEntityName al) ++ ".") n
                where
		notationName = lookup1 k_ndata
                upEntityName = lookup1 a_name

          checkNotationDecl nd
              = error ("checkNotationDecl: illegeal parameter:\n" ++ show nd)



-- |
-- Validation of Element declarations.
--
-- 1. Validates that an element is not declared multiple times.
--
--    Validity constraint: Unique Element Type Declaration (3.2 \/ p.21 in Spec) 
--
--
-- 2. Validates that an element name only appears once in a mixed-content declaration.
--
--    Validity constraint: No Duplicate Types (3.2 \/ p.21 in Spec) 
--
--
-- 3. Issues a warning if an element mentioned in a content model is not declared in the
--    DTD.
--
--    Optional warning: (3.2 \/ p.21 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter elemNames :  list of all element names declared in the DTD
--
--    - returns : a list of errors


validateElements :: XmlTrees -> [String] -> XmlTrees
validateElements dtdPart elemNames
    = checkForUniqueElement elements []
      ++
      checkMixedContents mixedContentElems
      ++
      checkContentModels elemNames elements

      where
      elements          = isElement $$ dtdPart
      mixedContentElems = isMixedContentElement $$ dtdPart


      -- Validates that an element is not declared multiple times
      checkForUniqueElement :: XmlTrees -> [String] -> XmlTrees
      checkForUniqueElement (x@(NTree (XDTD ELEMENT al) _):xs) used
          = if name `elem` used
	    then err ("Element type " ++ show name ++
		      " must not be declared more than once.") x
		 ++
		 checkForUniqueElement xs used

	    else checkForUniqueElement xs (name : used)
	    where
	    name = lookup1 a_name al

      checkForUniqueElement [] _ = []

      checkForUniqueElement nd _
          = error ("checkForUniqueElement: illegeal parameter:\n" ++ show nd)


      -- Validates that an element name only appears once in a mixed-content declaration
      checkMixedContents elems
          = concatMap (checkMixedContent) elems
	  where
          checkMixedContent (NTree (XDTD ELEMENT al) cs)
	      = checkMC (getChildren (head cs)) []
	      where
	      elemName = lookup1 a_name al

	      checkMC :: XmlTrees -> [String] -> XmlTrees
	      checkMC (x@(NTree (XDTD NAME al') _):xs) used
                  = if name `elem` used
	            then err ("The element type " ++ show name ++
			      " was already specified in the mixed-content model of the element declaration " ++
		              show elemName ++ ".") x
		         ++
		         checkMC xs used

	            else checkMC xs (name : used)
	            where
	            name = lookup1 a_name al'

              checkMC [] _ = []

              checkMC nd _
	          = error ("checkMC: illegeal parameter:\n" ++ show nd)

	  checkMixedContent nd
	      = error ("checkMixedContent: illegeal parameter:\n" ++ show nd)


      -- Issues a warning if an element mentioned in a content model is not
      -- declared in the DTD.
      checkContentModels names elems
          = concatMap checkContentModel elems
	  where
          checkContentModel :: XmlFilter
          checkContentModel (NTree (XDTD ELEMENT al) cs)
              = validateContent (lookup1 a_type al)
	      where
	      elemName = lookup1 a_name al

	      validateContent :: String -> XmlTrees
	      validateContent cm
	          | cm == v_children = checkContent (head cs)
	          | cm == v_mixed    = checkContent (head cs)
	          | otherwise        = []  -- no child elements to check

	      checkContent :: XmlFilter
	      checkContent n@(NTree (XDTD NAME al') _)
	          = if childElemName `elem` names
	            then []
	            else warn ("The element type "++ show childElemName ++
		               ", used in content model of element "++ show elemName ++
			       ", is not declared.") n
	            where
		    childElemName = lookup1 a_name al'

	      checkContent (NTree (XDTD CONTENT _) cs')
	          = concatMap (checkContent) cs'

	      checkContent nd
	          = error ("checkContent: illegeal parameter:\n" ++ show nd)

          checkContentModel nd
              = error ("checkContentModel: illegeal parameter:\n" ++ show nd)


-- |
-- Validation of Attribute declarations.
--
-- (1) Issues a warning if an attribute is declared for an element type not itself
--    decared.
--
--    Optinal warning: (3.3 \/ p. 24 in Spec)
--
--
-- 2. Issues a warning if more than one definition is provided for the same
--    attribute of a given element type. Fist declaration is binding, later
--    definitions are ignored.
--
--    Optional warning: (3.3 \/ p.24 in Spec)
--
--
-- 3. Issues a warning if the same Nmtoken occures more than once in enumerated
--    attribute types of a single element type.
--
--    Optional warning: (3.3.1 \/ p.27 in Spec) 
--
--
-- 4. Validates that an element type has not more than one ID attribute defined.
--
--    Validity constraint: One ID per Element Type (3.3.1 \/ p.26 in Spec) 
--
--
-- 5. Validates that an element type has not more than one NOTATION attribute defined.
--
--    Validity constraint: One Notation per Element Type (3.3.1 \/ p.27 in Spec) 
--
--
-- 6. Validates that an ID attributes has the type #IMPLIED or #REQUIRED.
--
--    Validity constraint: ID Attribute Default (3.3.1 \/ p.26 in Spec) 
--
--
-- 7. Validates that all referenced notations are declared.
--
--    Validity constraint: Notation Attributes (3.3.1 \/ p.27 in Spec) 
--
--
-- 8. Validates that notations are not declared for EMPTY elements.
--
--    Validity constraint: No Notation on Empty Element (3.3.1 \/p.27 in Spec) 
--
--
-- 9. Validates that the default value matches the lexical constraints of it's type.
--
--    Validity constraint: Attribute default legal (3.3.2 \/ p.28 in Spec)
--
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter elemNames :  list of all element names declared in the DTD
--
--    - 3.parameter notationNames :  list of all notation names declared in the DTD
--
--    - returns : a list of errors

validateAttributes :: XmlTrees -> [String] -> [String] -> XmlTrees
validateAttributes dtdPart elemNames notationNames
    = -- 1. Find attributes for which no elements are declared
      ((checkDeclaredElements elemNames) $$ attributes)
      ++
      -- 2. Find attributes which are declared more than once
      (checkForUniqueAttributeDeclaration attributes [])
      ++
      -- 3. Find enumerated attribute types which nmtokens are declared more than once
      (checkEnumeratedTypes
      `o`
      (isEnumAttrType `orElse` isNotationAttrType) $$ attributes)
      ++
      -- 4. Validate that there exists only one ID attribute for an element
      (checkForUniqueId idAttributes [])
      ++
      -- 5. Validate that there exists only one NOTATION attribute for an element
      (checkForUniqueNotation notationAttributes [])
      ++
      -- 6. Validate that ID attributes have the type #IMPLIED or #REQUIRED
      (checkIdKindConstraint $$ idAttributes)
      ++
      -- 7. Validate that all referenced notations are declared
      ((checkNotationDeclaration notationNames) $$ notationAttributes)
      ++
      -- 8. Validate that notations are not declared for EMPTY elements
      ((checkNoNotationForEmptyElement
      	  (
    	      getXTextValues (getDTDValue a_name $$ (isEmptyElement $$ dtdPart))
       	  )
        ) $$ notationAttributes)
      ++
      -- 9. Validate that the default value matches the lexical constraints of it's type
      ((checkDefaultValueTypes dtdPart) $$ (isDefaultAttrKind $$ attributes))

      where
      --------------------------------------------------------------------------
      -- helper functions
      attributes         = isAttlist $$ dtdPart
      idAttributes       = isIdAttrType $$ attributes
      notationAttributes = isNotationAttrType $$ attributes


      elemName = lookup1 a_name
      attName  = lookup1 a_value

      --------------------------------------------------------------------------
      -- 1. Find attributes for which no elements are declared
      checkDeclaredElements :: [String] -> XmlFilter
      checkDeclaredElements elemNames' n@(NTree (XDTD ATTLIST al) _)
          = if (elemName al) `elem` elemNames'
	    then []
	    else warn ("The element type \""++ elemName al ++ "\" used in declaration "++
	               "of attribute \""++ attName al ++"\" is not declared.") n

      checkDeclaredElements _ nd
          = error ("checkDeclaredElements: illegeal parameter:\n" ++ show nd)

      --------------------------------------------------------------------------
      -- 2. Find attributes which are declared more than once
      checkForUniqueAttributeDeclaration :: XmlTrees -> [String] -> XmlTrees
      checkForUniqueAttributeDeclaration (x@(NTree (XDTD ATTLIST al) _):xs) used
          = if name `elem` used
	    then warn ("Attribute \""++ aname ++"\" for element type \""++
		        ename ++"\" is already declared. First "++
		        "declaration will be used.") x
		 ++
		 checkForUniqueAttributeDeclaration xs used

	    else checkForUniqueAttributeDeclaration xs (name : used)
	    where
	    ename = elemName al
	    aname = attName al
	    name  = ename ++ "|" ++ aname

      checkForUniqueAttributeDeclaration [] _ = []

      checkForUniqueAttributeDeclaration nd _
          = error ("checkForUniqueAttributeDeclaration: illegeal parameter:\n" ++ show nd)


      --------------------------------------------------------------------------
      -- 3. Find enumerated attribute types which nmtokens are declared more than once
      checkEnumeratedTypes :: XmlFilter
      checkEnumeratedTypes (NTree (XDTD ATTLIST al) cs)
          = checkForUniqueType cs []
	  where
	  checkForUniqueType :: XmlTrees -> [String] -> XmlTrees
          checkForUniqueType (x@(NTree (XDTD NAME al') _):xs) used
              = if nmtoken `elem` used
	        then warn ("Nmtoken \""++ nmtoken ++"\" should not "++
		        "occur more than once in attribute \""++ attName al ++
			"\" for element \""++ elemName al ++ "\".") x
		     ++
		     checkForUniqueType xs used

	        else checkForUniqueType xs (nmtoken : used)
	        where
	        nmtoken = lookup1 a_name al'

          checkForUniqueType [] _ = []

          checkForUniqueType nd _
              = error ("checkForUniqueType: illegeal parameter:\n" ++ show nd)

      checkEnumeratedTypes nd
          = error ("checkEnumeratedTypes: illegeal parameter:\n" ++ show nd)


      --------------------------------------------------------------------------
      -- 4. Validate that there exists only one ID attribute for an element
      checkForUniqueId :: XmlTrees -> [String] -> XmlTrees
      checkForUniqueId (x@(NTree (XDTD ATTLIST al) _):xs) used
          = if ename `elem` used
	    then err ("Element \""++ ename ++ "\" already has attribute of type "++
		       "ID, another attribute \""++ attName al ++ "\" of type ID is "++
		       "not permitted.") x
		 ++
		 checkForUniqueId xs used

	    else checkForUniqueId xs (ename : used)
	    where
	    ename = elemName al

      checkForUniqueId [] _ = []

      checkForUniqueId nd _
          = error ("checkForUniqueId: illegeal parameter:\n" ++ show nd)

      --------------------------------------------------------------------------
      -- 5. Validate that there exists only one NOTATION attribute for an element
      checkForUniqueNotation :: XmlTrees -> [String] -> XmlTrees
      checkForUniqueNotation (x@(NTree (XDTD ATTLIST al) _):xs) used
          = if ename `elem` used
	    then err ("Element \""++ elemName al ++ "\" already has attribute of type "++
		       "NOTATION, another attribute \""++ attName al ++ "\" of type NOTATION "++
		       "is not permitted.") x
		 ++
		 checkForUniqueNotation xs used

	    else checkForUniqueNotation xs (ename : used)
	    where
	    ename = elemName al

      checkForUniqueNotation [] _ = []

      checkForUniqueNotation nd _
          = error ("checkForUniqueNotation: illegeal parameter:\n" ++ show nd)

      --------------------------------------------------------------------------
      -- 6. Validate that ID attributes have the type #IMPLIED or #REQUIRED
      checkIdKindConstraint :: XmlFilter
      checkIdKindConstraint nd@(NTree (XDTD ATTLIST al) _)
          = if (attKind == k_implied) || (attKind == k_required)
	    then []
	    else err ("ID attribute \""++ attName al ++"\" must have a declared default "++
	              "of \"#IMPLIED\" or \"REQUIRED\"") nd
	    where
	    attKind = lookup1 a_kind al

      checkIdKindConstraint nd
          = error ("checkIdKindConstraint: illegeal parameter:\n" ++ show nd)


      --------------------------------------------------------------------------
      -- 7. Validate that all referenced notations are declared
      checkNotationDeclaration :: [String] -> XmlFilter
      checkNotationDeclaration notations (NTree (XDTD ATTLIST al) cs)
          = checkNotations $$ cs
	  where
	  checkNotations :: XmlFilter
	  checkNotations nd@(NTree (XDTD NAME al') _)
	      = if notation `elem` notations
	        then []
	        else err ("The notation \""++ notation ++"\" must be declared when "++
		          "referenced in the notation type list for attribute \""++ attName al ++
			  "\" of element \""++ elemName al ++"\".") nd
                where
	        notation = lookup1 a_name al'

          checkNotations nd
              = error ("checkNotations: illegeal parameter:\n" ++ show nd)

      checkNotationDeclaration _ nd
          = error ("checkNotationDeclaration: illegeal parameter:\n" ++ show nd)

      --------------------------------------------------------------------------
      -- 8. Validate that notations are not declared for EMPTY elements
      checkNoNotationForEmptyElement :: [String] -> XmlFilter
      checkNoNotationForEmptyElement emptyElems nd@(NTree (XDTD ATTLIST al) _)
          = if (elemName al) `elem` emptyElems
	    then err ("Attribute \""++ attName al ++"\" of type NOTATION must not be "++
	              "declared on the element \""++ elemName al ++"\" declared EMPTY.")
		      nd
	    else []

      checkNoNotationForEmptyElement _ nd
          = error ("checkNoNotationForEmptyElement: illegeal parameter:\n" ++ show nd)

      --------------------------------------------------------------------------
      -- 9. Validate that default values meet the lexical constraints of the attribute types
      checkDefaultValueTypes :: XmlTrees -> XmlFilter
      checkDefaultValueTypes dtdPart' n@(NTree (XDTD ATTLIST _) _)
          = checkAttributeValue dtdPart' n n

      checkDefaultValueTypes _ nd
          = error ("checkDefaultValueTypes: illegeal parameter:\n" ++ show nd)



-- ------------------------------------------------------------

-- |
-- Removes doublicate declarations from the DTD, which first declaration is
-- binding. This is the case for ATTLIST and ENTITY declarations.
--
--    - returns : A function that replaces the children of DOCTYPE nodes by a list
--               where all multiple declarations are removed.

removeDoublicateDefs :: XmlFilter
removeDoublicateDefs n@(NTree (XDTD DOCTYPE _) cs)
    = replaceChildren (removeDoubleDefs [] cs) n
      where
      removeDoubleDefs :: [String] -> XmlTrees -> XmlTrees
      removeDoubleDefs used (x@(NTree (XDTD ATTLIST al) _):xs)
          = if elemAttr `elem` used
            then removeDoubleDefs used xs
            else x
	         :
	         removeDoubleDefs (elemAttr : used) xs
            where
	    elemAttr = elemName ++ "|" ++ attrName
            attrName = lookup1 a_value al
            elemName = lookup1 a_name al


      removeDoubleDefs used (x@(NTree (XDTD ENTITY al) _):xs)
          = if name `elem` used
            then removeDoubleDefs used xs
            else x
	         :
	         removeDoubleDefs (name : used) xs
            where
            name = lookup1 a_name al


      removeDoubleDefs used (x:xs)
          = x : removeDoubleDefs used xs

      removeDoubleDefs _ []
          = []


removeDoublicateDefs n
    = [n]

