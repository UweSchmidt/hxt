-- ------------------------------------------------------------

{- |
   This module provides functions for validating the DTD of XML documents
   represented as XmlTree.

   Unlike other popular XML validation tools the validation process returns
   a list of errors instead of aborting after the first error was found.


   Unlike validation of the document, the DTD branch is traversed four times:

    - Validation of Notations

    - Validation of Unparsed Entities

    - Validation of Element declarations

    - Validation of Attribute
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Monad.DTDValidation.DTDValidation
    ( removeDoublicateDefs
    , validateDTD
    )
where

import           Text.XML.HXT.Monad.DTDValidation.AttributeValueValidation
import           Text.XML.HXT.Monad.DTDValidation.TypeDefs

-- ------------------------------------------------------------

-- |
-- Validate a DTD.
--
--    - returns : a functions which takes the DTD subset of the XmlTree, checks
--                  if the DTD is valid and returns a list of errors

validateDTD :: XmlArrow
validateDTD -- dtdPart
    = isDTDDoctype
      `guards`
      ( listA getChildren
        >=>
        ( validateParts $<< (getNotationNames &=& getElemNames) )
      )
    where
    validateParts notationNames elemNames
        = validateNotations
          <++>
          validateEntities notationNames
          <++>
          validateElements elemNames
          <++>
          validateAttributes elemNames notationNames

    getNotationNames    :: LA [XmlTree] [String]
    getNotationNames    = listA $ unlistA >=> isDTDNotation >=> getDTDAttrValue a_name

    getElemNames        :: LA [XmlTree] [String]
    getElemNames        = listA $ unlistA >=> isDTDElement  >=> getDTDAttrValue a_name

-- ------------------------------------------------------------

checkName       :: String -> SLA [String] XmlTree XmlTree -> SLA [String] XmlTree XmlTree
checkName name msg
    = ifA ( getState
            >=>
            isA (name `elem`)
          )
      msg
      (nextState (name:) >=> none)

-- ------------------------------------------------------------

-- |
-- Validation of Notations, checks if all notation names are unique.
-- Validity constraint: Unique Notation Name (4.7 \/ p.44 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - returns : a list of errors

validateNotations :: LA XmlTrees XmlTree
validateNotations
    = fromSLA [] ( unlistA
                   >=>
                   isDTDNotation
                   >=>
                   (checkForUniqueNotation $< getDTDAttrl)
                 )
      where
      checkForUniqueNotation :: Attributes -> SLA [String] XmlTree XmlTree
      checkForUniqueNotation al
          = checkName name $
            err ( "Notation "++ show name ++ " was already specified." )
          where
          name = dtd_name al

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

validateEntities        :: [String] -> LA XmlTrees XmlTree
validateEntities notationNames
    = ( fromSLA [] ( unlistA
                     >=>
                     isDTDEntity
                     >=>
                     (checkForUniqueEntity $< getDTDAttrl)
                   )
      )
      <++>
      ( unlistA
        >=>
        isUnparsedEntity
        >=>
        (checkNotationDecl $< getDTDAttrl)
      )
      where

      -- Check if entities are declared multiple times

      checkForUniqueEntity      :: Attributes -> SLA [String] XmlTree XmlTree
      checkForUniqueEntity al
          = checkName name $
            warn ( "Entity "++ show name ++ " was already specified. " ++
                    "First declaration will be used." )
          where
          name = dtd_name al

      -- Find unparsed entities for which no notation is specified

      checkNotationDecl         :: Attributes -> XmlArrow
      checkNotationDecl al
          | notationName `elem` notationNames
              = none
          | otherwise
              = err ( "The notation " ++ show notationName ++ " must be declared " ++
                      "when referenced in the unparsed entity declaration for " ++
                      show upEntityName ++ "."
                    )
          where
          notationName = lookup1 k_ndata al
          upEntityName = dtd_name  al

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


validateElements        :: [String] -> LA XmlTrees XmlTree
validateElements elemNames -- dtdPart
    = ( fromSLA [] ( unlistA
                     >=>
                     isDTDElement
                     >=>
                     (checkForUniqueElement $< getDTDAttrl)
                   )
      )
      <++>
      ( unlistA
        >=>
        isMixedContentElement
        >=>
        (checkMixedContent $< getDTDAttrl)
      )
      <++>
      ( unlistA
        >=>
        isDTDElement
        >=>
        (checkContentModel elemNames $< getDTDAttrl)
      )
      where

      -- Validates that an element is not declared multiple times

      checkForUniqueElement :: Attributes -> SLA [String] XmlTree XmlTree
      checkForUniqueElement al
          = checkName name $
            err ( "Element type " ++ show name ++
                  " must not be declared more than once." )
          where
          name = dtd_name al

      -- Validates that an element name only appears once in a mixed-content declaration

      checkMixedContent :: Attributes -> XmlArrow
      checkMixedContent al
          = fromSLA [] ( getChildren
                         >=>
                         getChildren
                         >=>
                         isDTDName
                         >=>
                         (check $< getDTDAttrl)
                       )
            where
            elemName = dtd_name al
            check al'
                = checkName name $
                  err ( "The element type " ++ show name ++
                         " was already specified in the mixed-content model of the element declaration " ++
                         show elemName ++ "." )
                where
                name = dtd_name al'

      -- Issues a warning if an element mentioned in a content model is not
      -- declared in the DTD.
      checkContentModel :: [String] -> Attributes -> XmlArrow
      checkContentModel names al
          | cm `elem` [v_children, v_mixed]
              = getChildren >=> checkContent
          | otherwise
              = none
          where
          elemName = dtd_name al
          cm       = dtd_type al

          checkContent :: XmlArrow
          checkContent
              = choiceA
                [ isDTDName    :-> ( checkName' $< getDTDAttrl )
                , isDTDContent :-> ( getChildren >=> checkContent )
                , this         :-> none
                ]
              where
              checkName' al'
                  | childElemName `elem` names
                      = none
                  | otherwise
                      = warn ( "The element type "++ show childElemName ++
                               ", used in content model of element "++ show elemName ++
                               ", is not declared."
                             )
                  where
                  childElemName = dtd_name al'

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

validateAttributes :: [String] -> [String] -> LA XmlTrees XmlTree
validateAttributes elemNames notationNames
    = -- 1. Find attributes for which no elements are declared
      ( runCheck this (checkDeclaredElements elemNames) )
      <++>
      -- 2. Find attributes which are declared more than once
      ( runNameCheck this checkForUniqueAttributeDeclaration )
      <++>
      -- 3. Find enumerated attribute types which nmtokens are declared more than once
      ( runCheck (isEnumAttrType `orElse` isNotationAttrType) checkEnumeratedTypes )
      <++>
      -- 4. Validate that there exists only one ID attribute for an element
      ( runNameCheck isIdAttrType checkForUniqueId )
      <++>
      -- 5. Validate that there exists only one NOTATION attribute for an element
      ( runNameCheck isNotationAttrType checkForUniqueNotation )
      <++>
      -- 6. Validate that ID attributes have the type #IMPLIED or #REQUIRED
      ( runCheck isIdAttrType checkIdKindConstraint )
      <++>
      -- 7. Validate that all referenced notations are declared
      ( runCheck isNotationAttrType (checkNotationDeclaration notationNames) )
      <++>
      -- 8. Validate that notations are not declared for EMPTY elements
      ( checkNoNotationForEmptyElements $< listA ( unlistA
                                                   >=>
                                                   isEmptyElement
                                                   >=>
                                                   getDTDAttrValue a_name
                                                 )
      )
      <++>
      -- 9. Validate that the default value matches the lexical constraints of it's type
      ( checkDefaultValueTypes $< this )

      where
      -- ------------------------------------------------------------
      -- control structures

      runCheck select check
          = unlistA >=> isDTDAttlist
            >=>
            select
            >=>
            (check $< getDTDAttrl)

      runNameCheck select check
          = fromSLA [] $ runCheck select check

      --------------------------------------------------------------------------

      -- 1. Find attributes for which no elements are declared

      checkDeclaredElements :: [String] -> Attributes -> XmlArrow
      checkDeclaredElements elemNames' al
          | en `elem` elemNames'
              = none
          | otherwise
              = warn ( "The element type \""++ en ++ "\" used in dclaration "++
                       "of attribute \""++ an ++"\" is not declared."
                     )
          where
          en = dtd_name al
          an = dtd_value al

      --------------------------------------------------------------------------

      -- 2. Find attributes which are declared more than once

      checkForUniqueAttributeDeclaration ::  Attributes -> SLA [String] XmlTree XmlTree
      checkForUniqueAttributeDeclaration al
          = checkName name $
            warn ( "Attribute \""++ aname ++"\" for element type \""++
                   ename ++"\" is already declared. First "++
                   "declaration will be used." )
          where
          ename = dtd_name al
          aname = dtd_value al
          name  = ename ++ "|" ++ aname

      --------------------------------------------------------------------------

      -- 3. Find enumerated attribute types which nmtokens are declared more than once

      checkEnumeratedTypes :: Attributes -> XmlArrow
      checkEnumeratedTypes al
          = fromSLA [] ( getChildren
                         >=>
                         isDTDName
                         >=>
                         (checkForUniqueType $< getDTDAttrl)
                       )
          where
          checkForUniqueType :: Attributes -> SLA [String] XmlTree XmlTree
          checkForUniqueType al'
              = checkName nmtoken $
                warn ( "Nmtoken \""++ nmtoken ++"\" should not "++
                       "occur more than once in attribute \""++ dtd_value al ++
                       "\" for element \""++ dtd_name al ++ "\"." )
              where
              nmtoken = dtd_name al'

      --------------------------------------------------------------------------

      -- 4. Validate that there exists only one ID attribute for an element

      checkForUniqueId :: Attributes -> SLA [String] XmlTree XmlTree
      checkForUniqueId al
          = checkName ename $
            err ( "Element \""++ ename ++ "\" already has attribute of type "++
                  "ID, another attribute \""++ dtd_value al ++ "\" of type ID is "++
                  "not permitted." )
          where
          ename = dtd_name al

      --------------------------------------------------------------------------

      -- 5. Validate that there exists only one NOTATION attribute for an element

      checkForUniqueNotation :: Attributes -> SLA [String] XmlTree XmlTree
      checkForUniqueNotation al
          = checkName ename $
            err ( "Element \""++ ename ++ "\" already has attribute of type "++
                  "NOTATION, another attribute \""++ dtd_value al ++ "\" of type NOTATION "++
                  "is not permitted." )
          where
          ename = dtd_name al

      --------------------------------------------------------------------------

      -- 6. Validate that ID attributes have the type #IMPLIED or #REQUIRED

      checkIdKindConstraint :: Attributes -> XmlArrow
      checkIdKindConstraint al
          | attKind `elem` [k_implied, k_required]
              = none
          | otherwise
              = err ( "ID attribute \""++ dtd_value al ++"\" must have a declared default "++
                      "of \"#IMPLIED\" or \"REQUIRED\"")
          where
          attKind = dtd_kind al


      --------------------------------------------------------------------------

      -- 7. Validate that all referenced notations are declared

      checkNotationDeclaration :: [String] -> Attributes -> XmlArrow
      checkNotationDeclaration notations al
          = getChildren
            >=>
            isDTDName
            >=>
            (checkNotations $< getDTDAttrl)
          where
          checkNotations :: Attributes -> XmlArrow
          checkNotations al'
              | notation `elem` notations
                  = none
              | otherwise
                  = err ( "The notation \""++ notation ++"\" must be declared when "++
                          "referenced in the notation type list for attribute \""++ dtd_value al ++
                          "\" of element \""++ dtd_name al ++"\"."
                        )
              where
              notation = dtd_name al'

      --------------------------------------------------------------------------

      -- 8. Validate that notations are not declared for EMPTY elements

      checkNoNotationForEmptyElements :: [String] -> LA XmlTrees XmlTree
      checkNoNotationForEmptyElements emptyElems
          = unlistA
            >=>
            isDTDAttlist
            >=>
            isNotationAttrType
            >=>
            (checkNoNotationForEmptyElement $< getDTDAttrl)
          where
          checkNoNotationForEmptyElement :: Attributes -> XmlArrow
          checkNoNotationForEmptyElement al
              | ename `elem` emptyElems
                  = err ( "Attribute \""++ dtd_value al ++"\" of type NOTATION must not be "++
                          "declared on the element \""++ ename ++"\" declared EMPTY."
                        )
              | otherwise
                  = none
              where
              ename = dtd_name al

      --------------------------------------------------------------------------

      -- 9. Validate that default values meet the lexical constraints of the attribute types

      checkDefaultValueTypes :: XmlTrees -> LA XmlTrees XmlTree
      checkDefaultValueTypes dtdPart'
          = unlistA >=> isDTDAttlist
            >=>
            isDefaultAttrKind
            >=>
            (checkAttributeValue dtdPart' $< this)

-- ------------------------------------------------------------

-- |
-- Removes doublicate declarations from the DTD, which first declaration is
-- binding. This is the case for ATTLIST and ENTITY declarations.
--
--    - returns : A function that replaces the children of DOCTYPE nodes by a list
--               where all multiple declarations are removed.

removeDoublicateDefs :: XmlArrow
removeDoublicateDefs
    = replaceChildren
      ( fromSLA [] ( getChildren
                     >=>
                     choiceA [ isDTDAttlist :-> (removeDoubleAttlist $< getDTDAttrl)
                             , isDTDEntity  :-> (removeDoubleEntity  $< getDTDAttrl)
                             , this         :-> this
                             ]
                   )
      )
      `whenA`
      isDTDDoctype
    where
    checkName' n'
        = ifA ( getState
                >=>
                isA (n' `elem`)
              )
          none
          (this >=> perform (nextState (n':)))

    removeDoubleAttlist :: Attributes -> SLA [String] XmlTree XmlTree
    removeDoubleAttlist al
        = checkName' elemAttr
        where
        elemAttr = elemName ++ "|" ++ attrName
        attrName = dtd_value al
        elemName = dtd_name al

    removeDoubleEntity  :: Attributes -> SLA [String] XmlTree XmlTree
    removeDoubleEntity al
        = checkName' (dtd_name al)

-- ------------------------------------------------------------
