-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG.Simplification
   Copyright  : Copyright (C) 2008 Torben Kuseler, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   The modul creates the simplified form of a Relax NG schema.
   See also chapter 4 of the Relax NG specification.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.RelaxNG.Simplification
  ( createSimpleForm
  , getErrors
  , resetStates
  )

where


import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.Edit                  ( removeWhiteSpace
                                                )
import Text.XML.HXT.Arrow.Namespace             ( processWithNsEnv
                                                )
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.BasicArrows
import Text.XML.HXT.RelaxNG.CreatePattern
import Text.XML.HXT.RelaxNG.DataTypeLibraries
import Text.XML.HXT.RelaxNG.Utils
import Text.XML.HXT.RelaxNG.Validation
import Text.XML.HXT.RelaxNG.Schema        as S
import Text.XML.HXT.RelaxNG.SchemaGrammar as SG

import Data.Maybe
    ( fromJust
    , fromMaybe
    )
import Data.List
    ( (\\)
    )
import Data.Map
    ( Map, fromListWithKey, toList )

infixr 1 !>>>

-- ------------------------------------------------------------

{-
- 4.1. Annotations: Foreign attributes and elements are removed.
- 4.2. Whitespace:
    - For each element other than value and param, each child that is a string
      containing only whitespace characters is removed.
    - Leading and trailing whitespace characters are removed from the value of each name,
      type and combine attribute and from the content of each name element.
- 4.3. datatypeLibrary attribute:
    - The value of each datatypeLibary attribute is transformed by escaping disallowed characters
    - For any data or value element that does not have a datatypeLibrary attribute,
      a datatypeLibrary attribute is added.
    - Any datatypeLibrary attribute that is on an element other than data or value is removed.
- 4.4. type attribute of value element
-}
simplificationStep1 :: IOSArrow XmlTree XmlTree
simplificationStep1
    = ( {-
        - 4.5. href attribute
        - The value of the href attribute on an externalRef or include element is first
        - transformed by escaping disallowed characters
        - The URI reference is then resolved into an absolute form
        - The value of the href attribute will be used to construct an element.
        -}
        ( processHref $< getBaseURI )
        >>>
        -- 4.10 QNames
        processWithNsEnv processEnvNames (toNsEnv [("xml", xmlNamespace)])
        >>>
        -- 4.4 For any data or value element that does not have a datatypeLibrary attribute,
        -- a datatypeLibrary attribute is added.
        -- Wird vorgezogen, da danach der Rest in einem Baumdurchlauf erledigt werden kann
        processdatatypeLib ""
        >>>
        processTopDownWithAttrl
        (
         ( -- 4.1 Foreign attributes and elements are removed
           none
           `when`
           ( ( isElem >>> neg isRoot
               >>>
               getNamespaceUri
               >>>
               isA (\ uri -> (not $ compareURI uri relaxNamespace))
             )
             `orElse`
             ( isAttr
               >>>
               getNamespaceUri
               >>>
               isA (\ uri -> (uri /= "" && (not $ compareURI uri relaxNamespace)))
             )
           )
         )
         >>>
         ( -- 4.2 For each element other than value and param, each child that
           -- is a string containing only whitespace characters is removed.
           ( processChildren removeWhiteSpace
             `whenNot`
             (isRngParam `orElse` isRngValue)
           )
           `when` isElem
         )
         >>>
         ( -- 4.2 Leading and trailing whitespace characters are removed from the value
           -- of each name, type and combine attribute ...
           changeAttrValue normalizeWhitespace
           `when`
           ( isRngAttrName `orElse` isRngAttrType `orElse` isRngAttrCombine)
         )
         >>>
         ( -- 4.2 ... and from the content of each name element.
           processChildren (changeText normalizeWhitespace)
           `when`
           isRngName
         )
         >>>
         ( -- 4.3 The value of each datatypeLibary attribute is transformed
           -- by escaping disallowed characters
           changeAttrValue escapeURI
           `when`
           isRngAttrDatatypeLibrary
         )
         >>>
         ( -- The value of the datatypeLibary attribute has to be a valid URI
           ( mkRelaxError "" $< ( getRngAttrDatatypeLibrary
                                  >>>
                                  arr (\ a -> ( "datatypeLibrary attribute: " ++
                                                a ++ " is not a valid URI"
                                              )
                                      )
                                )
           )
           `when`
           ( isElem
             >>>
             hasRngAttrDatatypeLibrary
             >>>
             getRngAttrDatatypeLibrary >>> isA (not . isRelaxAnyURI)
           )
         )
         >>>
         ( -- 4.3 Any datatypeLibrary attribute that is on an element
           -- other than data or value is removed.
           removeAttr "datatypeLibrary"
           `when`
           ( isElem
             >>>
             neg (isRngData `orElse` isRngValue)
             >>>
             hasRngAttrDatatypeLibrary
           )
         )
         >>>
         ( -- 4.4 For any value element that does not have a type attribute,
           -- a type attribute is added with value token and the value of
           -- the datatypeLibrary attribute is changed to the empty string.
           ( addAttr "type" "token"
             >>>
             addAttr "datatypeLibrary" ""
           )
           `when`
           ( isRngValue >>> neg hasRngAttrType )
         )
        )
      ) `when` collectErrors
    where
    processHref :: String -> IOSArrow XmlTree XmlTree
    processHref uri
        = processChildren
          ( choiceA
            [ ( isElem >>> hasAttr "xml:base" )
              :-> ( ifA ( isExternalRefInclude >>> hasRngAttrHref )
                    ( -- The value of the href attribute is transformed by
                      -- escaping disallowed characters
                      (processAttrl (changeAttrValue escapeURI `when` isRngAttrHref))
                      >>> -- compute the new base uri from the old uri and the href attribute
                      (addAttr "href" $< (absURI "href" $< (absURI "xml:base" uri)))
                      >>>
                      (processHref $< absURI "xml:base" uri)
                    ) -- element without a href attribute, just compute the new base uri
                    (processHref $< absURI "xml:base" uri)
                  )
            , ( isExternalRefInclude >>> hasRngAttrHref )
              :-> ( -- The value of the href attribute is transformed by
                    -- escaping disallowed characters
                    (processAttrl (changeAttrValue escapeURI `when` isRngAttrHref))
                    >>>
                    (addAttr "href" $< absURI "href" uri)
                  )
            , this
              :-> processHref uri
            ]
          )
        where
        absURI :: String -> String -> IOSArrow XmlTree String
        absURI attrName u
            = ( getAttrValue attrName
                >>>
                arr (\ a -> fromMaybe "" (expandURIString a u))
                >>> -- the uri should not have a fragment-identifier (4.5)
                ( arr ("illegal URI, fragment identifier not allowed: " ++)
                  `whenNot`
                  (getFragmentFromURI >>> isA null)
                )
              )

    processEnvNames :: NsEnv -> IOSArrow XmlTree XmlTree
    processEnvNames env
        = ( ( (replaceQNames env $< getAttrValue "name")
              `when`
              ( (isRngElement `orElse` isRngAttribute)
                >>>
                hasRngAttrName
              )
            )
            >>>
            ( (addAttrl (getBaseURI >>> createAttrL))
              `when`
              isRngValue
            )
          )
        where

        createAttrL :: IOSArrow String XmlTree
        createAttrL
            = setBaseUri
              <+>
              ( fromLA $ txt "" >>> catA (map createAttr env) )
            where
{- old stuff
            createAttr :: (XName, XName) -> LA XmlTree XmlTree
            createAttr (pre, uri)
                = mkRngAttr nm (constA $ show uri)
                where
                nm  | isNullXName pre   = contextAttributesDefault
                    | otherwise         = contextAttributes ++ show pre
-}

            createAttr :: (XName, XName) -> LA XmlTree XmlTree
            createAttr (pre, uri)
                = mkAttr qn (txt (unXN uri))
                where
                qn :: QName
                qn  | isNullXName pre   = mkName $ contextAttributesDefault
                    | otherwise         = mkName $ contextAttributes ++ unXN pre

            setBaseUri :: IOSArrow String XmlTree
            setBaseUri = mkRngAttrContextBase this

        replaceQNames :: NsEnv -> String -> IOSArrow XmlTree XmlTree
        replaceQNames env' name
            | null px                                   -- no prefix, nothing to do
                = this
            | null ns                                   -- prefix there, but no namespace
                = mkRelaxError "" ( "No namespace mapping for the prefix " ++ show px ++
                                    " in the context of element: " ++ show name ++
                                    ", namespace env is " ++ show (map (unXN *** unXN) env')
                                  )
            | otherwise                                 -- build universal name
                = addAttr "name" (universalName qn)
            where
            qn = setNamespace env' . mkName $ name
            px = namePrefix   qn
            ns = namespaceUri qn

    -- The value of the added datatypeLibrary attribute is the value of the
    -- datatypeLibrary attribute of the nearest ancestor element that
    -- has a datatypeLibrary attribute, or the empty string
    -- if there is no such ancestor.
    processdatatypeLib :: (ArrowXml a) => String -> a XmlTree XmlTree
    processdatatypeLib lib
        = processChildren $
          choiceA
          [ hasRngAttrDatatypeLibrary
            :-> ( processdatatypeLib $< getRngAttrDatatypeLibrary )             -- set the new datatypeLibrary value

          , ( (isRngData `orElse` isRngValue)
              >>>
              neg hasRngAttrDatatypeLibrary
            )
            :-> ( addAttr "datatypeLibrary" lib                                 -- add a datatypeLibrary attribute
                  >>>
                  processdatatypeLib lib
                )

          , this
            :-> processdatatypeLib lib
          ]
          `when` isElem

-- ------------------------------------------------------------


{-
- 4.5. href attribute
    - see simplificationStep1
- 4.6. externalRef element
    - An element is constructed using the URI reference
      that is the value of href attribute
    - This element must match the syntax for pattern.
    - The element is transformed by recursively applying the rules from
      this subsection and from previous subsections of this section.
    - This must not result in a loop.
    - Any ns attribute on the externalRef element is transferred
      to the referenced element if the referenced element does
      not already have an ns attribute.
    - The externalRef element is then replaced by the referenced element.
- 4.7. include element
    - An element is constructed using the URI reference that is the
      value of href attribute
    - This element must be a grammar element, matching the syntax for grammar.
    - This grammar element is transformed by recursively applying the rules
      from this subsection and from previous subsections of this section.
    - This must not result in a loop.

    - If the include element has a start component, then the grammar element
      must have a start component.
    - If the include element has a start component, then all start components
      are removed from the grammar element.
    - If the include element has a define component, then the grammar element
      must have a define component with the same name.
    - For every define component of the include element, all define components
      with the same name are removed from the grammar element.

    - The include element is transformed into a div element.
    - The attributes of the div element are the attributes of the
      include element other than the href attribute.
    - The children of the div element are the grammar element
    - The grammar element is then renamed to div.
-}

simplificationStep2 :: Bool -> Bool -> [Uri] -> [Uri] -> IOSArrow XmlTree XmlTree
simplificationStep2 validateExternalRef validateInclude extHRefs includeHRefs =
  ( processTopDown (
      ( (importExternalRef $<< (getRngAttrNs &&& getRngAttrHref))
        `when`
        isRngExternalRef
      )
      >>>
      ( (importInclude $< getAttrValue "href")
        `when`
        isRngInclude
      )
    )
  ) `when` collectErrors
  where
  -- returns the contents of the (validated) schema
  -- or a relax error
  importExternalRef     :: String -> String -> IOSArrow XmlTree XmlTree
  importExternalRef ns href
      | href `elem` extHRefs
          = mkRelaxError ""
            (  "loop in externalRef-Pattern, " ++ formatStringListArr (reverse $ href:extHRefs) )
      | otherwise
          = readForRelax href
            >>>
            ( mkRelaxError "" (show href ++ ": can't read URI, referenced in externalRef-Pattern")
              `whenNot`
              documentStatusOk
            )
            !>>>
            ( if validateExternalRef                                            -- if validation parameters are set
              then ( mkRelaxError ""
                     ( "The content of the schema " ++ show href ++
                       ", referenced in externalRef does not " ++
                       "match the syntax for pattern"
                     )
                     `whenNot`
                     validateWithRelax S.relaxSchemaArrow                       -- the referenced schema is validated with respect to
                   )
                else this
              )
            !>>>
            ( simplificationStep1                                               -- perform the transformations from previous steps
              >>>
              simplificationStep2 validateExternalRef validateInclude (href:extHRefs) includeHRefs
              >>>
              getChildren                                                       -- remove the root node
              >>>
              isElem
              >>>
              ( -- Any ns attribute on the externalRef element
                -- is transferred to the referenced element
                addRngAttrNs ns
                `when`
                (getRngAttrNs >>> isA (\ a -> a == "" && ns /= ""))
              )
            )
            >>>
            traceDoc ("imported external ref: " ++ show href)

  importInclude :: String -> IOSArrow XmlTree XmlTree
  importInclude href
      | href `elem` includeHRefs
          = mkRelaxError ""
            ( "loop in include-Pattern, " ++ formatStringListArr (reverse $ href:includeHRefs) )
      | otherwise
          = processInclude' $< newDoc
      where
      processInclude' newDoc'
          | not . null . runLA isRngRelaxError $ newDoc'
              = constA newDoc'
          | otherwise
              = processInclude href newDoc'
      newDoc
          = readForRelax href
            >>>
            ( mkRelaxError "" (show href ++ ": can't read URI, referenced in include-Pattern")
              `whenNot`
              documentStatusOk
            )
            !>>>
            ( if validateInclude                                        -- if validation parameters are set
              then ( mkRelaxError ""
                     ( "The content of the schema " ++ show href ++
                       ", referenced in include does not " ++
                       "match the syntax for grammar"
                     )
                     `whenNot`
                     validateWithRelax SG.relaxSchemaArrow              -- the referenced schema is validated with respect to
                   )
              else this
            )
            !>>>
            ( simplificationStep1                                       -- perform the transformations from previous steps
              >>>
              simplificationStep2 validateExternalRef validateInclude extHRefs (href:includeHRefs)
              >>>
              getChildren                                               -- remove the root node
              >>>
              isElem
            )

  processInclude :: String -> XmlTree -> IOSArrow XmlTree XmlTree
  processInclude href newDoc
    = -- The include element is transformed into a div element.
      setRngNameDiv
      >>>
      -- The attributes of the div element are the attributes of
      -- the include element other than the href attribute.
      removeAttr "href"
      >>>
      checkInclude href newDoc


  insertNewDoc :: XmlTree -> Bool -> [String] -> IOSArrow XmlTree XmlTree
  insertNewDoc newDoc hasStart defNames
    = insertChildrenAt 0 $
        constA newDoc
        >>>
        -- If the include element has a start component, then all start components
        -- are removed from the grammar element.
        (removeStartComponent `whenP` (const hasStart))
        >>>
        -- For every define component of the include element, all define components
        -- with the same name are removed from the grammar element.
        ((removeDefineComponent defNames) `whenP` (const $ defNames /= []))
        >>>
        -- The grammar element is then renamed to div.
        setRngNameDiv


  checkInclude :: String -> XmlTree -> IOSArrow XmlTree XmlTree
  checkInclude href newDoc
    = ifA ( -- If the include element has a start component, then the grammar element
            -- must have a start component.
            hasStartComponent &&& (constA newDoc >>> hasStartComponent)
            >>>
            isA (\ (a, b) -> if a then b else True)
          )
        ( ifA ( -- If the include element has a define component, then the grammar element
                -- must have a define component with the same name.
                getDefineComponents &&& (constA newDoc >>> getDefineComponents)
                >>>
                isA (\ (a, b) -> (diff a b) == [])
              )
            (insertNewDoc newDoc $<< hasStartComponent &&& getDefineComponents)
            ( mkRelaxError ""
              ( "Define-pattern missing in schema " ++ show href ++
                ", referenced in include-pattern"
              )
            )
        )
        ( mkRelaxError ""
          ( "Grammar-element without a start-pattern in schema " ++
            show href ++ ", referenced in include-pattern"
          )
        )
    where
    diff a b = (noDoubles a) \\ (noDoubles b)

  removeStartComponent :: IOSArrow XmlTree XmlTree
  removeStartComponent
    = processChildren $
        choiceA [
          isRngStart :-> none,
          isRngDiv   :-> removeStartComponent,
          this       :-> this
        ]

  removeDefineComponent :: [String] -> IOSArrow XmlTree XmlTree
  removeDefineComponent defNames
    = processChildren $
        choiceA [
          ( isRngDefine
            >>>
            getRngAttrName
            >>>
            isA (\n -> elem n defNames))          :-> none,
          (isElem >>> getName >>> isA (== "div")) :-> (removeDefineComponent defNames),
          (constA "foo" >>> isA (== "foo"))       :-> this
        ]

  hasStartComponent :: IOSArrow XmlTree Bool
  hasStartComponent = listA hasStartComponent' >>> arr (any id)
    where
    hasStartComponent' :: IOSArrow XmlTree Bool
    hasStartComponent'
      = getChildren
        >>>
        choiceA [
          isRngStart :-> (constA True),
          isRngDiv   :-> hasStartComponent',
          this       :-> (constA False)
        ]

  getDefineComponents :: IOSArrow XmlTree [String]
  getDefineComponents = listA getDefineComponents'
                        >>>
                        arr (\xs -> [x | x <- xs, x /= ""])
    where
    getDefineComponents' :: IOSArrow XmlTree String
    getDefineComponents'
      = getChildren
        >>>
        choiceA
        [ isRngDefine :-> getRngAttrName
        , isRngDiv    :-> getDefineComponents'
        , this        :-> constA ""
        ]


-- ------------------------------------------------------------


{-
- 4.8. name attribute of element and attribute elements
    - The name attribute on an element or attribute element
      is transformed into a name child element.
    - If an attribute element has a name attribute but no ns attribute,
      then an ns="" attribute is added to the name child element.
- 4.9. ns attribute
    - For any name, nsName or value element that does not have
      an ns attribute, an ns attribute is added.
    - any ns attribute that is on an element other than name,
      nsName or value is removed.
- 4.10. QNames
    - For any name element containing a prefix, the prefix is removed
      and an ns attribute is added replacing any existing ns attribute.
    - The value of the added ns attribute is the value to which the
      namespace map of the context of the name element maps the prefix.
    - The context must have a mapping for the prefix.
-}
simplificationStep3 :: IOSArrow XmlTree XmlTree
simplificationStep3 =
  ( processTopDown (
      ( -- 4.8 The name attribute on an element or attribute
        -- element is transformed into a name child element
        ( insertChildrenAt 0 (mkRngName none (txt $< getRngAttrName))
        )
        >>>
        ( -- 4.8 If an attribute element has a name attribute but no ns attribute,
          --  then an ns="" attribute is added to the name child element
           (processChildren (addRngAttrNs "" `when` isRngName))
           `when`
           (isRngAttribute >>> hasRngAttrName >>> neg hasRngAttrNs)
        )
        >>>
        removeAttr "name"
      )
      `when`
      ( (isRngElement `orElse` isRngAttribute) >>> hasRngAttrName )
    )
    >>>
    -- 4.9 For any name, nsName or value element that does not have
    -- an ns attribute, an ns attribute is added.
    processnsAttribute ""
    >>>
    processTopDown (
      ( -- 4.9 any ns attribute that is on an element other than name,
        -- nsName or value is removed.
        rmRngAttrNs
        `when`
        ( isElem
          >>>
          neg (isRngName `orElse` isRngNsName `orElse` isRngValue)
        )
      )
      >>>
      ( -- 4.10 For any name element containing a prefix, the prefix is removed and an ns attribute
        -- is added replacing any existing ns attribute.
        ( replaceNameAttr $< (getChildren >>> isText >>> getText) )
        `when`
        isRngName
      )
    )
  ) `when` collectErrors
  where
  replaceNameAttr :: (ArrowXml a) => String -> a XmlTree XmlTree
  replaceNameAttr name
      | '}' `elem` name
          = ( addRngAttrNs pre
              >>>
              processChildren (changeText $ const local)
            )
      | otherwise
          = this
    where
    (pre', local') = span (/= '}') name
    pre            = tail pre'
    local          = tail local'

  processnsAttribute :: String -> IOSArrow XmlTree XmlTree
  processnsAttribute name
    = processChildren $
        choiceA
        [ (isElem >>> hasRngAttrNs)                     -- set the new ns attribute value
          :-> (processnsAttribute $< getRngAttrNs)
        , ( isNameNsNameValue
            >>>
            neg hasRngAttrNs
          )                                             -- For any name, nsName or value element that does not have
          :-> ( addRngAttrNs name                       -- an ns attribute, an ns attribute is added.
                >>>
                processnsAttribute name
              )
        , this
          :-> processnsAttribute name
        ]

-- ------------------------------------------------------------


{-
- 4.11 Each div element is replaced by its children
- 4.12 Number of child elements
    - A define, oneOrMore, zeroOrMore, optional, list or mixed element
      is transformed so that it has exactly one child element
    - An element element is transformed so that it has exactly two child elements
    - A except element is transformed so that it has exactly one child element
    - If an attribute element has only one child element (a name class), then a text element is added.
    - A choice, group or interleave element is transformed so that it has exactly two child elements.
- 4.13 A mixed element is transformed into an interleaving with a text element
- 4.14 An optional element is transformed into a choice with empty
- 4.15 A zeroOrMore element is transformed into a choice between oneOrMore and empty
- 4.16. Constraints: no transformation is performed, but various constraints are checked.
    - An except element that is a child of an anyName element must not have any anyName descendant elements.
    - An except element that is a child of an nsName element must not have any nsName or anyName
      descendant elements.
    - A name element that occurs as the first child of an attribute element or as the descendant of the
      first child of an attribute element and that has an ns attribute with value equal to the empty
      string must not have content equal to xmlns.
    - A name or nsName element that occurs as the first child of an attribute element or as the
      descendant of the first child of an attribute element must not have an ns attribute with
      value http://www.w3.org/2000/xmlns.
    - A data or value element must be correct in its use of datatypes.
-}
simplificationStep4 :: IOSArrow XmlTree XmlTree
simplificationStep4 =
  ( processTopDown (
      ( -- Each div element is replaced by its children.
        (getChildren >>> simplificationStep4)
        `when`
        isRngDiv
      )
      >>>
      ( -- A define, oneOrMore, zeroOrMore, optional, list or mixed element
        -- is transformed so that it has exactly one child element
        ( replaceChildren
          ( mkRngGroup
            (setChangesAttr $< (getName >>> arr ("group-Pattern: " ++)))
            getChildren
          )
        )
        `when`
        (  isDefineOneOrMoreZeroOrMoreOptionalListMixed
           >>>
           noOfChildren (> 1)
        )
      )
      >>>
      ( -- An element element is transformed so that it has exactly two child elements
        ( replaceChildren
          ( ( getChildren >>> isNameAnyNameNsName )
            <+>
            ( mkRngGroup none
              ( getChildren
                >>>
                neg isNameAnyNameNsName
              )
            )
          )
        )
        `when`
        ( isRngElement >>> noOfChildren (> 2) )
      )
      >>>
      ( -- A except element is transformed so that it has exactly one child element.
        replaceChildren ( mkRngChoice none getChildren )
        `when`
        ( isRngExcept >>> noOfChildren (> 1) )
      )
      >>>
      ( -- If an attribute element has only one child element
        -- (a name class), then a text element is added.
        insertChildrenAt 1 (mkRngText none)
        `when`
        ( isRngAttribute >>> noOfChildren (== 1) )
      )
      >>>
      ( -- A choice, group or interleave element is transformed so
        -- that it has exactly two child elements.
        ((wrapPattern2Two $< getQName) >>> simplificationStep4)
        `when`
        (  isChoiceGroupInterleave
           >>>
           noOfChildren (\ i -> i > 2 || i == 1)
        )
      )
      >>>
      ( -- A mixed element is transformed into an interleaving with a text element
        ( mkRngInterleave
          ( setChangesAttr "mixed is transformed into an interleave" )
          ( getChildren
            <+>
            mkRngText
            ( setChangesAttr ( "new text-Pattern: mixed is transformed into " ++
                                  " an interleave with text"
                             )
            )
          )
        )
        `when`
        isRngMixed
      )
      >>>
      ( -- An optional element is transformed into a choice with empty
        ( mkRngChoice
          ( setChangesAttr "optional is transformed into a choice" )
          ( getChildren
            <+>
            mkRngEmpty
            ( setChangesAttr ( "new empty-Pattern: optional is transformed " ++
                               " into a choice with empty"
                             )
            )
          )
        )
        `when`
        isRngOptional
      )
      >>>
      ( -- A zeroOrMore element is transformed into a choice between oneOrMore and empty
        ( mkRngChoice
          ( setChangesAttr "zeroOrMore is transformed into a choice" )
          ( ( mkRngOneOrMore
              ( setChangesAttr ( "zeroOrMore is transformed into a " ++
                                   "choice between oneOrMore and empty"
                               )
              )
              getChildren
            )
            <+>
            ( mkRngEmpty
              ( setChangesAttr ( "new empty-Pattern: zeroOrMore is transformed " ++
                                   "into a choice between oneOrMore and empty"
                               )
              )
            )
          )
        )
        `when`
        isRngZeroOrMore
      )
    )
  ) `when` collectErrors


-- ------------------------------------------------------------


restrictionsStep1 :: IOSArrow XmlTree XmlTree
restrictionsStep1 =
  ( processTopDown (
      ( ( mkRelaxError ""
          ( "An except element that is a child of an anyName " ++
            "element must not have any anyName descendant elements"
          )
        )
        `when`
        ( isRngAnyName
          >>>
          getChildren
          >>>
          isRngExcept
          >>>
          deep isRngAnyName
        )
      )
      >>>
      ( ( mkRelaxError ""
          ( "An except element that is a child of an nsName element " ++
            "must not have any nsName or anyName descendant elements."
          )
        )
        `when`
        ( isRngNsName
          >>>
          getChildren
          >>>
          isRngExcept
          >>>
          deep (isRngAnyName `orElse` isRngNsName)
        )
      )
      >>>
      ( ( mkRelaxError ""
          ( "A name element that occurs as the first child or descendant of " ++
            "an attribute and has an ns attribute with an empty value must " ++
            "not have content equal to \"xmlns\""
          )
        )
        `when`
        ( isRngAttribute
          >>>
          firstChild
          >>>
          ( multi (isRngName >>> hasRngAttrNs) )
          >>>
          ( ( getRngAttrNs >>> isA null)
            `guards`
            (getChildren >>> getText >>> isA (== "xmlns"))
          )
        )
      )
      >>>
      ( ( mkRelaxError ""
          ( "A name or nsName element that occurs as the first child or " ++
            "descendant of an attribute must not have an ns attribute " ++
            "with value http://www.w3.org/2000/xmlns"
          )
        )
        `when`
        ( isRngAttribute
          >>>
          firstChild
          >>>
          ( multi (isNameNsName >>> hasRngAttrNs) )
          >>>
          getRngAttrNs
          >>>
          isA (compareURI xmlnsNamespace)
        )
      )
      >>> -- A data or value element must be correct in its use of datatypes.
      ( ( checkDatatype $<< getRngAttrDatatypeLibrary &&& getRngAttrType )
        `when`
        ( isRngData `orElse` isRngValue )
      )
    )
  ) `when` collectErrors
  where

  -- the datatypeLibrary attribute must identify a valid datatype library

  checkDatatype :: Uri -> DatatypeName -> IOSArrow XmlTree XmlTree
  checkDatatype libName typeName
      = ifP (const $ elem libName $ map fst datatypeLibraries)
        ( checkType libName typeName allowedDataTypes )
        ( mkRelaxError ""
          ( "DatatypeLibrary " ++ show libName ++ " not found" )
        )
    where
    DTC _ _ allowedDataTypes = fromJust $ lookup libName datatypeLibraries

  -- the type attribute must identify a datatype within the datatype library identified
  -- by the value of the datatypeLibrary attribute.

  checkType :: Uri -> DatatypeName -> AllowedDatatypes -> IOSArrow XmlTree XmlTree
  checkType libName typeName allowedTypes
      = ifP (const $ elem typeName $ map fst allowedTypes)
        ( checkParams typeName libName getParams $<
          ( listA (getChildren >>> isRngParam >>> getRngAttrName) )
        )
       ( mkRelaxError ""
         ( "Datatype " ++ show typeName ++
           " not declared for DatatypeLibrary " ++ show libName
         )
       )
    where
    getParams = fromJust $ lookup typeName allowedTypes

  -- For a data element, the parameter list must be one that is allowed by the datatype

  checkParams :: DatatypeName -> Uri -> AllowedParams -> [ParamName] -> IOSArrow XmlTree XmlTree
  checkParams typeName libName allowedParams paramNames
      = ( mkRelaxError ""
          ( "Param(s): " ++ formatStringListQuot diff ++
            " not allowed for Datatype " ++ show typeName ++
            " in Library " ++
            show ( if null libName
                   then relaxNamespace
                   else libName
                 )
          )
        )
        `when`
        ( isRngData >>> isA (const $ diff /= []) )
      where
      diff = filter (\param -> not $ elem param allowedParams) paramNames


-- ------------------------------------------------------------


{-
- 4.17. combine attribute
    - For each grammar element, all define elements with the same name are combined together.
    - Similarly, for each grammar element all start elements are combined together.
- 4.18. grammar element
    - A grammar must have a start child element.
    - Transform the top-level pattern p into <grammar><start>p</start></grammar>.
    - Rename define elements so that no two define elements anywhere in the schema
      have the same name. To rename a define element, change the value of its name
      attribute and change the value of the name attribute of all ref and parentRef
      elements that refer to that define element.
    - Move all define elements to be children of the top-level grammar element
    - Replace each nested grammar element by the child of its start element
    - Rename each parentRef element to ref.
-}
simplificationStep5 :: IOSArrow XmlTree XmlTree
simplificationStep5
    = ( processTopDown
        ( ( ( ( (deep isRngRelaxError)
                <+>
                ( mkRelaxError "" "A grammar must have a start child element" )
              )
              `when`
              (neg (getChildren >>> isRngStart))
            )
            >>>
            -- For each grammar element, all define elements with the same
            -- name are combined together.
            -- ( combinePatternList "define" $< (getPatternNamesInGrammar "define" >>> arr nub) )
            ( mergeCombinedPatternMap "define" $< getPatternNameMapInGrammar "define" (combinePatterns "define" True))
            >>>
            -- Similarly, for each grammar element all start elements
            -- are combined together.
            ( mergeCombinedPatternMap "start" $< (getPatternNameMapInGrammar "start" (combinePatterns "start" False)) )
          )
          `when`
          isRngGrammar
        )
        >>>
        ( -- transform the top-level pattern p into <grammar><start>p</start></grammar>.
          ( replaceChildren
            ( mkRngGrammar none
              ( mkRngStart none getChildren )
            )
          )
          `when`
          neg (getChildren >>> isRngGrammar)
        )
        >>>
        ( renameDefines $<<
          ( getPatternNamesInGrammar "define"
            >>>
            ( createUniqueNames
              &&&
              constA []
            )
          )
        )
        >>>
        -- Move all define elements to be children of the top-level grammar element
        ( processChildren
          ( -- root node
            processChildren
            ( -- the first grammar pattern remains unchanged
              ( deleteAllDefines
                <+>
                ( getAllDefines >>> processChildren deleteAllDefines )
              )
              >>>
              processTopDown
              ( ( -- Replace each nested grammar element by the child of its start element
                  ( getChildren >>> isRngStart >>> getChildren )
                  `when`
                  isRngGrammar
                )
                >>>
                ( -- Rename each parentRef element to ref.
                  ( setRngNameRef
                    `when`
                    isRngParentRef
                  )
                )
              )
            )
          )
        )
      ) `when` collectErrors
    where
    getPatternNameMapInGrammar :: (ArrowXml a) => String -> (String -> XmlTree -> XmlTree -> XmlTree)
                                  -> a XmlTree (Map String XmlTree)
    getPatternNameMapInGrammar pattern combinator
        = (
              getChildren 
              >>>
              allGrammarPatterns
              >>>
              (getRngAttrName &&& this)
          )
          >.
          fromListWithKey combinator
        where allGrammarPatterns 
                  = choiceA
                    [ hasRngElemName pattern
                      :->
                      this
                    , isRngGrammar 
                      :-> 
                      none
                    , this
                      :->
                      (getChildren >>> allGrammarPatterns)
                    ]

    getPatternNamesInGrammar :: (ArrowXml a) => String -> a XmlTree [String]
    getPatternNamesInGrammar pattern
        = processChildren
          ( processTopDown ( none `when` isRngGrammar ) )
          >>>
          listA ( (multi (hasRngElemName pattern))
                  >>>
                  getRngAttrName
                )

    renameDefines :: RefList -> RefList -> IOSArrow XmlTree XmlTree
    renameDefines ref parentRef
        = processChildren
          ( choiceA
            [ isRngDefine
              :-> (                                     -- the original name is needed for error messages
                    addAttr defineOrigName $< getRngAttrName
                    >>>
                                                        -- rename the define-pattern
                                                        -- the new name is looked up in the ref table
                    addAttr "name" $< ( getRngAttrName
                                        >>>
                                        arr (\n -> fromJust $ lookup n ref)
                                      )
                    >>>
                    renameDefines ref parentRef
                  )
            , isRngGrammar
              :-> ( renameDefines $<<
                    ( (                                 -- compute all define names in the grammar
                        getPatternNamesInGrammar "define"
                        >>>
                                                        -- create a new (unique) name for all define names
                        createUniqueNames
                      )
                      &&&
                                                        -- set the old ref list to be the new parentRef list
                      constA ref
                    )
                  )
            , isRngRef
              :-> ( ifA ( getRngAttrName
                          >>>
                          isA (\name -> (elem name (map fst ref)))
                        )
                    (                                   -- the original name is needed for error messages
                      addAttr defineOrigName $< getRngAttrName
                      >>>
                                                        -- rename the ref-pattern
                                                        -- the new name is looked up in the ref table
                      addAttr "name" $< ( getRngAttrName
                                          >>>
                                          arr (\n -> fromJust $ lookup n ref)
                                        )
                    )
                    (                                   -- the referenced pattern does not exist in the schema
                      mkRelaxError "" $< ( getRngAttrName
                                           >>>
                                           arr (\ n -> ( "Define-Pattern with name " ++ show n ++
                                                         " referenced in ref-Pattern not " ++
                                                         "found in schema"
                                                       )
                                               )
                                         )
                    )
                  )
            , isRngParentRef                            -- same as ref, but the parentRef list is used
              :-> ( ifA ( getRngAttrName
                          >>>
                          isA (\name -> (elem name (map fst parentRef)))
                        )
                    ( addAttr defineOrigName $< getRngAttrName
                      >>>
                      addAttr "name" $< ( getRngAttrName
                                          >>>
                                          arr (\n -> fromJust $ lookup n parentRef)
                                        )
                    )
                    ( mkRelaxError "" $<
                      ( getRngAttrName
                        >>>
                        arr (\ n -> ( "Define-Pattern with name " ++ show n ++
                                      " referenced in parentRef-Pattern " ++
                                      "not found in schema"
                                    )
                            )
                      )
                    )
                  )
            , this
              :-> renameDefines ref parentRef
            ]
          )


    getAllDefines :: IOSArrow XmlTree XmlTree
    getAllDefines = multi isRngDefine

    deleteAllDefines :: IOSArrow XmlTree XmlTree
    deleteAllDefines = processTopDown $ none `when` isRngDefine

    combinePatterns :: String -> Bool -> String -> XmlTree -> XmlTree -> XmlTree
    combinePatterns pattern keepName name t1 t2 = combined
        where [combined] = runLA (combine $<< parts) undefined
              combine (c1, d1) (c2, d2)
                  | c1 == "" && c2 == "" = mkRngRelaxError
                                           >>>
                                           addRngAttrDescr ("More than one " ++ pattern ++ "-Pattern: " ++ show name 
                                                            ++ " without a combine-attribute in the same grammar")
                  | c1 == "" = combineWith c2 d1 d2
                  | c2 == "" = combineWith c1 d1 d2
                  | c1 == c2 = combineWith c1 d1 d2
                  | otherwise = mkRngRelaxError
                                >>>
                                addRngAttrDescr ("Different combine-Attributes: " ++
                                                 (formatStringListQuot [c1, c2]) ++
                                                 " for the " ++ pattern ++ "-Pattern " ++
                                                 show name ++ " in the same grammar")
              combineWith :: String -> XmlTree -> XmlTree -> LA n XmlTree
              combineWith c d1 d2 = mkRngElement pattern
                                        (mkRngAttr "combine" (constA c) <+> if keepName then mkRngAttrName name else none)
                                        (mkRngElement c none $ arrL $ const [d1, d2])
              parts = (
                       (constA t1 >>> getRngAttrCombine &&& getChildren)
                       &&&
                       (constA t2 >>> getRngAttrCombine &&& getChildren)
                      )

    mergeCombinedPatternMap :: String -> Map String XmlTree -> IOSArrow XmlTree XmlTree
    mergeCombinedPatternMap pattern definitions
        = replaceChildren ((constL (toList definitions) >>> arr snd)
                           <+>
                           (getChildren >>> deleteDefinitions))
          where deleteDefinitions 
                   = choiceA
                     [ hasRngElemName pattern
                       :->
                       none
                     , isRngGrammar 
                       :-> 
                       this
                     , this
                       :->
                       processChildren deleteDefinitions
                     ]

-- ------------------------------------------------------------


{-
- 4.19. define and ref elements
    - Remove any define element that is not reachable.
    - Now, for each element element that is not the child of a define element,
      add a define element to the grammar element,
      and replace the element element by a ref element referring
      to the added define element.
    - For each ref element that is expandable and is a descendant
      of a start element or an element element, expand it by replacing
      the ref element by the child of the define element to which it refers
    - This must not result in a loop.
    - Remove any define element whose child is not an element element.
-}
simplificationStep6 :: IOSArrow XmlTree XmlTree
simplificationStep6 =
  ( -- Remove any define element that is not reachable.
    (removeUnreachableDefines $<<< getAllDeepDefines
                                   &&&
                                   constA []
                                   &&&
                                   getRefsFromStartPattern
    )
    >>>
    -- for each element element that is not the child of a define element,
    -- add a define element to the grammar element,
    ( processElements False
      >>>
      processChildren (insertChildrenAt 1 (getRelaxParam "elementTable"))
    )
    >>>
    -- For each ref element that is expandable...
    -- Remove any define element whose child is not an element element
    (replaceExpandableRefs [] $< getExpandableDefines >>> deleteExpandableDefines)
  ) `when` collectErrors
  where
  replaceExpandableRefs :: RefList -> Env -> IOSArrow XmlTree XmlTree
  replaceExpandableRefs foundNames defTable
    = choiceA [
        isRngRef
             :-> (ifA ( getRngAttrName
                        >>>
                        isA (\name -> elem name (map fst foundNames))
                      )
                    -- we have found a loop if the name is in the list
                    (mkRelaxError "" $< ( getAttrValue defineOrigName
                                          >>>
                                          arr (\ n -> ( "Recursion in ref-Pattern: " ++
                                                        formatStringListArr (reverse $ (n:) $ map snd foundNames)
                                                      )
                                              )
                                        )
                    )
                    (replaceRef $<< getRngAttrName &&& getAttrValue defineOrigName)
                 ),
        this :-> (processChildren $ replaceExpandableRefs foundNames defTable)
      ]
    where
    replaceRef :: NewName -> OldName -> IOSArrow XmlTree XmlTree
    replaceRef name oldname
      = ( constA (fromJust $ lookup name defTable)
          >>>
          getChildren
          >>>
          replaceExpandableRefs ((name,oldname):foundNames) defTable
        )
        `whenP`
        (const $ elem name $ map fst defTable)


  processElements :: Bool -> IOSArrow XmlTree XmlTree
  processElements parentIsDefine
    = processChildren
      ( choiceA
        [ isRngElement
          :-> ( ifP (const parentIsDefine)
                (processElements False)
                ( processElements' $<
                  ( listA getDefineName                 -- create a new define id
                    >>>
                    createUniqueNames
                  )
                )
              )
        , isRngDefine
          :-> processElements True
        , this
          :-> processElements False
        ])
    where
    getDefineName :: IOSArrow XmlTree String
    getDefineName
        = firstChild
          >>>
          fromLA createNameClass
          >>>
          arr show

    processElements' :: RefList -> IOSArrow XmlTree XmlTree
    processElements' [(oldname, name)]
      = storeElement name oldname
        >>>
        mkRngRef (createAttr name oldname) none
    processElements' l
        = error $ "processElements' called with illegal arg: " ++ show l

    storeElement :: NewName -> OldName -> IOSArrow XmlTree XmlTree
    storeElement name oldname
      = perform $
          ( mkRngDefine
             (createAttr name oldname) (processElements False)
          )
          &&&
          (listA $ getRelaxParam "elementTable")
          >>>
          arr2 (:)
          >>>
          setRelaxParam "elementTable"

    createAttr :: NewName -> OldName -> IOSArrow XmlTree XmlTree
    createAttr name oldname
      = mkRngAttrName name
        <+>
        mkRngAttrDefineOrigName ("created for element " ++ oldname)

  getExpandableDefines :: (ArrowXml a) => a XmlTree Env
  getExpandableDefines
    = listA $ (multi ( ( isRngDefine
                         >>>
                         getChildren
                         >>>
                         neg isRngElement
                       )
                       `guards`
                       this
                     )
              )
              >>>
              (getRngAttrName &&& this)

  deleteExpandableDefines :: (ArrowXml a) => a XmlTree XmlTree
  deleteExpandableDefines
    = processTopDown $ none
                       `when`
                       ( isRngDefine
                         >>>
                         getChildren
                         >>>
                         neg isRngElement
                       )


-- ------------------------------------------------------------


{-
- 4.20. notAllowed element
    - An attribute, list, group, interleave, or oneOrMore element that has
      a notAllowed child element is transformed into a notAllowed element.
    - A choice element that has two notAllowed child elements
      is transformed into a notAllowed element
    - A choice element that has one notAllowed child element
      is transformed into its other child element.
    - An except element that has a notAllowed child element is removed.
    - The preceding transformations are applied repeatedly
      until none of them is applicable any more.
    - Any define element that is no longer reachable is removed.
- 4.21. empty element
    - A group, interleave or choice element that has two empty child
      elements is transformed into an empty element.
    - A group or interleave element that has one empty child element
      is transformed into its other child element.
    - A choice element whose second child element is an empty element
      is transformed by interchanging its two child elements.
    - A oneOrMore element that has an empty child element
      is transformed into an empty element.
    - The preceding transformations are applied repeatedly
      until none of them is applicable any more.
-}

simplificationStep7 :: IOSArrow XmlTree XmlTree
simplificationStep7
    = ( markTreeChanged 0                               -- 0 = no changes, 1 = changes performed
        >>>
        processTopDownWithAttrl
        ( ( -- An attribute, list, group, interleave, or oneOrMore element that has a
            -- notAllowed child element is transformed into a notAllowed element.
            ( ( mkRngNotAllowed none none
                >>>
                markTreeChanged 1
              )
              `whenNot`                                 -- keep all errors
              (deep isRngRelaxError)
            )
            `when`
            ( isAttributeListGroupInterleaveOneOrMore
              >>>
              getChildren
              >>>
              isRngNotAllowed
            )
          )
          >>>
          ( -- A choice element that has two notAllowed child elements is
            -- transformed into a notAllowed element
            ( mkRngNotAllowed none none
              >>>
              markTreeChanged 1
            )
            `when`
            ( isRngChoice
              >>>
              listA (getChildren >>> isRngNotAllowed)
              >>>
              isA (\s -> length s == 2)
            )
          )
          >>>
          ( -- A choice element that has one notAllowed child element is
            -- transformed into its other child element.
            ( getChildren >>> neg isRngNotAllowed
              >>>
              markTreeChanged 1
            )
            `when`
            ( isRngChoice >>> getChildren >>> isRngNotAllowed )
          )
          >>>
          ( -- An except element that has a notAllowed child element is removed.
            ( ( markTreeChanged 1
                >>>
                none
              )
              `whenNot`                  -- keep all errors
              deep isRngRelaxError
            )
            `when`
            ( isRngExcept >>> getChildren >>> isRngNotAllowed )
          )
          >>> -- transforming the empty pattern (4.21)
          ( -- A group, interleave or choice element that has two empty child elements
            -- is transformed into an empty element.
            ( mkRngEmpty none
              >>>
              markTreeChanged 1
            )
            `when`
            ( isChoiceGroupInterleave
              >>>
              listA (getChildren >>> isRngEmpty)
              >>>
              isA (\s -> length s == 2)
            )
          )
          >>>
          ( -- A group or interleave element that has one empty child element
            -- is transformed into its other child element.
            ( getChildren
              >>>
              neg isRngEmpty
              >>>
              markTreeChanged 1
            )
            `when`
            ( isGroupInterleave >>> getChildren >>> isRngEmpty )
          )
          >>>
          ( -- A choice element whose second child element is an empty element is transformed
            -- by interchanging its two child elements.
            changeChoiceChildren
            `when`
            ( isRngChoice >>> getChildren >>> isRngEmpty )
          )
          >>>
          ( -- A oneOrMore element that has an empty child element
            -- is transformed into an empty element.
            ( mkRngEmpty none
              >>>
              markTreeChanged 1
            )
            `when`
            ( isRngOneOrMore >>> getChildren >>> isRngEmpty )
          )
        )
        >>>
        -- The preceding transformations are applied repeatedly
        -- until none of them is applicable any more.
        ( simplificationStep7
          `when`
          hasTreeChanged
        )
      ) `when` collectErrors
    where
    changeChoiceChildren :: IOSArrow XmlTree XmlTree
    changeChoiceChildren
        = ( ( replaceChildren
              ( mkRngEmpty none
                <+>
                (getChildren >>> neg isRngEmpty)
              )
              >>>
              markTreeChanged 1
            )
            `when`
            ( single (getChildren >>> isElem)           -- first child not "empty" elem
              >>>
              neg isRngEmpty
            )
          )

hasTreeChanged  :: IOSArrow b Int
hasTreeChanged
    = getSysAttrInt 0 "rng:changeTree"
      >>>
      isA (== 1)

markTreeChanged :: Int -> IOSArrow b b
markTreeChanged i
    = perform (setSysAttrInt "rng:changeTree" i)

-- ------------------------------------------------------------


simplificationStep8 :: IOSArrow XmlTree XmlTree
simplificationStep8                     -- Remove any define element that is not reachable.
    = ( ( removeUnreachableDefines $<<<
          ( getAllDeepDefines
            &&&
            constA []
            &&&
            getRefsFromStartPattern
          )
        )
        `when` collectErrors
      )


-- ------------------------------------------------------------


restrictionsStep2 :: IOSArrow XmlTree XmlTree
restrictionsStep2 =
  processTopDown (
    choiceA [
-- 7.1.1. attribute pattern, the following paths are prohibited:
--        attribute//(ref | attribute)
      isRngAttribute :->
        ( ( deep isRngRelaxError
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA ( getChildren
                                          >>>
                                          deep isAttributeRef
                                          >>>
                                          (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  >>>
                                  arr (\n -> formatStringListPatt n ++
                                             "Pattern not allowed as descendent(s)" ++
                                             " of a attribute-Pattern"
                                      )
                                )
                               )
            )
          )
          `when`
          ( getChildren >>> deep isAttributeRef )
        ),

-- 7.1.2. oneOrMore pattern, the following paths are prohibited:
--        oneOrMore//(group | interleave)//attribute
      isRngOneOrMore :->
        ( ( deep isRngRelaxError
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA ( getChildren
                                          >>>
                                          deep isGroupInterleave
                                          >>>
                                          (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  &&&
                                  getChangesAttr
                                  >>>
                                  arr2 (\ n c -> ( formatStringListPatt n ++
                                                   "Pattern not allowed as descendent(s) " ++
                                                   "of a oneOrMore-Pattern" ++
                                                   (if null c then "" else " " ++ show c) ++
                                                   " followed by an attribute descendent"
                                                 )
                                       )
                                )
                               )
            )
          )
          `when`
          ( getChildren >>> deep isGroupInterleave
            >>>
            getChildren >>> deep isRngAttribute
          )
        ),

-- 7.1.3. list pattern, the following paths are prohibited:
--        list//( list | ref | attribute | text | interleave)
      isRngList :->
        ( ( deep isRngRelaxError
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA ( getChildren
                                          >>>
                                          deep isAttributeRefTextListInterleave
                                          >>>
                                          (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  >>>
                                  arr (\n -> formatStringListPatt n ++
                                             "Pattern not allowed as descendent(s) of a list-Pattern")
                                )
                               )
            )
          )
          `when`
          ( getChildren
            >>>
            deep isAttributeRefTextListInterleave
          )
        ),

-- 7.1.4. except in data pattern, the following paths are prohibited:
--        data/except//(attribute | ref | text | list | group | interleave | oneOrMore | empty)
      isRngData :->
        ( ( deep isRngRelaxError
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA (getChildren
                                         >>>
                                         deep isAttributeRefTextListGroupInterleaveOneOrMoreEmpty
                                         >>>
                                         (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  >>>
                                  arr (\n -> formatStringListPatt n ++
                                             "Pattern not allowed as descendent(s) of a data/except-Pattern")
                                )
                               )
            )
          )
          `when`
          ( getChildren
            >>>
            isRngExcept
            >>>
            deep isAttributeRefTextListGroupInterleaveOneOrMoreEmpty
          )
        ),

-- 7.1.5. start element, the following paths are prohibited:
--        start//(attribute | data | value | text | list | group | interleave | oneOrMore | empty)
      isRngStart :->
        ( ( deep isRngRelaxError
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA (getChildren
                                         >>>
                                         deep (checkElemName [ "attribute", "data", "value", "text", "list",
                                                               "group", "interleave", "oneOrMore", "empty"])
                                         >>>
                                         (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  >>>
                                  arr (\n -> formatStringListPatt n ++
                                             "Pattern not allowed as descendent(s) of a start-Pattern")
                                )
                               )
            )
          )
          `when`
          ( getChildren
            >>>
            deep (checkElemName [ "attribute", "data", "value", "text", "list",
                                  "group", "interleave", "oneOrMore", "empty"])
          )
        ),

        this :-> this
      ]
  ) `when` collectErrors


-- ------------------------------------------------------------


restrictionsStep3 :: IOSArrow XmlTree XmlTree
restrictionsStep3
    = processTopDown
      ( ( deep isRngRelaxError
          <+>
          ( mkRelaxError "" $<
            ( -- getRngAttrName
              ( getChildren >>> isRngName >>> getChildren >>> getText )
              >>>
              arr (\ n -> ( "Content of element " ++ show n ++ " contains a pattern that can match " ++
                            "a child and a pattern that matches a single string"
                          )
                  )
            )
          )
        )
        `when`
        ( isRngElement
          >>>
          ( getChildren >>. (take 1 . reverse) )
          >>>
          getContentType >>> isA (== CTNone)
        )
      ) `when` collectErrors



getContentType :: IOSArrow XmlTree ContentType
getContentType
    = choiceA
      [ isRngValue      :-> (constA CTSimple)
      , isRngData       :-> processData
      , isRngList       :-> (constA CTSimple)
      , isRngText       :-> (constA CTComplex)
      , isRngRef        :-> (constA CTComplex)
      , isRngEmpty      :-> (constA CTEmpty)
      , isRngAttribute  :-> processAttribute
      , isRngGroup      :-> processGroup
      , isRngInterleave :-> processInterleave
      , isRngOneOrMore  :-> processOneOrMore
      , isRngChoice     :-> processChoice
      ]
    where
    processData :: IOSArrow XmlTree ContentType
    processData
        = ifA (neg (getChildren >>> isRngExcept))
          (constA CTSimple)
          ( getChildren
            >>>
            isRngExcept
            >>>
            getChildren
            >>>
            getContentType
            >>>
            ifP (/= CTNone) (constA CTSimple) (constA CTNone)
          )
    processAttribute :: IOSArrow XmlTree ContentType
    processAttribute
        = ifA ( lastChild
                >>>
                getContentType
                >>>
                isA (/= CTNone)
              )
          (constA CTEmpty)
          (constA CTNone)

    processGroup :: IOSArrow XmlTree ContentType
    processGroup
        = get2ContentTypes
          >>>
          arr2 (\a b -> if isGroupable a b then max a b else CTNone)

    processInterleave :: IOSArrow XmlTree ContentType
    processInterleave
        = get2ContentTypes
          >>>
          arr2 (\a b -> if isGroupable a b then max a b else CTNone)

    processOneOrMore :: IOSArrow XmlTree ContentType
    processOneOrMore
        = ifA ( getChildren
                >>>
                getContentType >>> isA (/= CTNone)
                >>>
                isA (\t -> isGroupable t t)
              )
          ( getChildren >>> getContentType )
          ( constA CTNone )

    processChoice :: IOSArrow XmlTree ContentType
    processChoice
        = get2ContentTypes
          >>>
          arr2 max

    isGroupable :: ContentType -> ContentType -> Bool
    isGroupable CTEmpty   _         = True
    isGroupable _         CTEmpty   = True
    isGroupable CTComplex CTComplex = True
    isGroupable _         _         = False


checkPattern :: IOSArrow (XmlTree, ([NameClass], [NameClass])) XmlTree
checkPattern
    = (\ (_, (a, b)) -> isIn a b) `guardsP` (arr fst)
    where
    isIn :: [NameClass] -> [NameClass] -> Bool
    isIn _ []      = False
    isIn [] _      = False
    isIn (x:xs) ys = (any (overlap x) ys) || isIn xs ys


occur :: String -> IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
occur name fct
    = choiceA
      [ hasRngElemName name
        :->
        fct
      , isChoiceGroupInterleaveOneOrMore
        :->
        (getChildren >>> occur name fct)
      ]

get2ContentTypes :: IOSArrow XmlTree (ContentType, ContentType)
get2ContentTypes
    = ( ( firstChild >>> getContentType )
        &&&
        ( lastChild  >>> getContentType )
      )

-- ------------------------------------------------------------


-- Duplicate attributes are not allowed. -> fertig
-- Attributes using infinite name classes must be repeated; an attribute element that
-- has an anyName or nsName descendant element must have a oneOrMore ancestor element. -> fertig

-- berechnet alle define-Namen (fuer ref-Pattern) und Nameclasses der element-Pattern

restrictionsStep4 :: IOSArrow XmlTree XmlTree
restrictionsStep4
    = ( restrictionsStep4' $<
        listA ( deep isRngDefine                                -- get all defines
                >>>
                ( getRngAttrName                                -- get define name
                  &&&
                  ( single ( getChildren
                             >>>
                             getChildren
                             >>>
                             fromLA createNameClass             -- compute the name class from 1. grandchild
                           )
                    `orElse`
                    (constA AnyName)
                  )
                )
              )
      ) `when` collectErrors

restrictionsStep4' :: [(String, NameClass)] -> IOSArrow XmlTree XmlTree
restrictionsStep4' nc =
  processTopDown (
    (
      ( deep isRngRelaxError
        <+>
        ( mkRelaxError "" $<
          ( getRngAttrName
            >>>
            arr (\ n -> ( "Both attribute-pattern occuring in an " ++
                          show n ++ " belong to the same name-class"
                        )
                )
          )
        )
      )
      `when`
      ( (isRngGroup `orElse` isRngInterleave)
        >>>
        ( getChildren
          &&&
          ( firstChild
            >>>
            listA ( occur "attribute" (single getChildren)
                    >>>
                    fromLA createNameClass
                  )
          )
          &&&
          ( lastChild
            >>>
            listA ( occur "attribute" (single getChildren)
                    >>>
                    fromLA createNameClass
                  )
          )
        )
        >>> checkPattern
      )
    )
    >>>
    (
      ( deep isRngRelaxError
        <+>
        ( mkRelaxError ""
          ( "An attribute that has an anyName or nsName descendant element " ++
            "must have a oneOrMore ancestor element"
          )
        )
      )
      `when`
      (isRngElement >>> checkInfiniteAttribute)
    )
    >>>
    ( ( deep isRngRelaxError
        <+>
        ( mkRelaxError ""
          ( "Both element-pattern occuring in an interleave " ++
            "belong to the same name-class"
          )
        )
      )
      `when`
      ( isRngInterleave
        >>>
        ( getChildren
          &&&
          (firstChild >>> listA (occur "ref" this >>> getRngAttrName))
          &&&
          (lastChild  >>> listA (occur "ref" this >>> getRngAttrName))
        )
        >>>
        checkNames
      )
    )
    >>>
    ( ( deep isRngRelaxError
        <+>
        ( mkRelaxError "" "A text pattern must not occur in both children of an interleave" )
      )
      `when`
      (isRngInterleave >>> checkText)
    )
  )
  where
  checkInfiniteAttribute :: IOSArrow XmlTree XmlTree
  checkInfiniteAttribute
    = getChildren
      >>>
      choiceA
      [ isRngOneOrMore :-> none
      , ( isRngAttribute
          >>>
          deep (isRngAnyName `orElse` isRngNsName)
        ) :-> this
      , this :-> checkInfiniteAttribute
      ]

  checkNames :: IOSArrow (XmlTree, ([String], [String])) XmlTree
  checkNames = (arr fst)
               &&&
               (arr (\(_, (a, _)) -> getNameClasses nc a))
               &&&
               (arr (\(_, (_, b)) -> getNameClasses nc b))
               >>>
               checkPattern
    where
    getNameClasses :: [(String, NameClass)] -> [String] -> [NameClass]
    getNameClasses nc' l = map (\x -> fromJust $ lookup x nc') l

  checkText :: IOSArrow XmlTree XmlTree
  checkText
      = ( firstChild >>> occur "text" this )
        `guards`
        ( lastChild  >>> occur "text" this )

-- ------------------------------------------------------------


overlap         :: NameClass -> NameClass -> Bool
overlap nc1 nc2
    = any (bothContain nc1 nc2) (representatives nc1 ++ representatives nc2)

bothContain     :: NameClass -> NameClass -> QName -> Bool
bothContain nc1 nc2 qn
    = contains nc1 qn && contains nc2 qn

illegalLocalName        :: LocalName
illegalLocalName        = ""

illegalUri              :: Uri
illegalUri              = "\x1"

representatives         :: NameClass -> [QName]
representatives AnyName
    = [mkQName "" illegalLocalName illegalUri]

representatives (AnyNameExcept nc)
    = (mkQName "" illegalLocalName illegalUri) : (representatives nc)

representatives (NsName ns)
    = [mkQName "" illegalLocalName ns]

representatives (NsNameExcept ns nc)
    = (mkQName "" illegalLocalName ns) : (representatives nc)

representatives (Name ns ln)
    = [mkQName "" ln ns]

representatives (NameClassChoice nc1 nc2)
    = (representatives nc1) ++ (representatives nc2)

representatives _
    = []

-- -------------------------------------------------------------------------------------------------------

resetStates :: IOSArrow XmlTree XmlTree
resetStates
    = ( perform (constA 0  >>> setSysVar theRelaxDefineId)
        >>>
        perform (constA 0  >>> setSysVar theRelaxNoOfErrors)
        >>>
        perform (constA [] >>> setRelaxParam "elementTable" )
      )


getAllDeepDefines :: IOSArrow XmlTree Env
getAllDeepDefines
    = listA $ deep isRngDefine
      >>>
      ( getRngAttrName &&& this )


createUniqueNames :: IOSArrow [String] RefList
createUniqueNames
    = createUnique $< incrSysVar theRelaxDefineId
    where
    createUnique num
        = arr (unique num)                              -- assign numbers to names
          >>>
          ( this
            ***
            perform (setSysVar theRelaxDefineId)        -- store next unused number
          )
          >>>
          arr fst
        where
        unique :: Int -> [String] -> (RefList, Int)
        unique n0 l
            = ( zipWith (\ x n -> (x, show n)) l [n0 ..]
              , n0 + length l
              )

-- | Return all reachable defines from the start pattern

getRefsFromStartPattern :: IOSArrow XmlTree [String]
getRefsFromStartPattern
  = listA
    ( getChildren
      >>>
      isRngGrammar
      >>>
      getChildren
      >>>
      isRngStart
      >>>
      deep isRngRef
      >>>
      getRngAttrName
    )

removeUnreachableDefines :: Env -> [String] -> [String] -> IOSArrow XmlTree XmlTree
removeUnreachableDefines allDefs processedDefs reachableDefs
    = ifP (const $ unprocessedDefs /= [])
      ( removeUnreachableDefines allDefs (nextTreeName : processedDefs) $< newReachableDefs )
      ( processChildren $ -- root node
        processChildren $ -- first grammar
        ( none
          `when`
          ( isRngDefine
            >>>
            getRngAttrName
            >>>
            isA (\n -> not $ elem n reachableDefs)
          )
        )
      )
    where
    unprocessedDefs :: [String]
    unprocessedDefs
        = reachableDefs \\ processedDefs

    newReachableDefs :: IOSArrow n [String]
    newReachableDefs
        = constA getTree
          >>>
          listA ( deep isRngRef
                  >>>
                  getRngAttrName
                )
          >>>
          arr (noDoubles . (reachableDefs ++))

    getTree :: XmlTree
    getTree
        = fromJust $ lookup nextTreeName allDefs

    nextTreeName :: String
    nextTreeName
        = head unprocessedDefs


-- -------------------------------------------------------------------------------------------------------


checkElemName :: [String] -> IOSArrow XmlTree XmlTree
checkElemName l
    = ( isElem >>> getLocalPart >>> isA (\s -> elem s l) )
      `guards`
      this

wrapPattern2Two :: (ArrowXml a) => QName -> a XmlTree XmlTree
wrapPattern2Two name
  = choiceA
    [ noOfChildren (> 2)
      :-> ( replaceChildren ( (mkElement name none
                               (getChildren >>. take 2)
                              )
                              <+>
                              (getChildren >>. drop 2)
                            )
            >>>
            wrapPattern2Two name
          )
    , noOfChildren (== 1)
      :-> getChildren
    , this
      :-> this
    ]

(!>>>)          :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
f !>>> g
    = f
      >>>
      ifA (getSysVar theRelaxNoOfErrors >>> isA (> 0))
          this
          g

mkRelaxError :: String -> String -> IOSArrow n XmlTree
mkRelaxError changesStr errStr
  = perform (constA 1 >>> chgSysVar theRelaxNoOfErrors (+))
    >>>
    mkRngRelaxError
    >>>
    addRngAttrDescr errStr
    >>>
    ( if null changesStr
      then this
      else addRngAttrChanges changesStr
    )

collectErrors :: IOSArrow XmlTree XmlTree
collectErrors
  = none
    `when`
    ( (getSysVar theRelaxCollectErrors >>> isA not)
      >>>
      errorsFound
    )

-- | errors found?
errorsFound :: IOSArrow a a
errorsFound
    = ( getSysVar theRelaxNoOfErrors >>> isA (> 0) )
      `guards`
      this

-- | Returns the list of simplification errors or 'none'
getErrors :: IOSArrow XmlTree XmlTree
getErrors = errorsFound
            `guards`
            multi isRngRelaxError

setChangesAttr :: String -> IOSArrow XmlTree XmlTree
setChangesAttr str
  = ifA (hasRngAttrRelaxSimplificationChanges)
      ( processAttrl $
          changeAttrValue (++ (", " ++ str))
          `when`
          isRngAttrRelaxSimplificationChanges
      )
      (mkRngAttrRelaxSimplificationChanges str)


getChangesAttr :: IOSArrow XmlTree String
getChangesAttr
  = getAttrValue a_relaxSimplificationChanges
    &&&
    getSysAttr a_output_changes
    >>>
    ifP (\(changes, param) -> changes /= "" && param == "1")
      (arr2 $ \l _ -> " (" ++ l ++ ")")
      (constA "")

-- -------------------------------------------------------------------------------------------------------

-- | Creates the simple form of a Relax NG schema
-- 
-- The schema document has to be parsed with namespace propagation

createSimpleForm :: Bool -> Bool -> Bool -> IOSArrow XmlTree XmlTree
createSimpleForm checkRestrictions validateExternalRef validateInclude
    = traceMsg 2 ("createSimpleForm: " ++ show (checkRestrictions,validateExternalRef, validateInclude))
      >>>
      ( if checkRestrictions
        then createSimpleWithRest
        else createSimpleWithoutRest
      )
    where

    createSimpleWithRest :: IOSArrow XmlTree XmlTree
    createSimpleWithRest
        = foldr (!>>>) this $
          concat [ return $ traceDoc "relax NG: simplificationPart1 starts"
                 , simplificationPart1
                 , return $ traceDoc "relax NG: simplificationPart1 done"
                 , restrictionsPart1
                 , return $ traceDoc "relax NG: restrictionsPart1 done"
                 , simplificationPart2
                 , return $ traceDoc "relax NG simplificationPart2 done"
                 , restrictionsPart2
                 , return $ traceDoc "relax NG: restrictionsPart2 done"
                 , finalCleanUp
                 , return $ traceDoc "relax NG: finalCleanUp done"
                 ]

    createSimpleWithoutRest :: IOSArrow XmlTree XmlTree
    createSimpleWithoutRest
        = foldr (!>>>) this $
          concat [ simplificationPart1
                 , simplificationPart2
                 , finalCleanUp
                 ]

    simplificationPart1 :: [IOSArrow XmlTree XmlTree]
    simplificationPart1
        = [ simplificationStep1
          , simplificationStep2 validateExternalRef validateInclude [] []
          , simplificationStep3
          , simplificationStep4
          ]

    simplificationPart2 :: [IOSArrow XmlTree XmlTree]
    simplificationPart2
        = [ simplificationStep5
          , simplificationStep6
          , simplificationStep7
          , simplificationStep8
          ]

    restrictionsPart1 :: [IOSArrow XmlTree XmlTree]
    restrictionsPart1
        = [ restrictionsStep1 ]

    restrictionsPart2 :: [IOSArrow XmlTree XmlTree]
    restrictionsPart2
        = [ restrictionsStep2
          , restrictionsStep3
          , restrictionsStep4
          ]

    finalCleanUp :: [IOSArrow XmlTree XmlTree]
    finalCleanUp
        = [ cleanUp
          ]

    cleanUp :: IOSArrow XmlTree XmlTree
    cleanUp = processTopDown $
              removeAttr a_relaxSimplificationChanges
              >>>
              removeAttr defineOrigName

-- -------------------------------------------------------------------------------------------------------

setRelaxParam           :: String -> IOStateArrow s XmlTrees XmlTree
setRelaxParam n         = chgSysVar theRelaxAttrList (addEntry n)
                          >>>
                          arrL id

getRelaxParam           :: String -> IOStateArrow s b XmlTree
getRelaxParam n         = getSysVar theRelaxAttrList
                          >>>
                          arrL (lookup1 n)

-- -------------------------------------------------------------------------------------------------------
