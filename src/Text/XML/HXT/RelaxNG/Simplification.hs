-- |
-- The modul creates the simplified form of a Relax NG schema.
-- See also chapter 4 of the Relax NG specification.


module Text.XML.HXT.RelaxNG.Simplification
  ( createSimpleForm
  , getErrors
  )

where


import Control.Arrow.ListArrows

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow
    hiding
    ( mkText
    , mkError
    , mkElement
    , hasName
    )

import qualified Text.XML.HXT.Arrow.XmlArrow as A
    ( mkElement
    , hasName
    )

import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.Namespace
    ( processWithNsEnv
    , propagateNamespaces
    )

import Text.XML.HXT.Arrow.Edit
    ( removeWhiteSpace
    )


import qualified Text.XML.HXT.Arrow.XmlNode as XN
    ( mkAttr
    , mkText
    )

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.CreatePattern
import Text.XML.HXT.RelaxNG.DataTypeLibraries
import Text.XML.HXT.RelaxNG.Utils
import Text.XML.HXT.RelaxNG.Validation
import Text.XML.HXT.RelaxNG.Schema        as S
import Text.XML.HXT.RelaxNG.SchemaGrammar as SG

import Data.Maybe
import Char
import List
import Directory
  ( doesFileExist )

  
-- ------------------------------------------------------------

hasName :: ArrowXml a => String -> a XmlTree XmlTree
hasName s = A.hasName s 
            `orElse`
            (hasLocalPart s >>> hasNamespaceUri relaxNamespace)


{- 
- 4.1. Annotations: Foreign attributes and elements are removed.
- 4.2. Whitespace:
    - For each element other than value and param, each child that is a string 
      containing only whitespace characters is removed. -> fertig
    - Leading and trailing whitespace characters are removed from the value of each name,
      type and combine attribute and from the content of each name element. -> fertig
- 4.3. datatypeLibrary attribute:
    - The value of each datatypeLibary attribute is transformed by escaping disallowed characters
    - For any data or value element that does not have a datatypeLibrary attribute, 
      a datatypeLibrary attribute is added.
    - Any datatypeLibrary attribute that is on an element other than data or value is removed.
- 4.4. type attribute of value element
-}
simplificationStep1 :: IOSArrow XmlTree XmlTree
simplificationStep1 =
  ( {- 
    - 4.5. href attribute
        - The value of the href attribute on an externalRef or include element is first 
          transformed by escaping disallowed characters
        - The URI reference is then resolved into an absolute form
        - The value of the href attribute will be used to construct an element.
    -}
    processHref $< getBaseURI
    >>>
    -- 4.10 QNames
    processWithNsEnv processEnvNames [("xml",xmlNamespace)]
    >>>
    -- 4.4 For any data or value element that does not have a datatypeLibrary attribute,
    -- a datatypeLibrary attribute is added.
    -- Wird vorgezogen, da danach der Rest in einem Baumdurchlauf erledigt werden kann
    processdatatypeLib ""
    >>>
    processTopDownWithAttrl (
      ( -- 4.1 Foreign attributes and elements are removed
        none
        `when` 
        ( ( isElem >>> neg isRoot 
            >>> 
            getNamespaceUri
            >>>
            isA (\uri -> (not $ compareURI uri relaxNamespace))
          )
          `orElse`
          ( isAttr
            >>>
            getNamespaceUri
            >>>
            isA (\uri -> (uri /= "" && (not $ compareURI uri relaxNamespace)))
          )
        )
      )
      >>>
      ( -- 4.2 For each element other than value and param, each child that 
        -- is a string containing only whitespace characters is removed.
        ( (processChildren removeWhiteSpace) 
          `whenNot` 
          (hasName "param" `orElse` hasName "value")
        )
        `when` isElem 
      )
      >>>       
      ( -- 4.2 Leading and trailing whitespace characters are removed from the value 
        -- of each name, type and combine attribute ...
        (changeAttrValue normalizeWhitespace)
        `when`
        (isAttr >>> (hasName "name" `orElse` hasName "type" `orElse` hasName "combine"))
      )
      >>>
      ( -- 4.2 ... and from the content of each name element.
        (processChildren $ changeText normalizeWhitespace)
        `when`
        (isElem >>> hasName "name")
      )
      >>>
      ( -- 4.3 The value of each datatypeLibary attribute is transformed
        -- by escaping disallowed characters
        (changeAttrValue escapeURI)
        `when`
        (isAttr >>> hasName "datatypeLibrary")
      )
      >>>
      ( -- The value of the datatypeLibary attribute has to be a valid URI
        ( mkRelaxError "" $< ( getAttrValue "datatypeLibrary"
                               >>> 
                               arr (\a -> "datatypeLibrary attribute: " ++ 
                                          a ++ " is not a valid URI"
                                   )
                             )
        )
        `when`
        ( isElem >>> hasAttr "datatypeLibrary"
          >>> 
          getAttrValue "datatypeLibrary" >>> isA (not . isRelaxAnyURI)
        )
      )
      >>>       
      ( -- 4.3 Any datatypeLibrary attribute that is on an element 
        -- other than data or value is removed.
        removeAttr "datatypeLibrary"
        `when`
        ( isElem >>> neg (hasName "data" `orElse` hasName "value") 
          >>> 
          hasAttr "datatypeLibrary"
        )
      )
      >>>       
      ( -- 4.4 For any value element that does not have a type attribute,
        -- a type attribute is added with value token and the value of 
        -- the datatypeLibrary attribute is changed to the empty string.
        (addAttr "type" "token" >>> addAttr "datatypeLibrary" "")
        `when`
        (isElem >>> hasName "value" >>> neg (hasAttr "type"))
      ) 
    )    
  ) `when` collectErrors
  where
  processHref :: String -> IOSArrow XmlTree XmlTree
  processHref uri
    = processChildren (
        choiceA [
          (isElem >>> hasAttr "xml:base")
            :-> ( ifA (checkElemName ["externalRef", "include"] >>> hasAttr "href")
                   ( -- The value of the href attribute is transformed by
                     -- escaping disallowed characters
                     (processAttrl (changeAttrValue escapeURI `when` hasName "href"))
                     >>> -- compute the new base uri from the old uri and the href attribute
                     (addAttr "href" $< (absURI "href" $< (absURI "xml:base" uri)))
                     >>> 
                     (processHref $< absURI "xml:base" uri)
                   ) -- element without a href attribute, just compute the new base uri
                   (processHref $< absURI "xml:base" uri)
                ),
          (checkElemName ["externalRef", "include"] >>> hasAttr "href")
            :-> ( -- The value of the href attribute is transformed by 
                  -- escaping disallowed characters
                  (processAttrl (changeAttrValue escapeURI `when` hasName "href"))
                  >>>
                  (addAttr "href" $< absURI "href" uri)
                ),
          this :-> (processHref uri)
        ]
      )
    where
    absURI :: String -> String -> IOSArrow XmlTree String
    absURI attrName u = getAttrValue attrName
                        >>> 
                        arr (\a -> fromMaybe "" (expandURIString a u))
                        >>> -- the uri should not have a fragment-identifier (4.5)
                        ( (arr (++ ", it is not a valid URI for text/xml"))
                          `whenNot`
                          (getFragmentFromURI >>> isA(== ""))
                        )

  
  processEnvNames :: [(String, String)] -> IOSArrow XmlTree XmlTree
  processEnvNames env
    = ( (replaceQNames env $< getAttrValue "name")
        `when`
        ( isElem >>> (hasName "element" `orElse ` hasName "attribute")
          >>>
          getAttrValue "name" >>> isA (elem ':')
        )
      )
      >>>
      ( (addAttrl (getBaseURI >>> createAttrL))      
        `when`
        (isElem >>> hasName "value")
      )
    where
    createAttrL :: IOSArrow String XmlTree
    createAttrL = setBaseUri &&& constA (map createAttr env) >>> arr2L (:)
      where
      createAttr :: (String, String) -> XmlTree
      createAttr (pre, uri)
        = if pre == "" -- default namespace
          then XN.mkAttr (QN "" "RelaxContextDefault" "") [XN.mkText uri]
          else XN.mkAttr (QN "" (contextAttributes++pre) "") [XN.mkText uri]
      setBaseUri :: IOSArrow String XmlTree
      setBaseUri = mkAttr (QN "" contextBaseAttr "") (txt $< this)
 
    replaceQNames :: [(String, String)] -> String -> IOSArrow XmlTree XmlTree                        
    replaceQNames e name
      = ifP (const $ uri == Nothing) 
          ( mkRelaxError "" $ "No Namespace-Mapping for the prefix " ++ pre ++ 
                              " in the Context of Element: " ++ name
          )
          (addAttr "name" ('{' : (fromJust uri) ++ "}" ++ local))
      where
      (pre, local') = span (/= ':') name
      local         = tail local'
      uri :: Maybe String
      uri           = lookup pre e

  -- The value of the added datatypeLibrary attribute is the value of the 
  -- datatypeLibrary attribute of the nearest ancestor element that 
  -- has a datatypeLibrary attribute, or the empty string 
  -- if there is no such ancestor.
  processdatatypeLib :: (ArrowXml a) => String -> a XmlTree XmlTree
  processdatatypeLib lib 
    = processChildren $
        choiceA [ 
          (isElem >>> hasAttr "datatypeLibrary")
               -- set the new datatypeLibrary value
               :-> (processdatatypeLib $< getAttrValue "datatypeLibrary"),
          
          ( isElem >>> (hasName "data" `orElse` hasName "value")
            >>> 
            neg (hasAttr "datatypeLibrary")
               -- add a datatypeLibrary attribute
          )    :-> (addAttr "datatypeLibrary" lib >>> processdatatypeLib lib),
          
          this :-> (processdatatypeLib lib)
        ]


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

simplificationStep2 :: Attributes -> Bool -> Bool -> [Uri] -> [Uri] -> IOSArrow XmlTree XmlTree
simplificationStep2 readOptions validateExternalRef validateInclude extHRefs includeHRefs =
  ( processTopDown (
      ( (importExternalRef $<< (getAttrValue "ns" &&& getAttrValue "href"))
        `when`
        (isElem >>> hasName "externalRef")
      )
      >>>
      ( (importInclude $< getAttrValue "href")
        `when`
        (isElem >>> hasName "include")
      )
    )
  ) `when` collectErrors
  where
  importExternalRef :: String -> String -> IOSArrow XmlTree XmlTree
  importExternalRef ns href
    = ifA ( -- test whether the referenced schema exists
            neg $ constA href >>> getPathFromURI >>> isIOA doesFileExist
          )
        ( mkRelaxError "" ( "Can't read " ++ href ++
			    ", referenced in externalRef-Pattern" )
	)
        ( ifP (const $ elem href extHRefs)
           -- if the referenced name already exists in the list of processed ref attributes
           -- we have found a loop
            (mkRelaxError "" $ "loop in externalRef-Pattern, " ++ 
                               formatStringList " -> " (reverse $ href:extHRefs)
            )
            ( ifA ( if validateExternalRef 					-- if validation parameters are set
		    then validateDocWithRelax S.relaxSchemaArrow [] href	-- the referenced schema is validated with respect to
		    else none							-- the Relax NG spezification
                  )
                ( mkRelaxError "" ( "The content of the schema " ++ href ++ 
				    ", referenced in externalRef does not " ++
				    "match the syntax for pattern" )
                )
                ( readForRelax readOptions href
                  >>>
                  simplificationStep1						-- perform the transformations from previous steps
                  >>>
                  simplificationStep2 readOptions validateExternalRef validateInclude (href:extHRefs) includeHRefs
                  >>>
                  getChildren -- remove the root node
                  >>>
                  ( -- Any ns attribute on the externalRef element
                    -- is transferred to the referenced element
                    addAttr "ns" ns
                    `when`
                    (getAttrValue "ns" >>> isA (\a -> a == "" && ns /= ""))
                  )
                )
            )
        )
  
  importInclude :: String -> IOSArrow XmlTree XmlTree
  importInclude href
    = ifA ( -- test whether the referenced schema exists
            neg $ constA href >>> getPathFromURI >>> isIOA doesFileExist
          )
        ( mkRelaxError "" ( "Can't read " ++ href ++
                            ", referenced in include-Pattern" )
        )
        ( ifP (const $ elem href includeHRefs)
           -- if the referenced name still exists in the list of processed
           -- ref attributes we have found a loop
            (mkRelaxError "" $ "loop in include-Pattern, " ++ 
                               formatStringList " -> " (reverse $ href:includeHRefs)
            )
            ( ifA ( if validateInclude						-- if the parameter is set
		    then validateDocWithRelax SG.relaxSchemaArrow [] href	-- the referenced schema is validated with respect to
                    else none							-- the Relax NG grammar element
                  )
                ( mkRelaxError "" ( "The content of the schema " ++ href ++ 
                                    ", referenced in include does not match " ++
                                    "the syntax for grammar" )
                )
                ( processInclude href $< ( readForRelax readOptions href
                                           >>>
                                           simplificationStep1				-- perform the transformations from previous steps
                                           >>> 
                                           simplificationStep2 readOptions validateExternalRef validateInclude extHRefs (href:includeHRefs)
                                           >>>
                                           getChildren					-- remove the root node
                                         )
                )
            )
        )
  
  processInclude :: String -> XmlTree -> IOSArrow XmlTree XmlTree
  processInclude href newDoc
    = -- The include element is transformed into a div element.
      setElemName (QN "" "div" relaxNamespace)
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
        setElemName (QN "" "div" relaxNamespace)

  
  checkInclude :: String -> XmlTree -> IOSArrow XmlTree XmlTree
  checkInclude href newDoc
    = ifA ( -- If the include element has a start component, then the grammar element
            -- must have a start component.
            hasStartComponent &&& (constA newDoc >>> hasStartComponent)
            >>> 
            isA (\(a, b) -> if a then b else True)
          )
        ( ifA ( -- If the include element has a define component, then the grammar element
                -- must have a define component with the same name.
                getDefineComponents &&& (constA newDoc >>> getDefineComponents)
                >>> 
                isA (\(a, b) -> (diff a b) == [])
              )
            (insertNewDoc newDoc $<< hasStartComponent &&& getDefineComponents)
            (mkRelaxError "" $ "Define-pattern missing in schema " ++ href ++ 
                               ", referenced in include-pattern")
        )
        ( mkRelaxError "" $ "Grammar-element without a start-pattern in schema " ++
                            href ++ ", referenced in include-pattern"
        )
    where
    diff a b = (noDoubles a) \\ (noDoubles b)

  removeStartComponent :: IOSArrow XmlTree XmlTree
  removeStartComponent
    = processChildren $
        choiceA [
          (isElem >>> hasName "start") :-> none,
          (isElem >>> hasName "div")   :-> removeStartComponent,
          this                         :-> this
        ]

  removeDefineComponent :: [String] -> IOSArrow XmlTree XmlTree
  removeDefineComponent defNames
    = processChildren $
        choiceA [
          ( isElem >>> hasName "define"
            >>>
            getAttrValue "name"
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
          (isElem >>> hasName "start") :-> (constA True),
          (isElem >>> hasName "div")   :-> hasStartComponent',
          this                         :-> (constA False)
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
        choiceA [
          (isElem >>> hasName "define") :-> (getAttrValue "name"),
          (isElem >>> hasName "div")    :-> getDefineComponents',
          this                          :-> (constA "")
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
        ( insertChildrenAt 0 (A.mkElement (QN "" "name" relaxNamespace)
                                          none (txt $< getAttrValue "name"))
        )
        >>>
        ( -- 4.8 If an attribute element has a name attribute but no ns attribute,
          --  then an ns="" attribute is added to the name child element
           (processChildren (addAttr "ns" "" `when` (isElem >>> hasName "name")))
           `when` 
           (isElem >>> hasName "attribute" >>> hasAttr "name" >>> neg (hasAttr "ns"))
        )
        >>>
        removeAttr "name"      
      )
      `when`
      (isElem >>> (hasName "element" `orElse` hasName "attribute") >>> hasAttr "name")       
    )
    >>>
    -- 4.9 For any name, nsName or value element that does not have 
    -- an ns attribute, an ns attribute is added.
    processnsAttribute ""
    >>>
    processTopDown (        
      ( -- 4.9 any ns attribute that is on an element other than name, 
        -- nsName or value is removed.
        (removeAttr "ns")
        `when`
        (isElem >>> neg (hasName "name" `orElse` hasName "nsName" `orElse` hasName "value"))
      )
      >>>
      ( -- 4.10 For any name element containing a prefix, the prefix is removed and an ns attribute 
        -- is added replacing any existing ns attribute.
        (replaceNameAttr $< (getChildren >>> isText >>> getText))
        `when`
        (isElem >>> hasName "name")
      )
    )
  ) `when` collectErrors
  where
  replaceNameAttr :: (ArrowXml a) => String -> a XmlTree XmlTree
  replaceNameAttr name 
    = (addAttr "ns" pre >>> processChildren (changeText $ const local))
      `whenP`
      (const $ elem '}' name)
    where 
    (pre', local') = span (/= '}') name
    pre            = tail pre'
    local          = tail local'
          
  processnsAttribute :: String -> IOSArrow XmlTree XmlTree        
  processnsAttribute name 
    = processChildren $
        choiceA [
          -- set the new ns attribute value
          (isElem >>> hasAttr "ns") 
               :-> (processnsAttribute $< getAttrValue "ns"),
          -- For any name, nsName or value element that does not have
          -- an ns attribute, an ns attribute is added.               
          ( checkElemName ["name", "nsName", "value"] >>> neg (hasAttr "ns"))
               :-> (addAttr "ns" name >>> processnsAttribute name),
          this :-> (processnsAttribute name)
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
        (isElem >>> hasName "div")
      )
      >>>
      ( -- A define, oneOrMore, zeroOrMore, optional, list or mixed element
        -- is transformed so that it has exactly one child element
        ( replaceChildren $ A.mkElement (QN "" "group" relaxNamespace)
                              (setChangesAttr $< (getName >>> arr ("group-Pattern: " ++)))  
                              getChildren
        )
        `when`
        (  checkElemName ["define", "oneOrMore", "zeroOrMore", "optional", "list", "mixed"]
           >>> listA getChildren >>> isA (\cl -> length cl > 1)
        )
      )
      >>>
      ( -- An element element is transformed so that it has exactly two child elements       
        ( replaceChildren $ (getChildren >>> checkElemName ["name", "anyName", "nsName"])
                            <+> 
                            ( A.mkElement (QN "" "group" relaxNamespace) none 
                                ( getChildren
                                  >>>
                                  neg (checkElemName ["name", "anyName", "nsName"])
                                )
                            )
        )
        `when`
        ( isElem >>> hasName "element"
          >>>
          listA getChildren
          >>>
          isA (\cl -> length cl > 2)
        )
      )
      >>>
      ( -- A except element is transformed so that it has exactly one child element.
        (replaceChildren $ A.mkElement (QN "" "choice" relaxNamespace) none getChildren)
        `when`
        (isElem >>> hasName "except" >>> listA getChildren >>> isA (\cl -> length cl > 1))
      )
      >>>
      ( -- If an attribute element has only one child element 
        -- (a name class), then a text element is added.
        insertChildrenAt 1 (A.mkElement (QN "" "text" relaxNamespace) none none)
        `when`
        ( isElem >>> hasName "attribute"
          >>>
          listA getChildren
          >>>
          isA (\cl -> length cl == 1)
        )
      )
      >>>
      ( -- A choice, group or interleave element is transformed so
        -- that it has exactly two child elements.
        ((wrapPattern2Two $< getName) >>> simplificationStep4)
        `when` 
        (  checkElemName ["choice", "group", "interleave"]
           >>>
           listA getChildren
           >>>
           isA (\cl -> length cl > 2 || length cl == 1)
        )
      )
      >>>
      ( -- A mixed element is transformed into an interleaving with a text element     
        ( A.mkElement (QN "" "interleave" relaxNamespace) 
            (setChangesAttr "mixed is transformed into an interleave")
            ( getChildren
              <+>
              A.mkElement (QN "" "text" relaxNamespace) 
                (setChangesAttr $ "new text-Pattern: mixed is transformed into " ++
                                  " an interleave with text"
                )
                none
            )
        )
        `when`
        (isElem >>> hasName "mixed")
      )
      >>>  
      ( -- An optional element is transformed into a choice with empty     
        ( A.mkElement (QN "" "choice" relaxNamespace) 
            (setChangesAttr "optional is transformed into a choice")  
            ( getChildren
              <+>
              A.mkElement (QN "" "empty" relaxNamespace) 
                  (setChangesAttr $ "new empty-Pattern: optional is transformed " ++
                                    " into a choice with empty"
                  )
                  none
            )
        )
        `when`
        (isElem >>> hasName "optional")
      )
      >>>
      ( -- A zeroOrMore element is transformed into a choice between oneOrMore and empty
        ( A.mkElement (QN "" "choice" relaxNamespace) 
            (setChangesAttr "zeroOrMore is transformed into a choice")
            ( (A.mkElement (QN "" "oneOrMore" relaxNamespace) 
                 (setChangesAttr $ "zeroOrMore is transformed into a " ++
                                   "choice between oneOrMore and empty"
                 )
                 getChildren
              )
              <+>
              (A.mkElement (QN "" "empty" relaxNamespace) 
                 (setChangesAttr $ "new empty-Pattern: zeroOrMore is transformed " ++
                                   "into a choice between oneOrMore and empty"
                 )
                 none
              )
            )
        )
        `when`
        (isElem >>> hasName "zeroOrMore")
      )
    )
  ) `when` collectErrors


-- ------------------------------------------------------------


restrictionsStep1 :: IOSArrow XmlTree XmlTree
restrictionsStep1 =
  ( processTopDown (
      ( (mkRelaxError "" $ "An except element that is a child of an anyName " ++
                           "element must not have any anyName descendant elements")
        `when` 
        ( isElem >>> hasName "anyName"
          >>> getChildren
          >>>
          isElem >>> hasName "except"
          >>>
          deep (isElem >>> hasName "anyName")
        )
      )
      >>>
      ( (mkRelaxError "" $ "An except element that is a child of an nsName element " ++
                           "must not have any nsName or anyName descendant elements."
        )
        `when` 
        ( isElem >>> hasName "nsName"
          >>>
          getChildren
          >>> 
          isElem >>> hasName "except"
          >>> 
          deep (isElem >>> (hasName "anyName" `orElse` hasName "nsName"))
        )
      )
      >>>
      ( (mkRelaxError "" $ "A name element that occurs as the first child or descendant of " ++
                           "an attribute and has an ns attribute with an empty value must " ++
                           "not have content equal to xmlns"
        )
        `when` 
        ( isElem >>> hasName "attribute"
          >>>
          listA getChildren
          >>>
          arr head
          >>>
          ( multi (isElem >>> hasName "name" >>> hasAttr "ns") )
          >>>
          ( ( getAttrValue "ns" >>> isA (== ""))
            `guards`
            (getChildren >>> getText >>> isA (== "xmlns"))
          )
        )
      ) 
      >>>
      ( (mkRelaxError "" $ "A name or nsName element that occurs as the first child or " ++
                           "descendant of an attribute must not have an ns attribute " ++
                           "with value http://www.w3.org/2000/xmlns"
        )
        `when` 
        ( isElem >>> hasName "attribute"
          >>>
          listA getChildren
          >>>
          arr head
          >>>
          ( multi (checkElemName ["name", "nsName"] >>> hasAttr "ns") )
          >>>
          getAttrValue "ns"
          >>>
          isA (compareURI xmlnsNamespace)
        )
      ) 
      >>> -- A data or value element must be correct in its use of datatypes.
      ( (checkDatatype $<< getAttrValue "datatypeLibrary" &&& getAttrValue "type")
        `when`
        (isElem >>> (hasName "data" `orElse` hasName "value"))
      )
    )
  ) `when` collectErrors
  where 
  -- the datatypeLibrary attribute must identify a valid datatype library
  checkDatatype :: Uri -> DatatypeName -> IOSArrow XmlTree XmlTree
  checkDatatype libName typeName 
    = ifP (const $ elem libName $ map fst datatypeLibraries)    
        (checkType libName typeName allowedDataTypes)
        (mkRelaxError "" $ "DatatypeLibrary " ++ libName ++ " not found")
    where
    DTC _ _ allowedDataTypes = fromJust $ lookup libName datatypeLibraries
            
  -- the type attribute must identify a datatype within the datatype library identified
  -- by the value of the datatypeLibrary attribute. 
  checkType :: Uri -> DatatypeName -> AllowedDatatypes -> IOSArrow XmlTree XmlTree
  checkType libName typeName allowedTypes
    = ifP (const $ elem typeName $ map fst allowedTypes)
        (checkParams typeName libName getParams $< 
             (listA (getChildren >>> hasName "param" >>> getAttrValue "name"))
        )
        ( mkRelaxError "" $ "Datatype " ++ typeName ++ 
                            " not declared for DatatypeLibrary " ++ libName
        )
    where
    getParams = fromJust $ lookup typeName allowedTypes
            
  -- For a data element, the parameter list must be one that is allowed by the datatype               
  checkParams :: DatatypeName -> Uri -> AllowedParams -> [ParamName] -> IOSArrow XmlTree XmlTree
  checkParams typeName libName allowedParams paramNames
    = (mkRelaxError "" $ "Param(s): " ++ formatStringList ", " diff ++ 
                         " not allowed for Datatype " ++ typeName ++ 
                         " in Library " ++ (if libName == "" then relaxNamespace else libName)
      )
      `when`
      (isElem >>> hasName "data" >>> isA (const $ diff /= [])) 
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
simplificationStep5 =
  ( processTopDown ( 
      ( ( ( (deep (isElem >>> hasName "relaxError"))               
            <+>
            (mkRelaxError "" "A grammar must have a start child element")
          )
          `when`
          (neg (getChildren >>> hasName "start"))
        )
        >>>
        -- For each grammar element, all define elements with the same 
        -- name are combined together.
        (combinePatternList "define" $< (getPatternNamesInGrammar "define" >>> arr nub))
        >>>
        -- Similarly, for each grammar element all start elements 
        -- are combined together.
        (combinePatternList "start" $< (getPatternNamesInGrammar "start" >>> arr nub))
      )  
      `when`
      (isElem >>> hasName "grammar")
    )  
    >>>
    ( -- transform the top-level pattern p into <grammar><start>p</start></grammar>.
      ( replaceChildren (A.mkElement (QN "" "grammar" relaxNamespace) none 
                           (A.mkElement (QN "" "start" relaxNamespace) none getChildren)
                        )
      )
      `when`
      (neg (getChildren >>> hasName "grammar"))      
    )
    >>>
    renameDefines $<< ( getPatternNamesInGrammar "define"
                        >>>
                        (createUniqueNames $< (getAndSetCounter "define_id" >>> arr read))
                        &&&
                        constA []
                      )
    >>>
    -- Move all define elements to be children of the top-level grammar element
    ( processChildren ( -- root node
        processChildren ( -- the first grammar pattern remains unchanged
          ( deleteAllDefines
            <+>
            (getAllDefines >>> processChildren deleteAllDefines)          
          )
          >>>
          processTopDown (
            ( -- Replace each nested grammar element by the child of its start element
              (getChildren >>> hasName "start" >>> getChildren)
              `when`
              (isElem >>> hasName "grammar")
            )
            >>> 
            ( -- Rename each parentRef element to ref.
              (setElemName (QN "" "ref" relaxNamespace))
              `when`
              (isElem >>> hasName "parentRef")
            )
          )
        )  
      )
    )
  ) `when` collectErrors
  where
  getPatternNamesInGrammar :: (ArrowXml a) => String -> a XmlTree [String]
  getPatternNamesInGrammar pattern
    = processChildren (
        processTopDown (
          none `when` (isElem >>> hasName "grammar")
        )
      )
      >>>
      listA ( (multi (isElem >>> hasName pattern))
              >>> 
              getAttrValue "name"
            )


  createUniqueNames :: Int -> IOSArrow [String] RefList
  createUniqueNames num
    = arr (\l -> unique l num)
      >>>
      perform (setParamInt "define_id" $< arr (max num . getNextValue))
    where
    unique :: [String] -> Int -> RefList
    unique []     _    = []
    unique (x:xs) num' = (x, (show num')):(unique xs (num'+1))
    getNextValue :: RefList -> Int
    getNextValue [] = 0
    getNextValue rl = maximum (map (read . snd) rl) + 1    

           
  renameDefines :: RefList -> RefList -> IOSArrow XmlTree XmlTree
  renameDefines ref parentRef
    = processChildren (
        choiceA [
          (isElem >>> hasName "define")
            :-> ( -- the original name is needed for error messages
                  addAttr defineOrigName $< (getAttrValue "name")
                  >>>
                  -- rename the define-pattern
                  -- the new name is looked up in the ref table
                  addAttr "name" $< ( getAttrValue "name"
                                      >>>
                                      arr (\n -> fromJust $ lookup n ref)
                                    )
                  >>>
                  renameDefines ref parentRef
                ),
          (isElem >>> hasName "grammar") 
            :-> (renameDefines $<< ( -- compute all define names in the grammar
                                     getPatternNamesInGrammar "define"
                                     >>>
                                     -- create a new (unique) name for all define names
                                     (createUniqueNames $< (getParamInt 0 "define_id"))
                                   ) 
                                   &&&
                                   -- set the old ref list to be the new parentRef list
                                   constA ref
                ),
          (isElem >>> hasName "ref") 
            :-> (ifA ( getAttrValue "name"
                       >>>
                       isA (\name -> (elem name (map fst ref)))
                     )
                   ( -- the original name is needed for error messages
                     addAttr defineOrigName $< (getAttrValue "name")
                     >>>
                     -- rename the ref-pattern
                     -- the new name is looked up in the ref table
                     addAttr "name" $< ( getAttrValue "name"
                                         >>>
                                         arr (\n -> fromJust $ lookup n ref)
                                       )
                   )
                   ( -- the referenced pattern does not exist in the schema
                     mkRelaxError "" $< ( getAttrValue "name"
                                         >>>
                                         arr (\n -> "Define-Pattern with name " ++ n ++ 
                                                    " referenced in ref-Pattern not " ++
                                                    "found in schema"
                                             )
                                       )
                   )
                ),
          (isElem >>> hasName "parentRef") -- same as ref, but the parentRef list is used
            :-> (ifA ( getAttrValue "name"
                       >>>
                       isA (\name -> (elem name (map fst parentRef)))
                     )
                   ( addAttr defineOrigName $< (getAttrValue "name")
                     >>>
                     addAttr "name" $< ( getAttrValue "name"
                                         >>>
                                         arr (\n -> fromJust $ lookup n parentRef)
                                       )
                   )
                   (mkRelaxError "" $< ( getAttrValue "name" >>> 
                                         arr (\n -> "Define-Pattern with name " ++ n ++ 
                                                    " referenced in parentRef-Pattern " ++
                                                    "not found in schema"
                                             )
                                       )
                   )
                ),
          this :-> (renameDefines ref parentRef)
        ]
      )


  getAllDefines :: IOSArrow XmlTree XmlTree
  getAllDefines = multi $ isElem >>> hasName "define"

  deleteAllDefines :: IOSArrow XmlTree XmlTree      
  deleteAllDefines = processTopDown $ none `when` (isElem >>> hasName "define")


  combinePatternList :: String -> [String] -> IOSArrow XmlTree XmlTree
  combinePatternList _ [] = this
  combinePatternList pattern (x:xs)
    = (replaceChildren $ combinePattern pattern x)
      >>>
      combinePatternList pattern xs
        
  -- combine a define- or start-pattern (first parameter) with a
  -- specific name (second parameter)
  combinePattern :: String -> String -> IOSArrow XmlTree XmlTree 
  combinePattern pattern name
     = createPatternElems pattern name
       <+>
       (getChildren >>> deletePatternElems pattern name)           

  createPatternElems :: String -> String -> IOSArrow XmlTree XmlTree 
  createPatternElems pattern name 
    = ( ( (listA (getElems pattern name >>> getAttrValue "combine"))
          >>>
          checkPatternCombine pattern name
        ) 
        -- After determining this unique value, the combine attributes are removed.
        &&&
        (listA (getElems pattern name >>> removeAttr "combine")))                      
        >>> -- ((errorCode::Int,errorMessage::String), result::XmlTrees)
        choiceA [
          isA (\((code,_) , _)   -> code == 0)
                 :-> (mkRelaxError "" $< arr (snd . fst)),
          isA (\((code,str) , _) -> code == 1 && str == "")
                 :-> (arrL snd),
          isA (\((code,str) , _) -> code == 1 && str /= "")
                 :-> (createPatternElem pattern name $<< 
                       ( arr (snd . fst) &&& (arr snd) )
                     ),                 
          this   :-> (mkRelaxError "" $ "Can't create Pattern: " ++ pattern ++ 
                                        " with name " ++ name ++ " in createPatternElems"
                     )
        ]


  createPatternElem :: (ArrowXml a) => String -> String -> String -> XmlTrees -> a n XmlTree  
  createPatternElem pattern name combine trees
    = A.mkElement (QN "" pattern relaxNamespace) (mkAttr (QN "" "name" "") (txt name)) 
       ( (A.mkElement (QN "" combine relaxNamespace) none 
             (arrL (const trees) >>> getChildren)
         )
         >>>
         wrapPattern2Two combine
       )
                                         
  checkPatternCombine :: (ArrowXml a) => String -> String -> a [String] (Int, String)
  checkPatternCombine pattern name 
    = choiceA [
         -- just one pattern with that name -> ok, no combine is needed
        (isA (\cl -> length cl == 1)) :-> constA (1, ""),
        (isA (\cl -> (length $ elemIndices "" cl) > 1)) 
             :-> constA (0, "More than one " ++ pattern ++ "-Pattern: " ++ name ++ 
                            " without an combine-attribute in the same grammar"),
        (isA (\cl -> (length $ nub $ deleteBy (==) "" cl) > 1)) 
             :-> arr (\cl -> (0, "Different combine-Attributes: " ++ 
                                 (formatStringList ", " $ noDoubles cl) ++
                                 " for the " ++ pattern ++ "-Pattern " ++
                                 name ++ " in the same grammar")
                     ),
        -- ok -> combine value is returned
        this :-> arr (\cl -> (1, fromJust $ find (/= "") cl))
      ]

  getElems :: (ArrowXml a) => String -> String -> a XmlTree XmlTree
  getElems pattern name
    = getChildren
      >>> 
      choiceA [
        ( isElem >>> hasName pattern
          >>> 
          getAttrValue "name" >>> isA (== name))     :-> (this <+> getElems pattern name),
        (isElem >>> getName >>> isA (== "grammar") ) :-> none,
        (constA "foo" >>> isA (== "foo"))            :-> (getElems pattern name)        
      ]

  deletePatternElems :: (ArrowXml a) => String -> String -> a XmlTree XmlTree
  deletePatternElems pattern name
    = choiceA [
        ( isElem >>> hasName pattern
          >>> 
          getAttrValue "name" >>> isA (== name))
                                          :-> none,
        (isElem >>> getName >>> isA (== "grammar"))
                                          :-> this,
        (constA "foo" >>> isA (== "foo")) :-> ( processChildren $
                                                  deletePatternElems pattern name
                                              )
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
      processChildren (insertChildrenAt 1 (getParam "elementTable"))
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
        (isElem >>> hasName "ref")
             :-> (ifA ( getAttrValue "name"
                        >>>
                        isA (\name -> elem name (map fst foundNames))
                      )
                    -- we have found a loop if the name is in the list
                    (mkRelaxError "" $< ( getAttrValue defineOrigName
                                          >>>
                                          arr (\n -> "Recursion in ref-Pattern: " ++ 
                                                     formatStringList " -> " 
                                                     (reverse $ (n:) $ map snd foundNames)
                                              )
                                        )
                    )
                    (replaceRef $<< getAttrValue "name" &&& getAttrValue defineOrigName)
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
    = processChildren(
        choiceA [
          (isElem >>> hasName "element")
              :-> ( ifP (const parentIsDefine)
                      (processElements False)
                      ( processElements' $<< getAndSetCounter "define_id" 
                                             &&&
                                             getDefineName
                      )
                  ),
          (isElem >>> hasName "define")  :-> (processElements True),
          this                           :-> (processElements False)
        ])
    where
    getDefineName :: IOSArrow XmlTree String
    getDefineName = listA getChildren 
                    >>>
                    arr head
                    >>>
                    fromLA createNameClass
                    >>>
                    arr show
    
    processElements' :: NewName -> OldName -> IOSArrow XmlTree XmlTree
    processElements' name oldname
      = storeElement name oldname
        >>> 
        A.mkElement (QN "" "ref" relaxNamespace) (createAttr name oldname) none

    storeElement :: NewName -> OldName -> IOSArrow XmlTree XmlTree
    storeElement name oldname
      = perform $ 
          ( A.mkElement (QN "" "define" relaxNamespace)
             (createAttr name oldname) (processElements False)
          )
          &&&
          (listA $ getParam "elementTable")
          >>>
          arr2 (:)
          >>>
          setParamList "elementTable"

    createAttr :: NewName -> OldName -> IOSArrow XmlTree XmlTree
    createAttr name oldname
      = mkAttr (QN "" "name" "") (txt name) 
        <+>
        mkAttr (QN "" defineOrigName "") (txt $ "created for element " ++ oldname)
       
  getExpandableDefines :: (ArrowXml a) => a XmlTree Env 
  getExpandableDefines 
    = listA $ (multi ( ( isElem >>> hasName "define"
                         >>>
                         getChildren
                         >>>
                         neg (hasName "element")
                       )
                       `guards`
                       this
                     )
              )
              >>> 
              (getAttrValue "name" &&& this)
  
  deleteExpandableDefines :: (ArrowXml a) => a XmlTree XmlTree
  deleteExpandableDefines 
    = processTopDown $ none
                       `when` 
                       ( isElem >>> hasName "define"
                         >>> 
                         getChildren
                         >>>
                         neg (hasName "element")
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
simplificationStep7 =
  ( perform (setParamInt "changeTree" 0) -- 0 = no changes, 1 = changes performed
    >>>
    processTopDownWithAttrl ( 
      ( -- An attribute, list, group, interleave, or oneOrMore element that has a 
        -- notAllowed child element is transformed into a notAllowed element.       
        ( ( A.mkElement (QN "" "notAllowed" relaxNamespace) none none
            >>>
            perform (setParamInt "changeTree" 1)
          )
          `whenNot` -- keep all errors
          (deep (isElem >>> hasName "relaxError"))
        )
        `when`
        ( checkElemName ["attribute", "list", "group", "interleave", "oneOrMore"] 
          >>>
          getChildren
          >>>
          hasName "notAllowed"
        )
      )
      >>>                                   
      ( -- A choice element that has two notAllowed child elements is 
        -- transformed into a notAllowed element
        ( A.mkElement (QN "" "notAllowed" relaxNamespace) none none
          >>>
          perform (setParamInt "changeTree" 1)
        )
        `when`
        ( isElem >>> hasName "choice"
          >>>
          listA (getChildren >>> hasName "notAllowed")
          >>> 
          isA (\s -> length s == 2)
        )
      )
      >>>
      ( -- A choice element that has one notAllowed child element is 
        -- transformed into its other child element.        
        ( getChildren >>> neg (hasName "notAllowed")
          >>>
          perform (setParamInt "changeTree" 1)
        )
        `when`
        (isElem >>> hasName "choice" >>> getChildren >>> hasName "notAllowed")
      )
      >>>       
      ( -- An except element that has a notAllowed child element is removed.
        ( (none >>> perform (setParamInt "changeTree" 1)) 
          `whenNot` -- keep all errors
          (deep (isElem >>> hasName "relaxError"))
        )
        `when`
        (isElem >>> hasName "except" >>> getChildren >>> hasName "notAllowed")
      )
      >>> -- transforming the empty pattern (4.21)
      ( -- A group, interleave or choice element that has two empty child elements
        -- is transformed into an empty element.
        ( A.mkElement (QN "" "empty" relaxNamespace) none none 
          >>>
          perform (setParamInt "changeTree" 1)
        )
        `when`
        ( checkElemName ["group", "interleave", "choice"]
          >>>
          listA (getChildren >>> hasName "empty")
          >>>
          isA (\s -> length s == 2)
        )
      )
      >>>
      ( -- A group or interleave element that has one empty child element 
        -- is transformed into its other child element.
        (getChildren >>> neg (hasName "empty") >>> perform (setParamInt "changeTree" 1))
        `when`
        (checkElemName ["group", "interleave"] >>> getChildren >>> hasName "empty")
      )
      >>>
      ( -- A choice element whose second child element is an empty element is transformed 
        -- by interchanging its two child elements.
        changeChoiceChildren
        `when`
        (isElem >>> hasName "choice" >>> getChildren >>> hasName "empty")
      )
      >>>
      ( -- A oneOrMore element that has an empty child element
        -- is transformed into an empty element.
        ( A.mkElement (QN "" "empty" relaxNamespace) none none
          >>>
          perform (setParamInt "changeTree" 1)
        )
        `when`
        (isElem >>> hasName "oneOrMore" >>> getChildren >>> hasName "empty")
      )
    )
    >>>
    -- The preceding transformations are applied repeatedly
    -- until none of them is applicable any more.
    simplificationStep7 `when` (getParamInt 0 "changeTree" >>> isA (== 1))
  ) `when` collectErrors
  where
  changeChoiceChildren :: IOSArrow XmlTree XmlTree
  changeChoiceChildren
    = ( replaceChildren ( A.mkElement (QN "" "empty" relaxNamespace) none none
                          <+> 
                          (getChildren >>> neg (hasName "empty"))
                        ) 
        >>>
        perform (setParamInt "changeTree" 1)
      )
      `when`
      ( listA (getChildren >>> getName)
        >>>
        isA (\childNames -> head childNames /= "empty")
      )


-- ------------------------------------------------------------


simplificationStep8 :: IOSArrow XmlTree XmlTree
simplificationStep8 = -- Remove any define element that is not reachable.
  (removeUnreachableDefines $<<< getAllDeepDefines
                                 &&&
                                 constA []
                                 &&&
                                 getRefsFromStartPattern
  )
  `when` collectErrors
               

-- ------------------------------------------------------------


restrictionsStep2 :: IOSArrow XmlTree XmlTree
restrictionsStep2 =
  processTopDown (
    choiceA [
-- 7.1.1. attribute pattern, the following paths are prohibited:
--        attribute//(ref | attribute)
      (isElem >>> hasName "attribute") :-> 
        ( ( (deep (isElem >>> hasName "relaxError"))               
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA ( getChildren
                                          >>> 
                                          deep (checkElemName ["attribute", "ref"])
                                          >>>
                                          (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  >>>
                                  arr (\n -> formatStringList "-, " n ++ 
                                             "-Pattern not allowed as descendent(s)" ++
                                             " of a attribute-Pattern"
                                      )
                                )
                               )
            ) 
          )
          `when` 
          (getChildren >>> deep (checkElemName ["attribute", "ref"]))
        ),

-- 7.1.2. oneOrMore pattern, the following paths are prohibited:
--        oneOrMore//(group | interleave)//attribute
      (isElem >>> hasName "oneOrMore") :->
        ( ( (deep (isElem >>> hasName "relaxError"))               
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA ( getChildren
                                          >>> 
                                          deep (checkElemName ["group", "interleave"])
                                          >>>
                                          (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  &&&
                                  getChangesAttr
                                  >>>
                                  arr2 (\n c -> formatStringList "-, " n ++ 
                                                "-Pattern not allowed as descendent(s) " ++
                                                "of a oneOrMore-Pattern" ++ c ++ 
                                                " followed by an attribute descendent"
                                       )
                                )
                               )
            ) 
          )
          `when` 
          ( getChildren >>> deep (checkElemName ["group", "interleave"])
            >>> 
            getChildren >>> deep (isElem >>> hasName "attribute")
          )
        ),

-- 7.1.3. list pattern, the following paths are prohibited:
--        list//( list | ref | attribute | text | interleave)
      (isElem >>> hasName "list") :-> 
        ( ( (deep (isElem >>> hasName "relaxError"))               
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA ( getChildren
                                          >>> 
                                          deep (checkElemName ["list", "attribute", "ref"
                                                              , "text", "interleave"])
                                          >>>
                                          (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  >>> 
                                  arr (\n -> formatStringList "-, " n ++ 
                                             "-Pattern not allowed as descendent(s) of a list-Pattern")
                                )
                               )
            ) 
          )
          `when` 
          ( getChildren
            >>>
            deep (checkElemName ["list", "attribute", "ref", "text", "interleave"])
          )
        ), 

-- 7.1.4. except in data pattern, the following paths are prohibited:
--        data/except//(attribute | ref | text | list | group | interleave | oneOrMore | empty)
      (isElem >>> hasName "data") :-> 
        ( ( (deep (isElem >>> hasName "relaxError"))               
            <+>
            ( mkRelaxError $<< (getChangesAttr
                                &&&
                                ( listA (getChildren
                                         >>> 
                                         deep (checkElemName ["attribute", "ref", "text", "list", 
                                                              "group", "interleave", "oneOrMore", "empty"])
                                         >>>
                                         (getName &&& getChangesAttr >>> arr2 (++))
                                        )
                                  >>>
                                  arr (\n -> formatStringList "-, " n ++ 
                                             "-Pattern not allowed as descendent(s) of a data/except-Pattern")
                                )
                               )
            ) 
          )
          `when` 
          ( getChildren >>> isElem >>> hasName "except" >>> 
            deep ( checkElemName ["attribute", "ref", "text", "list", "group"
                                 , "interleave", "oneOrMore", "empty"])
          )
        ),

-- 7.1.5. start element, the following paths are prohibited:
--        start//(attribute | data | value | text | list | group | interleave | oneOrMore | empty)
      (isElem >>> hasName "start") :-> 
        ( ( (deep (isElem >>> hasName "relaxError"))               
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
                                  arr (\n -> formatStringList "-, " n ++ 
                                             "-Pattern not allowed as descendent(s) of a start-Pattern")
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
restrictionsStep3 =
  processTopDown (
      ( (deep (isElem >>> hasName "relaxError"))               
        <+>
        ( mkRelaxError "" $< 
           ( -- getAttrValue "name"
             (getChildren >>> hasName "name" >>> getChildren >>> getText)
             >>> 
             arr (\n -> "Content of element " ++ n ++ " contains a pattern that can match " ++
                        "a child and a pattern that matches a single string")
           )
        )
      )
      `when`
      ( isElem >>> hasName "element"
        >>>
        listA getChildren
        >>> 
        arr last
        >>>
        getContentType >>> isA (== CTNone)
      )
  ) `when` collectErrors

    
    
getContentType :: IOSArrow XmlTree ContentType
getContentType =
  choiceA [
    (isElem >>> hasName "value")      :-> (constA CTSimple),
    (isElem >>> hasName "data")       :-> processData,
    (isElem >>> hasName "list")       :-> (constA CTSimple),
    (isElem >>> hasName "text")       :-> (constA CTComplex),
    (isElem >>> hasName "ref")        :-> (constA CTComplex),
    (isElem >>> hasName "empty")      :-> (constA CTEmpty),
    (isElem >>> hasName "attribute")  :-> processAttribute,
    (isElem >>> hasName "group")      :-> processGroup,
    (isElem >>> hasName "interleave") :-> processInterleave,
    (isElem >>> hasName "oneOrMore")  :-> processOneOrMore,
    (isElem >>> hasName "choice")     :-> processChoice
  ]
  where
  processData :: IOSArrow XmlTree ContentType
  processData
    = ifA (neg (getChildren >>> hasName "except"))
        (constA CTSimple)
        ( getChildren
          >>>
          hasName "except"
          >>>
          getChildren
          >>>
          getContentType
          >>>
          ifP (/= CTNone) (constA CTSimple) (constA CTNone)
        )
  processAttribute :: IOSArrow XmlTree ContentType
  processAttribute
    = ifA ( listA getChildren >>> arr last
            >>>
            getContentType >>> isA (/= CTNone)
          )
        (constA CTEmpty)
        (constA CTNone)
  
  processGroup :: IOSArrow XmlTree ContentType
  processGroup
    = listA getChildren
      >>>
      ((arr head >>> getContentType) &&& (arr last >>> getContentType))
      >>>
      arr2 (\a b -> if isGroupable a b then max a b else CTNone)
  
  processInterleave :: IOSArrow XmlTree ContentType
  processInterleave
     = listA getChildren
       >>> 
       ((arr head >>> getContentType) &&& (arr last >>> getContentType))
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
        (getChildren >>> getContentType)
        (constA CTNone)
  
  processChoice :: IOSArrow XmlTree ContentType
  processChoice
    = listA getChildren
      >>> 
      ((arr head >>> getContentType) &&& (arr last >>> getContentType))
      >>> 
      arr2 max

  isGroupable :: ContentType -> ContentType -> Bool
  isGroupable CTEmpty   _         = True
  isGroupable _         CTEmpty   = True
  isGroupable CTComplex CTComplex = True
  isGroupable _         _         = False
    
    
    
    
checkPattern :: IOSArrow (XmlTree, ([NameClass], [NameClass])) XmlTree
checkPattern = (\ (_, (a, b)) -> isIn a b) `guardsP` (arr fst)
  where
  isIn :: [NameClass] -> [NameClass] -> Bool
  isIn _ []      = False
  isIn [] _      = False
  isIn (x:xs) ys = (any (overlap x) ys) || isIn xs ys


occur :: String -> IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
occur name fct
  = choiceA [ 
      (isElem >>> hasName name) :-> fct,
      (checkElemName ["choice", "interleave", "group", "oneOrMore"])
                                :-> (getChildren >>> occur name fct)
    ]

            
-- ------------------------------------------------------------            


-- Duplicate attributes are not allowed. -> fertig
-- Attributes using infinite name classes must be repeated; an attribute element that 
-- has an anyName or nsName descendant element must have a oneOrMore ancestor element. -> fertig
restrictionsStep4 :: IOSArrow XmlTree XmlTree          
restrictionsStep4 =
  ( -- berechnet alle define-Namen (fuer ref-Pattern) und Nameclasses der element-Pattern
    restrictionsStep4' $< listA ( deep (isElem >>> hasName "define")
                                  >>>
                                  ( getAttrValue "name" 
                                    &&& 
                                    ( getChildren
                                      >>>
                                      listA getChildren
                                      >>>
                                      arr head
                                      >>> 
                                      fromLA createNameClass
                                    )
                                  )
                                )
  ) `when` collectErrors

restrictionsStep4' :: [(String, NameClass)] -> IOSArrow XmlTree XmlTree          
restrictionsStep4' nc =
  processTopDown (
    ( 
      ( (deep (isElem >>> hasName "relaxError"))               
        <+>
        ( mkRelaxError "" $< 
          ( getAttrValue "name"
            >>>
            arr (\n -> "Both attribute-pattern occuring in an " ++ 
                       n ++ " belong to the same name-class")
          )
        )
      )    
      `when` 
      ( isElem >>> (hasName "group" `orElse` hasName "interleave")
        >>>
        listA getChildren
        >>> 
        ( (arrL id)
          &&& 
          ( arr head
            >>> 
            listA ( occur "attribute" (listA getChildren >>> arr head)
                    >>> 
                    fromLA createNameClass
                  )
          ) 
          &&&
          ( arr last
            >>> 
            listA ( occur "attribute" (listA getChildren >>> arr head)
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
      ( (deep (isElem >>> hasName "relaxError"))               
        <+>
        (mkRelaxError "" $ "An attribute that has an anyName or nsName descendant element " ++
                           "must have a oneOrMore ancestor element")
      )
      `when`
      (isElem >>> hasName "element" >>> checkInfiniteAttribute)
    )
    >>>
    ( ( (deep (isElem >>> hasName "relaxError"))               
        <+>
        ( mkRelaxError "" $ "Both element-pattern occuring in an interleave " ++
                            "belong to the same name-class"
        )
      )
      `when` 
      ( isElem >>> hasName "interleave" >>> listA getChildren
        >>> 
        ( (arrL id)
          &&&
          (arr head >>> listA (occur "ref" this >>> getAttrValue "name")) 
          &&&
          (arr last >>> listA (occur "ref" this >>> getAttrValue "name"))
        )
        >>>
        checkNames
      )
    )
    >>>     
    ( ( (deep (isElem >>> hasName "relaxError"))               
        <+> 
        (mkRelaxError "" "A text pattern must not occur in both children of an interleave")
      )
      `when` 
      (isElem >>> hasName "interleave" >>> checkText)
    )
  )
  where
  checkInfiniteAttribute :: IOSArrow XmlTree XmlTree
  checkInfiniteAttribute
    = getChildren
      >>>
      choiceA [ 
        (isElem >>> hasName "oneOrMore") :-> none,
        ( isElem >>> hasName "attribute"
          >>>
          deep (isElem >>> (hasName "anyName" `orElse` hasName "nsName"))
        )                                :-> this,
        this                             :-> checkInfiniteAttribute
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
  checkText = listA getChildren
              >>> 
              ( (arr head >>> occur "text" this)
                `guards` 
                (arr last >>> occur "text" this)
              )

       
-- ------------------------------------------------------------


overlap :: NameClass -> NameClass -> Bool
overlap nc1 nc2
  = any (bothContain nc1 nc2) (representatives nc1 ++ representatives nc2)

bothContain :: NameClass -> NameClass -> QName -> Bool
bothContain nc1 nc2 qn = contains nc1 qn && contains nc2 qn

illegalLocalName :: LocalName
illegalLocalName = ""

illegalUri :: Uri
illegalUri = "\x1"

representatives :: NameClass -> [QName]
representatives AnyName = [QN "" illegalLocalName illegalUri]
representatives (AnyNameExcept nc) =
  (QN "" illegalLocalName illegalUri) : (representatives nc)
representatives (NsName ns) = [QN "" illegalLocalName ns]
representatives (NsNameExcept ns nc) =
  (QN "" illegalLocalName ns) : (representatives nc)
representatives (Name ns ln) = [QN "" ln ns]
representatives (NameClassChoice nc1 nc2) =
  (representatives nc1) ++ (representatives nc2)       
representatives _ = []


-- -------------------------------------------------------------------------------------------------------            

resetStates :: IOSArrow XmlTree XmlTree
resetStates = perform (constA $ setParamInt "define_id" 0)
              >>>
              perform (constA [] >>> setParamList "elementTable")
              >>>
              perform (constA $ setParamInt a_numberOfErrors 0)


getAllDeepDefines :: IOSArrow XmlTree Env
getAllDeepDefines = listA $ deep (isElem >>> hasName "define")
                            >>> 
                            (getAttrValue "name" &&& this)


-- | Return all reachable defines from the start pattern
getRefsFromStartPattern :: IOSArrow XmlTree [String]
getRefsFromStartPattern
  = listA $ getChildren
            >>>
            isElem >>> hasName "grammar"
            >>>
            getChildren
            >>>
            isElem >>> hasName "start"
            >>> 
            deep (isElem >>> hasName "ref")
            >>>
            getAttrValue "name"


removeUnreachableDefines :: Env -> [String] -> [String] -> IOSArrow XmlTree XmlTree
removeUnreachableDefines allDefs processedDefs reachableDefs
  = ifP (const $ unprocessedDefs /= [])
      (removeUnreachableDefines allDefs (nextTreeName : processedDefs) $< newReachableDefs)
      ( processChildren $ -- root node
          processChildren $ -- first grammar
            none 
            `when`
            ( isElem >>> hasName "define"
              >>>
              getAttrValue "name"
              >>> 
              isA (\n -> not $ elem n reachableDefs)
            )
      )
  where
  unprocessedDefs :: [String]
  unprocessedDefs = reachableDefs \\ processedDefs
  newReachableDefs :: IOSArrow n [String]
  newReachableDefs = constA getTree
                     >>> 
                     listA ( deep (isElem >>> hasName "ref")
                             >>>
                             getAttrValue "name"
                           )
                     >>>
                     arr (noDoubles . (reachableDefs ++))
  getTree :: XmlTree
  getTree = fromJust $ lookup nextTreeName allDefs
  nextTreeName :: String
  nextTreeName = head unprocessedDefs


-- -------------------------------------------------------------------------------------------------------    
    

checkElemName :: [String] -> IOSArrow XmlTree XmlTree
checkElemName l = (isElem >>> getLocalPart >>> isA (\s -> elem s l))
                  `guards`
                  this



wrapPattern2Two :: (ArrowXml a) => String -> a XmlTree XmlTree
wrapPattern2Two name 
  = choiceA [
      (listA getChildren >>> isA (\cl -> length cl > 2))
          :-> ( replaceChildren ( (A.mkElement (QN "" name relaxNamespace) none 
                                      (listA getChildren >>> arrL (take 2))
                                  ) 
                                  <+> 
                                  (listA getChildren >>> arrL (drop 2))
                                )
                >>>
                wrapPattern2Two name
              ),
      (listA getChildren >>> isA (\cl -> length cl == 1)) :-> getChildren,
      (listA getChildren >>> this) :-> this
    ]

    

mkRelaxError :: String -> String -> IOSArrow n XmlTree
mkRelaxError changesStr errStr
  = perform (getAndSetCounter a_numberOfErrors)
    >>>
    A.mkElement (QN "" "relaxError" relaxNamespace) none none
    >>>
    addAttr "desc" errStr
    >>>
    ( addAttr "changes" changesStr
      `whenP`
      (const $ changesStr /= "")
    )

collectErrors :: IOSArrow XmlTree XmlTree
collectErrors
  = none
    `when`
    ( stopAfterFirstError
      >>>
      getParamInt 0 a_numberOfErrors >>> isA (>0)
    )
  where
  stopAfterFirstError = getParamString a_do_not_collect_errors
                        >>>
                        isA (== "1")
 

-- | Returns the list of simplification errors or 'none'
getErrors :: IOSArrow XmlTree XmlTree
getErrors = (getParamInt 0 a_numberOfErrors >>> isA (>0))
            `guards`
            (root [] [multi (isElem >>> hasName "relaxError")])

setChangesAttr :: String -> IOSArrow XmlTree XmlTree
setChangesAttr str
  = ifA (hasAttr a_relaxSimplificationChanges)
      ( processAttrl $
          changeAttrValue (++ (", " ++ str))
          `when`
          (hasName a_relaxSimplificationChanges)
      )
      (mkAttr (QN "" a_relaxSimplificationChanges "") (txt str))


getChangesAttr :: IOSArrow XmlTree String
getChangesAttr
  = getAttrValue a_relaxSimplificationChanges 
    &&& 
    getParamString a_output_changes
    >>>
    ifP (\(changes, param) -> changes /= "" && param == "1")
      (arr2 $ \l _ -> " (" ++ l ++ ")")
      (constA "")
      

getAndSetCounter :: String -> IOSArrow b String
getAndSetCounter name   
  = genNewId $< getParamInt 0 name
  where
  genNewId :: Int -> IOSArrow b String
  genNewId i = setParamInt name (i+1) >>> constA (show i)

     
-- -------------------------------------------------------------------------------------------------------            

-- | Creates the simple form of a Relax NG schema
-- (see also: 'relaxOptions')

createSimpleForm :: Attributes -> Bool -> Bool -> Bool -> IOSArrow XmlTree XmlTree
createSimpleForm remainingOptions checkRestrictions validateExternalRef validateInclude
    = if checkRestrictions
      then createSimpleWithRest
      else createSimpleWithoutRest
    where

    createSimpleWithRest :: IOSArrow XmlTree XmlTree
    createSimpleWithRest
	= seqA $ concat [ simplificationPart1
			, restrictionsPart1
			, simplificationPart2
			, restrictionsPart2
			, finalCleanUp
			]

    createSimpleWithoutRest :: IOSArrow XmlTree XmlTree
    createSimpleWithoutRest
	= seqA $ concat [ simplificationPart1
			, simplificationPart2
			, finalCleanUp
			]
    simplificationPart1 :: [IOSArrow XmlTree XmlTree]
    simplificationPart1
	= [ propagateNamespaces
	  , simplificationStep1
	  , simplificationStep2 remainingOptions validateExternalRef validateInclude [] []
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
	  , resetStates
	  ]

    cleanUp :: IOSArrow XmlTree XmlTree
    cleanUp = processTopDown $ 
              removeAttr a_relaxSimplificationChanges
	      >>>
              removeAttr defineOrigName
