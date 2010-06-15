-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlArrow
   Copyright  : Copyright (C) 2005-9 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Basic arrows for processing XML documents

   All arrows use IO and a global state for options, errorhandling, ...

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlArrow
    ( module Text.XML.HXT.Arrow.XmlArrow )
where

import           Control.Arrow                  -- classes
import           Control.Arrow.ArrowList
import           Control.Arrow.ArrowIf
import           Control.Arrow.ArrowTree

import           Control.Arrow.ListArrow                -- arrow types
import           Control.Arrow.StateListArrow
import           Control.Arrow.IOListArrow
import           Control.Arrow.IOStateListArrow

import           Data.Maybe

import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.DOM.Unicode       ( isXmlSpaceChar
                                                )
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.ShowXml as XS

-- ------------------------------------------------------------

{- | Arrows for processing 'Text.XML.HXT.DOM.TypeDefs.XmlTree's

These arrows can be grouped into predicates, selectors, constructors, and transformers.

All predicates (tests) act like 'Control.Arrow.ArrowIf.none' for failure and 'Control.Arrow.ArrowIf.this' for success.
A logical and can be formed by @ a1 >>> a2 @, a locical or by @ a1 \<+\> a2 @.

Selector arrows will fail, when applied to wrong input, e.g. selecting the text of a node with 'getText'
will fail when applied to a none text node.

Edit arrows will remain the input unchanged, when applied to wrong argument, e.g. editing the content of a text node
with 'changeText' applied to an element node will return the unchanged element node.
-}

infixl 7 +=

class (Arrow a, ArrowList a, ArrowTree a) => ArrowXml a where

    -- discriminating predicates

    -- | test for text nodes
    isText              :: a XmlTree XmlTree
    isText              = isA XN.isText

    -- | test for char reference, used during parsing
    isCharRef           :: a XmlTree XmlTree
    isCharRef           = isA XN.isCharRef

    -- | test for entity reference, used during parsing
    isEntityRef         :: a XmlTree XmlTree
    isEntityRef         = isA XN.isEntityRef

    -- | test for comment
    isCmt               :: a XmlTree XmlTree
    isCmt               = isA XN.isCmt

    -- | test for CDATA section, used during parsing
    isCdata             :: a XmlTree XmlTree
    isCdata             = isA XN.isCdata

    -- | test for processing instruction
    isPi                :: a XmlTree XmlTree
    isPi                = isA XN.isPi

    -- | test for processing instruction \<?xml ...\>
    isXmlPi             :: a XmlTree XmlTree
    isXmlPi             = isPi >>> hasName "xml"

    -- | test for element
    isElem              :: a XmlTree XmlTree
    isElem              = isA XN.isElem

    -- | test for DTD part, used during parsing
    isDTD               :: a XmlTree XmlTree
    isDTD               = isA XN.isDTD

    -- | test for attribute tree
    isAttr              :: a XmlTree XmlTree
    isAttr              = isA XN.isAttr

    -- | test for error message
    isError             :: a XmlTree XmlTree
    isError             = isA XN.isError

    -- | test for root node (element with name \"\/\")
    isRoot              :: a XmlTree XmlTree
    isRoot              = isA XN.isRoot

    -- | test for text nodes with text, for which a predicate holds
    --
    -- example: @hasText (all (\`elem\` \" \\t\\n\"))@ check for text nodes with only whitespace content

    hasText             :: (String -> Bool) -> a XmlTree XmlTree
    hasText p           = (isText >>> getText >>> isA p) `guards` this

    -- | test for text nodes with only white space
    --
    -- implemented with 'hasTest'

    isWhiteSpace        :: a XmlTree XmlTree
    isWhiteSpace        = hasText (all isXmlSpaceChar)

    -- |
    -- test whether a node (element, attribute, pi) has a name with a special property

    hasNameWith         :: (QName  -> Bool) -> a XmlTree XmlTree
    hasNameWith p       = (getQName        >>> isA p) `guards` this

    -- |
    -- test whether a node (element, attribute, pi) has a specific qualified name
    -- useful only after namespace propagation
    hasQName            :: QName  -> a XmlTree XmlTree
    hasQName n          = (getQName        >>> isA (== n)) `guards` this

    -- |
    -- test whether a node has a specific name (prefix:localPart ore localPart),
    -- generally useful, even without namespace handling
    hasName             :: String -> a XmlTree XmlTree
    hasName n           = (getName         >>> isA (== n)) `guards` this

    -- |
    -- test whether a node has a specific name as local part,
    -- useful only after namespace propagation
    hasLocalPart        :: String -> a XmlTree XmlTree
    hasLocalPart n      = (getLocalPart    >>> isA (== n)) `guards` this

    -- |
    -- test whether a node has a specific name prefix,
    -- useful only after namespace propagation
    hasNamePrefix       :: String -> a XmlTree XmlTree
    hasNamePrefix n     = (getNamePrefix   >>> isA (== n)) `guards` this

    -- |
    -- test whether a node has a specific namespace URI
    -- useful only after namespace propagation
    hasNamespaceUri     :: String -> a XmlTree XmlTree
    hasNamespaceUri n   = (getNamespaceUri >>> isA (== n)) `guards` this

    -- |
    -- test whether an element node has an attribute node with a specific name
    hasAttr             :: String -> a XmlTree XmlTree
    hasAttr n           = (getAttrl        >>> hasName n)  `guards` this

    -- |
    -- test whether an element node has an attribute node with a specific qualified name
    hasQAttr            :: QName -> a XmlTree XmlTree
    hasQAttr n          = (getAttrl        >>> hasQName n)  `guards` this

    -- |
    -- test whether an element node has an attribute with a specific value
    hasAttrValue        :: String -> (String -> Bool) -> a XmlTree XmlTree
    hasAttrValue n p    = (getAttrl >>> hasName n >>> xshow getChildren >>> isA p)  `guards` this

    -- |
    -- test whether an element node has an attribute with a qualified name and a specific value
    hasQAttrValue       :: QName -> (String -> Bool) -> a XmlTree XmlTree
    hasQAttrValue n p   = (getAttrl >>> hasQName n >>> xshow getChildren >>> isA p)  `guards` this

    -- constructor arrows ------------------------------------------------------------

    -- | text node construction arrow
    mkText              :: a String XmlTree
    mkText              = arr  XN.mkText

    -- | char reference construction arrow, useful for document output
    mkCharRef           :: a Int    XmlTree
    mkCharRef           = arr  XN.mkCharRef

    -- | entity reference construction arrow, useful for document output
    mkEntityRef         :: a String XmlTree
    mkEntityRef         = arr  XN.mkEntityRef

    -- | comment node construction, useful for document output
    mkCmt               :: a String XmlTree
    mkCmt               = arr  XN.mkCmt

    -- | CDATA construction, useful for document output
    mkCdata             :: a String XmlTree
    mkCdata             = arr  XN.mkCdata

    -- | error node construction, useful only internally
    mkError             :: Int -> a String XmlTree
    mkError level       = arr (XN.mkError level)

    -- | element construction:
    -- | the attributes and the content of the element are computed by applying arrows
    -- to the input
    mkElement           :: QName -> a n XmlTree -> a n XmlTree -> a n XmlTree
    mkElement n af cf   = (listA af &&& listA cf)
                          >>>
                          arr2 (\ al cl -> XN.mkElement n al cl)
    -- | attribute node construction:
    -- | the attribute value is computed by applying an arrow to the input
    mkAttr              :: QName -> a n XmlTree -> a n XmlTree
    mkAttr qn f         = listA f >>> arr (XN.mkAttr qn)

    -- | processing instruction construction:
    -- | the content of the processing instruction is computed by applying an arrow to the input
    mkPi                :: QName -> a n XmlTree -> a n XmlTree
    mkPi qn f           = listA f >>> arr (XN.mkPi   qn)

    -- convenient arrows for constructors --------------------------------------------------

    -- | convenient arrow for element construction, more comfortable variant of 'mkElement'
    --
    -- example for simplifying 'mkElement' :
    --
    -- > mkElement qn (a1 <+> ... <+> ai) (c1 <+> ... <+> cj)
    --
    -- equals
    --
    -- > mkqelem qn [a1,...,ai] [c1,...,cj]

    mkqelem             :: QName -> [a n XmlTree] -> [a n XmlTree] -> a n XmlTree
    mkqelem  n afs cfs  = mkElement n (catA afs) (catA cfs)

    -- | convenient arrow for element construction with strings instead of qualified names as element names, see also 'mkElement' and 'mkelem'
    mkelem              :: String -> [a n XmlTree] -> [a n XmlTree] -> a n XmlTree
    mkelem  n afs cfs   = mkElement (mkName n) (catA afs) (catA cfs)

    -- | convenient arrow for element constrution with attributes but without content, simple variant of 'mkelem' and 'mkElement'
    aelem               :: String -> [a n XmlTree]                  -> a n XmlTree
    aelem n afs         = catA afs >. \ al -> XN.mkElement (mkName n) al []

    -- | convenient arrow for simple element constrution without attributes, simple variant of 'mkelem' and 'mkElement'
    selem               :: String                  -> [a n XmlTree] -> a n XmlTree
    selem n cfs         = catA cfs >.         XN.mkElement (mkName n) []

    -- | convenient arrow for constrution of empty elements without attributes, simple variant of 'mkelem' and 'mkElement'
    eelem               :: String                                   -> a n XmlTree
    eelem n             = constA      (XN.mkElement (mkName n) [] [])

    -- | construction of an element node with name \"\/\" for document roots
    root                ::           [a n XmlTree] -> [a n XmlTree] -> a n XmlTree
    root                = mkelem t_root

    -- | alias for 'mkAttr'
    qattr               :: QName -> a n XmlTree -> a n XmlTree
    qattr               = mkAttr

    -- | convenient arrow for attribute constrution, simple variant of 'mkAttr'
    attr                :: String -> a n XmlTree -> a n XmlTree
    attr                = mkAttr . mkName

    -- constant arrows (ignoring the input) for tree construction ------------------------------

    -- | constant arrow for text nodes
    txt                 :: String -> a n XmlTree
    txt                 = constA .  XN.mkText

    -- | constant arrow for char reference nodes
    charRef             :: Int    -> a n XmlTree
    charRef             = constA .  XN.mkCharRef

    -- | constant arrow for entity reference nodes
    entityRef           :: String -> a n XmlTree
    entityRef           = constA .  XN.mkEntityRef

    -- | constant arrow for comment
    cmt                 :: String -> a n XmlTree
    cmt                 = constA .  XN.mkCmt

    -- | constant arrow for warning
    warn                :: String -> a n XmlTree
    warn                = constA . (XN.mkError c_warn)

    -- | constant arrow for errors
    err                 :: String -> a n XmlTree
    err                 = constA . (XN.mkError c_err)

    -- | constant arrow for fatal errors
    fatal               :: String -> a n XmlTree
    fatal               = constA . (XN.mkError c_fatal)

    -- | constant arrow for simple processing instructions, see 'mkPi'
    spi                 :: String -> String -> a n XmlTree
    spi piName piCont   = constA (XN.mkPi   (mkName piName) [XN.mkText piCont])

    -- | constant arrow for attribute nodes, attribute name is a qualified name and value is a text,
    -- | see also 'mkAttr', 'qattr', 'attr'
    sqattr              :: QName -> String -> a n XmlTree
    sqattr an av        = constA (XN.mkAttr an                 [XN.mkText av])

    -- | constant arrow for attribute nodes, attribute name and value are
    -- | given by parameters, see 'mkAttr'
    sattr               :: String -> String -> a n XmlTree
    sattr an av         = constA (XN.mkAttr (mkName an)     [XN.mkText av])

    -- selector arrows --------------------------------------------------

    -- | select the text of a text node
    getText             :: a XmlTree String
    getText             = arrL (maybeToList  . XN.getText)

    -- | select the value of a char reference
    getCharRef          :: a XmlTree Int
    getCharRef          = arrL (maybeToList  . XN.getCharRef)

    -- | select the name of a entity reference node
    getEntityRef        :: a XmlTree String
    getEntityRef        = arrL (maybeToList  . XN.getEntityRef)

    -- | select the comment of a comment node
    getCmt              :: a XmlTree String
    getCmt              = arrL (maybeToList  . XN.getCmt)

    -- | select the content of a CDATA node
    getCdata            :: a XmlTree String
    getCdata            = arrL (maybeToList  . XN.getCdata)

    -- | select the name of a processing instruction
    getPiName           :: a XmlTree QName
    getPiName           = arrL (maybeToList  . XN.getPiName)

    -- | select the content of a processing instruction
    getPiContent        :: a XmlTree XmlTree
    getPiContent        = arrL (fromMaybe [] . XN.getPiContent)

    -- | select the name of an element node
    getElemName         :: a XmlTree QName
    getElemName         = arrL (maybeToList  . XN.getElemName)

    -- | select the attribute list of an element node
    getAttrl            :: a XmlTree XmlTree
    getAttrl            = arrL (fromMaybe [] . XN.getAttrl)

    -- | select the DTD type of a DTD node
    getDTDPart          :: a XmlTree DTDElem
    getDTDPart          = arrL (maybeToList  . XN.getDTDPart)

    -- | select the DTD attributes of a DTD node
    getDTDAttrl         :: a XmlTree Attributes
    getDTDAttrl         = arrL (maybeToList  . XN.getDTDAttrl)

    -- | select the name of an attribute
    getAttrName         :: a XmlTree QName
    getAttrName         = arrL (maybeToList  . XN.getAttrName)

    -- | select the error level (c_warn, c_err, c_fatal) from an error node
    getErrorLevel       :: a XmlTree Int
    getErrorLevel       = arrL (maybeToList  . XN.getErrorLevel)

    -- | select the error message from an error node
    getErrorMsg         :: a XmlTree String
    getErrorMsg         = arrL (maybeToList  . XN.getErrorMsg)

    -- | select the qualified name from an element, attribute or pi
    getQName            :: a XmlTree QName
    getQName            = arrL (maybeToList  . XN.getName)

    -- | select the prefix:localPart or localPart from an element, attribute or pi
    getName             :: a XmlTree String
    getName             = arrL (maybeToList  . XN.getQualifiedName)

    -- | select the univeral name ({namespace URI} ++ localPart)
    getUniversalName    :: a XmlTree String
    getUniversalName    = arrL (maybeToList  . XN.getUniversalName)

    -- | select the univeral name (namespace URI ++ localPart)
    getUniversalUri     :: a XmlTree String
    getUniversalUri     = arrL (maybeToList  . XN.getUniversalUri)

    -- | select the local part
    getLocalPart        :: a XmlTree String
    getLocalPart        = arrL (maybeToList  . XN.getLocalPart)

    -- | select the name prefix
    getNamePrefix       :: a XmlTree String
    getNamePrefix       = arrL (maybeToList  . XN.getNamePrefix)

    -- | select the namespace URI
    getNamespaceUri     :: a XmlTree String
    getNamespaceUri     = arrL (maybeToList  . XN.getNamespaceUri)

    -- | select the value of an attribute of an element node,
    -- always succeeds with empty string as default value \"\"
    getAttrValue        :: String -> a XmlTree String
    getAttrValue n      = xshow (getAttrl >>> hasName n >>> getChildren)

    -- | like 'getAttrValue', but fails if the attribute does not exist
    getAttrValue0       :: String -> a XmlTree String
    getAttrValue0 n     = getAttrl >>> hasName n >>> xshow getChildren

    -- | like 'getAttrValue', but select the value of an attribute given by a qualified name,
    -- always succeeds with empty string as default value \"\"
    getQAttrValue       :: QName -> a XmlTree String
    getQAttrValue n     = xshow (getAttrl >>> hasQName n >>> getChildren)

    -- | like 'getQAttrValue', but fails if attribute does not exist
    getQAttrValue0      :: QName -> a XmlTree String
    getQAttrValue0 n    = getAttrl >>> hasQName n >>> xshow getChildren

    -- edit arrows --------------------------------------------------

    -- | edit the string of a text node
    changeText          :: (String -> String) -> a XmlTree XmlTree
    changeText cf       = arr (XN.changeText     cf) `when` isText

    -- | edit the comment string of a comment node
    changeCmt           :: (String -> String) -> a XmlTree XmlTree
    changeCmt  cf       = arr (XN.changeCmt      cf) `when` isCmt

    -- | edit an element-, attribute- or pi- name
    changeQName         :: (QName  -> QName) -> a XmlTree XmlTree
    changeQName cf      = arr (XN.changeName  cf) `when` getQName

    -- | edit an element name
    changeElemName      :: (QName  -> QName) -> a XmlTree XmlTree
    changeElemName cf   = arr (XN.changeElemName  cf) `when` isElem

    -- | edit an attribute name
    changeAttrName      :: (QName  -> QName) -> a XmlTree XmlTree
    changeAttrName cf   = arr (XN.changeAttrName cf) `when` isAttr

    -- | edit a pi name
    changePiName        :: (QName  -> QName) -> a XmlTree XmlTree
    changePiName cf     = arr (XN.changePiName  cf) `when` isPi

    -- | edit an attribute value
    changeAttrValue     :: (String -> String) -> a XmlTree XmlTree
    changeAttrValue cf  = replaceChildren ( xshow getChildren
                                            >>> arr cf
                                            >>> mkText
                                          )
                          `when` isAttr


    -- | edit an attribute list of an element node
    changeAttrl         :: (XmlTrees -> XmlTrees -> XmlTrees) -> a XmlTree XmlTree -> a XmlTree XmlTree
    changeAttrl cf f    = ( ( listA f &&& this )
                            >>>
                            arr2 changeAL
                          )
                          `when`
                          ( isElem <+> isPi )
                        where
                        changeAL as x = XN.changeAttrl (\ xs -> cf xs as) x

    -- | replace an element, attribute or pi name
    setQName            :: QName -> a XmlTree XmlTree
    setQName  n         = changeQName  (const n)

    -- | replace an element name
    setElemName         :: QName -> a XmlTree XmlTree
    setElemName  n      = changeElemName  (const n)

    -- | replace an attribute name
    setAttrName         :: QName -> a XmlTree XmlTree
    setAttrName n       = changeAttrName (const n)

    -- | replace an element name
    setPiName           :: QName -> a XmlTree XmlTree
    setPiName  n        = changePiName  (const n)

    -- | replace an atribute list of an element node
    setAttrl            :: a XmlTree XmlTree -> a XmlTree XmlTree
    setAttrl            = changeAttrl (const id)                -- (\ x y -> y)

    -- | add a list of attributes to an element
    addAttrl            :: a XmlTree XmlTree -> a XmlTree XmlTree
    addAttrl            = changeAttrl (XN.mergeAttrl)

    -- | add (or replace) an attribute
    addAttr             :: String -> String  -> a XmlTree XmlTree
    addAttr an av       = addAttrl (sattr an av)

    -- | remove an attribute
    removeAttr          :: String  -> a XmlTree XmlTree
    removeAttr an       = processAttrl (none `when` hasName an)

    -- | remove an attribute with a qualified name
    removeQAttr         :: QName  -> a XmlTree XmlTree
    removeQAttr an      = processAttrl (none `when` hasQName an)

    -- | process the attributes of an element node with an arrow
    processAttrl        :: a XmlTree XmlTree -> a XmlTree XmlTree
    processAttrl f      = setAttrl (getAttrl >>> f)

    -- | process a whole tree inclusive attribute list of element nodes
    -- see also: 'Control.Arrow.ArrowTree.processTopDown'

    processTopDownWithAttrl     :: a XmlTree XmlTree -> a XmlTree XmlTree
    processTopDownWithAttrl f   = processTopDown ( f >>> ( processAttrl (processTopDown f) `when` isElem))

    -- | convenient op for adding attributes or children to a node
    --
    -- usage: @ tf += cf @
    --
    -- the @tf@ arrow computes an element node, and all trees computed by @cf@ are
    -- added to this node, if a tree is an attribute, it is inserted in the attribute list
    -- else it is appended to the content list.
    --
    -- attention: do not build long content list this way because '+=' is implemented by ++
    --
    -- examples:
    --
    -- > eelem "a"
    -- >   += sattr "href" "page.html"
    -- >   += sattr "name" "here"
    -- >   += txt "look here"
    --
    -- is the same as
    --
    -- > mkelem [ sattr "href" "page.html"
    -- >        , sattr "name" "here"
    -- >        ]
    -- >        [ txt "look here" ]
    --
    -- and results in the XML fragment: \<a href=\"page.html\" name=\"here\"\>look here\<\/a\>
    --
    -- advantage of the '+=' operator is, that attributes and content can be added
    -- any time step by step.
    -- if @tf@ computes a whole list of trees, e.g. a list of \"td\" or \"tr\" elements,
    -- the attributes or content is added to all trees. useful for adding \"class\" or \"style\" attributes
    -- to table elements.

    (+=)                :: a b XmlTree -> a b XmlTree -> a b XmlTree
    tf += cf            = (tf &&& listA cf) >>> arr2 addChildren
                        where
                        addChildren     :: XmlTree -> XmlTrees -> XmlTree
                        addChildren t cs
                            = foldl addChild t cs
                        addChild        :: XmlTree -> XmlTree -> XmlTree
                        addChild t c
                            | not (XN.isElem t)
                                = t
                            | XN.isAttr c
                                = XN.changeAttrl (XN.addAttr c) t
                            | otherwise
                                = XN.changeChildren (++ [c]) t

    -- | apply an arrow to the input and convert the resulting XML trees into a string representation
    xshow               :: a n XmlTree -> a n String
    xshow f             = f >. XS.xshow

{- | Document Type Definition arrows

These are separated, because they are not needed for document processing,
only when processing the DTD, e.g. for generating access funtions for the toolbox
from a DTD (se example DTDtoHaskell in the examples directory)
-}


class (ArrowXml a) => ArrowDTD a where
    isDTDDoctype        :: a XmlTree XmlTree
    isDTDDoctype        = isA (maybe False (== DOCTYPE ) . XN.getDTDPart)

    isDTDElement        :: a XmlTree XmlTree
    isDTDElement        = isA (maybe False (== ELEMENT ) . XN.getDTDPart)

    isDTDContent        :: a XmlTree XmlTree
    isDTDContent        = isA (maybe False (== CONTENT ) . XN.getDTDPart)

    isDTDAttlist        :: a XmlTree XmlTree
    isDTDAttlist        = isA (maybe False (== ATTLIST ) . XN.getDTDPart)

    isDTDEntity         :: a XmlTree XmlTree
    isDTDEntity         = isA (maybe False (== ENTITY  ) . XN.getDTDPart)

    isDTDPEntity        :: a XmlTree XmlTree
    isDTDPEntity        = isA (maybe False (== PENTITY ) . XN.getDTDPart)

    isDTDNotation       :: a XmlTree XmlTree
    isDTDNotation       = isA (maybe False (== NOTATION) . XN.getDTDPart)

    isDTDCondSect       :: a XmlTree XmlTree
    isDTDCondSect       = isA (maybe False (== CONDSECT) . XN.getDTDPart)

    isDTDName           :: a XmlTree XmlTree
    isDTDName           = isA (maybe False (== NAME    ) . XN.getDTDPart)

    isDTDPERef          :: a XmlTree XmlTree
    isDTDPERef          = isA (maybe False (== PEREF   ) . XN.getDTDPart)

    hasDTDAttr          :: String -> a XmlTree XmlTree
    hasDTDAttr n        = isA (isJust . lookup n . fromMaybe [] . XN.getDTDAttrl)

    getDTDAttrValue     :: String -> a XmlTree String
    getDTDAttrValue n   = arrL (maybeToList . lookup n . fromMaybe [] . XN.getDTDAttrl)

    setDTDAttrValue     :: String -> String -> a XmlTree XmlTree
    setDTDAttrValue n v = arr (XN.changeDTDAttrl (addEntry n v)) `when` isDTD

    mkDTDElem           :: DTDElem -> Attributes -> a n XmlTree -> a n XmlTree
    mkDTDElem e al cf   = listA cf >>> arr (XN.mkDTDElem e al)

    mkDTDDoctype        :: Attributes -> a n XmlTree -> a n XmlTree
    mkDTDDoctype        = mkDTDElem DOCTYPE

    mkDTDElement        :: Attributes -> a n XmlTree
    mkDTDElement al     = mkDTDElem ELEMENT al none

    mkDTDEntity         :: Attributes -> a n XmlTree
    mkDTDEntity al      = mkDTDElem ENTITY al none

    mkDTDPEntity        :: Attributes -> a n XmlTree
    mkDTDPEntity al     = mkDTDElem PENTITY al none

instance ArrowXml LA
instance ArrowXml (SLA s)
instance ArrowXml IOLA
instance ArrowXml (IOSLA s)

instance ArrowDTD LA
instance ArrowDTD (SLA s)
instance ArrowDTD IOLA
instance ArrowDTD (IOSLA s)

-- ------------------------------------------------------------
