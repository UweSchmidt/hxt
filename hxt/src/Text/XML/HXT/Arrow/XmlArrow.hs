-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlArrow
   Copyright  : Copyright (C) 2011 Uwe Schmidt
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

import           Data.Char.Properties.XMLCharProps        ( isXmlSpaceChar )
import           Data.Maybe

import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.DOM.XmlNode                 ( XmlNode )
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
    isText              :: XmlNode xn => a xn xn
    isText              = isA XN.isText
    {-# INLINE isText #-}

    isBlob              :: XmlNode xn => a xn xn
    isBlob              = isA XN.isBlob
    {-# INLINE isBlob #-}

    -- | test for char reference, used during parsing
    isCharRef           :: XmlNode xn => a xn xn
    isCharRef           = isA XN.isCharRef
    {-# INLINE isCharRef #-}

    -- | test for entity reference, used during parsing
    isEntityRef         :: XmlNode xn => a xn xn
    isEntityRef         = isA XN.isEntityRef
    {-# INLINE isEntityRef #-}

    -- | test for comment
    isCmt               :: XmlNode xn => a xn xn
    isCmt               = isA XN.isCmt
    {-# INLINE isCmt #-}

    -- | test for CDATA section, used during parsing
    isCdata             :: XmlNode xn => a xn xn
    isCdata             = isA XN.isCdata
    {-# INLINE isCdata #-}

    -- | test for processing instruction
    isPi                :: XmlNode xn => a xn xn
    isPi                = isA XN.isPi
    {-# INLINE isPi #-}

    -- | test for processing instruction \<?xml ...\>
    isXmlPi             :: XmlNode xn => a xn xn
    isXmlPi             = isPi >>> hasName "xml"

    -- | test for element
    isElem              :: XmlNode xn => a xn xn
    isElem              = isA XN.isElem
    {-# INLINE isElem #-}

    -- | test for DTD part, used during parsing
    isDTD               :: XmlNode xn => a xn xn
    isDTD               = isA XN.isDTD
    {-# INLINE isDTD #-}

    -- | test for attribute tree
    isAttr              :: XmlNode xn => a xn xn
    isAttr              = isA XN.isAttr
    {-# INLINE isAttr #-}

    -- | test for error message
    isError             :: XmlNode xn => a xn xn
    isError             = isA XN.isError
    {-# INLINE isError #-}

    -- | test for root node (element with name \"\/\")
    isRoot              :: XmlNode xn => a xn xn
    isRoot              = isA XN.isRoot
    {-# INLINE isRoot #-}

    -- | test for text nodes with text, for which a predicate holds
    --
    -- example: @hasText (all (\`elem\` \" \\t\\n\"))@ check for text nodes with only whitespace content

    hasText             :: XmlNode xn => (String -> Bool) -> a xn xn
    hasText p           = (isText >>> getText >>> isA p) `guards` this

    -- | test for text nodes with only white space
    --
    -- implemented with 'hasTest'

    isWhiteSpace        :: XmlNode xn => a xn xn
    isWhiteSpace        = hasText (all isXmlSpaceChar)
    {-# INLINE isWhiteSpace #-}

    -- |
    -- test whether a node (element, attribute, pi) has a name with a special property

    hasNameWith         :: XmlNode xn => (QName  -> Bool) -> a xn xn
    hasNameWith p       = (getQName        >>> isA p) `guards` this
    {-# INLINE hasNameWith #-}

    -- |
    -- test whether a node (element, attribute, pi) has a specific qualified name
    -- useful only after namespace propagation
    hasQName            :: XmlNode xn => QName  -> a xn xn
    hasQName n          = (getQName        >>> isA (== n)) `guards` this
    {-# INLINE hasQName #-}

    -- |
    -- test whether a node has a specific name (prefix:localPart ore localPart),
    -- generally useful, even without namespace handling
    hasName             :: XmlNode xn => String -> a xn xn
    hasName n           = (getName         >>> isA (== n)) `guards` this
    {-# INLINE hasName #-}

    -- |
    -- test whether a node has a specific name as local part,
    -- useful only after namespace propagation
    hasLocalPart        :: XmlNode xn => String -> a xn xn
    hasLocalPart n      = (getLocalPart    >>> isA (== n)) `guards` this
    {-# INLINE hasLocalPart #-}

    -- |
    -- test whether a node has a specific name prefix,
    -- useful only after namespace propagation
    hasNamePrefix       :: XmlNode xn => String -> a xn xn
    hasNamePrefix n     = (getNamePrefix   >>> isA (== n)) `guards` this
    {-# INLINE hasNamePrefix #-}

    -- |
    -- test whether a node has a specific namespace URI
    -- useful only after namespace propagation
    hasNamespaceUri     :: XmlNode xn => String -> a xn xn
    hasNamespaceUri n   = (getNamespaceUri >>> isA (== n)) `guards` this
    {-# INLINE hasNamespaceUri #-}

    -- |
    -- test whether an element node has an attribute node with a specific name
    hasAttr             :: XmlNode xn => String -> a xn xn
    hasAttr n           = (getAttrl        >>> hasName n)  `guards` this
    {-# INLINE hasAttr #-}

    -- |
    -- test whether an element node has an attribute node with a specific qualified name
    hasQAttr            :: XmlNode xn => QName -> a xn xn
    hasQAttr n          = (getAttrl        >>> hasQName n)  `guards` this
    {-# INLINE hasQAttr #-}

    -- |
    -- test whether an element node has an attribute with a specific value
    hasAttrValue        :: XmlNode xn => String -> (String -> Bool) -> a xn xn
    hasAttrValue n p    = (getAttrl >>> hasName n >>> xshow getChildren >>> isA p)  `guards` this

    -- |
    -- test whether an element node has an attribute with a qualified name and a specific value
    hasQAttrValue       :: XmlNode xn => QName -> (String -> Bool) -> a xn xn
    hasQAttrValue n p   = (getAttrl >>> hasQName n >>> xshow getChildren >>> isA p)  `guards` this

    -- constructor arrows ------------------------------------------------------------

    -- | text node construction arrow
    mkText              :: XmlNode xn => a String xn
    mkText              = arr  XN.mkText
    {-# INLINE mkText #-}

    -- | blob node construction arrow
    mkBlob              :: XmlNode xn => a Blob xn
    mkBlob              = arr  XN.mkBlob
    {-# INLINE mkBlob #-}

    -- | char reference construction arrow, useful for document output
    mkCharRef           :: XmlNode xn => a Int xn
    mkCharRef           = arr  XN.mkCharRef
    {-# INLINE mkCharRef #-}

    -- | entity reference construction arrow, useful for document output
    mkEntityRef         :: XmlNode xn => a String xn
    mkEntityRef         = arr  XN.mkEntityRef
    {-# INLINE mkEntityRef #-}

    -- | comment node construction, useful for document output
    mkCmt               :: XmlNode xn => a String xn
    mkCmt               = arr  XN.mkCmt
    {-# INLINE mkCmt #-}

    -- | CDATA construction, useful for document output
    mkCdata             :: XmlNode xn => a String xn
    mkCdata             = arr  XN.mkCdata
    {-# INLINE mkCdata #-}

    -- | error node construction, useful only internally
    mkError             :: XmlNode xn => Int -> a String xn
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
    {-# INLINE mkqelem #-}

    -- | convenient arrow for element construction with strings instead of qualified names as element names, see also 'mkElement' and 'mkelem'
    mkelem              :: String -> [a n XmlTree] -> [a n XmlTree] -> a n XmlTree
    mkelem  n afs cfs   = mkElement (mkName n) (catA afs) (catA cfs)
    {-# INLINE mkelem #-}

    -- | convenient arrow for element constrution with attributes but without content, simple variant of 'mkelem' and 'mkElement'
    aelem               :: String -> [a n XmlTree]                  -> a n XmlTree
    aelem n afs         = catA afs >. \ al -> XN.mkElement (mkName n) al []
    {-# INLINE aelem #-}

    -- | convenient arrow for simple element constrution without attributes, simple variant of 'mkelem' and 'mkElement'
    selem               :: String                  -> [a n XmlTree] -> a n XmlTree
    selem n cfs         = catA cfs >.         XN.mkElement (mkName n) []
    {-# INLINE selem #-}

    -- | convenient arrow for constrution of empty elements without attributes, simple variant of 'mkelem' and 'mkElement'
    eelem               :: String                                   -> a n XmlTree
    eelem n             = constA      (XN.mkElement (mkName n) [] [])
    {-# INLINE eelem #-}

    -- | construction of an element node with name \"\/\" for document roots
    root                ::           [a n XmlTree] -> [a n XmlTree] -> a n XmlTree
    root                = mkelem t_root
    {-# INLINE root #-}

    -- | alias for 'mkAttr'
    qattr               :: QName -> a n XmlTree -> a n XmlTree
    qattr               = mkAttr
    {-# INLINE qattr #-}

    -- | convenient arrow for attribute constrution, simple variant of 'mkAttr'
    attr                :: String -> a n XmlTree -> a n XmlTree
    attr                = mkAttr . mkName
    {-# INLINE attr #-}

    -- constant arrows (ignoring the input) for tree construction ------------------------------

    -- | constant arrow for text nodes
    txt                 :: String -> a n XmlTree
    txt                 = constA .  XN.mkText
    {-# INLINE txt #-}

    -- | constant arrow for blob nodes
    blb                 :: Blob -> a n XmlTree
    blb                 = constA .  XN.mkBlob
    {-# INLINE blb #-}

    -- | constant arrow for char reference nodes
    charRef             :: Int    -> a n XmlTree
    charRef             = constA .  XN.mkCharRef
    {-# INLINE charRef #-}

    -- | constant arrow for entity reference nodes
    entityRef           :: String -> a n XmlTree
    entityRef           = constA .  XN.mkEntityRef
    {-# INLINE entityRef #-}

    -- | constant arrow for comment
    cmt                 :: String -> a n XmlTree
    cmt                 = constA .  XN.mkCmt
    {-# INLINE cmt #-}

    -- | constant arrow for warning
    warn                :: String -> a n XmlTree
    warn                = constA . (XN.mkError c_warn)
    {-# INLINE warn #-}

    -- | constant arrow for errors
    err                 :: String -> a n XmlTree
    err                 = constA . (XN.mkError c_err)
    {-# INLINE err #-}

    -- | constant arrow for fatal errors
    fatal               :: String -> a n XmlTree
    fatal               = constA . (XN.mkError c_fatal)
    {-# INLINE fatal #-}

    -- | constant arrow for simple processing instructions, see 'mkPi'
    spi                 :: String -> String -> a n XmlTree
    spi piName piCont   = constA (XN.mkPi (mkName piName) [XN.mkAttr (mkName a_value) [XN.mkText piCont]])
    {-# INLINE spi #-}

    -- | constant arrow for attribute nodes, attribute name is a qualified name and value is a text,
    -- | see also 'mkAttr', 'qattr', 'attr'
    sqattr              :: QName -> String -> a n XmlTree
    sqattr an av        = constA (XN.mkAttr an                 [XN.mkText av])
    {-# INLINE sqattr #-}

    -- | constant arrow for attribute nodes, attribute name and value are
    -- | given by parameters, see 'mkAttr'
    sattr               :: String -> String -> a n XmlTree
    sattr an av         = constA (XN.mkAttr (mkName an)     [XN.mkText av])
    {-# INLINE sattr #-}

    -- selector arrows --------------------------------------------------

    -- | select the text of a text node
    getText             :: XmlNode xn => a xn String
    getText             = arrL (maybeToList  . XN.getText)
    {-# INLINE getText #-}

    -- | select the value of a char reference
    getCharRef          :: XmlNode xn => a xn Int
    getCharRef          = arrL (maybeToList  . XN.getCharRef)
    {-# INLINE getCharRef #-}

    -- | select the name of a entity reference node
    getEntityRef        :: XmlNode xn => a xn String
    getEntityRef        = arrL (maybeToList  . XN.getEntityRef)
    {-# INLINE getEntityRef #-}

    -- | select the comment of a comment node
    getCmt              :: XmlNode xn => a xn String
    getCmt              = arrL (maybeToList  . XN.getCmt)
    {-# INLINE getCmt #-}

    -- | select the content of a CDATA node
    getCdata            :: XmlNode xn => a xn String
    getCdata            = arrL (maybeToList  . XN.getCdata)
    {-# INLINE getCdata #-}

    -- | select the name of a processing instruction
    getPiName           :: XmlNode xn => a xn QName
    getPiName           = arrL (maybeToList  . XN.getPiName)
    {-# INLINE getPiName #-}

    -- | select the content of a processing instruction
    getPiContent        :: XmlNode xn => a xn XmlTree
    getPiContent        = arrL (fromMaybe [] . XN.getPiContent)
    {-# INLINE getPiContent #-}

    -- | select the name of an element node
    getElemName         :: XmlNode xn => a xn QName
    getElemName         = arrL (maybeToList  . XN.getElemName)
    {-# INLINE getElemName #-}

    -- | select the attribute list of an element node
    getAttrl            :: XmlNode xn => a xn XmlTree
    getAttrl            = arrL (fromMaybe [] . XN.getAttrl)
    {-# INLINE getAttrl #-}

    -- | select the DTD type of a DTD node
    getDTDPart          :: XmlNode xn => a xn DTDElem
    getDTDPart          = arrL (maybeToList  . XN.getDTDPart)
    {-# INLINE getDTDPart #-}

    -- | select the DTD attributes of a DTD node
    getDTDAttrl         :: XmlNode xn => a xn Attributes
    getDTDAttrl         = arrL (maybeToList  . XN.getDTDAttrl)
    {-# INLINE getDTDAttrl #-}

    -- | select the name of an attribute
    getAttrName         :: XmlNode xn => a xn QName
    getAttrName         = arrL (maybeToList  . XN.getAttrName)
    {-# INLINE getAttrName #-}

    -- | select the error level (c_warn, c_err, c_fatal) from an error node
    getErrorLevel       :: XmlNode xn => a xn Int
    getErrorLevel       = arrL (maybeToList  . XN.getErrorLevel)
    {-# INLINE getErrorLevel #-}

    -- | select the error message from an error node
    getErrorMsg         :: XmlNode xn => a xn String
    getErrorMsg         = arrL (maybeToList  . XN.getErrorMsg)
    {-# INLINE getErrorMsg #-}

    -- | select the qualified name from an element, attribute or pi
    getQName            :: XmlNode xn => a xn QName
    getQName            = arrL (maybeToList  . XN.getName)
    {-# INLINE getQName #-}

    -- | select the prefix:localPart or localPart from an element, attribute or pi
    getName             :: XmlNode xn => a xn String
    getName             = arrL (maybeToList  . XN.getQualifiedName)
    {-# INLINE getName #-}

    -- | select the univeral name ({namespace URI} ++ localPart)
    getUniversalName    :: XmlNode xn => a xn String
    getUniversalName    = arrL (maybeToList  . XN.getUniversalName)
    {-# INLINE getUniversalName #-}

    -- | select the univeral name (namespace URI ++ localPart)
    getUniversalUri     :: XmlNode xn => a xn String
    getUniversalUri     = arrL (maybeToList  . XN.getUniversalUri)
    {-# INLINE getUniversalUri #-}

    -- | select the local part
    getLocalPart        :: XmlNode xn => a xn String
    getLocalPart        = arrL (maybeToList  . XN.getLocalPart)
    {-# INLINE getLocalPart #-}

    -- | select the name prefix
    getNamePrefix       :: XmlNode xn => a xn String
    getNamePrefix       = arrL (maybeToList  . XN.getNamePrefix)
    {-# INLINE getNamePrefix #-}

    -- | select the namespace URI
    getNamespaceUri     :: XmlNode xn => a xn String
    getNamespaceUri     = arrL (maybeToList  . XN.getNamespaceUri)
    {-# INLINE getNamespaceUri #-}

    -- | select the value of an attribute of an element node,
    -- always succeeds with empty string as default value \"\"
    getAttrValue        :: XmlNode xn => String -> a xn String
    getAttrValue n      = xshow (getAttrl >>> hasName n >>> getChildren)

    -- | like 'getAttrValue', but fails if the attribute does not exist
    getAttrValue0       :: XmlNode xn => String -> a xn String
    getAttrValue0 n     = getAttrl >>> hasName n >>> xshow getChildren

    -- | like 'getAttrValue', but select the value of an attribute given by a qualified name,
    -- always succeeds with empty string as default value \"\"
    getQAttrValue       :: XmlNode xn => QName -> a xn String
    getQAttrValue n     = xshow (getAttrl >>> hasQName n >>> getChildren)

    -- | like 'getQAttrValue', but fails if attribute does not exist
    getQAttrValue0      :: XmlNode xn => QName -> a xn String
    getQAttrValue0 n    = getAttrl >>> hasQName n >>> xshow getChildren

    -- edit arrows --------------------------------------------------

    -- | edit the string of a text node
    changeText          :: XmlNode xn => (String -> String) -> a xn xn
    changeText cf       = arr (XN.changeText     cf) `when` isText

    -- | edit the blob of a blob node
    changeBlob          :: XmlNode xn => (Blob -> Blob) -> a xn xn
    changeBlob cf       = arr (XN.changeBlob     cf) `when` isBlob

    -- | edit the comment string of a comment node
    changeCmt           :: XmlNode xn => (String -> String) -> a xn xn
    changeCmt  cf       = arr (XN.changeCmt      cf) `when` isCmt

    -- | edit an element-, attribute- or pi- name
    changeQName         :: XmlNode xn => (QName  -> QName) -> a xn xn
    changeQName cf      = arr (XN.changeName  cf) `when` getQName

    -- | edit an element name
    changeElemName      :: XmlNode xn => (QName  -> QName) -> a xn xn
    changeElemName cf   = arr (XN.changeElemName  cf) `when` isElem

    -- | edit an attribute name
    changeAttrName      :: XmlNode xn => (QName  -> QName) -> a xn xn
    changeAttrName cf   = arr (XN.changeAttrName cf) `when` isAttr

    -- | edit a pi name
    changePiName        :: XmlNode xn => (QName  -> QName) -> a xn xn
    changePiName cf     = arr (XN.changePiName  cf) `when` isPi

    -- | edit an attribute value
    changeAttrValue     :: (XmlNode xn, ToXmlTree t xn) => (String -> String) -> a (t xn) (t xn)
    changeAttrValue cf  = replaceChildren ( xshow getChildren
                                            >>> arr cf
                                            >>> mkText
                                          )
                          `when` isAttr


    -- | edit an attribute list of an element node
    changeAttrl         :: XmlNode xn => (XmlTrees -> [b] -> XmlTrees) -> a xn b -> a xn xn
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
    {-# INLINE setQName #-}

    -- | replace an element name
    setElemName         :: QName -> a XmlTree XmlTree
    setElemName  n      = changeElemName  (const n)
    {-# INLINE setElemName #-}

    -- | replace an attribute name
    setAttrName         :: QName -> a XmlTree XmlTree
    setAttrName n       = changeAttrName (const n)
    {-# INLINE setAttrName #-}

    -- | replace an element name
    setPiName           :: QName -> a XmlTree XmlTree
    setPiName  n        = changePiName  (const n)
    {-# INLINE setPiName #-}

    -- | replace an attribute list of an element node
    setAttrl            :: XmlNode xn => a xn XmlTree -> a xn xn
    setAttrl            = changeAttrl (const id)                -- (\ x y -> y)
    {-# INLINE setAttrl #-}

    -- | add a list of attributes to an element
    addAttrl            :: XmlNode xn => a xn XmlTree -> a xn xn
    addAttrl            = changeAttrl (XN.mergeAttrl)
    {-# INLINE addAttrl #-}

    -- | add (or replace) an attribute
    addAttr             :: XmlNode xn => String -> String  -> a xn xn
    addAttr an av       = addAttrl (sattr an av)
    {-# INLINE addAttr #-}

    -- | remove an attribute
    removeAttr          :: XmlNode xn => String  -> a xn xn
    removeAttr an       = processAttrl (none `when` hasName an)

    -- | remove an attribute with a qualified name
    removeQAttr         :: XmlNode xn => QName  -> a xn xn
    removeQAttr an      = processAttrl (none `when` hasQName an)

    -- | process the attributes of an element node with an arrow
    processAttrl        :: XmlNode xn => a XmlTree XmlTree -> a xn xn
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

    xshow               :: ToXmlTree t x => a n (t x) -> a n String
    xshow f             = f >. XS.xshow
    {-# INLINE xshow #-}


    -- | apply an arrow to the input and convert the resulting XML trees into a string representation

    xshowBlob           :: a n XmlTree -> a n Blob
    xshowBlob f         = f >. XS.xshowBlob
    {-# INLINE xshowBlob #-}


{- | Document Type Definition arrows

These are separated, because they are not needed for document processing,
only when processing the DTD, e.g. for generating access funtions for the toolbox
from a DTD (se example DTDtoHaskell in the examples directory)
-}


class (ArrowXml a) => ArrowDTD a where
    isDTDDoctype        :: XmlNode xn => a xn xn
    isDTDDoctype        = isA (maybe False (== DOCTYPE ) . XN.getDTDPart)

    isDTDElement        :: XmlNode xn => a xn xn
    isDTDElement        = isA (maybe False (== ELEMENT ) . XN.getDTDPart)

    isDTDContent        :: XmlNode xn => a xn xn
    isDTDContent        = isA (maybe False (== CONTENT ) . XN.getDTDPart)

    isDTDAttlist        :: XmlNode xn => a xn xn
    isDTDAttlist        = isA (maybe False (== ATTLIST ) . XN.getDTDPart)

    isDTDEntity         :: XmlNode xn => a xn xn
    isDTDEntity         = isA (maybe False (== ENTITY  ) . XN.getDTDPart)

    isDTDPEntity        :: XmlNode xn => a xn xn
    isDTDPEntity        = isA (maybe False (== PENTITY ) . XN.getDTDPart)

    isDTDNotation       :: XmlNode xn => a xn xn
    isDTDNotation       = isA (maybe False (== NOTATION) . XN.getDTDPart)

    isDTDCondSect       :: XmlNode xn => a xn xn
    isDTDCondSect       = isA (maybe False (== CONDSECT) . XN.getDTDPart)

    isDTDName           :: XmlNode xn => a xn xn
    isDTDName           = isA (maybe False (== NAME    ) . XN.getDTDPart)

    isDTDPERef          :: XmlNode xn => a xn xn
    isDTDPERef          = isA (maybe False (== PEREF   ) . XN.getDTDPart)

    hasDTDAttr          :: XmlNode xn => String -> a xn xn
    hasDTDAttr n        = isA (isJust . lookup n . fromMaybe [] . XN.getDTDAttrl)

    getDTDAttrValue     :: XmlNode xn => String -> a xn String
    getDTDAttrValue n   = arrL (maybeToList . lookup n . fromMaybe [] . XN.getDTDAttrl)

    setDTDAttrValue     :: XmlNode xn => String -> String -> a xn xn
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
