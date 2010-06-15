-- |
-- basic XmlTree filter

module Text.XML.HXT.DOM.XmlTreeFilter
    ( module Text.XML.HXT.DOM.XmlTreeFilter
    )
where

import Text.XML.HXT.DOM.XmlTreeTypes
import Text.XML.HXT.DOM.XmlTreeFunctions
import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.Unicode ( isXmlSpaceChar        )
import Text.XML.HXT.DOM.Util    ( decimalStringToInt    )

import Data.List                ( partition     )

infixl 7 += , ++=

-- -----------------------------------------------------------------------------
--
-- filter predicates

-- |
-- test whether the root of a tree contains a document root node.

isRoot                  :: XmlFilter
isRoot                  = isOfNode (isTagNode t_root)

-- |
-- test whether the root of a tree contains a tag node.
--
-- see also: 'isNsTag'

isTag                   :: String -> XmlFilter
isTag tn                = isOfNode (isTagNode tn)

-- |
-- namespace aware test whether the root of a tree contains a tag node. Parameters are the local part and namespace.
-- Only usable after namespace propagation.
--
-- see also: 'isTag'

isNsTag                 :: String -> String -> XmlFilter
isNsTag lp ns           = isOfTag (\ t -> localPart t == lp && namespaceUri t == ns)

-- |
-- test whether the root of a tree has a given local name
-- see also : 'hasNamespace', 'hasPrefix', 'isTag', 'isAttr'

hasLocalPart            :: String -> XmlFilter
hasLocalPart lp         = isOf ((== lp) . localPartOf)

-- |
-- test whether the root of a tree has a given prefix name
-- see also : 'hasNamespace', 'hasLocalPart', 'isTag', 'isAttr'

hasPrefix               :: String -> XmlFilter
hasPrefix px            = isOf ((== px) . prefixOf)

-- |
-- test whether the root of a tree belongs to a given namespace
-- see also : 'isTag', 'isAttr', 'hasLocalPart', 'hasPrefix'

hasNamespace            :: String -> XmlFilter
hasNamespace ns         = isOf ((== ns) . namespaceOf)

-- |
-- test whether the root of a tree contains a tag node with a special name.

isOfTag                 :: (TagName -> Bool) -> XmlFilter
isOfTag                 = isOfNode . isOfTagNode

-- |
-- test whether the node of a tree is a XTag node or a XPi node with an attibute of a specific name
--
-- see also: 'isAttr', 'hasNsAttr'

hasAttr                 :: String -> XmlFilter
hasAttr an              = (getAttrl .> isAttr an) `guards` this

-- |
-- test whether the tree is a XTag node with an attibute of a specific local name and namespace uri
--
-- see also: 'hasAttr', 'isNsAttr'

hasNsAttr               :: String -> String -> XmlFilter
hasNsAttr an ns         = (getAttrl .> isNsAttr an ns) `guards` this

-- |
-- test whether the given node is a XTag node or a XPI node with an attribute with a value with a specific property.
-- In case of a match, the attribute value represented by a text node is returned as single element list,
-- else the empty list is the result.
--
-- see also : 'getValue'

hasValue                :: String -> (String -> Bool) -> XmlFilter
hasValue a p t
   | (not . null) att
     &&
     p val
       = xtext val
   | otherwise
       = []
   where
   att = getAttrl .> isAttr a $ t
   val = xshow . getChildren $$ att

-- |
-- test whether the tree is a processing instruction with a given name.

isPi                    :: String -> XmlFilter
isPi pn                 = isOfNode (isPiNode pn)

-- |
-- test whether the tree is a \<?xml ... ?\> declaration

isXmlPi                 :: XmlFilter
isXmlPi                 = isPi t_xml

-- |
-- test whether the root of a tree contains a processing instruction of a special name.

isOfPi                  :: (TagName -> Bool) -> XmlFilter
isOfPi                  = isOfNode . isOfPiNode

-- -----------------------------------------------------------------------------

-- simple XNode Filter predicates
--

-- |
-- test whether the root of a tree contains a CDATA node.

isXCdata                :: XmlFilter
isXCdata                = isOfNode isXCdataNode

-- |
-- test whether the root of a tree contains a character reference node.

isXCharRef              :: XmlFilter
isXCharRef              = isOfNode isXCharRefNode

-- |
-- test whether the root of a tree contains a comment node.

isXCmt                  :: XmlFilter
isXCmt                  = isOfNode isXCmtNode

-- |
-- test whether the root of a tree contains a DTD part.

isXDTD                  :: XmlFilter
isXDTD                  = isOfNode isXDTDNode

-- |
-- test whether the root of a tree contains an entity reference node.

isXEntityRef            :: XmlFilter
isXEntityRef            = isOfNode isXEntityRefNode

-- |
-- test whether the root of a tree contains an error node.

isXError                :: XmlFilter
isXError                = isOfNode isXErrorNode

-- |
-- test whether the root of a tree contains a processing instruction node.

isXPi                   :: XmlFilter
isXPi                   = isOfNode isXPiNode

-- |
-- test whether the root of a tree contains a tag node.

isXTag                  :: XmlFilter
isXTag                  = isOfNode isXTagNode

-- |
-- test whether the root of a tree contains an attribute node.

isXAttr                 :: XmlFilter
isXAttr                 = isOfNode isXAttrNode

-- |
-- test whether the root of a tree is an attribute node for a given attribute name

isAttr                  :: String -> XmlFilter
isAttr an               = isOfNode (isAttrNode an)

-- |
-- namespace aware test whether the tree contains an attribute node. Parameters are the local part of the atribute name and the namespace.
-- Only usable after namespace propagation.
--
-- see also: 'isNsTag', 'isAttr', 'hasNsAttr'

isNsAttr                :: String -> String -> XmlFilter
isNsAttr lp ns          = isOfAttr (\ a -> localPart a == lp && namespaceUri a == ns)

-- |
-- general test for an attribute name

isOfAttr                :: (AttrName -> Bool) -> XmlFilter
isOfAttr                = isOfNode . isOfAttrNode

-- |
-- test whether the root of a tree contains a text node.

isXText                 :: XmlFilter
isXText                 = isOfNode isXTextNode

-- |
-- test whether the root of a tree contains a special text.

isText                  :: String -> XmlFilter
isText t                = isOfNode (isTextNode t)

-- |
-- test whether the root of a tree contains a text node with a special property

isOfText                :: (String -> Bool) -> XmlFilter
isOfText                = isOfNode . isOfTextNode

-- |
-- test whether the root of a tree contains a text node only with whitespace.

isWhiteSpace            :: XmlFilter
isWhiteSpace
    = isOfNode isWS
      where
      isWS n
          = isXTextNode n
            && all isXmlSpaceChar (textOfXNode n)

-- ------------------------------------------------------------
--
-- XDTD Predicates

-- |
-- test whether the root of a tree contains a DOCTYPE DTD part.

isDoctype                       :: XmlFilter
isDoctype                       = isOfNode (isDTDElemNode DOCTYPE)

-- |
-- test whether the root of a tree contains an ATTLIST DTD part.

isAttlist                       :: XmlFilter
isAttlist                       = isOfNode (isDTDElemNode ATTLIST)

-- |
-- test whether the root of a tree contains an ELEMENT DTD part.

isElement                       :: XmlFilter
isElement                       = isOfNode (isDTDElemNode ELEMENT)

-- |
-- test whether the root of a tree contains an ENTITY DTD part.

isEntity                        :: XmlFilter
isEntity                        = isOfNode (isDTDElemNode ENTITY)

-- |
-- test whether the root of a tree contains a parameter ENTITY reference.

isPeRef                         :: XmlFilter
isPeRef                         = isOfNode (isDTDElemNode PEREF)

-- |
-- test whether the root of a tree contains a DTD name part.

isDTDName                       :: XmlFilter
isDTDName                       = isOfNode (isDTDElemNode NAME)

-- |
-- test whether the root of a tree contains a conditional section DTD part.

isCondSect                      :: XmlFilter
isCondSect                      = isOfNode (isDTDElemNode CONDSECT)

-- |
-- test whether the root of a tree contains a parameter entity declaration.

isParameterEntity               :: XmlFilter
isParameterEntity               = isOfNode (isDTDElemNode PENTITY)

-- |
-- test whether the root of a tree contains a NOTATION DTD part.

isNotation                      :: XmlFilter
isNotation                      = isOfNode (isDTDElemNode NOTATION)

-- ATTLIST predicates

isDefaultAttrKind               :: XmlFilter
isDefaultAttrKind               = isOf (a_kind `ofDTDequals` k_default)

isEnumAttrType                  :: XmlFilter
isEnumAttrType                  = isOf (a_type `ofDTDequals` k_enumeration)

isFixedAttrKind                 :: XmlFilter
isFixedAttrKind                 = isOf (a_kind `ofDTDequals` k_fixed)

isIdAttrType                    :: XmlFilter
isIdAttrType                    = isOf (a_type `ofDTDequals` k_id)

isIdRefAttrType                 :: XmlFilter
isIdRefAttrType                 = isOf ((`elem` [k_idref, k_idrefs]) . valueOfDTD a_type)

isNotationAttrType              :: XmlFilter
isNotationAttrType              = isOf (a_type `ofDTDequals` k_notation)

isRequiredAttrKind              :: XmlFilter
isRequiredAttrKind              = isOf (a_kind `ofDTDequals` k_required)

isAttlistParameterEntity        :: XmlFilter
isAttlistParameterEntity        = isAttlist .> isOf (a_type `ofDTDequals` k_peref)

-- ELEMENT predicates

isEmptyElement                  :: XmlFilter
isEmptyElement                  = isOf (a_type `ofDTDequals` k_empty)

isMixedContentElement           :: XmlFilter
isMixedContentElement           = isOf (a_type `ofDTDequals` v_mixed)

isElemWithContent               :: XmlFilter
isElemWithContent               = isElement
                                  .>
                                  isOf ((`elem` [v_mixed, v_children]) . valueOfDTD a_type)

isAttlistOfElement              :: String -> XmlFilter
isAttlistOfElement el           = isAttlist
                                  .>
                                  isOf (a_name `ofDTDequals` el)

isElemContentParamEntity        :: XmlFilter
isElemContentParamEntity        = isElement
                                  .>
                                  isOf (a_type `ofDTDequals` k_peref)

-- ENTITY predicates

isUnparsedEntity                :: XmlFilter
isUnparsedEntity                = isOf (hasEntry k_ndata . attrlOfDTD)

isExternalParameterEntity       :: XmlFilter
isExternalParameterEntity       = isParameterEntity
                                  .>
                                  isOf (hasEntry k_system . attrlOfDTD)

isInternalParameterEntity       :: XmlFilter
isInternalParameterEntity       = isParameterEntity
                                  .> isOf (not . hasEntry k_system . attrlOfDTD)

-- -----------------------------------------------------------------------------

-- |
-- test whether the root of a tree contains an error node for a warning.

isWarning                       :: XmlFilter
isWarning                       = isOfNode $ isErrorNode c_warn

-- |
-- test whether the root of a tree contains an error node for an error.

isError                         :: XmlFilter
isError                         = isOfNode $ isErrorNode c_err

-- |
-- test whether the root of a tree contains an error node for a fatal error.

isFatalError                    :: XmlFilter
isFatalError                    = isOfNode $ isErrorNode c_fatal

-- -----------------------------------------------------------------------------
--
-- constructor filter

-- |
-- constructor filter for a tag node.
-- a new tree is constructed.
-- the attributes and the children are computed by applying the aproprate filter to the input tree
--
--    * 1.parameter n :  the tag name
--
--    - 2.parameter af :  the filter for the attribute list
--
--    - 3.parameter cf :  the filter for the children
--
--    - returns : the constructor filter

mkXTag          :: String -> XmlFilter -> XmlFilter -> XmlFilter
mkXTag n af cf
    = \ t -> [ mkXTagTree n (af t) (cf t) ]

-- | Version with qualified names of 'mkXTag'

mkQTag          :: QName -> XmlFilter -> XmlFilter -> XmlFilter
mkQTag q af cf
    = \ t -> [ mkQTagTree q (af t) (cf t) ]

-- |
-- constructor filter for a tag node.
-- a new tree is constructed.
-- the attributes and the children are computed by applying the aproprate filter to the input tree
--
--    * 1.parameter n :  the tag name in form of prefix:localpart
--
--    - 2.parameter ns:   the namespace uri
--
--    - 3.parameter af :  the filter for the attribute list
--
--    - 4.parameter cf :  the filter for the children
--
--    - returns : the constructor filter

mkXNsTag        :: String -> String -> XmlFilter -> XmlFilter -> XmlFilter
mkXNsTag n ns af cf
    = \ t -> [ mkXNsTagTree n ns (af t) (cf t) ]

-- |
-- filter for attribute construction.
-- a new tree with attribute name and a value computed by a filter
-- is build.

mkXAttr         :: String -> XmlFilter -> XmlFilter
mkXAttr n af
    = \ t -> [ mkXAttrTree n (af t) ]

-- | Qualified version 'mkXAttr'

mkQAttr         :: QName -> XmlFilter -> XmlFilter
mkQAttr q af
    = \ t -> [ mkQAttrTree q (af t) ]

-- |
-- filter for attribute construction.
-- a new tree with attribute name and namespace and a value computed by a filter
-- is build.

mkXNsAttr       :: String -> String -> XmlFilter -> XmlFilter
mkXNsAttr n ns af
    = \ t -> [ mkXNsAttrTree n ns (af t) ]

-- |
-- constructor filter for a text node.
-- a new tree is constructed.
-- the input tree is ignored.

mkXText         :: String                          -> XmlFilter
mkXText         = mkNTree . mkXTextTree

-- |
-- constructor filter for a character reference node.
-- a new tree is constructed.
-- the input tree is ignored.

mkXCharRef      :: Int                             -> XmlFilter
mkXCharRef      = mkNTree . mkXCharRefTree

-- |
-- constructor filter for an entity reference node.
-- a new tree is constructed.
-- the input tree is ignored.

mkXEntityRef    :: String                          -> XmlFilter
mkXEntityRef    = mkNTree . mkXEntityRefTree

-- |
-- constructor filter for a comment node.
-- a new tree is constructed.
-- the xml string representation of the filter result
-- forms the comment

mkXCmt          :: XmlFilter -> XmlFilter
mkXCmt cf
    = \ t -> [ mkXCmtTree ((xshow . cf) t) ]

-- |
-- constructor filter for a DTD part.
-- a new tree is constructed.
-- the input tree is ignored.

mkXDTD          :: DTDElem -> Attributes -> XmlTrees -> XmlFilter
mkXDTD n al     = mkNTree . mkXDTDTree n al

-- |
-- constructor filter for a CDATA section node.
-- a new tree is constructed.
-- the input tree is ignored.

mkXCdata        :: XmlFilter -> XmlFilter
mkXCdata cf
    = \ t -> [ mkXCdataTree ((xshow . cf) t) ]

-- |
-- constructor filter for a processing instruction
-- a new tree is constructed from the text representation
-- of the input tree

mkXPi           :: String -> XmlFilter -> XmlFilter
mkXPi piName cf
    = \ t -> [ mkXPiTree piName ((xshow . cf) t) ]

-- |
-- constructor filter for an error message node.
-- a new tree is constructed.
-- the input tree is ignored.

mkXError        :: Int -> String                   -> XmlFilter
mkXError l s    = this . mkXErrorTree l s . this

-- -----------------------------------------------------------------------------
--
-- selector filters

-- |
-- filter for selecting the name of a tag node, an attribute node or a pi node.
-- Result of the filter is a single element list with a text node or the empty list

getName ::             XmlFilter
getName t
    | null s    = []
    | otherwise = xtext s
    where
    s = nameOf t

-- |
-- filter for selecting the attibute list

getAttrl        :: XmlFilter
getAttrl
    = selAttrl . getNode
    where
    selAttrl (XTag _ al) = al
    selAttrl (XPi  _ al) = al
    selAttrl _           = []

-- |
-- filter for selecting the value of an attribute in a tag node.
-- Result of the filter is a single element list with a text node or the empty list
--
-- see also : 'hasValue', 'getNsValue'

getValue                :: String -> XmlFilter
getValue a              = xmlTreesToText . (getAttrl .> isAttr a .> getChildren)

-- |
-- filter for selecting the value of an attribute with namespace in a tag node.
-- Result of the filter is a single element list with a text node or the empty list
--
-- see also : 'getValue', 'isNsAttr'

getNsValue              :: String -> String -> XmlFilter
getNsValue lp ns        = xmlTreesToText . (getAttrl .> isNsAttr lp ns .> getChildren)

-- |
-- filter for selecting an attribute of a DTD node.
-- Result of the filter is a single element list with a text node or the empty list

getDTDValue             :: String -> XmlFilter
getDTDValue a           = xtext . valueOfDTD a

-- |
-- filter for selecting content of a comment.
-- Result of the filter is a single element list with a text node or the empty list

getXCmt                 :: XmlFilter
getXCmt
    = selCmt . getNode
      where
      selCmt (XCmt c)   = xtext c
      selCmt _          = []

-- |
-- filter for selecting the CDATA content.
-- Result of the filter is a single element list with a text node or the empty list

getXCdata               :: XmlFilter
getXCdata
    = selData . getNode
      where
      selData (XCdata d)        = xtext d
      selData _                 = []

-- -----------------------------------------------------------------------------
--
-- edit filter

-- |
-- edit filter for changing the name of a tag node, an attribute or a pi.
-- result of the filter is a single element list with a tag node or the empty list

replaceQName                            :: String -> XmlFilter
replaceQName n                          = modifyQName (const (mkName n))

-- |
-- edit filter for changing the text of a text node.
-- result of the filter is a single element list with a text node or the empty list
--
-- example for editing all text nodes of a tree with an edit function @f@:
--
-- @processBottomUp (modifyText f \`when\` isXText)@

modifyText                              :: (String -> String) -> XmlFilter
modifyText f t@(NTree n _)
    | isXTextNode n                     = replaceNode (XText (f (textOfXNode n))) t
modifyText _ _                          = []

-- |
-- edit filter for changing the name of a tag node.
-- result of the filter is a single element list with a text node or the empty list

modifyQName                             ::  (TagName -> TagName) -> XmlFilter
modifyQName f (NTree (XTag n al) cs)    = [NTree (XTag (f n) al) cs]
modifyQName f (NTree (XPi  n al) cs)    = [NTree (XPi  (f n) al) cs]
modifyQName f (NTree (XAttr n)   cs)    = [NTree (XAttr (f n)  ) cs]
modifyQName _ _                 = []

-- -----------------------------------------------------------------------------

-- |
-- process the attribute list of a tag node with a tree list filter.
-- for other trees this filter acts like 'none'

processAttrl                            :: XmlSFilter -> XmlFilter
processAttrl f t
    = replaceAttrl (f al) t
      where
      al = getAttrl t

-- |
-- elementwise processing of the attributes of a tag.
-- for other trees this filter acts like 'none'
--
-- see also : 'processAttrl'
processAttr                             :: XmlFilter -> XmlFilter
processAttr f
    = processAttrl (f $$)

-- |
-- replace an attribute list
-- to be renamed when replaceAttrl is eliminated

replaceAttrl                            :: XmlTrees -> XmlFilter
replaceAttrl al (NTree (XTag n _al) cs) = [ NTree (XTag n al) cs ]
replaceAttrl al (NTree (XPi  n _al) cs) = [ NTree (XPi  n al) cs ]
replaceAttrl _ _                        = []

-- |
-- delete an attribute from the attribute list of a tag tree

del1Attr                                :: String -> XmlFilter
del1Attr an                             = processAttr (none `when` isAttr an)

-- |
-- add an attribute to the attribute list of a tag.
-- If the attribute already exists, it\'s substituted,
--
-- see also: 'sattr', '+='

add1Attr                                :: XmlTree -> XmlFilter
add1Attr a@(NTree (XAttr an) _av) t
    = replaceAttrl (replace al) t
      where
      al = getAttrl t
      replace [] = [a]
      replace (a1 : as)
          | satisfies (isAttr (qualifiedName an)) a1
              = a : as
          | otherwise
              = a1 : replace as

add1Attr _ t
    = this t

-- |
-- adds an attribute list computed by a filter, uses 'add1Attr'.
--
-- see also: '+='

addAttrl                                :: XmlFilter -> XmlFilter
addAttrl attrFilter t
    = ((foldl (.>) this . map add1Attr) al) t
      where
      al = attrFilter t

-- |
-- add or change an attribute with a given string as value for a XTag or XPi tree,
-- uses 'add1Attr'.

addAttr                                 :: String -> String -> XmlFilter
addAttr an av
    = add1Attr (mkXAttrTree an (if null av then [] else xtext av))

-- |
-- add or change an attribute with an Int value.
-- uses 'addAttr'.

addAttrInt                              :: String -> Int -> XmlFilter
addAttrInt an av
    = addAttr an (show av)

-- |
-- edit filter for changing the value of an attribute in a tag node.
-- result of the filter is a single element list with the tag node or the empty list.
--
--    * 1.parameter n :  the name of the attribute
--
--    - 2.parameter f :  the edit function for the attribute value
--
--    - returns : the edit filter

modifyAttr                              :: String -> (String -> String) -> XmlFilter
modifyAttr an f
    = processAttr (modifyValue `when` isAttr an)
      where
      modifyValue = modifyChildren ((modifyText f $$) . xmlTreesToText)

-- |
-- add or change an attribute of a DTD tree

addDTDAttr                              :: String -> String -> XmlFilter
addDTDAttr an av (NTree (XDTD e al) cs)
    = [NTree (XDTD e (addEntry an av al)) cs]

addDTDAttr _ _ _
    = []


-- -----------------------------------------------------------------------------
--

-- |
-- convenient function for tag node tree construction
--
-- infixl 7
--
-- filter combinator for tag tree constrcution
-- take a 1. filter for computing a tag node tree (or a whole list of tag node trees)
-- then add all trees computed by the 2. filter to the attribute list when they represent attributes
-- else append them to the list of children.
--
-- if the 1. filter computes a list of tag nodes, the results of the 2. filter are added to all trees
--
-- example: @ etag \"a\" += sattr \"href\" \"\#42\" += txt \"the answer\" @
-- gives the tree @ \<a href=\"\#42\"\>the answer\<\/a\> @
--
-- example: @ ( etag \"a\" +++ etag \"b\" ) += sattr \"x\" \"42\" @
-- gives the tree @ \<a x=\"42\"\/\>\<b x=\"42\"\/\> @
--
-- see also : 'etag', 'tag', 'add1Attr', 'modifyChildren', '++='

(+=)    :: XmlFilter -> XmlFilter -> XmlFilter
(+=) tf cf t
    = concat
      [ ( isXTag `guards` ( addaf .> addcf )) $ t'
        | t' <- tf t
      ]
    where
    (as, cs) = partition (satisfies isXAttr) . cf $ t

    addaf | null as   = this
          | otherwise = addAttrl (const as)

    addcf | null cs   = this
          | otherwise = modifyChildren (++ cs)

-- |
-- convenient filter function adding a whole list of trees, just for not writing to many ( ... ).
--
-- infixl 7
--
-- @ f ++= gl  == f += cat gl @
--
-- see also : '+='

(++=)   :: XmlFilter -> [XmlFilter] -> XmlFilter
tf ++= cfs
    = tf += cat cfs

-- -----------------------------------------------------------------------------
--
-- string access functions

-- |
-- combination of 'getValue' and conversion into a String

valueOf                 :: String -> XmlTree -> String
valueOf a               = xshow . getValue a

-- |
-- combination of 'getValue' and conversion to a Int

intValueOf              :: String -> XmlTree -> Int
intValueOf a            = decimalStringToInt . valueOf a

-- -----------------------------------------------------------------------------
--
-- convenient filter (short cuts)

-- |
-- variant of mkXTag with a list of filters for the attributes and a list of filters for the children.
-- this variant leads to a more readable source for a complicated construction filter
-- than the simple solution with a combination of 'mkXTag' and 'cat'.
--
-- see also : 'mkXTag', 'stag', 'etag', 'cat', '+='

tag             :: String -> [XmlFilter] -> [XmlFilter] -> XmlFilter
tag n afs cfs   = mkXTag n (cat afs) (cat cfs)

-- |
-- variant of tag, useful for tags without attributes and with a list of filters for
-- constructing the children
--
-- see also : 'mkXTag', 'tag', 'etag', 'cat', '+='

stag            :: String -> [XmlFilter] -> XmlFilter
stag n cfs      = mkXTag n none (cat cfs)

-- |
-- variant of tag, useful for tags with attributes but without children
--
-- see also : 'mkXTag', 'tag', 'stag', 'etag', 'cat'

atag            :: String -> [XmlFilter] -> XmlFilter
atag n cfs      = mkXTag n (cat cfs) none

-- |
-- Short cut for empty tags without attributes
--
-- see also : 'tag', 'atag', 'stag', 'mkXTag' and '+='

etag            :: String -> XmlFilter
etag n          = mkXTag n none none

-- | Qualified version of etag

qetag           :: QName -> XmlFilter
qetag q         = mkQTag q none none

-- | Alias for mkQTag

qtag            :: QName -> XmlFilter -> XmlFilter -> XmlFilter
qtag = mkQTag


-- |
-- filter for creating a document root node with a list of filters for the attributes and a list of filters for the document.
--
-- see also : 'tag'

rootTag         :: [XmlFilter] -> [XmlFilter] -> XmlFilter
rootTag afs cfs = mkXTag t_root (cat afs) (cat cfs)

-- |
-- Alias for 'mkXAttr'

attr            :: String -> XmlFilter -> XmlFilter
attr            = mkXAttr

-- | Alias for mkQAttr
qattr           :: QName -> XmlFilter -> XmlFilter
qattr = mkQAttr

-- |
-- short cut for attribute construction with string constants
--
-- set also : 'attr', 'mkXAttr' and 'mkXText'

sattr           :: String -> String -> XmlFilter
sattr a v       = mkXAttr a (mkXText v)

-- |
-- short cut for 'mkXText'

txt             :: String  -> XmlFilter
txt             = mkXText

-- |
-- short cut for simple comment
-- the input tree is ignored
--
-- see also : 'mkXCmt'

cmt             :: String  -> XmlFilter
cmt c           = mkXCmt (txt c)

-- |
-- short cut for generating simple processing instructions (spi)
-- the input tree is ignored
--
-- @spi \"xyz\" \"abc\"@ is equal to @mkXPi \"xyz\" (txt \"abc\")@
-- (the name pi is already used in prelude)

spi             :: String -> String -> XmlFilter
spi n c         = mkXPi n (txt c)

-- |
-- short cut for generating simple cdata sections,
-- the input tree is ignored

cdata           :: String -> XmlFilter
cdata s         = mkXCdata (txt s)

-- |
-- DTD part generation with filter for attributes and children
-- see also: 'mkXDTDTree'

dtd             :: DTDElem -> [XmlFilter] -> [XmlFilter] -> XmlFilter
dtd dtdpart afs cfs t
    = mkXDTD dtdpart (toAttrl . cat afs $ t) (cat cfs t) $ t

-- |
-- short cut for mkXError c_warn.
--
-- see also : 'mkXError'

warn            :: String -> XmlFilter
warn            = mkXError c_warn

-- |
-- short cut for mkXError c_fatal.
--
-- see also : 'mkXError'

err             :: String -> XmlFilter
err             = mkXError c_err

-- |
-- short cut for mkXError c_fatal.
--
-- see also : 'mkXError'

fatal           ::  String -> XmlFilter
fatal           = mkXError c_fatal

-- |
-- check whether an option is set
--
-- reads the value of an attribute, usually applied to a document root node,
-- and checks if the value represents True. The following strings are interpreted
-- as true: \"1\", \"True\", \"true\", \"yes\", \"Yes\".

hasOption       :: String -> XmlFilter
hasOption opt
    = getValue opt .> isOfText isSet
      where
      isSet x
          = x `elem` ["1", "True", "true", "yes", "Yes"]


-- -----------------------------------------------------------------------------

