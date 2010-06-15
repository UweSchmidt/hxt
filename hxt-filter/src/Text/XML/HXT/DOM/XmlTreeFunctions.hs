-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DOM.XmlTreeFunctions
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Basic XmlTree functions

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DOM.XmlTreeFunctions
    ( module Text.XML.HXT.DOM.XmlTreeFunctions
    , module Text.XML.HXT.DOM.ShowXml
    )
where

import Text.XML.HXT.DOM.XmlTreeTypes
import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.ShowXml

import Data.Maybe

-- -----------------------------------------------------------------------------
--
-- XNode Predicates
--

isXCdataNode                    :: XNode -> Bool
isXCdataNode (XCdata _)         = True
isXCdataNode _                  = False

isXCharRefNode                  :: XNode -> Bool
isXCharRefNode (XCharRef _)     = True
isXCharRefNode _                = False

isXCmtNode                      :: XNode -> Bool
isXCmtNode (XCmt _)             = True
isXCmtNode _                    = False

isXDTDNode                      :: XNode -> Bool
isXDTDNode (XDTD _ _)           = True
isXDTDNode _                    = False

isXAttrNode                     :: XNode -> Bool
isXAttrNode (XAttr _)           = True
isXAttrNode _                   = False

isXEntityRefNode                :: XNode -> Bool
isXEntityRefNode (XEntityRef _) = True
isXEntityRefNode _              = False

isXErrorNode                    :: XNode -> Bool
isXErrorNode (XError _ _)       = True
isXErrorNode _                  = False

isXPiNode                       :: XNode -> Bool
isXPiNode (XPi _ _)             = True
isXPiNode _                     = False

isXTagNode                      :: XNode -> Bool
isXTagNode  (XTag _ _)          = True
isXTagNode _                    = False

isXTextNode                     :: XNode -> Bool
isXTextNode (XText _)           = True
isXTextNode _                   = False

-- -----------------------------------------------------------------------------

isRootNode                      :: XNode -> Bool
isRootNode                      = isTagNode t_root

isTagNode                       :: String -> XNode -> Bool
isTagNode n                     = isOfTagNode ((== n) . qualifiedName)

isOfTagNode                     :: (TagName -> Bool) -> XNode -> Bool
isOfTagNode p (XTag n _)        = p n
isOfTagNode _ _                 = False

isAttrNode                      :: String -> XNode -> Bool
isAttrNode n                    = isOfAttrNode ((== n) . qualifiedName)

isOfAttrNode                    :: (AttrName -> Bool) -> XNode -> Bool
isOfAttrNode p (XAttr n)        = p n
isOfAttrNode _ _                = False

isTextNode                      :: String -> XNode -> Bool
isTextNode t                    = isOfTextNode (== t)

isOfTextNode                    :: (String -> Bool) -> XNode -> Bool
isOfTextNode p (XText t)        = p t
isOfTextNode _ _                = False

isPiNode                        :: String -> XNode -> Bool
isPiNode n                      = isOfPiNode ((== n) . qualifiedName)

isOfPiNode                      :: (TagName -> Bool) -> XNode -> Bool
isOfPiNode p (XPi n _)          = p n
isOfPiNode _ _                  = False

isDTDElemNode                   :: DTDElem -> XNode -> Bool
isDTDElemNode e (XDTD n _)      = n == e
isDTDElemNode _ _               = False

isErrorNode                     :: Int -> XNode -> Bool
isErrorNode l (XError l'  _)    = l == l'
isErrorNode _ _  = False

-- -----------------------------------------------------------------------------

textOfXNode                     :: XNode -> String
textOfXNode (XText t)           = t
textOfXNode _                   = ""

-- -----------------------------------------------------------------------------
--
-- XmlTree constructors

-- |
-- Create a tree with a tag node.
--
--    * 1.parameter n :  the name of the tag
--
--    - 2.parameter al :  the tag attribte list
--
--    - 3.parameter cs :  the list of children
--
--    - returns : the new tree

mkXTagTree              :: String -> XmlTrees -> XmlTrees -> XmlTree
mkXTagTree n al cs      = mkNode (XTag (mkName n) al) cs

-- | Version with qualified name of 'mkXTagTree'

mkQTagTree              :: QName -> XmlTrees -> XmlTrees -> XmlTree
mkQTagTree q al cs      = mkNode (XTag q al) cs

-- |
-- create a tree with a namespace aware tag node.
--
--    * 1.parameter n :  the prefix:localpart of the tag
--
--    - 2.parameter ns:  the namespace uri
--
--    - 3.parameter al :  the tag attribte list
--
--    - 4.parameter cs :  the list of children
--
--    - returns : the new tree
--
-- see also: 'mkXTagTree'

mkXNsTagTree            :: String -> String -> XmlTrees -> XmlTrees -> XmlTree
mkXNsTagTree n ns al cs = mkNode (XTag (mkNsName n ns) al) cs

-- |
-- creates a new document tree with empty contents.
--
--   * 1.parameter al : the attribute list for the root node
--
-- returns a single node tree with tag name \"\/\" indicating a root and
-- with empty list of children
--
-- see also : 'emptyRoot'

newRoot                 :: XmlTrees -> XmlTree
newRoot al              = mkXTagTree t_root al []

-- | the empty document tree
--
-- see also : 'newRoot'

emptyRoot               :: XmlTree
emptyRoot               = newRoot []

-- |
-- create a new empty document with source name as parameter

newDocument             :: String -> XmlTree
newDocument n
    = newDocument' [(a_source, n), (a_status, show c_ok)]

-- |
-- create a new empty document with a list of attributes for source location and options
--
-- see also : 'newDocument'

newDocument'            :: Attributes -> XmlTree
newDocument' al
    = newRoot (fromAttrl al)

-- |
-- create a document root tree.
--
--    * 1.parameter al :  the attribute list for the root. This list must contain at
--                least an attribute \"source\" that contains the URI of the document to be processed
--
--    - 2.parameter cs :  the list for the document content
--
--    - returns : the document root

mkRootTree              :: XmlTrees -> XmlTrees -> XmlTree
mkRootTree al cs        = mkXTagTree t_root al cs

-- |
-- create a leaf for a text element.
--
--    * 1.parameter txt :  the text
--
--    - returns : the tree with the single node containing the text

mkXTextTree             :: String -> XmlTree
mkXTextTree s           = mkLeaf (XText s)

-- |
-- create a leaf for a char reference.
--
--    * 1.parameter i :  the integer representing the Unicode char
--
--    - returns : the tree with the single node containing the char reference

mkXCharRefTree          :: Int -> XmlTree
mkXCharRefTree s        = mkLeaf (XCharRef s)

-- |
-- create a leaf for an entity reference.
--
--    * 1.parameter n :  the name of the entity reference
--
--    - returns : the tree with the single node containing the entity reference

mkXEntityRefTree        :: String -> XmlTree
mkXEntityRefTree s      = mkLeaf (XEntityRef s)

-- |
-- create a leaf for a comment,
--
--    * 1.parameter c :  the comment text
--
--    - returns : the tree with the single node containing the comment

mkXCmtTree              :: String -> XmlTree
mkXCmtTree c            = mkLeaf (XCmt c)

-- |
-- create a tree for a part of a DTD
--
--    * 1.parameter d :  the type of the DTD part
--
--    - 2.parameter al :  the attribute list for the DTD part
--
--    - 3.parameter ds :  the possibly empty list of components for the DTD part
--
--    - returns : the tree with the composed DTD part

mkXDTDTree              :: DTDElem -> Attributes -> XmlTrees -> XmlTree
mkXDTDTree d al ds      = mkNode (XDTD d al) ds


-- |
-- create an attribute tree as part of a tag attribute list of tag nodes
--
--    * 1.parameter al : the attribute name
--
--    - 2.parameter av : the attribute value as tree list, usually containing a single text node

mkXAttrTree             :: String -> XmlTrees -> XmlTree
mkXAttrTree an av       = mkNode (XAttr (mkName an)) av

-- | Qualified version of 'mkXAttrTree'

mkQAttrTree             :: QName -> XmlTrees -> XmlTree
mkQAttrTree aq av       = mkNode (XAttr aq) av


-- |
-- create an attribute tree with a namespace
--
--    * 1.parameter al : the attribute name
--
--    - 2.parameter ns : namespace uri
--
--    - 3.parameter av : the attribute value as tree list, usually containing a single text node
--
-- see also: 'mkXAttrTree', 'mkXNsTagTree'

mkXNsAttrTree           :: String -> String -> XmlTrees -> XmlTree
mkXNsAttrTree an ns av  = mkNode (XAttr (mkNsName an ns)) av

-- |
-- create a parameter entity reference DTD part.
--
--    * 1.parameter ref :  the name of the reference
--
--    - returns : the DTD part for a PERef

mkXPERefTree            :: String -> XmlTree
mkXPERefTree ref        = mkLeaf (XDTD PEREF [(a_peref, ref)])

-- |
-- create a processing instruction tree.
--
--    * 1.parameter n :  the name of the PI
--
--    - 2.parameter str :  the content of a PI
--
--    - returns : the processing instruction tree with a single attribute \"value\"
--      with the str parameter as attribute value, with @valueOf a_value@ applied to the result tree
--      the content of the PI can be selected

mkXPiTree       :: String -> String -> XmlTree
mkXPiTree n str = mkLeaf (XPi (mkName n) (xattr a_value str))

-- |
-- create xml declaration

mkXmlDeclTree   :: XmlTrees -> XmlTree
mkXmlDeclTree al = mkLeaf (XPi (mkName t_xml) al)

-- |
-- create a CDATA section tree.
--
--    * 1.parameter s :  the content of the CDATA section
--
--    - returns : the tree for the CDATA section

mkXCdataTree            :: String -> XmlTree
mkXCdataTree s          = mkLeaf (XCdata s)

-- |
-- create an error tree.
--
--    * 1.parameter l :  the level of the error (warning, error fatal)
--
--    - 2.parameter msg :  the error message
--
--    - 3.parameter cs :  the context, where the error was detected

mkXErrorTree            :: Int -> String -> XmlTrees -> XmlTree
mkXErrorTree l s cs     = mkNode (XError l s) cs

maybeString2XText       :: Maybe String -> XmlTrees
maybeString2XText       = map mkXTextTree . maybeToList

-- ------------------------------------------------------------
--
-- text selection

showXText       :: XmlTrees -> String
showXText
    = concatMap showT
      where
      showT (NTree (XText      t) _) = t
      showT _                        = ""

showXCharRef    :: XmlTrees -> String
showXCharRef
    = concatMap showT
      where
      showT (NTree (XCharRef   r) _) = "&#" ++ show r ++ ";"
      showT _                        = ""

showXEntityRef  :: XmlTrees -> String
showXEntityRef
    = concatMap showT
      where
      showT (NTree (XEntityRef r) _) = "&" ++ r ++ ";"
      showT _                        = ""

showXErrors     :: XmlTrees -> String
showXErrors
    = concatMap showE
      where
      showE (NTree (XError level str) _) = msg level ++ ": " ++ str ++ "\n"
      showE _                            = ""

      msg :: Int -> String
      msg l
          | l == c_warn = "Warning"
          | l == c_err  = "Error"
          | otherwise   = "Fatal error"

-- ------------------------------------------------------------
--
-- the toString conversion functions

-- |
-- old name for 'xshow' (deprecated)

xmlTreesToString        :: XmlTrees -> String
xmlTreesToString        = xshow

-- |
-- conversion of a filter result into a text node
--
-- see also : 'xshow'

xmlTreesToText          :: XmlSFilter
xmlTreesToText ts@[(NTree (XText _) _)] = ts            -- special case optimisation
xmlTreesToText ts@[]                    = ts
xmlTreesToText ts                       = xtext . xshow $ ts

-- ------------------------------------------------------------

xmlContentModelToString :: XmlTree -> String
xmlContentModelToString (NTree (XDTD ELEMENT al) cs)
    = showElemType (lookup1 a_type al) cs ""

xmlContentModelToString _
    = ""

-- -----------------------------------------------------------------------------
--
-- string access functions

-- |
-- select the name of a node. For tags, attributes and pi\'s the name string
-- is returned, else the empty string.

nameOf                          :: XmlTree -> String
nameOf
    = selName . getNode
      where
      selName (XTag  n _)       = qualifiedName n
      selName (XAttr n  )       = qualifiedName n
      selName (XPi   n _)       = qualifiedName n
      selName _                 = ""

-- |
-- select the local part of a name of a node. For tags, attributes the name string
-- is returned, for pi's the whole name, else the empty string.

localPartOf                             :: XmlTree -> String
localPartOf
    = selName . getNode
      where
      selName (XTag  n _)       = localPart n
      selName (XAttr n  )       = localPart n
      selName (XPi   n _)       = qualifiedName n
      selName _                 = ""

-- |
-- select the namespace URI of a tag or an attribute tree, else the empty string is returned
-- see also : 'nameOf'

namespaceOf                             :: XmlTree -> String
namespaceOf
    = selName . getNode
      where
      selName (XTag n _)        = namespaceUri n
      selName (XAttr n )        = namespaceUri n
      selName _                 = ""

-- |
-- select the namespace prefix of a tag or an attribute tree, else the empty string is returned
-- see also : 'nameOf', 'localPartOf'

prefixOf                                :: XmlTree -> String
prefixOf
    = selName . getNode
      where
      selName (XTag n _)        = namePrefix n
      selName (XAttr n )        = namePrefix n
      selName _                 = ""

-- |
-- select the universal name (namespace uri ++ localPart) of a tag or an attribute tree, else the empty string is returned
-- see also : 'nameOf', 'namespaceOf'

universalNameOf                         :: XmlTree -> String
universalNameOf
    = selName . getNode
      where
      selName (XTag n _)        = universalName n
      selName (XAttr n )        = universalName n
      selName _                 = ""

-- |
-- select the attributes of a dtd tree

attrlOfDTD                              :: XmlTree -> Attributes
attrlOfDTD (NTree (XDTD _ al) _)        = al
attrlOfDTD _                            = []


-- |
-- select a special attribute of a DTD part

valueOfDTD              :: String -> XmlTree -> String
valueOfDTD n            = lookup1 n . attrlOfDTD

-- |
-- test an attribute of a DTD part

ofDTDequals     :: String -> String -> XmlTree -> Bool
ofDTDequals n v = (== v) . valueOfDTD n

-- -----------------------------------------------------------------------------
--
-- convenient functions

xcmt            :: String -> XmlTrees
xcmt cmt        = [ mkXCmtTree cmt ]

xerr            :: String -> XmlTrees
xerr msg        = [ mkXErrorTree c_err msg []]

xwarn           :: String -> XmlTrees
xwarn msg       = [ mkXErrorTree c_warn msg []]

xtext           :: String -> XmlTrees
xtext t         = [ mkXTextTree t]

xtag            :: String -> XmlTrees -> XmlTrees -> XmlTrees
xtag t al cl    = [ mkXTagTree t al cl ]

xattr           :: String -> String -> XmlTrees
xattr n v       = [ mkXAttrTree n (xtext v) ]

-- -----------------------------------------------------------------------------
--
-- conversion functions: XmlTrees <-> Attributes

toTreel         :: XmlTrees -> AssocList String XmlTrees
toTreel
    = concatMap toTree
      where
      toTree (NTree (XAttr n) cs) = [(qualifiedName n, cs)]
      toTree _                    = []

toAttrl         :: XmlTrees -> Attributes
toAttrl
    = map (\ (k,tl) -> (k, xshow tl)) . toTreel


fromTreel       :: AssocList String XmlTrees -> XmlTrees
fromTreel
    = map (\ (k,tl) -> mkXAttrTree k tl)

fromAttrl       :: Attributes -> XmlTrees
fromAttrl
    = fromTreel . map (\ (k,v) -> (k, xtext v))

-- -----------------------------------------------------------------------------
