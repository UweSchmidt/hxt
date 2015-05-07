{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DOM.TypeDefs
   Copyright  : Copyright (C) 2008-2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   The core data types of the HXT DOM.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DOM.TypeDefs
    ( module Data.AssocList
    , module Text.XML.HXT.DOM.TypeDefs
    , module Text.XML.HXT.DOM.QualifiedName
    )

where

import           Control.DeepSeq
import           Control.FlatSeq

import           Data.AssocList

import           Data.Binary
import qualified Data.ByteString.Lazy            as BS
import qualified Data.ByteString.Lazy.Char8      as CS

import           Data.Tree.NTree.TypeDefs
import           Data.Tree.NTree.Zipper.TypeDefs

import           Data.Typeable

import           Text.XML.HXT.DOM.QualifiedName

-- -----------------------------------------------------------------------------
--
-- Basic types for xml tree and filters

-- | Rose tree with XML nodes (XNode)

type XmlTree    = NTree    XNode

-- | List of rose trees with XML nodes

type XmlTrees   = NTrees   XNode

-- | Navigatable rose tree with XML nodes

type XmlNavTree = NTZipper XNode

-- | List of navigatable rose trees with XML nodes

type XmlNavTrees = [NTZipper XNode]

-- -----------------------------------------------------------------------------
--
-- XNode

-- | Represents elements

data XNode      = XText           String                        -- ^ ordinary text                                       (leaf)
                | XBlob           Blob                          -- ^ text represented more space efficient as bytestring (leaf)
                | XCharRef        Int                           -- ^ character reference                                 (leaf)
                | XEntityRef      String                        -- ^ entity reference                                    (leaf)
                | XCmt            String                        -- ^ comment                                             (leaf)
                | XCdata          String                        -- ^ CDATA section                                       (leaf)
                | XPi             QName XmlTrees                -- ^ Processing Instr with qualified name                (leaf)
                                                                --   with list of attributes.
                                                                --   If tag name is xml, attributes are \"version\", \"encoding\", \"standalone\",
                                                                --   else attribute list is empty, content is a text child node
                | XTag            QName XmlTrees                -- ^ tag with qualified name and list of attributes (inner node or leaf)
                | XDTD            DTDElem  Attributes           -- ^ DTD element with assoc list for dtd element features
                | XAttr           QName                         -- ^ attribute with qualified name, the attribute value is stored in children
                | XError          Int  String                   -- ^ error message with level and text
                  deriving (Eq, Show, Typeable)

instance NFData XNode where
    rnf (XText s)               = rnf s
    rnf (XTag qn cs)            = rnf qn `seq` rnf cs
    rnf (XAttr qn)              = rnf qn
    rnf (XCharRef i)            = rnf i
    rnf (XEntityRef n)          = rnf n
    rnf (XCmt c)                = rnf c
    rnf (XCdata s)              = rnf s
    rnf (XPi qn ts)             = rnf qn `seq` rnf ts
    rnf (XDTD de al)            = rnf de `seq` rnf al
    rnf (XBlob b)               = BS.length b `seq` ()
    rnf (XError n e)            = rnf n  `seq` rnf e

instance WNFData XNode where
    rwnf (XText s)              = rwnf s
    rwnf (XTag qn cs)           = rwnf qn `seq` rwnf cs
    rwnf (XAttr qn)             = rwnf qn
    rwnf (XCharRef i)           = i `seq` ()
    rwnf (XEntityRef n)         = rwnf n
    rwnf (XCmt c)               = rwnf c
    rwnf (XCdata s)             = rwnf s
    rwnf (XPi qn ts)            = rwnf qn `seq` rwnf ts
    rwnf (XDTD de al)           = rwnf de `seq` rwnfAttributes al
    rwnf (XBlob _b)             = () -- BS.length b `seq` () -- lazy bytestrings are not evaluated
    rwnf (XError n e)           = n `seq` rwnf e

-- | Evaluate an assoc list of strings
rwnfAttributes                  :: Attributes -> ()
rwnfAttributes [] = ()
rwnfAttributes ((k, v) : as)    = rwnf k `seq` rwnf v `seq` rwnfAttributes as


instance Binary XNode where
    put (XText s)               = put ( 0::Word8) >> put s
    put (XTag qn cs)            = put ( 6::Word8) >> put qn >> put cs
    put (XAttr qn)              = put ( 8::Word8) >> put qn
    put (XCharRef i)            = put ( 1::Word8) >> put i
    put (XEntityRef n)          = put ( 2::Word8) >> put n
    put (XCmt c)                = put ( 3::Word8) >> put c
    put (XCdata s)              = put ( 4::Word8) >> put s
    put (XPi qn ts)             = put ( 5::Word8) >> put qn >> put ts
    put (XDTD de al)            = put ( 7::Word8) >> put de >> put al
    put (XError n e)            = put ( 9::Word8) >> put n  >> put e
    put (XBlob b)               = put (10::Word8) >> put b

    get                         = do
                                  tag <- getWord8
                                  case tag of
                                    0  -> get >>= return . XText
                                    1  -> get >>= return . XCharRef
                                    2  -> get >>= return . XEntityRef
                                    3  -> get >>= return . XCmt
                                    4  -> get >>= return . XCdata
                                    5  -> do
                                          qn <- get
                                          get >>= return . XPi qn
                                    6  -> do
                                          qn <- get
                                          get >>= return . XTag qn
                                    7  -> do
                                          de <- get
                                          get >>= return . XDTD de
                                    8  -> get >>= return . XAttr
                                    9  -> do
                                          n <- get
                                          get >>= return . XError n
                                    10 -> get >>= return . XBlob
                                    _  -> error "XNode.get: error while decoding XNode"

-- -----------------------------------------------------------------------------
--
-- DTDElem

-- | Represents a DTD element

data DTDElem    = DOCTYPE       -- ^ attr: name, system, public,        XDTD elems as children
                | ELEMENT       -- ^ attr: name, kind
                                --
                                --  name: element name
                                --
                                --  kind: \"EMPTY\" | \"ANY\" | \"\#PCDATA\" | children | mixed
                | CONTENT       -- ^ element content
                                --
                                --  attr: kind, modifier
                                --
                                --  modifier: \"\" | \"?\" | \"*\" | \"+\"
                                --
                                --  kind: seq | choice
                | ATTLIST       -- ^ attributes:
                                --  name - name of element
                                --
                                --  value - name of attribute
                                --
                                --  type: \"CDATA\" | \"ID\" | \"IDREF\" | \"IDREFS\" | \"ENTITY\" | \"ENTITIES\" |
                                --
                                --        \"NMTOKEN\" | \"NMTOKENS\" |\"NOTATION\" | \"ENUMTYPE\"
                                --
                                --  kind: \"#REQUIRED\" | \"#IMPLIED\" | \"DEFAULT\"
                | ENTITY        -- ^ for entity declarations
                | PENTITY       -- ^ for parameter entity declarations
                | NOTATION      -- ^ for notations
                | CONDSECT      -- ^ for INCLUDEs, IGNOREs and peRefs: attr: type
                                --
                                --  type = INCLUDE, IGNORE or %...;
                | NAME          -- ^ attr: name
                                --
                                --  for lists of names in notation types or nmtokens in enumeration types
                | PEREF         -- ^ for Parameter Entity References in DTDs
                  deriving (Eq, Ord, Enum, Show, Read, Typeable)

instance NFData DTDElem
    where rnf x = seq x ()

instance WNFData DTDElem

instance Binary DTDElem where
    put de = put ((toEnum . fromEnum $ de)::Word8)         -- DTDElem is not yet instance of Enum
    get    = do tag <- getWord8
                return $! (toEnum . fromEnum $ tag)

-- -----------------------------------------------------------------------------

-- | Binary large object implemented as a lazy bytestring

type Blob       = BS.ByteString

blobToString    :: Blob -> String
blobToString    = CS.unpack
{-# INLINE blobToString #-}

stringToBlob    :: String -> Blob
stringToBlob    = CS.pack
{-# INLINE stringToBlob #-}

-- -----------------------------------------------------------------------------

-- | Attribute list
--
-- used for storing option lists and features of DTD parts

type Attributes = AssocList String String

-- -----------------------------------------------------------------------------
--
-- Constants for error levels

-- | no error, everything is ok
c_ok    :: Int
c_ok    = 0

-- | Error level for XError, type warning
c_warn  :: Int
c_warn  = c_ok + 1

-- | Error level for XError, type error
c_err   :: Int
c_err   = c_warn + 1

-- | Error level for XError, type fatal error
c_fatal :: Int
c_fatal = c_err + 1

-- -----------------------------------------------------------------------------

-- | data type for representing a set of nodes as a tree structure
--
-- this structure is e.g. used to repesent the result of an XPath query
-- such that the selected nodes can be processed or selected later in
-- processing a document tree

data XmlNodeSet = XNS { thisNode   :: Bool         -- ^ is this node part of the set ?
                      , attrNodes  :: [QName]      -- ^ the set of attribute nodes
                      , childNodes :: ChildNodes   -- ^ the set of child nodes, a list of pairs of index and node set
                      }
                  deriving (Eq, Show, Typeable)

type ChildNodes = [(Int, XmlNodeSet)]

-- -----------------------------------------------------------------------------
