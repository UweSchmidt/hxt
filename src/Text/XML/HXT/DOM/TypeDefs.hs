-- |
-- The core data types of HDOM.
--
-- Version : $Id: TypeDefs.hs,v 1.13 2006/11/09 20:27:42 hxml Exp $

module Text.XML.HXT.DOM.TypeDefs
    ( module Data.AssocList
    , module Text.XML.HXT.DOM.TypeDefs
    )

where

import Data.AssocList
import Data.Char		(toLower)
import Data.Tree.NTree.TypeDefs
import Data.Typeable

-- -----------------------------------------------------------------------------
--
-- Basic types for xml tree and filters

-- | Node of xml tree representation

type XmlTree	= NTree    XNode

-- | List of nodes of xml tree representation

type XmlTrees	= NTrees   XNode

-- -----------------------------------------------------------------------------
--
-- XNode

-- | Represents elements

data XNode	= XText		  String			-- ^ ordinary text				(leaf)
		| XCharRef	  Int				-- ^ character reference			(leaf)
		| XEntityRef	  String			-- ^ entity reference				(leaf)
		| XCmt		  String			-- ^ comment					(leaf)
		| XCdata	  String			-- ^ CDATA section				(leaf)
		| XPi		  QName XmlTrees		-- ^ Processing Instr with qualified name	(leaf)
								--   with list of attributes.
								--   If tag name is xml, attributs are \"version\", \"encoding\", \"standalone\",
								--   else attribute list is empty, content is a text child node
		| XTag		  QName XmlTrees		-- ^ tag with qualified name and list of attributes (inner node or leaf)
		| XDTD		  DTDElem  Attributes		-- ^ DTD element with assoc list for dtd element features
		| XAttr		  QName				-- ^ attribute with qualified name, the attribute value is stored in children
		| XError	  Int  String			-- ^ error message with level and text
		  deriving (Eq, Ord, Show, Read, Typeable)

-- -----------------------------------------------------------------------------
--
-- DTDElem

-- | Represents a DTD element

data DTDElem	= DOCTYPE	-- ^ attr: name, system, public,	XDTD elems as children
		| ELEMENT	-- ^ attr: name, kind
		                --
				--  name: element name
		                --
				--  kind: \"EMPTY\" | \"ANY\" | \"\#PCDATA\" | children | mixed
		| CONTENT	-- ^ element content
		                --
				--  attr: kind, modifier
		                --
				--  modifier: \"\" | \"?\" | \"*\" | \"+\"
		                --
				--  kind: seq | choice
		| ATTLIST	-- ^ attributes:
		                --  name - name of element
		                --
				--  value - name of attribute
		                --
				--  type: \"CDATA\" | \"ID\" | \"IDREF\" | \"IDREFS\" | \"ENTITY\" | \"ENTITIES\" |
		                --
				--        \"NMTOKEN\" | \"NMTOKENS\" |\"NOTATION\" | \"ENUMTYPE\"
		                --
				--  kind: \"#REQUIRED\" | \"#IMPLIED\" | \"DEFAULT\"
		| ENTITY	-- ^ for entity declarations
		| PENTITY	-- ^ for parameter entity declarations
		| NOTATION	-- ^ for notations
		| CONDSECT	-- ^ for INCLUDEs, IGNOREs and peRefs: attr: type
		                --
				--  type = INCLUDE, IGNORE or %...;
		| NAME		-- ^ attr: name
		                --
				--  for lists of names in notation types or nmtokens in enumeration types
		| PEREF		-- ^ for Parameter Entity References in DTDs
		  deriving (Eq, Ord, Show, Read)

-- -----------------------------------------------------------------------------

-- | Attribute list
--
-- used for storing option lists and features of DTD parts

type Attributes	= AssocList String String

-- -----------------------------------------------------------------------------
--
-- |
-- Namespace support for element and attribute names.
--
-- A qualified name consists of a name prefix, a local name
-- and a namespace uri.
-- All modules, which are not namespace aware, use only the 'localPart' component.
-- When dealing with namespaces, the document tree must be processed by "propagateNamespaces"
-- to split names of structure \"prefix:localPart\" and label the name with the apropriate namespace uri

data QName = QN { namePrefix	:: String	-- ^ the name prefix part of a qualified name \"namePrefix:localPart\"
		, localPart	:: String	-- ^ the local part of a qualified name \"namePrefix:localPart\"
		, namespaceUri	:: String	-- ^ the associated namespace uri
		}
	     deriving (Eq, Ord, Show, Read)

-- |
-- builds the full name \"prefix:localPart\", if prefix is not null, else the local part is the result

qualifiedName		:: QName -> String
qualifiedName n
    | null px
	= lp
    | otherwise
	= px ++ (':' : lp)
    where
    px = namePrefix n
    lp = localPart  n

-- |
-- builds the \"universal\" name, that is the namespace uri surrounded with \"{\" and \"}\" followed by the local part
-- (specialisation of 'buildUniversalName')

universalName	:: QName -> String
universalName	= buildUniversalName (\ ns lp -> '{' : (ns ++ ('}' : lp)))

-- |
-- builds an \"universal\" uri, that is the namespace uri followed by the local part. This is usefull for RDF applications,
-- where the subject, predicate and object often are concatenated from namespace uri and local part
-- (specialisation of 'buildUniversalName')

universalUri	:: QName -> String
universalUri	= buildUniversalName (++)

-- |
-- builds a string from the namespace uri and the local part. If the namespace uri is empty, the local part is returned, else
-- namespace uri and local part are combined with the combining function given by the first parameter

buildUniversalName	:: (String -> String -> String) -> QName -> String
buildUniversalName bf n
    | null ns
	= lp
    | otherwise
	= bf ns lp
    where
    ns = namespaceUri n
    lp = localPart    n

-- |
-- constructs a simple, namespace unaware name, 'namePrefix' and 'namespaceUri' are set to the empty string.

mkName	:: String -> QName
mkName s
    = QN { namePrefix	= ""
	 , localPart	= s
	 , namespaceUri	= ""
	 }

-- |
-- constructs a simple name, with prefix and localPart but without a namespace uri.
--
-- see also 'mkName', 'mkNsName', 'mkSNsName'

mkPrefixLocalPart	:: String -> String -> QName
mkPrefixLocalPart p l
    = QN { namePrefix	= p
	 , localPart	= l
	 , namespaceUri	= ""
	 }

-- |
-- constructs a simple, namespace aware name, with prefix:localPart as first parameter, namspace uri as second.
--
-- see also 'mkName', 'mkPrefixLocalPart'

mkNsName	:: String -> String -> QName
mkNsName n ns
    = QN { namePrefix	= p
	 , localPart	= l
	 , namespaceUri	= ns
	 }
      where
      (x1, x2) = span (/= ':') n
      (p, l)
	  | null x2	= ("", x1)
	  | otherwise	= (x1, tail x2)


-- |
-- constructs a simple name, with prefix:localPart as 1 parameter, with empty namspace uri, same as 'mkPrefixLocalPart, but with a single parameter
--
-- see also 'mkNsName', 'mkPrefixLocalPart'

mkSNsName	:: String -> QName
mkSNsName n	= mkNsName n ""

-- | Empty QName

nullQName	:: QName
nullQName	= QN "" "" ""

-- | Equality of QNames: Two QNames are equal, if the local parts are equal
-- and the namespace URIs are equal.
-- The comparison works with and without namespace propagation.
-- If namespaces have been propagated, the name is split into prefix and local part
-- and the namespace uri is set. In this case the prefix is not significant for equality test.
-- If namespaces have not been propagated, the local part contains the full name, prefix
-- and namespace URI are empty. The full name (prefix and local part) is used for comparison.

equalQName	:: QName -> QName -> Bool
equalQName 	= equalQNameBy (==)

-- | Equivalent QNames are defined as follows: The URIs are normalized before comparison.
-- Comparison is done with 'equalQNameBy' and 'equivUri'

equivQName	:: QName -> QName -> Bool
equivQName	= equalQNameBy equivUri

-- | Comparison of normalized namespace URIs using 'normalizeNsUri'

equivUri	:: String -> String -> Bool
equivUri x y	= normalizeNsUri x == normalizeNsUri y

-- | Sometimes a weaker equality relation than 'equalQName' is appropriate, e.g no case significance in names, ...
-- a name normalization function can be applied to the strings before comparing. Called by 'equalQName' and
-- 'equivQName'

equalQNameBy	:: (String -> String -> Bool) -> QName -> QName -> Bool
equalQNameBy equiv q1 q2
    = localPart q1 == localPart q2
      &&
      (namespaceUri q1 `equiv` namespaceUri q2)

-- |  Normalization of URIs: Normalization is done by conversion into lowercase letters. A trailing \"\/\" is ignored

normalizeNsUri	:: String -> String
normalizeNsUri
    = map toLower . stripSlash
    where
    stripSlash ""	= ""
    stripSlash s
	| last s == '/'	= init s
	| otherwise	= s

-- -----------------------------------------------------------------------------
--

-- |
-- Type for the namespace association list, used when propagating namespaces by
-- modifying the 'QName' values in a tree

type NsEnv = AssocList String String

-- -----------------------------------------------------------------------------
--
-- Constants for error levels

-- | no error, everything is ok
c_ok	:: Int
c_ok	= 0

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

data XmlNodeSet	= XNS { thisNode	:: Bool		-- ^ is this node part of the set ?
		      , attrNodes	:: [QName]	-- ^ the set of attribute nodes
		      , childNodes	:: ChildNodes	-- ^ the set of child nodes, a list of pairs of index and node set 
		      }
		  deriving (Show)

type ChildNodes	= [(Int, XmlNodeSet)]

-- -----------------------------------------------------------------------------
