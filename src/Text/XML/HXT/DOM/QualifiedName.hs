-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DOM.QualifiedName
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   The core data types of the HXT DOM.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DOM.QualifiedName
    ( module Text.XML.HXT.DOM.QualifiedName
    )

where
import Control.Strategies.DeepSeq

import Data.Char		(toLower)
import Data.Typeable

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
	     deriving (Ord, Show, Read, Typeable)

-- | Two QNames are equal if (1. case) namespaces are both empty and the qualified names
-- (prefix:localpart) are the same or (2. case) namespaces are set and namespaces and
-- local parts both are equal
    
instance Eq QName where
    q1 == q2
	| null ns1 && null ns2
	    = qualifiedName q1 == qualifiedName q2
	| otherwise
	    = localPart q1 == localPart q2
	      &&
	      ns1 == ns2
	where
	ns1 = namespaceUri q1
	ns2 = namespaceUri q2

instance DeepSeq QName where
    deepSeq (QN np lp ns) y	= deepSeq np $ deepSeq lp $ deepSeq ns y

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
universalName	= buildUniversalName (\ ns lp -> '{' : ns ++ '}' : lp)

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
-- constructs a complete qualified name with 'namePrefix', 'localPart' and 'namespaceUri'.

mkQName	:: String -> String -> String -> QName
mkQName p l n
    = QN { namePrefix	= p
	 , localPart	= l
	 , namespaceUri	= n
	 }

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
