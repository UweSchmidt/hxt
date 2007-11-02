-- |
-- basic Namespace and QName predicates and functions
--
-- $Id: NamespacePredicates.hs,v 1.3 2006/11/12 14:53:00 hxml Exp $
--

module Text.XML.HXT.DOM.NamespacePredicates
    ( module Text.XML.HXT.DOM.NamespacePredicates
    )
where

import Text.XML.HXT.DOM.XmlTreeTypes		-- NTree and XNode types
import Text.XML.HXT.DOM.XmlKeywords		-- keywords for DTD

import Text.XML.HXT.DOM.Unicode
    ( isXmlNCNameStartChar
    , isXmlNCNameChar
    )

-- -----------------------------------------------------------------------------
--

-- |
-- Compute the name prefix and the namespace uri for a qualified name.
--
-- This function does not test whether the name is a wellformed qualified name.
-- see Namespaces in XML Rule [6] to [8]. Error checking is done with separate functions,
-- see 'isWellformedQName' and 'isWellformedQualifiedName' for error checking.

setNamespace	:: NsEnv -> QName -> QName
setNamespace nst n
    = uncurry ns . span (/= ':') $ qn
    where
    qn = qualifiedName n	-- using qualifiedName instead of localPart enables recomputing of setNamespace

    ns :: String -> String -> QName
    ns lp ""					-- no ":" found in name
	= QN { namePrefix   = ""		-- use default namespace uri
	     , localPart    = lp
	     , namespaceUri = lookup1 "" nst
	     }

    ns px@(_:_) (':' : lp@(_:_))		-- none empty prefix and none empty local part found
	= QN { namePrefix   = px
	     , localPart    = lp
	     , namespaceUri = lookup1 px nst
	     }

    ns _ _					-- not a legal qualified name, don't change name
	= n

xmlnsQN	:: QName
xmlnsQN	= QN { namePrefix	= ""
	     , localPart	= a_xmlns
	     , namespaceUri	= xmlnsNamespace
	     }

-- -----------------------------------------------------------------------------
--

-- |
-- test for wellformed NCName, rule [4] XML Namespaces

isNCName	:: String -> Bool
isNCName []
    = False
isNCName n
    = and ( zipWith ($)
	    (isXmlNCNameStartChar : repeat isXmlNCNameChar)
	    n
	  )

-- |
-- test for wellformed QName, rule [6] XML Namespaces
-- predicate is used in filter 'valdateNamespaces'.

isWellformedQualifiedName	:: String -> Bool
isWellformedQualifiedName s
    | null lp
	= isNCName px
    | otherwise
	= isNCName px && isNCName (tail lp)
    where
    (px, lp) = span (/= ':') s

-- |
-- test for wellformed QName values.
-- A QName is wellformed, if the local part is a NCName, the namePrefix, if not empty, is also a NCName.
-- predicate is used in filter 'valdateNamespaces'.

isWellformedQName	:: QName -> Bool
isWellformedQName n
    = isNCName lp				-- rule [8] XML Namespaces
      &&
      ( null px					-- rule [6] XML Namespaces
	||
	isNCName px				-- rule [7] XML Namespaces
      )
    where
    px = namePrefix n
    lp = localPart n

-- |
-- test for a wellformed namespace declaration
-- all namespace prefixes starting with \"xml\" are reserved for XML related definitions.
-- predicate is used in filter 'valdateNamespaces'.

isWellformedNSDecl	:: QName -> Bool
isWellformedNSDecl n
    = null px
      ||
      px /= a_xmlns
      ||
      (take 3 lp) /= a_xml
    where
    lp = localPart  n
    px = namePrefix n

-- |
-- 
-- predicate is used in filter 'valdateNamespaces'.

isDeclaredNamespace	:: QName -> Bool
isDeclaredNamespace n
    | null px					-- no namespace used
	= True
    | px == a_xmlns				-- "xmlns" has a predefined namespace uri
	= ns == xmlnsNamespace
    | px == a_xml				-- "xml" has a predefiend namespace"
	= ns == xmlNamespace
    | otherwise					-- namespace values are not empty
	= (not . null) ns
    where
    px = namePrefix   n
    ns = namespaceUri n

-- -----------------------------------------------------------------------------
