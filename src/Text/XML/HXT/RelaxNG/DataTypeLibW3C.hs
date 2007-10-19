-- |
-- Datatype library for the W3C XML schema datatypes

module Text.XML.HXT.RelaxNG.DataTypeLibW3C
  ( w3cNS
  , w3cDatatypeLib
  , xsd_NCName
  , xsd_anyURI
  , xsd_QName
  , xsd_string
  , xsd_length
  , xsd_maxLength
  , xsd_minLength
  , xsd_pattern
  , xsd_enumeration
  )
where

import Text.XML.HXT.RelaxNG.DataTypeLibUtils  

import Network.URI
  ( isURIReference )

import Text.XML.HXT.DOM.NamespacePredicates
  ( isWellformedQualifiedName
  , isNCName
  )

import Data.Maybe
  
-- ------------------------------------------------------------

-- | Namespace of the W3C XML schema datatype library
w3cNS	:: String
w3cNS	= "http://www.w3.org/2001/XMLSchema-datatypes"


xsd_anyURI
 , xsd_QName
 , xsd_string
 , xsd_normalizedString
 , xsd_token
 , xsd_NMTOKEN
 , xsd_Name
 , xsd_NCName
 , xsd_ID
 , xsd_IDREF
 , xsd_ENTITY :: String

xsd_anyURI		= "anyURI"
xsd_QName		= "QName"
xsd_string		= "string"
xsd_normalizedString	= "normalizedString"
xsd_token		= "token"
xsd_NMTOKEN		= "NMTOKEN"
xsd_Name		= "Name"
xsd_NCName		= "NCName"
xsd_ID			= "ID"
xsd_IDREF		= "IDREF"
xsd_ENTITY		= "ENTITY"

xsd_length
 , xsd_maxLength
 , xsd_minLength
 , xsd_pattern
 , xsd_enumeration :: String

xsd_length	= rng_length
xsd_maxLength	= rng_maxLength
xsd_minLength	= rng_minLength
xsd_pattern	= "pattern"
xsd_enumeration	= "enumeration"


-- | The main entry point to the W3C XML schema datatype library.
--
-- The 'DTC' constructor exports the list of supported datatypes and params.
-- It also exports the specialized functions to validate a XML instance value with
-- respect to a datatype.
w3cDatatypeLib :: DatatypeLibrary
w3cDatatypeLib = (w3cNS, DTC datatypeAllowsW3C datatypeEqualW3C w3cDatatypes)


-- | All supported datatypes of the library
w3cDatatypes :: AllowedDatatypes
w3cDatatypes = [ (xsd_anyURI,	stringParams)
               , (xsd_QName,	stringParams)
               , (xsd_string,	stringParams)
	       , (xsd_normalizedString, stringParams)
               , (xsd_token,	stringParams)
               , (xsd_NMTOKEN,	stringParams)
	       , (xsd_Name,	stringParams)
	       , (xsd_NCName,	stringParams)
	       , (xsd_ID,	stringParams)
	       , (xsd_IDREF,	stringParams)
	       , (xsd_ENTITY,	stringParams)
               ]

-- | List of allowed params for the string datatypes
stringParams :: AllowedParams
stringParams = [ xsd_length
	       , xsd_maxLength
	       , xsd_minLength
	       ]

-- | Tests whether a XML instance value matches a data-pattern.
-- (see also: 'checkString')
datatypeAllowsW3C :: DatatypeAllows
datatypeAllowsW3C d params value _
    | d == xsd_string
	= checkString d value 0 (-1) params

    | d == xsd_normalizedString
	= checkString d value3 0 (-1) params

    | d == xsd_token
	= checkString d value1 0 (-1) params

    | d == xsd_NMTOKEN
	= if isNmtoken value1
	  then checkString d value1 0 (-1) params
	  else err1

    | d == xsd_Name
	= if isName value1
	  then checkString d value1 0 (-1) params
	  else err1

    | d `elem` [ xsd_NCName
	       , xsd_ID
	       , xsd_IDREF
	       , xsd_ENTITY
	       ]
	= if isNCName value1
          then checkString d value 0 (-1) params 
	  else err1

    | d == xsd_anyURI
	= if isURIReference value2
          then checkString d value 0 (-1) params
	  else err1

    | d == xsd_QName
	= if isWellformedQualifiedName value1 && not (null value1)
          then checkString d value 0 (-1) params
	  else err1

    | otherwise
	= err2
    where
    value1 = normalizeWhitespace value
    value2 = escapeURI value1
    value3 = normalizeBlanks value
    err1   = Just $ errorMsgDataLibQName value d w3cNS
    err2   = Just $ errorMsgDataTypeNotAllowed d params value w3cNS

-- | Tests whether a XML instance value matches a value-pattern.
datatypeEqualW3C :: DatatypeEqual
datatypeEqualW3C d s1 _ s2 _
    | isJust nf
	= check (fromJust nf)
    | otherwise
	= Just $ errorMsgDataTypeNotAllowed0 d w3cNS
    where
    check f
	| s1' == s2' = Nothing
	| otherwise  = Just $ errorMsgEqual d s1' s2'
	where
	s1' = f s1
	s2' = f s2
    nf = lookup d norm
    norm = [ (xsd_string,		id			)
	   , (xsd_normalizedString,	normalizeBlanks		)
	   , (xsd_token,		normalizeWhitespace	)
	   , (xsd_NMTOKEN,		normalizeWhitespace	)
	   , (xsd_Name,			normalizeWhitespace	)
	   , (xsd_NCName,		normalizeWhitespace	)
	   , (xsd_ID,			normalizeWhitespace	)
	   , (xsd_IDREF,		normalizeWhitespace	)
	   , (xsd_ENTITY,		normalizeWhitespace	)
	   , (xsd_anyURI,		escapeURI . normalizeWhitespace	)
	   , (xsd_QName,		normalizeWhitespace	)
	   ]