-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG.XmlSchema.DataTypeLibW3C
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Datatype library for the W3C XML schema datatypes

-}

-- ------------------------------------------------------------

module Text.XML.HXT.RelaxNG.XmlSchema.DataTypeLibW3C
  ( w3cNS
  , w3cDatatypeLib

  , xsd_string			-- data type names
  , xsd_normalizedString
  , xsd_token
  , xsd_NMTOKEN
  , xsd_NMTOKENS
  , xsd_Name
  , xsd_NCName
  , xsd_ID
  , xsd_IDREF
  , xsd_IDREFS
  , xsd_ENTITY
  , xsd_ENTITIES
  , xsd_anyURI
  , xsd_QName
  , xsd_NOTATION
  , xsd_hexBinary
  , xsd_base64Binary

  , xsd_length			-- facet names
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

import Text.XML.HXT.RelaxNG.XmlSchema.Regex
    ( Regex
    , match
    )
import Text.XML.HXT.RelaxNG.XmlSchema.RegexParser
    ( parseRegex )

import Data.Maybe
  
-- ------------------------------------------------------------

-- | Namespace of the W3C XML schema datatype library

w3cNS	:: String
w3cNS	= "http://www.w3.org/2001/XMLSchema-datatypes"


xsd_string
 , xsd_normalizedString
 , xsd_token
 , xsd_NMTOKEN
 , xsd_NMTOKENS
 , xsd_Name
 , xsd_NCName
 , xsd_ID
 , xsd_IDREF
 , xsd_IDREFS
 , xsd_ENTITY
 , xsd_ENTITIES
 , xsd_anyURI
 , xsd_QName
 , xsd_NOTATION
 , xsd_hexBinary
 , xsd_base64Binary :: String

xsd_string		= "string"
xsd_normalizedString	= "normalizedString"
xsd_token		= "token"
xsd_NMTOKEN		= "NMTOKEN"
xsd_NMTOKENS		= "NMTOKENS"
xsd_Name		= "Name"
xsd_NCName		= "NCName"
xsd_ID			= "ID"
xsd_IDREF		= "IDREF"
xsd_IDREFS		= "IDREFS"
xsd_ENTITY		= "ENTITY"
xsd_ENTITIES		= "ENTITIES"
xsd_anyURI		= "anyURI"
xsd_QName		= "QName"
xsd_NOTATION		= "NOTATION"
xsd_hexBinary		= "hexBinary"
xsd_base64Binary	= "base64Binary"

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
w3cDatatypes = [ (xsd_string,		stringParams)
	       , (xsd_normalizedString, stringParams)
               , (xsd_token,		stringParams)
               , (xsd_NMTOKEN,		stringParams)
               , (xsd_NMTOKENS,		listParams  )
	       , (xsd_Name,		stringParams)
	       , (xsd_NCName,		stringParams)
	       , (xsd_ID,		stringParams)
	       , (xsd_IDREF,		stringParams)
	       , (xsd_IDREFS,		listParams  )
	       , (xsd_ENTITY,		stringParams)
	       , (xsd_ENTITIES,		listParams  )
               , (xsd_anyURI,		stringParams)
               , (xsd_QName,		stringParams)
               , (xsd_NOTATION,		stringParams)
	       , (xsd_hexBinary,	stringParams)
	       , (xsd_base64Binary,	stringParams)
               ]

-- | List of allowed params for the string datatypes
stringParams	:: AllowedParams
stringParams	= map fst $ fctTableString ++ fctTablePattern

-- | List of allowed params for the list datatypes
listParams	:: AllowedParams
listParams	= map fst $ fctTableList ++ fctTablePattern

-- | Function table for pattern tests,
-- XML document value is first operand, schema value second

fctTablePattern :: FunctionTable
fctTablePattern
    = [ (xsd_pattern,	patParamValid) ]

patParamValid :: String -> String -> Bool
patParamValid a regex
    = case parseRegex regex of
      (Left _  )	-> False
      (Right ex)	-> isNothing . match ex $ a

isNameList	:: (String -> Bool) -> String -> Bool
isNameList p w
    = not (null ts) && all p ts
      where
      ts = words w

rexHexBinary	:: Regex
rexHexBinary	= either undefined id . parseRegex $ "([A-Fa-f0-9]{2})*"

rexBase64Binary	:: Regex
rexBase64Binary	= either undefined id . parseRegex $
		  "(" ++ b64 ++ "{4})*((" ++ b64 ++ "{2}==)|(" ++ b64 ++ "{3}=)|)"
		  where
		  b64     = "[A-Za-z0-9+/]"

isHexBinary	:: String -> Bool
isHexBinary	= isNothing . match rexHexBinary

isBase64Binary	:: String -> Bool
isBase64Binary	= isNothing . match rexBase64Binary

normBase64	:: String -> String
normBase64	= filter isB64
		  where
		  isB64 c = ( 'A' <= c && c <= 'Z')
			    ||
			    ( 'a' <= c && c <= 'z')
			    ||
			    ( '0' <= c && c <= '9')
			    ||
			    c == '+'
			    ||
			    c == '/'
			    ||
			    c == '='

-- | Tests whether a XML instance value matches a data-pattern.
-- (see also: 'stringValid')

datatypeAllowsW3C :: DatatypeAllows
datatypeAllowsW3C d params value _
    = performCheck check value
    where
    validString normFct
	= validPattern
	  >>>
	  arr normFct
	  >>>
	  validLength

    validNormString
	= validString normalizeWhitespace

    validPattern
	= stringValidFT fctTablePattern  d 0 (-1) (filter ((== xsd_pattern) . fst) params)

    validLength
	= stringValidFT fctTableString  d 0 (-1) (filter ((/= xsd_pattern) . fst) params)

    validList
	= validPattern
	  >>>
	  arr normalizeWhitespace
	  >>>
	  validListLength

    validListLength
	= stringValidFT fctTableList  d 0 (-1) (filter ((/= xsd_pattern) . fst) params)

    validName isN
	= assert isN errW3C

    validNCName
	= validNormString >>> validName isNCName

    validQName
	= validNormString >>> validName isWellformedQualifiedName

    check	:: CheckString
    check	= fromMaybe notFound . lookup d $ checks

    notFound	= failure $ errorMsgDataTypeNotAllowed w3cNS d params

    checks	:: [(String, CheckA String String)]
    checks	= [ (xsd_string,		validString id)
		  , (xsd_normalizedString,	validString normalizeBlanks)
		  , (xsd_token,			validNormString)
		  , (xsd_NMTOKEN,		validNormString >>> validName isNmtoken)
		  , (xsd_NMTOKENS,		validList       >>> validName (isNameList isNmtoken))
		  , (xsd_Name,			validNormString >>> validName isName)
		  , (xsd_NCName,		validNCName)
		  , (xsd_ID,			validNCName)
		  , (xsd_IDREF,			validNCName)
		  , (xsd_IDREFS,		validList       >>> validName (isNameList isNCName))
		  , (xsd_ENTITY,		validNCName)
		  , (xsd_ENTITIES,		validList       >>> validName (isNameList isNCName))
		  , (xsd_anyURI,		validName isURIReference >>> validString escapeURI)
		  , (xsd_QName,			validQName)
		  , (xsd_NOTATION,		validQName)
		  , (xsd_hexBinary,		validString id  >>> assert isHexBinary errW3C)
		  , (xsd_base64Binary,		validString normBase64 >>> assert isBase64Binary errW3C)
		  ]
    errW3C	= errorMsgDataLibQName w3cNS d

-- ----------------------------------------

-- | Tests whether a XML instance value matches a value-pattern.

datatypeEqualW3C :: DatatypeEqual
datatypeEqualW3C d s1 _ s2 _
    = performCheck check (s1, s2)
    where
    check	:: CheckA (String, String) (String, String)
    check	= maybe notFound found . lookup d $ norm

    notFound	= failure $ const (errorMsgDataTypeNotAllowed0 w3cNS d)

    found nf	= arr (\ (x1, x2) -> (nf x1, nf x2))			-- normalize both values
		  >>>
		  assert (uncurry (==)) (uncurry $ errorMsgEqual d)	-- and check on (==)

    norm = [ (xsd_string,		id			)
	   , (xsd_normalizedString,	normalizeBlanks		)
	   , (xsd_token,		normalizeWhitespace	)
	   , (xsd_NMTOKEN,		normalizeWhitespace	)
	   , (xsd_NMTOKENS,		normalizeWhitespace	)
	   , (xsd_Name,			normalizeWhitespace	)
	   , (xsd_NCName,		normalizeWhitespace	)
	   , (xsd_ID,			normalizeWhitespace	)
	   , (xsd_IDREF,		normalizeWhitespace	)
	   , (xsd_IDREFS,		normalizeWhitespace	)
	   , (xsd_ENTITY,		normalizeWhitespace	)
	   , (xsd_ENTITIES,		normalizeWhitespace	)
	   , (xsd_anyURI,		escapeURI . normalizeWhitespace	)
	   , (xsd_QName,		normalizeWhitespace	)
	   , (xsd_NOTATION,		normalizeWhitespace	)
	   , (xsd_hexBinary,		id			)
	   , (xsd_base64Binary,		normBase64		)
	   ]

-- ----------------------------------------
