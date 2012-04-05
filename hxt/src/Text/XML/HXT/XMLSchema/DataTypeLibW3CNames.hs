-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XMLSchema.DataTypeLibW3C
   Copyright  : Copyright (C) 2005-2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Datatype library for the W3C XML schema datatypes

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XMLSchema.DataTypeLibW3CNames
where

-- ------------------------------------------------------------

-- | Namespace of the W3C XML schema datatype library

w3cNS   :: String
w3cNS   = "http://www.w3.org/2001/XMLSchema-datatypes"

xsd_string
 , xsd_normalizedString
 , xsd_token
 , xsd_language
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
 , xsd_decimal
 , xsd_integer
 , xsd_nonPositiveInteger
 , xsd_negativeInteger
 , xsd_nonNegativeInteger
 , xsd_positiveInteger
 , xsd_long
 , xsd_int
 , xsd_short
 , xsd_byte
 , xsd_unsignedLong
 , xsd_unsignedInt
 , xsd_unsignedShort
 , xsd_unsignedByte

 , xsd_boolean
 , xsd_float
 , xsd_double
 , xsd_time
 , xsd_duration
 , xsd_date
 , xsd_dateTime
 , xsd_gDay
 , xsd_gMonth
 , xsd_gMonthDay
 , xsd_gYear
 , xsd_gYearMonth :: String

xsd_string              = "string"
xsd_normalizedString    = "normalizedString"
xsd_token               = "token"
xsd_language            = "language"
xsd_NMTOKEN             = "NMTOKEN"
xsd_NMTOKENS            = "NMTOKENS"
xsd_Name                = "Name"
xsd_NCName              = "NCName"
xsd_ID                  = "ID"
xsd_IDREF               = "IDREF"
xsd_IDREFS              = "IDREFS"
xsd_ENTITY              = "ENTITY"
xsd_ENTITIES            = "ENTITIES"
xsd_anyURI              = "anyURI"
xsd_QName               = "QName"
xsd_NOTATION            = "NOTATION"
xsd_hexBinary           = "hexBinary"
xsd_base64Binary        = "base64Binary"
xsd_decimal             = "decimal"
xsd_integer             = "integer"
xsd_nonPositiveInteger  = "nonPositiveInteger"
xsd_negativeInteger     = "negativeInteger"
xsd_nonNegativeInteger  = "nonNegativeInteger"
xsd_positiveInteger     = "positiveInteger"
xsd_long                = "long"
xsd_int                 = "int"
xsd_short               = "short"
xsd_byte                = "byte"
xsd_unsignedLong        = "unsignedLong"
xsd_unsignedInt         = "unsignedInt"
xsd_unsignedShort       = "unsignedShort"
xsd_unsignedByte        = "unsignedByte"

xsd_boolean             = "boolean"
xsd_float               = "float"
xsd_double              = "double"
xsd_time                = "time"
xsd_duration            = "duration"
xsd_date                = "date"
xsd_dateTime            = "dateTime"
xsd_gDay                = "gDay"
xsd_gMonth              = "gMonth"
xsd_gMonthDay           = "gMonthDay"
xsd_gYear               = "gYear"
xsd_gYearMonth          = "gYearMonth"

xsd_length
 , xsd_maxLength
 , xsd_minLength
 , xsd_maxExclusive
 , xsd_minExclusive
 , xsd_maxInclusive
 , xsd_minInclusive
 , xsd_totalDigits
 , xsd_fractionDigits
 , xsd_pattern
 , xsd_enumeration
 , xsd_whiteSpace :: String

xsd_length              = "length"
xsd_maxLength           = "maxLength"
xsd_minLength           = "minLength"

xsd_maxExclusive        = "maxExclusive"
xsd_minExclusive        = "minExclusive"
xsd_maxInclusive        = "maxInclusive"
xsd_minInclusive        = "minInclusive"

xsd_totalDigits         = "totalDigits"
xsd_fractionDigits      = "fractionDigits"

xsd_pattern             = "pattern"
xsd_enumeration         = "enumeration"

xsd_whiteSpace          = "whiteSpace"

-- ----------------------------------------
