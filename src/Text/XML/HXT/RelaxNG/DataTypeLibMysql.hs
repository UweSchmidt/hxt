-- |
-- Datatype library for the MySQL datatypes
--
-- $Id: DataTypeLibMysql.hs,v 1.1 2005/09/02 17:09:39 hxml Exp $

module Text.XML.HXT.RelaxNG.DataTypeLibMysql
  ( mysqlNS
  , mysqlDatatypeLib
  )
where

import Text.XML.HXT.RelaxNG.DataTypeLibUtils  

import Data.Maybe

-- ------------------------------------------------------------

-- | Namespace of the MySQL datatype library

mysqlNS :: String
mysqlNS = "http://www.mysql.com"


-- | The main entry point to the MySQL datatype library.
--
-- The 'DTC' constructor exports the list of supported datatypes and params.
-- It also exports the specialized functions to validate a XML instance value with
-- respect to a datatype.

mysqlDatatypeLib :: DatatypeLibrary
mysqlDatatypeLib = (mysqlNS, DTC datatypeAllowsMysql datatypeEqualMysql mysqlDatatypes)


-- | All supported datatypes of the library
mysqlDatatypes :: AllowedDatatypes
mysqlDatatypes = [ -- numeric types
                   ("SIGNED-TINYINT", numericParams)
                 , ("UNSIGNED-TINYINT", numericParams)
                 , ("SIGNED-SMALLINT", numericParams)
                 , ("UNSIGNED-SMALLINT", numericParams)
                 , ("SIGNED-MEDIUMINT", numericParams)
                 , ("UNSIGNED-MEDIUMINT", numericParams)
                 , ("SIGNED-INT", numericParams)
                 , ("UNSIGNED-INT", numericParams)
                 , ("SIGNED-BIGINT", numericParams)
                 , ("UNSIGNED-BIGINT", numericParams)
                 
                 -- string types
                 , ("CHAR", stringParams)
                 , ("VARCHAR", stringParams)                 
                 , ("BINARY", stringParams)
                 , ("VARBINARY", stringParams)                 
                 , ("TINYTEXT", stringParams)
                 , ("TINYBLOB", stringParams)                 
                 , ("TEXT", stringParams)
                 , ("BLOB", stringParams)                 
                 , ("MEDIUMTEXT", stringParams)
                 , ("MEDIUMBLOB", stringParams)                 
                 , ("LONGTEXT", stringParams)
                 , ("LONGBLOB", stringParams)                 
                 ]


-- | List of supported string datatypes
stringTypes :: [String]
stringTypes = [ "CHAR"
	      , "VARCHAR"
	      , "BINARY"
	      , "VARBINARY"
              , "TINYTEXT"
	      , "TINYBLOB"
	      , "TEXT"
	      , "BLOB"
              , "MEDIUMTEXT"
	      , "MEDIUMBLOB"
	      , "LONGTEXT"
              , "LONGBLOB"
              ]


-- | List of supported numeric datatypes
numericTypes :: [String]
numericTypes = [ "SIGNED-TINYINT"
	       , "UNSIGNED-TINYINT"
	       , "SIGNED-SMALLINT"
               , "UNSIGNED-SMALLINT"
	       , "SIGNED-MEDIUMINT"
               , "UNSIGNED-MEDIUMINT"
	       , "SIGNED-INT"
	       , "UNSIGNED-INT"
               , "SIGNED-BIGINT"
	       , "UNSIGNED-BIGINT"
               ]


-- | List of allowed params for the numeric datatypes
numericParams :: AllowedParams
numericParams = [ rng_maxExclusive
		, rng_minExclusive
                , rng_maxInclusive
		, rng_minInclusive
                ]
                

-- | List of allowed params for the string datatypes
stringParams :: AllowedParams
stringParams = [ rng_length
	       , rng_maxLength
	       , rng_minLength
	       ]

-- ------------------------------------------------------------
--
-- | Tests whether a XML instance value matches a data-pattern.
                
datatypeAllowsMysql :: DatatypeAllows
datatypeAllowsMysql d params value _
    | isJust ndt
	= (uncurry (numberValid d) . fromJust $ ndt) params value
    | isJust sdt
	= (uncurry (stringValid d) . fromJust $ sdt) params value
    | otherwise
	= Just $ errorMsgDataTypeNotAllowed d params value mysqlNS
    where
    ndt = lookup d $
	  [ ("SIGNED-TINYINT", ((-128), 127))
	  , ("UNSIGNED-TINYINT", (0, 255))
	  , ("SIGNED-SMALLINT", ((-32768), 32767))
	  , ("UNSIGNED-SMALLINT", (0, 65535))
	  , ("SIGNED-MEDIUMINT", ((-8388608), 8388607))
	  , ("UNSIGNED-MEDIUMINT", (0, 16777215))
	  , ("SIGNED-INT", ((-2147483648), 2147483647))
	  , ("UNSIGNED-INT", (0, 4294967295))
	  , ("SIGNED-BIGINT", ((-9223372036854775808), 9223372036854775807))
	  , ("UNSIGNED-BIGINT", (0, 18446744073709551615))
	  ]
    sdt = lookup d $
	  [ ("CHAR", (0, 255))
	  , ("VARCHAR", (0, 65535))
	  , ("BINARY", (0, 255))    
	  , ("VARBINARY", (0, 65535))
	  , ("TINYTEXT", (0, 256))
	  , ("TINYBLOB", (0, 256))    
	  , ("TEXT", (0, 65536))
	  , ("BLOB", (0, 65536))
	  , ("MEDIUMTEXT", (0, 16777216))
	  , ("MEDIUMBLOB", (0, 16777216))
	  , ("LONGTEXT", (0, 4294967296))
	  , ("LONGBLOB", (0, 4294967296))
	  ]

{-
datatypeAllowsMysql :: DatatypeAllows

datatypeAllowsMysql d@"SIGNED-TINYINT" params value _ 
    = checkNumeric d value (-128) 127 params

datatypeAllowsMysql d@"UNSIGNED-TINYINT" params value _ 
    = checkNumeric d value 0 255 params

datatypeAllowsMysql d@"SIGNED-SMALLINT" params value _ 
    = checkNumeric d value (-32768) 32767 params

datatypeAllowsMysql d@"UNSIGNED-SMALLINT" params value _ 
    = checkNumeric d value 0 65535 params

datatypeAllowsMysql d@"SIGNED-MEDIUMINT" params value _ 
    = checkNumeric d value (-8388608) 8388607 params

datatypeAllowsMysql d@"UNSIGNED-MEDIUMINT" params value _ 
    = checkNumeric d value 0 16777215 params

datatypeAllowsMysql d@"SIGNED-INT" params value _ 
    = checkNumeric d value (-2147483648) 2147483647 params

datatypeAllowsMysql d@"UNSIGNED-INT" params value _ 
    = checkNumeric d value 0 4294967295 params

datatypeAllowsMysql d@"SIGNED-BIGINT" params value _ 
    = checkNumeric d value (-9223372036854775808) 9223372036854775807 params

datatypeAllowsMysql d@"UNSIGNED-BIGINT" params value _ 
    = checkNumeric d value 0 18446744073709551615 params


datatypeAllowsMysql d@"CHAR" params value _ 
    = checkString d value 0 255 params

datatypeAllowsMysql d@"VARCHAR" params value _ 
    = checkString d value 0 65535 params

datatypeAllowsMysql d@"BINARY" params value _ 
    = checkString d value 0 255 params
    
datatypeAllowsMysql d@"VARBINARY" params value _ 
    = checkString d value 0 65535 params

datatypeAllowsMysql d@"TINYTEXT" params value _ 
    = checkString d value 0 256 params -- 2^8

datatypeAllowsMysql d@"TINYBLOB" params value _ 
    = checkString d value 0 256 params -- 2^8
    
datatypeAllowsMysql d@"TEXT" params value _ 
    = checkString d value 0 65536 params -- 2^16

datatypeAllowsMysql d@"BLOB" params value _ 
    = checkString d value 0 65536 params -- 2^16

datatypeAllowsMysql d@"MEDIUMTEXT" params value _ 
    = checkString d value 0 16777216 params -- 2^24

datatypeAllowsMysql d@"MEDIUMBLOB" params value _ 
    = checkString d value 0 16777216 params -- 2^24

datatypeAllowsMysql d@"LONGTEXT" params value _ 
    = checkString d value 0 4294967296 params -- 2^32

datatypeAllowsMysql d@"LONGBLOB" params value _ 
    = checkString d value 0 4294967296 params -- 2^32


datatypeAllowsMysql t p v _
    = Just $ errorMsgDataTypeNotAllowed t p v mysqlNS
-}

-- | Tests whether a XML instance value matches a value-pattern.
datatypeEqualMysql :: DatatypeEqual
datatypeEqualMysql d s1 _ s2 _ 
    | elem d stringTypes  = if (s1 == s2)
			    then Nothing
			    else Just $ errorMsgEqual d s1 s2

    | elem d numericTypes = if (normalizeNumber s1 == normalizeNumber s2)
                            then Nothing
			    else Just $ errorMsgEqual d s1 s2

    | otherwise           = Just $ errorMsgDataTypeNotAllowed2 d s1 s2 mysqlNS

