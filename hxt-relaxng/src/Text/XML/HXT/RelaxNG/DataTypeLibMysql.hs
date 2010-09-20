-- |
-- Datatype library for the MySQL datatypes
--

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
    = performCheck check value
    where
    check
        | isJust ndt    = checkNum (fromJust ndt)
        | isJust sdt    = checkStr (fromJust sdt)
        | otherwise     = failure $ errorMsgDataTypeNotAllowed mysqlNS d params
    checkNum r  = uncurry (numberValid d) r params
    checkStr r  = uncurry (stringValid d) r params
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

-- ------------------------------------------------------------

-- | Tests whether a XML instance value matches a value-pattern.

datatypeEqualMysql :: DatatypeEqual
datatypeEqualMysql d s1 _ s2 _
    = performCheck check (s1, s2)
      where
      cmp nf    = arr (\ (x1, x2) -> (nf x1, nf x2))
                  >>>
                  assert (uncurry (==)) (uncurry $ errorMsgEqual d)
      check
          | d `elem` stringTypes        = cmp id
          | d `elem` numericTypes       = cmp normalizeNumber
          | otherwise                   = failure $ const (errorMsgDataTypeNotAllowed0 mysqlNS d)

-- ------------------------------------------------------------
