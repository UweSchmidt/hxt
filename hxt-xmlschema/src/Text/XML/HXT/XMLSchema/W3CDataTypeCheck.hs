{- |
   Module     : Text.XML.HXT.XMLSchema.W3CDataTypeCheck
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Contains functions to check basic W3C datatypes and params.
-}

module Text.XML.HXT.XMLSchema.W3CDataTypeCheck

  ( module Text.XML.HXT.XMLSchema.DataTypeLibW3CNames
  , ParamList
  , datatypeAllowsW3C
  )

where

import Text.XML.HXT.XMLSchema.W3CDataTypeCheckUtils
import Text.XML.HXT.XMLSchema.DataTypeLibW3CNames

import Text.Regex.XMLSchema.String    ( Regex
                                      , matchRE
                                      , parseRegex
                                      , isZero
                                      )

import Text.XML.HXT.DOM.QualifiedName ( isWellformedQualifiedName
                                      , isNCName
                                      )
import Text.XML.HXT.DOM.Util          ( normalizeWhitespace
                                      , normalizeBlanks
                                      , escapeURI
                                      )

import Data.Maybe                     ( fromMaybe )
import Data.Ratio                     ( numerator
                                      , denominator
                                      , (%)
                                      )

import Network.URI                    ( isURIReference )

-- ----------------------------------------

-- | Tests whether a pattern is valid
patternValid :: ParamList -> CheckA String String
patternValid params
  = foldr (>>>) ok . map paramPatternValid $ params
    where
    paramPatternValid (pn, pv)
      | pn == xsd_pattern = assert (patParamValid pv) $ errorMsgParam pn pv
      | otherwise         = ok

-- | Helper function to test pattern params
patParamValid :: String -> String -> Bool
patParamValid regex a
  | isZero ex = False
  | otherwise = matchRE ex a
  where
  ex = parseRegex regex

-- ----------------------------------------

-- | Function table for decimal tests
fctTableDecimal :: [(String, String -> Rational -> Bool)]
fctTableDecimal
  = [ (xsd_maxExclusive,   cvd (>))
    , (xsd_minExclusive,   cvd (<))
    , (xsd_maxInclusive,   cvd (>=))
    , (xsd_minInclusive,   cvd (<=))
    , (xsd_totalDigits,    cvi (\ l v ->    totalDigits v == l))
    , (xsd_fractionDigits, cvi (\ l v -> fractionDigits v == l))
    ]
    where
    cvd :: (Rational -> Rational -> Bool) -> (String -> Rational -> Bool)
    cvd op = \ x y -> isDecimal x && readDecimal x `op` y

    cvi :: (Int -> Rational -> Bool) -> (String -> Rational -> Bool)
    cvi op = \ x y -> isNumber x && read x `op` y

-- | Tests whether a decimal is valid
decimalValid :: ParamList -> CheckA Rational Rational
decimalValid params
  = foldr (>>>) ok . map paramDecimalValid $ params
    where
    paramDecimalValid (pn, pv)
      = assert
        ((fromMaybe (const . const $ True) . lookup pn $ fctTableDecimal) pv)
        (errorMsgParam pn pv . showDecimal)

-- ----------------------------------------

-- | Function table for integer tests
fctTableInteger :: [(String, String -> Integer -> Bool)]
fctTableInteger
  = [ (xsd_maxExclusive, cvi (>))
    , (xsd_minExclusive, cvi (<))
    , (xsd_maxInclusive, cvi (>=))
    , (xsd_minInclusive, cvi (<=))
    , (xsd_totalDigits,  cvi (\ l v -> totalD v == toInteger l))
    ]
    where
    cvi :: (Integer -> Integer -> Bool) -> (String -> Integer -> Bool)
    cvi op = \ x y -> isNumber x && read x `op` y

    totalD i =  toInteger . length . show . abs $ i

-- | Tests whether an integer is valid
integerValid :: DatatypeName -> ParamList -> CheckA Integer Integer
integerValid datatype params
  = assertInRange
    >>>
    (foldr (>>>) ok . map paramIntegerValid $ params)
    where
    assertInRange :: CheckA Integer Integer
    assertInRange
      = assert
        (fromMaybe (const True) . lookup datatype $ integerRangeTable)
        (\ v -> ( "Datatype " ++ show datatype ++
                  " with value = " ++ show v ++
                  " not in integer value range."
                )
        )
    paramIntegerValid (pn, pv)
      = assert
        ((fromMaybe (const . const $ True) . lookup pn $ fctTableInteger) pv)
        (errorMsgParam pn pv . show)

-- | Table for range tests on integer values
integerRangeTable :: [(String, Integer -> Bool)]
integerRangeTable = [ (xsd_integer,            const True)
                    , (xsd_nonPositiveInteger, (<=0)   )
                    , (xsd_negativeInteger,    ( <0)   )
                    , (xsd_nonNegativeInteger, (>=0)   )
                    , (xsd_positiveInteger,    ( >0)   )
                    , (xsd_long,               inR 9223372036854775808)
                    , (xsd_int,                inR 2147483648)
                    , (xsd_short,              inR 32768)
                    , (xsd_byte,               inR 128)
                    , (xsd_unsignedLong,       inP 18446744073709551616)
                    , (xsd_unsignedInt,        inP 4294967296)
                    , (xsd_unsignedShort,      inP 65536)
                    , (xsd_unsignedByte,       inP 256)
                    ]
                    where
                    inR b i = (0 - b) <= i && i < b
                    inP b i = 0 <= i       && i < b

-- ----------------------------------------

-- | Tests whether a string matches a name list
isNameList :: (String -> Bool) -> String -> Bool
isNameList p w
  = not (null ts) && all p ts
    where
    ts = words w

-- ----------------------------------------

-- | Creates a regex from a string
rex :: String -> Regex
rex regex
  | isZero ex = error $ "syntax error in regexp " ++ show regex ++ "."
  | otherwise = ex
  where
  ex = parseRegex regex

-- ----------------------------------------

-- | Creates a language regex
rexLanguage :: Regex
rexLanguage = rex "[A-Za-z]{1,8}(-[A-Za-z]{1,8})*"

-- | Creates a hex binary regex
rexHexBinary :: Regex
rexHexBinary = rex "([A-Fa-f0-9]{2})*"

-- | Creates a base64 binary regex
rexBase64Binary :: Regex
rexBase64Binary = rex $
                  "(" ++ b64 ++ "{4})*((" ++ b64 ++ "{2}==)|(" ++ b64 ++ "{3}=)|)"
                  where
                  b64     = "[A-Za-z0-9+/]"

-- | Creates a decimal regex
rexDecimal :: Regex
rexDecimal = rex "(\\+|-)?(([0-9]+(\\.[0-9]*)?)|(\\.[0-9]+))"

-- | Creates an integer regex
rexInteger :: Regex
rexInteger = rex "(\\+|-)?[0-9]+"

-- | Tests whether a string matches a language value
isLanguage :: String -> Bool
isLanguage = matchRE rexLanguage

-- | Tests whether a string matches a hex binary
isHexBinary :: String -> Bool
isHexBinary = matchRE rexHexBinary

-- | Tests whether a string matches a base64 binary
isBase64Binary :: String -> Bool
isBase64Binary = matchRE rexBase64Binary

-- | Tests whether a string matches a decimal
isDecimal :: String -> Bool
isDecimal = matchRE rexDecimal

-- | Tests whether a string matches an integer
isInteger :: String -> Bool 
isInteger = matchRE rexInteger

-- ----------------------------------------

-- | Transforms a base64 value
normBase64 :: String -> String
normBase64 = filter isB64
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

-- ----------------------------------------

-- | Reads a decimal from a string
readDecimal :: String -> Rational
readDecimal ('+':s) = readDecimal' s
readDecimal ('-':s) = negate $ readDecimal' s
readDecimal      s  = readDecimal' s

-- | Helper function to read a decimal from a string
readDecimal' :: String -> Rational
readDecimal' s
  | f == 0    = (n % 1)
  | otherwise = (n % 1) + (f % (10 ^ (toInteger $ length fs)))
  where
  (ns, fs') = span (/= '.') s
  fs = drop 1 fs'

  f :: Integer
  f | null fs   = 0
    | otherwise = read fs
  n :: Integer
  n | null ns   = 0
    | otherwise = read ns

-- | Reads total digits of a rational
totalDigits :: Rational -> Int
totalDigits r
  | r == 0    = 0
  | r < 0     = totalDigits' . negate  $ r
  | otherwise = totalDigits'           $ r

-- | Helper function to read total digits of a rational
totalDigits' :: Rational -> Int
totalDigits' r
  | denominator r == 1 = length . show . numerator  $ r
  | r < (1%1)          = (\ x -> x-1) . totalDigits' . (+ (1%1))    $ r
  | otherwise          = totalDigits' . (* (10 % 1)) $ r

-- | Reads fraction digits of a rational
fractionDigits :: Rational -> Int
fractionDigits r
  | denominator r == 1 = 0
  | otherwise          = (+1) . fractionDigits . (* (10 % 1)) $ r

-- | Transforms a decimal into a string
showDecimal :: Rational -> String
showDecimal d
  | d < 0     = ('-':) . showDecimal' . negate    $ d
  | d < 1     = drop 1 . showDecimal' . (+ (1%1)) $ d
  | otherwise =          showDecimal'             $ d

-- | Helper function to transform a decimal into a string
showDecimal' :: Rational -> String
showDecimal' d
  | denominator d == 1 = show . numerator $ d
  | otherwise          = times10 0        $ d
  where
  times10 i' d'
    | denominator d' == 1 = let
                            (x, y) = splitAt i' . reverse . show . numerator $ d'
                            in
                            reverse y ++ "." ++ reverse x
    | otherwise           = times10 (i' + 1) (d' * (10 % 1))

-- ----------------------------------------

-- | Tests whether value matches a given datatype and parameter list
datatypeAllowsW3C :: DatatypeName -> ParamList -> String -> Maybe String
datatypeAllowsW3C d params value
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
      = patternValid params

    validLength
      = stringValid d 0 (-1) params

    validList
      = validPattern
        >>>
        arr normalizeWhitespace
        >>>
        validListLength

    validListLength
      = listValid d params

    validName isN
      = assertW3C isN

    validNCName
      = validNormString >>> validName isNCName

    validQName
      = validNormString >>> validName isWellformedQualifiedName

    validDecimal
      = arr normalizeWhitespace
        >>>
        assertW3C isDecimal
        >>>
        checkWith readDecimal (decimalValid params)

    validInteger inRange
      = validPattern
        >>>
        arr normalizeWhitespace
        >>>
        assertW3C isInteger
        >>>
        checkWith read (integerValid inRange params)

    check :: CheckA String String
    check = fromMaybe notFound . lookup d $ checks

    notFound = failure $ errorMsgDataTypeNotAllowed d params

    checks :: [(String, CheckA String String)]
    checks = [ (xsd_string,             validString id)
             , (xsd_normalizedString,   validString normalizeBlanks)
             , (xsd_token,              validNormString)
             , (xsd_language,           validNormString >>> assertW3C isLanguage)
             , (xsd_NMTOKEN,            validNormString >>> validName isNmtoken)
             , (xsd_NMTOKENS,           validList       >>> validName (isNameList isNmtoken))
             , (xsd_Name,               validNormString >>> validName isName)
             , (xsd_NCName,             validNCName)
             , (xsd_ID,                 validNCName)
             , (xsd_IDREF,              validNCName)
             , (xsd_IDREFS,             validList       >>> validName (isNameList isNCName))
             , (xsd_ENTITY,             validNCName)
             , (xsd_ENTITIES,           validList       >>> validName (isNameList isNCName))
             , (xsd_anyURI,             validName isURIReference >>> validString escapeURI)
             , (xsd_QName,              validQName)
             , (xsd_NOTATION,           validQName)
             , (xsd_hexBinary,          validString id         >>> assertW3C isHexBinary)
             , (xsd_base64Binary,       validString normBase64 >>> assertW3C isBase64Binary)
             , (xsd_decimal,            validPattern >>> validDecimal)
             , (xsd_integer,            validInteger xsd_integer)
             , (xsd_nonPositiveInteger, validInteger xsd_nonPositiveInteger)
             , (xsd_negativeInteger,    validInteger xsd_negativeInteger)
             , (xsd_nonNegativeInteger, validInteger xsd_nonNegativeInteger)
             , (xsd_positiveInteger,    validInteger xsd_positiveInteger)
             , (xsd_long,               validInteger xsd_long)
             , (xsd_int,                validInteger xsd_int)
             , (xsd_short,              validInteger xsd_short)
             , (xsd_byte,               validInteger xsd_byte)
             , (xsd_unsignedLong,       validInteger xsd_unsignedLong)
             , (xsd_unsignedInt,        validInteger xsd_unsignedInt)
             , (xsd_unsignedShort,      validInteger xsd_unsignedShort)
             , (xsd_unsignedByte,       validInteger xsd_unsignedByte)
             ]
    assertW3C p = assert p errW3C
    errW3C      = errorMsgDataDoesNotMatch d

-- ----------------------------------------

