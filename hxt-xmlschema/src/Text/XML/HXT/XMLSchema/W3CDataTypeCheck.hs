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

import Text.Regex.XMLSchema.Generic   ( Regex
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

import Data.Char                      ( isAlpha
                                      , isDigit
                                      )
import Data.Function                  ( on )
import Data.Maybe                     ( fromMaybe
                                      , isJust
                                      )
import Data.Ratio                     ( numerator
                                      , denominator
                                      , (%)
                                      )
import Data.Time                      ( Day
                                      , DiffTime
                                      , NominalDiffTime
                                      , UTCTime(..)
                                      , addDays
                                      , addGregorianYearsRollOver
                                      , addGregorianMonthsRollOver
                                      , addUTCTime
                                      , diffUTCTime
                                      , fromGregorian
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

-- | enumeration checks must be done in the value space,
-- so the strings must be converted into values before testing on equality
enumerationValid' :: (String -> Bool) -> (String -> a) -> (a -> a -> Bool) -> ParamList -> CheckA String String
enumerationValid' isVal readVal eqVal params
    | null values
        = ok
    | all isVal values
        = assert chek err1
    | otherwise
        = failure err2
    where
      values = map snd . filter ((== xsd_enumeration) . fst) $ params
      chek v = isVal v && (or . map (eqVal (readVal v) . readVal) $ values)

      err1 v = unwords ["value", show v, "not element of enumeration", show values]
      err2 _ = unwords ["some enumeration values are illegal in", show values]

whiteSpaceNorm :: ParamList -> (String -> String)
whiteSpaceNorm params
    = maybe id wsf $ lookup xsd_whiteSpace params
      where
        wsf "collapse" = normalizeWhitespace
        wsf "replace"  = normalizeBlanks
        wsf _          = id

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

-- | Function table for floating tests
fctTableFloating :: (Floating n, Read n, Ord n) => [(String, String -> n -> Bool)]
fctTableFloating
  = [ (xsd_maxExclusive, cvf (>))
    , (xsd_minExclusive, cvf (<))
    , (xsd_maxInclusive, cvf (>=))
    , (xsd_minInclusive, cvf (<=))
    ]
    where
    cvf :: (Floating n, Read n) => (n -> n -> Bool) -> (String -> n -> Bool)
    cvf op = \ x y -> isFloating x && readFloating x `op` y

readFloating :: (Floating n, Read n) => String -> n
readFloating  "INF" =  1.0 / 0.0
readFloating "-INF" = -1.0 / 0.0
readFloating  "NaN" =  0.0 / 0.0
readFloating s      =  read s

-- | Tests whether an floating value is valid
floatingValid :: (Floating n, Ord n, Read n, Show n) => DatatypeName -> ParamList -> CheckA n n
floatingValid _datatype params
  = (foldr (>>>) ok . map paramFloatingValid $ params)
    where
    paramFloatingValid (pn, pv)
      = assert
        ((fromMaybe (const . const $ True) . lookup pn $ fctTableFloating) pv)
        (errorMsgParam pn pv . show)

floatValid :: DatatypeName -> ParamList -> CheckA Float Float
floatValid = floatingValid

doubleValid :: DatatypeName -> ParamList -> CheckA Double Double
doubleValid = floatingValid

floatEq :: Float -> Float -> Bool
floatEq = (==)

doubleEq :: Double -> Double -> Bool
doubleEq = (==)

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

-- | Creates a boolean regex
rexBoolean :: Regex
rexBoolean = rex "true|false|1|0"

-- | Creates a decimal regex
rexDecimal :: Regex
rexDecimal = rex "(\\+|-)?(([0-9]+(\\.[0-9]*)?)|(\\.[0-9]+))"

-- | Creates an integer regex
rexInteger :: Regex
rexInteger = rex "(\\+|-)?[0-9]+"

rexFloating :: Regex
rexFloating = rex "(-?INF)|NaN|(\\+|-)?([0-9]+(.[0-9]*)?|.[0-9]+)([Ee](\\+|-)?[0-9]+)?"

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

isFloating :: String -> Bool
isFloating = matchRE rexFloating

readInteger :: String -> Integer
readInteger = read

-- ----------------------------------------

rexDuration :: Regex
rexDuration
    = rex $ sign ++ "P" ++ alt (ymd ++ opt tim) tim
    where
      sign = "-?"
      ymd = alt (y ++ opt md) md
      md  = alt (m ++ opt  d)  d
      y   = n ++ "Y"
      m   = n ++ "M"
      d   = n ++ "D"

      tim = "T" ++ hms
      hms = alt (h  ++ opt ms) ms
      ms  = alt (m' ++ opt  s)  s
      h   = n ++ "H"
      m'  = n ++ "M"
      s   = n'n ++ "S"

      n   = "[0-9]+"
      n'n = n ++ opt ("[.]" ++ n)

      opt x     = "(" ++ x ++ ")?"
      alt s1 s2 = "((" ++ s1 ++ ")|(" ++ s2 ++ "))"

isDuration :: String -> Bool
isDuration = matchRE rexDuration


data Duration =
    Dur { dNeg     :: Bool
        , dYears   :: Integer
        , dMonths  :: Integer
        , dDays    :: Integer
        , dSeconds :: Rational
    } deriving Show

nullDuration :: Duration
nullDuration
    = Dur { dNeg     = False
          , dYears   = 0
          , dMonths  = 0
          , dDays    = 0
          , dSeconds = fromInteger 0
          }

addSeconds :: Rational -> Duration -> Duration
addSeconds sec d
    = d { dDays    = dDays d + q
        , dSeconds = dSeconds d + fromInteger r
        }
    where
      (q, r) = sec' `quotRem` ds
      ds     = 24 * 60 * 60
      sec'   = floor sec

addDuration :: Duration -> UTCTime -> UTCTime
addDuration dur ut0
    = UTCTime (addDur ud1) ut1
      where
        (UTCTime ud1 ut1) = addUTCTime (fromRational $ neg $ dSeconds dur) ut0
        addDur
            = addGregorianYearsRollOver    (neg $ dYears  dur)
              . addGregorianMonthsRollOver (neg $ dMonths dur)
              . addDays                    (neg $ dDays   dur)

        neg :: Num a => a -> a
        neg | dNeg dur  = negate
            | otherwise = id

readDuration :: String -> Duration
readDuration ('-' : s)
    = (readDuration s) {dNeg = True}

readDuration s0@('P' : s)
    = readHourMinSecD (drop 1 s2) `addSeconds` readYearMonthDay s1
    where
      (s1, s2) = span (/= 'T') s

      errDur = error $ "readDuration: wrong argument " ++ show s0

      readYearMonthDay :: String -> Duration
      readYearMonthDay ""
          = nullDuration
      readYearMonthDay x
          | head x2 == 'Y'
              = (readYearMonthDay (tail x2)) {dYears = read x1}
          | head x2 == 'M'
              = (readYearMonthDay (tail x2)) {dMonths = read x1}
          | head x2 == 'D'
              = nullDuration {dDays = read x1}
          | otherwise
              = errDur
          where
            (x1, x2) = span isDigit x

      readHourMinSecD :: String -> Rational
      readHourMinSecD ""
          = fromInteger 0
      readHourMinSecD x
          | head x2 == 'H'
              = fromInteger ((60 * 60) * read x1) + readHourMinSecD (tail x2)
          | head x2 == 'M'
              = fromInteger ((     60) * read x1) + readHourMinSecD (tail x2)
          | head x2 == 'S'
              = readDecimal x1

          | otherwise
              = errDur
          where
            (x1, x2) = span (not . isAlpha) x
readDuration s0
    = error $ "readDuration: illegal argument " ++ show s0

showDuration :: Duration -> String
showDuration d
    | dNeg d
        = '-' : (showDuration $ d {dNeg = False})
    | otherwise
        = addP . years $ d
    where
      addP "" = "P0D"
      addP s  = 'P' : s

      addT "" = ""
      addT s  = 'T' : s

      ymds 0 _ = ""
      ymds i c = show i ++ [c]

      times scale unit next x
          | r == 0 = res
          | otherwise = show r ++ [unit] ++ res
          where
            r :: Integer
            r = truncate $ x / scale
            y = x - fromInteger r * scale
            res = next y

      years   x = ymds (dYears  x) 'Y' ++ months x
      months  x = ymds (dMonths x) 'M' ++ days x
      days    x = ymds (dDays   x) 'D' ++ (addT . hours $ dSeconds x)

      hours     = times (60 * 60)  'H' minutes
      minutes   = times        60  'M' seconds
      seconds d'
          | d' == fromInteger 0 = ""
          | otherwise           = showDecimal d' ++ "S"

fourDates :: [UTCTime]
fourDates
    = map (dUTCTime . readDateTime)
      [ "1696-09-01T00:00:00Z"
      , "1697-02-01T00:00:00Z"
      , "1903-03-01T00:00:00Z"
      , "1903-07-01T00:00:00Z"
      ]

cmpDuration :: (UTCTime -> UTCTime -> Bool) -> (Duration -> Duration -> Bool)
cmpDuration op d1 d2
    = and $ zipWith op (map (addDuration d1) fourDates)
                       (map (addDuration d2) fourDates)

-- | Function table for integer tests
--
-- TODO: the 4 comparison operators can not be implemented by the relatinal ops of Duration,
-- in the standard ("http://www.w3.org/TR/xmlschema-2/#duration")
-- there is a description of comparing durations: they must be compared
-- combining with 4 special dates and if all comparisons reflect the required relation,
-- the restriction is considdered as valid.

fctTableDuration :: [(String, String -> Duration -> Bool)]
fctTableDuration
  = [ (xsd_maxExclusive, cvi $ cmpDuration (>))
    , (xsd_minExclusive, cvi $ cmpDuration (<))
    , (xsd_maxInclusive, cvi $ cmpDuration (>=))
    , (xsd_minInclusive, cvi $ cmpDuration (<=))
    ]
    where
    cvi :: (Duration -> Duration -> Bool) -> (String -> Duration -> Bool)
    cvi op = \ x y -> isDuration x && readDuration x `op` y

-- | Tests whether an duration value is valid
durationValid :: ParamList -> CheckA Duration Duration
durationValid params
  = (foldr (>>>) ok . map paramDurationValid $ params)
    where
    paramDurationValid (pn, pv)
      = assert
        ((fromMaybe (const . const $ True) . lookup pn $ fctTableDuration) pv)
        (errorMsgParam pn pv . showDuration)

-- ----------------------------------------
--
-- Days are represented here
-- as in ISO 8601:2000 Second Edition:
--    ISO (International Organization for Standardization).
--    Representations of dates and times, second edition, 2000-12-15.
--
-- NOT as in
-- ISO 8601
--    ISO (International Organization for Standardization).
--    Representations of dates and times, 1988-06-15.
--
-- The main difference is dealing with year 0.
-- in the older ISO standard, this is excluded and
-- "-0001" is the representation of year 1 Before Common Era "-1 BCE".
-- In the latter standard "0000" represents "-1 BCE" and "-0001" represents "-2 BCE"

data Date =
    Date { dUTCTime :: UTCTime
         , _dTZ     :: MaybeTimeZone
         }
    deriving (Show)

type MaybeTimeZone = Maybe Seconds

type Seconds = Int

instance Eq Date where
    (==) = (==) `on` toUTCTime

instance Ord Date where
    compare = compare `on` toUTCTime

mkDateTime :: Day -> DiffTime -> MaybeTimeZone -> Date
mkDateTime d t z
    = Date (UTCTime d t) z

toUTCTime :: Date -> UTCTime
toUTCTime (Date d Nothing) = d
toUTCTime (Date d (Just tz)) = addUTCTime (fromInteger . toInteger $ tz) d

-- ----------------------------------------

rexDates :: [Regex]
rexDates
    = map rex [dateTime, date, time, gYearMonth, gYear, gMonthDay, gMonth, gDay]
    where
      dateTime   = ymd ++ "T" ++ hms ++ tz
      time       =               hms ++ tz
      date       = ymd               ++ tz
      gYearMonth = ym                ++ tz
      gYear      = y                 ++ tz

      gMonthDay  = "--" ++ m2 ++ "-" ++ t2 ++ tz
      gMonth     = "--" ++ m2              ++ tz
      gDay       = "--"       ++ "-" ++ t2 ++ tz

      y     = "-?" ++ y4'
      ym    = y           ++ "-" ++ m2
      ymd   = ym                       ++ "-" ++ t2

      hms   = alt (h2 ++ ":" ++ i2 ++ ":" ++ s2 ++ fr)
                  ("24:00:00" ++ opt ".0+")             -- 24:00 is legal

      tz    = opt (alt tz0 "Z")
      tz0   = (alt "\\-" "\\+") ++ tz1
      tz1   = alt (h13 ++ ":" ++ i2) "14:00:00"

      m2    = alt "0[1-9]" "1[0-2]"			-- Month
      t2    = alt "0[1-9]" (alt "[12][0-9]" "3[01]")    -- Tag
      h2    = alt "[01][0-9]" "2[0-3]"                  -- Hour
      i2    = "[0-5][0-9]"                              -- mInute
      s2    = i2                                        -- Seconds

{-                                                      -- this conforms to ISO 8601 from 1988
      y1    = "000[1-9]"                                -- "0000" isn't a year, "-0001" represents "-1 BCE"
      y2    = "00[1-9][0-9]"                            -- leading 0-s are only allowd for year < 1000
      y3    = "0[1-9][0-9]{2}"
      y4    = "[1-9][0-9]{3,}"
      y4'   = alt y4 $ alt y3 $ alt y2 y1
-- -}

-- {-                                                   -- this conforms to ISO 8601 Second Edition from 2000
      y4    = "[0-9]{4}"                                -- year "0000" is legal and represents "-1 BCE"
      y4'   = opt "[1-9][0-9]*" ++ y4
-- -}

      fr    = opt ".[0-9]+"

      h13   = alt "0[0-9]" "1[0-3]"

      opt x     = "(" ++ x ++ ")?"
      alt x1 x2 = "((" ++ x1 ++ ")|(" ++ x2 ++ "))"

isDateTime, isDate, isTime, isGYearMonth, isGYear, isGMonthDay, isGMonth, isGDay :: String -> Bool
[isDateTime, isDate, isTime, isGYearMonth, isGYear, isGMonthDay, isGMonth, isGDay]
    = map matchRE rexDates

readTimeZone :: String -> MaybeTimeZone
readTimeZone ""
    = Nothing
readTimeZone "Z"
    = Just 0
readTimeZone (s : xs)
    = Just .
      ( if s == '-' then negate else id ) .
      readZone $ xs
    where
      readZone s'
          = 60 * (60 * read hs + read ms)
          where
            (hs, (_ : ms)) = span (/= ':') s'

readYearMonthDayS :: String -> (Day, String)
readYearMonthDayS s0
    = (fromGregorian (sign $ read year) (read month) (read day), rest)
    where
      (sign,          s ) = if head s0 == '-'
                            then (negate, tail s0)
                            else (id,          s0)
      (year,  (_ : rest1)) = span (/= '-') s
      (month, (_ : rest2)) = span (/= '-') rest1
      (day,        rest  ) = span isDigit rest2

readYearMonthS :: String -> (Day, String)
readYearMonthS s0
    = (fromGregorian (sign $ read year) (read month) 1, rest)
    where
      (sign,          s ) = if head s0 == '-'
                            then (negate, tail s0)
                            else (id,          s0)
      (year,  (_ : rest1)) = span (/= '-') s
      (month,      rest  ) = span isDigit rest1

readYearS :: String -> (Day, String)
readYearS s0
    = (fromGregorian (sign $ read year) 1 1, rest)
    where
      (sign,          s ) = if head s0 == '-'
                            then (negate, tail s0)
                            else (id,          s0)
      (year,       rest  ) = span isDigit s

readMonthDayS :: String -> (Day, String)
readMonthDayS s0
    = (fromGregorian 1 (read month) (read day), rest)
    where
      (month, (_ : rest1)) = span isDigit . drop 2 $ s0
      (day,        rest  ) = span isDigit rest1

readMonthS :: String -> (Day, String)
readMonthS s0
    = (fromGregorian 1 (read month) 1, rest)
    where
      (month,       rest ) = span isDigit . drop 2 $ s0

readDayS :: String -> (Day, String)
readDayS s0
    = (fromGregorian 1 1 (read day), rest)
    where
      (day,         rest ) = span isDigit . drop 3 $ s0

readHourMinSec :: String -> DiffTime
readHourMinSec s
    = fromInteger (60 * (60 * read hours + read minutes))
      +
      fromRational (readDecimal seconds)
    where
      (hours,   (_ :    rest)) = span (/= ':') s
      (minutes, (_ : seconds)) = span (/= ':') rest

readDateTime :: String -> Date
readDateTime s
    = mkDateTime day (readHourMinSec time) (readTimeZone zone)
    where
      (day,  (_ : rest)) = readYearMonthDayS s
      (time,       zone) = span (\ x -> isDigit x || x `elem` ":.") rest

readDate' :: (String -> (Day, String)) -> String -> Date
readDate' read' s
    = mkDateTime day nullTime (readTimeZone zone)
    where
      (day, zone) = read' s

readDate
  , readGYearMonth
  , readGYear
  , readGMonthDay
  , readGMonth
  , readGDay :: String -> Date

readDate       = readDate' readYearMonthDayS
readGYearMonth = readDate' readYearMonthS
readGYear      = readDate' readYearS
readGMonthDay  = readDate' readMonthDayS
readGMonth     = readDate' readMonthS
readGDay       = readDate' readDayS

readTime :: String -> Date
readTime s
    = mkDateTime nullDay (readHourMinSec time) (readTimeZone zone)
    where
      (time, zone) = span (\ x -> isDigit x || x `elem` ":.") s

nullTime :: DiffTime
nullTime = fromInteger 0

nullDay :: Day
nullDay = fromGregorian 1 1 1

-- --------------------
-- the show must go on

showDateTime :: Date -> String
showDateTime (Date d tz)
    = ymd ++ "T" ++ hms ++ showTimeZone tz
    where
      (ymd : hms : _) = words . show $ d

showDate' :: (String -> String) -> Date -> String
showDate' fmt (Date d tz)
    = fmt ymd ++ showTimeZone tz
    where
      (ymd : _) = words . show $ d

dropRev :: Int -> String -> String
dropRev i = reverse . drop i . reverse

showDate :: Date -> String
showDate = showDate' $ id

showGYearMonth :: Date -> String
showGYearMonth = showDate' $ dropRev 3

showGYear :: Date -> String
showGYear = showDate' $ dropRev 6

showGMonthDay :: Date -> String
showGMonthDay = showDate' $ ('-' :) . reverse . take 6 . reverse

showGMonth :: Date -> String
showGMonth = showDate' $ ('-' :) . reverse . take 3 . drop 3 . reverse

showGDay :: Date -> String
showGDay = showDate' $ ('-' :) . ('-' :) . reverse . take 3 . reverse

-- it's
showTime :: Date -> String
showTime (Date d tz)
    = hms ++ showTimeZone tz
    where
      (_ymd : hms : _) = words . show $ d

showTimeZone :: MaybeTimeZone -> String
showTimeZone Nothing
    = ""
showTimeZone (Just s)
    | s == 0    = "Z"
    | s >  0    = '+' : showHourMin s
    | otherwise = '-' : showHourMin (negate s)

showHourMin :: Int -> String
showHourMin s0
    = showDec 2 (s `div` 60) ++ ":" ++ showDec 2 (s `mod` 60)
    where
      s = s0 `div` 60

showDec :: Int -> Int -> String
showDec n = reverse . toStr n
    where
      toStr 0 _ = ""
      toStr l i = show (i `mod` 10) ++ toStr (l-1) (i `div` 10)

-- --------------------
--
-- the real checks for date and time

-- Comparison of dates with timezone with dates without timezone
-- is only defined, when the difference is larger than 14 hours

fuzzyCmp :: (Date -> Date -> Bool) -> (Date -> Date -> Bool)
fuzzyCmp op d1@(Date _ tz1) d2@(Date _ tz2)
    | isJust tz1 == isJust tz2
        = d1 `op` d2
    | otherwise
        = d1 `op` d2 && distLarger14Hours
    where
      distLarger14Hours
          = abs (toUTCTime d1 `diffUTCTime` toUTCTime d2) > hours'14

hours'14 :: NominalDiffTime
hours'14 = fromInteger (14 * 60 * 60)

fctTableDateTime :: (String -> Bool) -> (String -> Date) -> [(String, String -> Date -> Bool)]
fctTableDateTime isDT readDT
  = [ (xsd_maxExclusive, cvi (>))
    , (xsd_minExclusive, cvi (<))
    , (xsd_maxInclusive, cvi (>=))
    , (xsd_minInclusive, cvi (<=))
    ]
    where
    cvi :: (Date -> Date -> Bool) -> (String -> Date -> Bool)
    cvi op = \ x y -> isDT x && readDT x `fuzzyop` y
        where
          fuzzyop = fuzzyCmp op

-- | Tests whether a date or time value is valid
dateTimeValid :: (String -> Bool) -> (String -> Date) -> (Date -> String) -> ParamList -> CheckA Date Date
dateTimeValid isDT readDT showDT params
  = (foldr (>>>) ok . map paramDateTimeValid $ params)
    where
    paramDateTimeValid (pn, pv)
      = assert
        ((fromMaybe (const . const $ True) . lookup pn $ fctTableDateTime isDT readDT) pv)
        (errorMsgParam pn pv . showDT)


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
      validPattern
          = patternValid params

      validPatternCollapse
          = validPattern >>> arr normalizeWhitespace

      validLength
          = stringValid d 0 (-1) params

      validList
          = validPatternCollapse
            >>> listValid d params
            >>> enumerationValid' (const True) id (==) params

      validString normFct
          = validPattern
            >>> arr normFct
            >>> validLength
            >>> enumerationValid' (const True) id (==) params

      validNormString
          = validString normalizeWhitespace

      validName isN
          = assertW3C isN

      validNCName
          = validNormString
            >>> validName isNCName

      validQName
          = validNormString
            >>> validName isWellformedQualifiedName

      validDecimal
          = validPatternCollapse
            >>> assertW3C isDecimal
            >>> checkWith readDecimal (decimalValid params)
            >>> enumerationValid' isDecimal readDecimal (==) params

      validInteger inRange
          = validPatternCollapse
            >>> assertW3C isInteger
            >>> checkWith read (integerValid inRange params)
            >>> enumerationValid' isInteger readInteger (==) params

      validFloating validFl eqFl
          = validPatternCollapse
            >>> assertW3C isFloating
            >>> checkWith readFloating (validFl d params)
            >>> enumerationValid' isFloating readFloating eqFl params

      validBoolean
          = validPattern	-- no enumeration allowed
            >>> arr normalizeWhitespace
            >>> assertW3C (matchRE rexBoolean)

      validDuration
          = validPatternCollapse
            >>> assertW3C isDuration
            >>> checkWith readDuration (durationValid params)
            >>> enumerationValid' isDuration readDuration (cmpDuration (==)) params

      validDateTime isDT readDT showDT
          = validPatternCollapse
            >>> assertW3C isDT
            >>> checkWith readDT (dateTimeValid isDT readDT showDT params)
            >>> enumerationValid' isDT readDT (fuzzyCmp (==)) params

      normWS = whiteSpaceNorm params

      check :: CheckA String String
      check = fromMaybe notFound . lookup d $ checks

      notFound = failure $ errorMsgDataTypeNotAllowed d params

      checks :: [(String, CheckA String String)]
      checks = [ (xsd_string,             validString normWS)
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

               , (xsd_boolean,            validBoolean)
               , (xsd_decimal,            validDecimal)
               , (xsd_double,             validFloating doubleValid doubleEq)
               , (xsd_float,              validFloating floatValid  floatEq)

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

               , (xsd_duration,           validDuration)
               , (xsd_dateTime,           validDateTime isDateTime   readDateTime   showDateTime  )
               , (xsd_date,               validDateTime isDate       readDate       showDate      )
               , (xsd_time,               validDateTime isTime       readTime       showTime      )
               , (xsd_gYearMonth,         validDateTime isGYearMonth readGYearMonth showGYearMonth)
               , (xsd_gYear,              validDateTime isGYear      readGYear      showGYear     )
               , (xsd_gMonthDay,          validDateTime isGMonthDay  readGMonthDay  showGMonthDay )
               , (xsd_gMonth,             validDateTime isGMonth     readGMonth     showGMonth    )
               , (xsd_gDay,               validDateTime isGDay       readGDay       showGDay      )
               ]

      assertW3C p = assert p errW3C
      errW3C      = errorMsgDataDoesNotMatch d

-- ----------------------------------------

