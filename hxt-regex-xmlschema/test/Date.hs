module Date
where

import Control.Arrow                                   ( second )
import Data.Char                                       ( toLower
                                                       , toUpper
                                                       )
import Data.Char.Properties.XMLCharProps
import Data.List                                       ( isPrefixOf )
import Data.Maybe

import Text.Parsec
import Text.Regex.XMLSchema.String

-- ------------------------------------------------------------

-- some little helpers for building r.e.s

star           = (++ "*") . pars
plus           = (++ "+") . pars
opt            = (++ "?") . pars
dot            = (++ "\\.")
pars           = ("(" ++) . (++ ")")
orr x y        = pars $ pars x ++ "|" ++ pars y
xor x y        = pars $ pars x ++ "{|}" ++ pars y
nocase (x:xs)  = '[' : toUpper x : toLower x : ']' : xs
alt            = pars . foldr1 orr
altNC          = pars . alt . map nocase
subex n e      = pars $ "{" ++ n ++ "}" ++ pars e

ws             = "\\s"
ws0            = star ws
ws1            = plus ws
s0 x y         = x ++ ws0 ++ y

-- the date and time r.e.s

day            = "(0?[1-9]|[12][0-9]|3[01])"
month          = "(0?[1-9]|1[0-2])"
year2          = "[0-5][0-9]"
year4          = "20" ++ year2
year           = year4 `orr` year2
year'          = "'" ++ year2

dayD           = dot day
monthD         = dot month

dayMonthYear   = dayD `s0` monthD `s0` year
dayMonth       = dayD `s0` monthD

dayOfWeekL     = altNC
                 [ "montag"
                 , "dienstag"
                 , "mittwoch"
                 , "donnerstag"
                 , "freitag"
                 , "samstag"
                 , "sonnabend"
                 , "sonntag"
                 ]
dayOfWeekA     = alt . map dot $
                 [ "Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"]
dayOfWeek      = dayOfWeekL `orr` dayOfWeekA

monthL         = altNC
                 [ "januar"
                 , "februar"
                 , "märz"
                 , "april"
                 , "mai"
                 , "juni"
                 , "juli"
                 , "august"
                 , "september"
                 , "oktober"
                 , "november"
                 , "dezember"
                 ]
monthA         = altNC . map dot $ map snd monthAbr

monthAbr       = (9, "sept") :
                 zip [1..12]
                 [ "jan", "feb", "mär", "apr", "mai", "jun", "jul", "aug", "sep", "okt", "nov", "dez"]

monthN         = pars $ monthL `orr` monthA

hour           = pars "([0-1]?[0-9])|(2[0-4])"
minute         = pars "(0?[0-9])|([1-5][0-9])"
uhr            = ws0 ++ nocase "uhr"
hourMin        = hour ++ ":" ++ minute ++ opt uhr

wsyear         = year ++ "/[0-9]{2}"
wsem           = ("Wi?Se?" `orr` nocase "Wintersemester") ++ ws0 ++ wsyear
ssem           = ("So?Se?" `orr` nocase "Sommersemester") ++ ws0 ++ year
sem            = wsem `orr` ssem

num            = "\\d+"

-- the token types

tokenRE         = foldr1 xor $
                 map (uncurry subex) $
                 [ ( "ddmmyyyy",    dayMonthYear )
                 , ( "ddMonthyyyy", dayD `s0` monthN `s0` (year `orr` year') )
                 , ( "ddmm",        dayMonth)
                 , ( "ddMonth",     dayD `s0` monthN )
                 , ( "yyyymmdd",    year ++ "[-/]" ++ month ++ "[-/]" ++ day )
                 , ( "yyyy",        year4 `orr` ("'" ++ year2) )
                 , ( "month",       monthN )
                 , ( "weekday",     dayOfWeek )
                 , ( "HHMM",        hourMin ++ opt uhr )
                 , ( "HH",          hour    ++ uhr )
                 , ( "wsem",        wsem)
                 , ( "ssem",        ssem)
                 , ( "word",        "[\\w\\d]+")
                 , ( "del",         "[^\\w\\d]+")
                 ]

-- ------------------------------------------------------------

type Token         = (String, String)
type TokenStream   = [Token]

type DateParser a  = Parsec [(String, String)] () a

type Text          = String -> String           -- for fast concatenation

-- must be extended for weekday or semester, if neccessay

data DateVal       = DT { _year   :: ! Int
                        , _month  :: ! Int
                        , _day    :: ! Int
                        , _hour   :: ! Int
                        , _min    :: ! Int
                        }
                     deriving (Eq, Show)

data DateParse     = DP { _pre    ::   Text
                        , _rep    ::   Text
                        , _dat    :: ! DateVal
                        }

-- just a helper for result output
data DateRep       = DR { _p ::   String
                        , _r ::   String
                        , _d :: ! DateVal
                        }
                     deriving (Eq, Show)

-- ------------------------------------------------------------

emptyText       :: Text
emptyText       = id

mkText          :: String -> Text
mkText          = (++)

concText        :: Text -> Text -> Text
concText        = (.)

textToString    :: Text -> String
textToString    = ($ [])

emptyDateVal    :: DateVal
emptyDateVal    = DT { _year   = -1
                     , _month  = -1
                     , _day    = -1
                     , _hour   = -1
                     , _min    = -1
                     }

emptyDateParse  :: DateParse
emptyDateParse  = DP { _pre = emptyText
                     , _rep = emptyText
                     , _dat = emptyDateVal
                     }

appPre          :: String -> DateParse -> DateParse
appPre s d      = d { _pre = _pre d `concText` mkText s }

appRep          :: String -> DateParse -> DateParse
appRep s d      = d { _rep = _rep d `concText` mkText s }

setDay          :: Int -> Int -> Int -> DateParse -> DateParse
setDay j m t d  = d { _dat = setDateVal j m t (-1) (-1) (_dat d) }

setHour         :: Int -> Int -> DateParse -> DateParse
setHour h m d   = d { _dat = setDateVal (-1) (-1) (-1) h m (_dat d) }

setDateVal      :: Int -> Int -> Int -> Int -> Int -> DateVal -> DateVal
setDateVal j m t s i (DT j' m' t' s' i' )
                = DT j'' m'' t'' s'' i''
    where
      j'' | j < 0     = j'              -- year not there
          | j < 100   = j + 2000        -- 2 digit year
          | otherwise = j               -- 4 digit year
      m''             = m `max` m'
      t''             = t `max` t'
      s''             = s `max` s'
      i''             = i `max` i'

datePToDateRep  :: DateParse -> DateRep
datePToDateRep dp
                = DR { _p = textToString $ _pre dp
                     , _r = textToString $ _rep dp
                     , _d =                _dat dp
                     }

-- ------------------------------------------------------------

-- a simple helper for showing the results

dateSearch'     :: TokenStream -> [DateRep]
dateSearch'     = map datePToDateRep .
                  dateSearch

-- look for a sequence of date specs, the last entry in the list
-- does not contain a valid date, but just the context behind the last real date

dateSearch      :: TokenStream -> [DateParse]
dateSearch ts   = either (const []) id .
                  parse (many (dateParser emptyDateParse)) "" $
                  ts

-- all date parsers thread a state the subparsers to accumulate
-- the parts of a date, the context, the external representation and
-- the pure data, year, month, day, ...

dateParser      :: DateParse -> DateParser DateParse
dateParser d    = ( do
                    s <- fillTok
                    dateParser0 (appPre s d)
                  )
                  <|>
                  parseDate d                   -- here is the hook for the real date parser
                  <|>
                  ( do
                    s <- textTok                -- the default case: if parseDate fails
                    dateParser0 (appPre s d)    -- the token is handled like a normal word
                  )

dateParser0     :: DateParse -> DateParser DateParse
dateParser0 d   = dateParser d <|> return d


parseDate       :: DateParse -> DateParser DateParse
parseDate d     = parseDate0 d
                  <|>
                  try
                  ( do
                    d1 <- parseWeekDay d
                    lookAheadN 3 parseDate0 d1  -- Freitag, den 13.
                  )

-- parse a date optionally followed by a time
parseDate0      :: DateParse -> DateParser DateParse
parseDate0 d    = ( do
                    d1 <- parseDay d
                    option d1 (parseFollowingHour d1)
                  )

-- parse a simple token for a day
parseDay        :: DateParse -> DateParser DateParse
parseDay d      = ( do
                    (s, d') <- parseDateTok "ddmmyyyy" d
                    let [t, m, j] = tokenize num s
                    return $ setDay (read j) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "ddMonthyyyy" d
                    let s' = sed ((++ ".") . monthToM) monthN s
                    let [t, m, j] = tokenize num s'
                    return $ setDay (read j) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "ddmm" d
                    let [t, m] = tokenize num s
                    return $ setDay (-1) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "ddMonth" d
                    let s'     = sed ((++ ".") . monthToM) monthN s
                    let [t, m] = tokenize num s'
                    return $ setDay (-1) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "yyyymmdd" d
                    let [j, m, t] = tokenize num s
                    return $ setDay (read j) (read m) (read t) d'
                  )

parseYear       :: DateParse -> DateParser DateParse
parseYear d     = ( do
                    (s, d') <- parseDateTok "yyyy" d
                    let [j] = tokenize num s
                    return $ setDay (read j) (-1) (-1) d'
                  )

-- parse a weekday and add it to the external rep.

parseWeekDay    :: DateParse -> DateParser DateParse
parseWeekDay d  = ( do
                    (_s, d') <- parseDateTok "weekday" d
                    return d'
                  )

-- parse a following hour spec, 5 fill tokens, words or delimiters are possible

parseFollowingHour      :: DateParse -> DateParser DateParse
parseFollowingHour
                = try .                         -- backtracking becomes neccessary
                  lookAheadN 5 parseHour        -- max 2 words and 3 delimiters

-- parse the simple time formats
parseHour       :: DateParse -> DateParser DateParse
parseHour d     = ( do
                    (s, d') <- parseDateTok "HHMM" d
                    let [h, m] = tokenize num s
                    return $ setHour (read h) (read m) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "HH" d
                    let [h] = tokenize num s
                    return $ setHour (read h) 0 d'
                  )

-- ------------------------------------------------------------
--
-- auxiliary parser combinators

-- parse a token of a given type and add the text to the external rep.

parseDateTok    :: String -> DateParse -> DateParser (String, DateParse)
parseDateTok tt d
                = dateTok (isTokType (== tt)) d

dateTok         :: DateParser String -> DateParse -> DateParser (String, DateParse)
dateTok t d     = ( do
                    s <- t
                    return (s, appRep s d)
                  )

-- try to apply a parser, but first skip a given # of fill tokens

lookAheadN      :: Int -> (DateParse -> DateParser DateParse) -> DateParse -> DateParser DateParse
lookAheadN n p d
    | n <= 0    = p d
    | otherwise = do
                  (_, d1) <- dateTok fillTok d
                  ( lookAheadN (n - 1) p d1 <|> p d1 )

-- ------------------------------------------------------------
--
-- basic token parsers

-- the interface to the primitive parsec token parser
tok             :: (Token -> Bool) -> DateParser Token
tok pred        = tokenPrim showTok nextPos testTok
    where
      showTok               = show . fst
      nextPos pos _tok _ts  = incSourceColumn pos 1
      testTok tok           = if pred tok then Just tok else Nothing

-- check for specific token type and in case of success return the text value
isTokType       :: (String -> Bool) -> DateParser String
isTokType isT   = tok (isT . fst) >>= return . snd

-- parse an arbitrary token and return the text value
textTok         :: DateParser String
textTok         = isTokType (const True)

-- a word
wordTok         :: DateParser String
wordTok         = isTokType (== "word")

-- a delimiter, whitespace is normalized, sequences are reduced to a single space char
delTok          :: DateParser String
delTok          = isTokType (== "del")
                  >>=
                  return . sed (const " ") ws1

-- tokens that don't contain date info

fillTok         :: DateParser String
fillTok         = delTok <|> wordTok

-- semester tokens, not yet interpreted
semTok'         :: String -> DateParser (String, Int, Bool)
semTok' sem     = do v <- isTokType (== sem)
                     return (v, read . head . tokenizeExt year $ v, sem == "ssem")

semTok          :: DateParser (String, Int, Bool)
semTok          = semTok' "ssem" <|> semTok' "wsem"

-- ------------------------------------------------------------

-- conversion from month names to 1..12
monthToM        :: String -> String
monthToM m
    = show .
      (\ l -> if null l then 99 else head l) .
      map fst .
      filter ((== True) . snd) .
      map (second (`isPrefixOf` map toLower m)) $
      monthAbr

-- ------------------------------------------------------------

t :: String
t = "Am Sonntag, dem 17. Februar '03 findet um 9 Uhr ein wichtiger Termin für das Sommersemester 2000 statt. "
    ++ "Dieser wird allerdings auf Montag verschoben. Und zwar auf den ersten Montag im Wintersemester 11/12, 12:30. "
    ++ "Ein wichtiger Termin findet im SoSe 2011 statt. Im Jahr '12 gibt es Termine, aber auch in WS 2010/11. "
    ++ "Ein weiterer Termin ist  am 2.4.11 um 12 Uhr. Oder war es doch Di. der 3.4.? Egal. "
    ++ "Ein weiterer wichtiger Termin findet am 2001-3-4 statt bzw. generell zwischen 01/3/4 - 01/6/4 um 13 Uhr. "
    ++ "Am kommenden Mittwoch findet Changemanagement in HS5 statt. Dies gilt dann auch für den 7. Juni "
    ++ "des Jahres 2011. Noch ein wichtiger Termin findet um 16:15 Uhr am Do., 1.2.03 statt. "
    ++ "Freitag, der 13. Juli ist kein Glückstag"
    ++ "und Freitag, der 13. Juli um 11:55 Uhr ist es zu spät."

r = map _r . dateSearch' . tokenizeSubex tokenRE $ t
d = map _d . dateSearch' . tokenizeSubex tokenRE $ t
a =          dateSearch' . tokenizeSubex tokenRE $ t

tt = tokenizeSubex tokenRE
dd = map _d . dateSearch' . tt
rr = map _r . dateSearch' . tt
pp = map _p . dateSearch' . tt

-- ------------------------------------------------------------
