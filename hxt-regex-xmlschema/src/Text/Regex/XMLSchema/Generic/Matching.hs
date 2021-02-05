{-# LANGUAGE BangPatterns #-}

-- ------------------------------------------------------------

{- |
   Module     : Text.Regex.XMLSchema.Generic
   Copyright  : Copyright (C) 2014- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt <uwe@fh-wedel.de>
   Stability  : stable
   Portability: portable

   Convenient functions for W3C XML Schema Regular Expression Matcher.
   For internals see 'Text.Regex.XMLSchema.Regex'

   Grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>

-}

-- ------------------------------------------------------------

module Text.Regex.XMLSchema.Generic.Matching
    ( grep
    , grepExt
    , grepRE
    , grepREwithLineNum

    , match
    , matchExt
    , matchSubex

    , sed
    , sedExt

    , split
    , splitExt
    , splitSubex

    , tokenize
    , tokenizeExt
    , tokenize'
    , tokenizeExt'
    , tokenizeSubex

    , matchRE
    , matchSubexRE
    , sedRE
    , splitRE
    , splitSubexRE
    , tokenizeRE
    , tokenizeRE'
    , tokenizeSubexRE
    )
where

import           Control.Arrow

import           Data.Maybe

import           Text.Regex.XMLSchema.Generic.Regex
import           Text.Regex.XMLSchema.Generic.RegexParser
import           Text.Regex.XMLSchema.Generic.StringLike

{-
import Debug.Trace      (traceShow)

trc :: Show a => String -> a -> a
trc msg x = traceShow (msg, x) x

-- -}
-- ------------------------------------------------------------

-- | split a string by taking the longest prefix matching a regular expression
--
-- @Nothing@ is returned in case there is no matching prefix,
-- else the pair of prefix and rest is returned

splitRE         :: StringLike s => GenRegex s -> s -> Maybe (s, s)
splitRE re input
                = do
                  (sms, rest) <- splitWithRegex re input
                  return (snd . head $ sms, rest)

-- | convenient function for 'splitRE'
--
-- examples:
--
-- > split "a*b" "abc" = ("ab","c")
-- > split "a*"  "bc"  = ("", "bc")    -- "a*" matches ""
-- > split "a+"  "bc"  = ("", "bc")    -- "a+" does not match, no split
-- > split "["   "abc" = ("", "abc")   -- "["  syntax error, no split

split           :: StringLike s => s -> s -> (s, s)
split           = split' parseRegex

-- | split with extended syntax

splitExt        :: StringLike s => s -> s -> (s, s)
splitExt        = split' parseRegexExt

split'           :: StringLike s => (s -> GenRegex s) -> s -> s -> (s, s)
split' parseRe re input
                 = fromMaybe (emptyS, input)
                  . (splitRE . parseRe $ re) $ input

-- ------------------------------------------------------------

-- | split a string by removing the longest prefix matching a regular expression
-- and then return the list of subexpressions found in the matching part
--
-- @Nothing@ is returned in case of no matching prefix,
-- else the list of pairs of labels and submatches and the
-- rest is returned

splitSubexRE    :: StringLike s => GenRegex s -> s -> Maybe ([(s, s)], s)
splitSubexRE re input
                = do
                  (sms, rest) <- splitWithRegex re input
                  return (map (first fromJust) . drop 1 $ sms, rest)

-- | convenient function for 'splitSubex', uses extended syntax
--
-- examples:
--
-- > splitSubex "({1}a*)b"  "abc" = ([("1","a")],"c")
-- > splitSubex "({2}a*)"   "bc"  = ([("2","")], "bc")
-- > splitSubex "({1}a|b)+" "abc" = ([("1","a"),("1","b")],"c")        -- subex 1 matches 2 times
-- >
-- > splitSubex ".*({x}a*)" "aa"  = ([("x",""),("x","a"),("x","aa")],"")
-- >                                                                   -- nondeterminism: 3 matches for a*
-- >
-- > splitSubex "({1}do)|({2}[a-z]+)" "do you know"
-- >                                = ([("1","do"),("2","do")]," you know")
-- >                                                                   -- nondeterminism: 2 matches for do
-- >
-- > splitSubex "({1}do){|}({2}[a-z]+)" "do you know"
-- >                                = ([("1","do")]," you know")
-- >                                                                   -- no nondeterminism with {|}: 1. match for do
-- >
-- > splitSubex "({1}a+)"   "bcd" = ([], "bcd")                        -- no match
-- > splitSubex "["         "abc" = ([], "abc")                        -- syntax error


splitSubex      :: StringLike s => s -> s -> ([(s, s)], s)
splitSubex re inp
                = fromMaybe ([], inp) . (splitSubexRE . parseRegexExt $ re) $ inp

-- ------------------------------------------------------------

-- | The function, that does the real work for 'tokenize'

tokenizeRE      :: StringLike s => GenRegex s -> s -> [s]
tokenizeRE re
    = token''
    where
    fcs         = firstChars re
    re1         = mkDiff re mkUnit
    token''     = token' re  fcs
    token1''    = token' re1 fcs

    -- token'   :: StringLike s => GenRegex s -> CharSet -> s -> [s]
    token' re' fcs' inp
      | nullS inp  = []
      | otherwise  = evalRes . splitWithRegexCS re' fcs' $ inp
      where
        evalRes Nothing
          = token'' (dropS 1 inp)         -- re does not match any prefix

        evalRes (Just (toks, rest))
          | nullS tok  = tok : token'' (dropS 1 rest) -- re is nullable and only the empty prefix matches
                                                      -- discard one char and try again
          | otherwise = tok : token1'' rest           -- real token found, next token must not be empty
          where
            tok = snd . head $ toks

-- | split a string into tokens (words) by giving a regular expression
-- which all tokens must match.
--
-- Convenient function for 'tokenizeRE'
--
-- This can be used for simple tokenizers.
-- It is recommended to use regular expressions where the empty word does not match.
-- Else there will appear a lot of probably useless empty tokens in the output.
-- All none matching chars are discarded. If the given regex contains syntax errors,
-- @Nothing@ is returned
--
-- examples:
--
-- > tokenize "a" "aabba"      = ["a","a","a"]
-- > tokenize "a*" "aaaba"     = ["aaa","a"]
-- > tokenize "a*" "bbb"       = ["","",""]
-- > tokenize "a+" "bbb"       = []
-- >
-- > tokenize "a*b" ""         = []
-- > tokenize "a*b" "abc"      = ["ab"]
-- > tokenize "a*b" "abaab ab" = ["ab","aab","ab"]
-- >
-- > tokenize "[a-z]{2,}|[0-9]{2,}|[0-9]+[.][0-9]+" "ab123 456.7abc"
-- >                           = ["ab","123","456.7","abc"]
-- >
-- > tokenize "[a-z]*|[0-9]{2,}|[0-9]+[.][0-9]+" "cab123 456.7abc"
-- >                           = ["cab","123","456.7","abc"]
-- >
-- > tokenize "[^ \t\n\r]*" "abc def\t\n\rxyz"
-- >                           = ["abc","def","xyz"]
-- >
-- > tokenize ".*"   "\nabc\n123\n\nxyz\n"
-- >                           = ["","abc","123","","xyz"]
-- >
-- > tokenize ".*"             = lines
-- >
-- > tokenize "[^ \t\n\r]*"    = words

tokenize        :: StringLike s => s -> s -> [s]
tokenize        = tokenizeRE . parseRegex

-- | tokenize with extended syntax

tokenizeExt     :: StringLike s => s -> s -> [s]
tokenizeExt     = tokenizeRE . parseRegexExt

-- ------------------------------------------------------------

-- | split a string into tokens and delimierter by giving a regular expression
-- which all tokens must match
--
-- This is a generalisation of the above 'tokenizeRE' functions.
-- The none matching char sequences are marked with @Left@, the matching ones are marked with @Right@
--
-- If the regular expression contains syntax errors @Nothing@ is returned
--
-- The following Law holds:
--
-- > concat . map (either id id) . tokenizeRE' re == id

tokenizeRE'     :: StringLike s => GenRegex s -> s -> [Either s s]
tokenizeRE' re inp0
    = token'' (inp0, 0) inp0
    where
    fcs         = firstChars re
    re1         = mkDiff re mkUnit
    token''     = token' re  fcs
    token1''    = token' re1 fcs

    -- token'   :: StringLike s => GenRegex s -> CharSet -> (s, Int) -> s -> [Either s s]
    token' re' fcs' (uns, !n) inp
      | nullS inp     = addUnmatched []
      | otherwise     = evalRes . splitWithRegexCS re' fcs' $ inp
      where
        addUnmatched
          | n == 0     = id
          | otherwise  = ((Left $ takeS n uns) :)

        addMatched t
          = addUnmatched . ((Right t) :)

        evalRes Nothing
          = token'' (uns, n + 1) (dropS 1 inp)       -- re does not match any prefix

        evalRes (Just (toks, rest))
            | nullS tok = addMatched tok           -- re is nullable and only the empty prefix matches
                          $ token'' (rest, 1)
                                    (dropS 1 rest) -- discard one char and try again

            | otherwise = addMatched tok
                          $ token1'' (rest, 0) rest -- real token found, next token must not be empty
          where
            tok = snd . head $ toks

-- | convenient function for 'tokenizeRE''
--
-- When the regular expression parses as Zero, @[Left input]@ is returned, that means no tokens are found

tokenize'       :: StringLike s => s -> s -> [Either s s]
tokenize'       = tokenizeRE' . parseRegex

tokenizeExt'    :: StringLike s => s -> s -> [Either s s]
tokenizeExt'    = tokenizeRE' . parseRegexExt

-- ------------------------------------------------------------

-- | split a string into tokens (pair of labels and words) by giving a regular expression
-- containing labeled subexpressions.
--
-- This function should not be called with regular expressions
-- without any labeled subexpressions. This does not make sense, because the result list
-- will always be empty.
--
-- Result is the list of matching subexpressions
-- This can be used for simple tokenizers.
-- At least one char is consumed by parsing a token.
-- The pairs in the result list contain the matching substrings.
-- All none matching chars are discarded. If the given regex contains syntax errors,
-- @Nothing@ is returned

tokenizeSubexRE :: StringLike s => GenRegex s -> s -> [(s, s)]
tokenizeSubexRE re
    = token''
    where
    fcs         = firstChars re
    re1         = mkDiff re mkUnit
    token''     = token' re  fcs
    token1''    = token' re1 fcs

    -- token'   :: StringLike s => GenRegex s -> CharSet -> s -> [(s, s)]
    token' re' fcs' inp
      | nullS inp      = []
      | otherwise     = evalRes . splitWithRegexCS re' fcs' $ inp
      where
        evalRes Nothing
          = token'' (dropS 1 inp)            -- re does not match any prefix

        evalRes (Just (toks, rest))
          | nullS tok = res ++ token'' (dropS 1 rest) -- re is nullable and only the empty prefix matches
          | otherwise = res ++ token1'' rest         -- token found, tokenize the rest
          where
            res = map (first fromJust) . tail $ toks
            tok = snd . head $ toks

-- | convenient function for 'tokenizeSubexRE' a string
--
-- examples:
--
-- > tokenizeSubex "({name}[a-z]+)|({num}[0-9]{2,})|({real}[0-9]+[.][0-9]+)"
-- >                 "cab123 456.7abc"
-- >                                  = [("name","cab")
-- >                                    ,("num","123")
-- >                                    ,("real","456.7")
-- >                                    ,("name","abc")]
-- >
-- > tokenizeSubex "({real}({n}[0-9]+)([.]({f}[0-9]+))?)"
-- >                 "12.34"          = [("real","12.34")
-- >                                    ,("n","12")
-- >                                    ,("f","34")]
-- >
-- > tokenizeSubex "({real}({n}[0-9]+)([.]({f}[0-9]+))?)"
-- >                  "12 34"         = [("real","12"),("n","12")
-- >                                    ,("real","34"),("n","34")]
-- >
-- > tokenizeSubex "({real}({n}[0-9]+)(([.]({f}[0-9]+))|({f})))"
-- >                  "12 34.56"      = [("real","12"),("n","12"),("f","")
-- >                                    ,("real","34.56"),("n","34"),("f","56")]

tokenizeSubex   :: StringLike s => s -> s -> [(s, s)]
tokenizeSubex   = tokenizeSubexRE . parseRegexExt

-- ------------------------------------------------------------

-- | sed like editing function
--
-- All matching tokens are edited by the 1. argument, the editing function,
-- all other chars remain as they are

sedRE           :: StringLike s => (s -> s) ->  GenRegex s -> s -> s
sedRE edit re   = concatS . map (either id edit) . tokenizeRE' re

-- | convenient function for 'sedRE'
--
-- examples:
--
-- > sed (const "b") "a" "xaxax"       = "xbxbx"
-- > sed (\ x -> x ++ x) "a" "xax"     = "xaax"
-- > sed undefined       "[" "xxx"     = "xxx"

sed             :: StringLike s => (s -> s) -> s -> s -> s
sed edit        = sedRE edit . parseRegex

sedExt          :: StringLike s => (s -> s) -> s -> s -> s
sedExt edit     = sedRE edit . parseRegexExt

-- ------------------------------------------------------------

-- | match a string with a regular expression

matchRE         :: StringLike s => GenRegex s -> s -> Bool
matchRE         = matchWithRegex

-- | convenient function for 'matchRE'
--
-- Examples:
--
-- > match "x*" "xxx" = True
-- > match "x" "xxx"  = False
-- > match "[" "xxx"  = False

match           :: StringLike s => s -> s -> Bool
match           = matchWithRegex . parseRegex

-- | match with extended regular expressions

matchExt        :: StringLike s => s -> s -> Bool
matchExt        = matchWithRegex . parseRegexExt

-- ------------------------------------------------------------

-- | match a string with a regular expression
-- and extract subexpression matches

matchSubexRE            :: StringLike s => GenRegex s -> s -> [(s, s)]
matchSubexRE re         = map (first fromJust) . fromMaybe [] . matchWithRegex' re

-- | convenient function for 'matchRE'
--
-- Examples:
--
-- > matchSubex "({1}x*)"                 "xxx"      = [("1","xxx")]
-- > matchSubex "({1}x*)"                 "y"        = []
-- > matchSubex "({w}[0-9]+)x({h}[0-9]+)" "800x600"  = [("w","800"),("h","600")]
-- > matchSubex "[" "xxx"                            = []

matchSubex              :: StringLike s => s -> s -> [(s, s)]
matchSubex              = matchSubexRE . parseRegexExt

-- ------------------------------------------------------------

-- | grep like filter for lists of strings
--
-- The regular expression may be prefixed with the usual context spec \"^\" for start of string,
-- and "\\<" for start of word.
-- and suffixed with \"$\" for end of text and "\\>" end of word.
-- Word chars are defined by the multi char escape sequence "\\w"
--
-- Examples
--
-- > grep "a"    ["_a_", "_a", "a_", "a", "_"]      => ["_a_", "_a", "a_", "a"]
-- > grep "^a"   ["_a_", "_a", "a_", "a", "_"]      => ["a_", "a"]
-- > grep "a$"   ["_a_", "_a", "a_", "a", "_"]      => ["_a", "a"]
-- > grep "^a$"  ["_a_", "_a", "a_", "a", "_"]      => ["a"]
-- > grep "\\<a" ["x a b", " ax ", " xa ", "xab"]   => ["x a b", " ax "]
-- > grep "a\\>" ["x a b", " ax ", " xa ", "xab"]   => ["x a b", " xa "]

grep                    :: StringLike s => s -> [s] -> [s]
grep                    = grep' parseRegex'

-- | grep with extended regular expressions

grepExt                 :: StringLike s => s -> [s] -> [s]
grepExt                 = grep' parseRegexExt'

grep'                   :: StringLike s => (String -> GenRegex s) -> s -> [s] -> [s]
grep' parseRe           = grepRE . parseContextRegex parseRe

-- | grep with already prepared Regex (ususally with 'parseContextRegex')

grepRE                  :: StringLike s => GenRegex s-> [s] -> [s]
grepRE re               = filter (matchRE re)

-- | grep with Regex and line numbers

grepREwithLineNum       :: StringLike s => GenRegex s -> [s] -> [(Int, s)]
grepREwithLineNum re     = filter (matchRE re . snd) . zip [(1::Int)..]

-- ------------------------------------------------------------
