-- ------------------------------------------------------------

{- |
   Copyright  : Copyright (C) 2010- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt <uwe@fh-wedel.de>
   Stability  : stable
   Portability: portable

   Convenient functions for W3C XML Schema Regular Expression Matcher for Strings.
   A specialisation of Text.Regex.XMLSchema.Generic as
   compatibility module to old non generic version

   Grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>
-}

-- ------------------------------------------------------------

module Text.Regex.XMLSchema.String
    {-# DEPRECATED "use the more general 'Text.Regex.XMLSchema.Generic' instead" #-}
    ( Regex

    , grep
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

      -- Text.Regex.XMLSchema.Generic.Regex
    , mkZero
    , mkUnit
    , mkSym1
    , mkSymRng
    , mkWord
    , mkDot
    , mkStar
    , mkAll
    , mkAlt
    , mkElse
    , mkSeq
    , mkSeqs
    , mkRep
    , mkRng
    , mkOpt
    , mkDiff
    , mkIsect
    , mkExor
    , mkCompl
    , mkBr
    , isZero
    , errRegex

    -- Text.Regex.XMLSchema.Generic.RegexParser
    , parseRegex
    , parseRegexExt
    , parseContextRegex
    )
where

import           Text.Regex.XMLSchema.Generic             (Regex)
import qualified Text.Regex.XMLSchema.Generic             as G
import           Text.Regex.XMLSchema.Generic.Regex
import           Text.Regex.XMLSchema.Generic.RegexParser

-- ------------------------------------------------------------

-- | split a string by taking the longest prefix matching a regular expression
--
-- @Nothing@ is returned in case there is no matching prefix,
-- else the pair of prefix and rest is returned

splitRE         :: Regex -> String -> Maybe (String, String)
splitRE         = G.splitRE

-- | convenient function for 'splitRE'
--
-- examples:
--
-- > split "a*b" "abc" = ("ab","c")
-- > split "a*"  "bc"  = ("", "bc")    -- "a*" matches ""
-- > split "a+"  "bc"  = ("", "bc")    -- "a+" does not match, no split
-- > split "["   "abc" = ("", "abc")   -- "["  syntax error, no split

split           :: String -> String -> (String, String)
split           = G.split

-- | split with extended syntax

splitExt        :: String -> String -> (String, String)
splitExt        = G.splitExt

-- ------------------------------------------------------------

-- | split a string by removing the longest prefix matching a regular expression
-- and then return the list of subexpressions found in the matching part
--
-- @Nothing@ is returned in case of no matching prefix,
-- else the list of pairs of labels and submatches and the
-- rest is returned

splitSubexRE    :: Regex -> String -> Maybe ([(String, String)], String)
splitSubexRE    = G.splitSubexRE

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


splitSubex      :: String -> String -> ([(String,String)], String)
splitSubex      = G.splitSubex

-- ------------------------------------------------------------

-- | The function, that does the real work for 'tokenize'

tokenizeRE      :: Regex -> String -> [String]
tokenizeRE      = G.tokenizeRE

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

tokenize        :: String -> String -> [String]
tokenize        = G.tokenize

-- | tokenize with extended syntax

tokenizeExt     :: String -> String -> [String]
tokenizeExt     = G.tokenizeExt

-- ------------------------------------------------------------

-- | split a string into tokens and delimierter by giving a regular expression
-- wich all tokens must match
--
-- This is a generalisation of the above 'tokenizeRE' functions.
-- The none matching char sequences are marked with @Left@, the matching ones are marked with @Right@
--
-- If the regular expression contains syntax errors @Nothing@ is returned
--
-- The following Law holds:
--
-- > concat . map (either id id) . tokenizeRE' re == id

tokenizeRE'     :: Regex -> String -> [Either String String]
tokenizeRE'     =  G.tokenizeRE'

-- | convenient function for 'tokenizeRE''
--
-- When the regular expression parses as Zero,
-- @[Left input]@ is returned, that means no tokens are found

tokenize'       :: String -> String -> [Either String String]
tokenize'       = G.tokenize'

tokenizeExt'    :: String -> String -> [Either String String]
tokenizeExt'    = G.tokenizeExt'

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

tokenizeSubexRE :: Regex -> String -> [(String, String)]
tokenizeSubexRE = G.tokenizeSubexRE

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

tokenizeSubex   :: String -> String -> [(String,String)]
tokenizeSubex   = G.tokenizeSubex

-- ------------------------------------------------------------

-- | sed like editing function
--
-- All matching tokens are edited by the 1. argument, the editing function,
-- all other chars remain as they are

sedRE           :: (String -> String) ->  Regex -> String -> String
sedRE           = G.sedRE

-- | convenient function for 'sedRE'
--
-- examples:
--
-- > sed (const "b") "a" "xaxax"       = "xbxbx"
-- > sed (\ x -> x ++ x) "a" "xax"     = "xaax"
-- > sed undefined       "[" "xxx"     = "xxx"

sed             :: (String -> String) -> String -> String -> String
sed             = G.sed

sedExt          :: (String -> String) -> String -> String -> String
sedExt          = G.sedExt

-- ------------------------------------------------------------

-- | match a string with a regular expression

matchRE         :: Regex -> String -> Bool
matchRE         = G.matchRE

-- | convenient function for 'matchRE'
--
-- Examples:
--
-- > match "x*" "xxx" = True
-- > match "x" "xxx"  = False
-- > match "[" "xxx"  = False

match           :: String -> String -> Bool
match           = G.match

-- | match with extended regular expressions

matchExt        :: String -> String -> Bool
matchExt        = G.matchExt

-- ------------------------------------------------------------

-- | match a string with a regular expression
-- and extract subexpression matches

matchSubexRE            :: Regex -> String -> [(String, String)]
matchSubexRE            = G.matchSubexRE

-- | convenient function for 'matchRE'
--
-- Examples:
--
-- > matchSubex "({1}x*)"                 "xxx"      = [("1","xxx")]
-- > matchSubex "({1}x*)"                 "y"        = []
-- > matchSubex "({w}[0-9]+)x({h}[0-9]+)" "800x600"  = [("w","800"),("h","600")]
-- > matchSubex "[" "xxx"                            = []

matchSubex              :: String -> String -> [(String, String)]
matchSubex              = G.matchSubex

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

grep                    :: String -> [String] -> [String]
grep                    = G.grep

-- | grep with extended regular expressions

grepExt                 :: String -> [String] -> [String]
grepExt                 = G.grepExt

-- | grep with already prepared Regex (ususally with 'parseContextRegex')

grepRE                  :: Regex -> [String] -> [String]
grepRE                  = G.grepRE

-- | grep with Regex and line numbers

grepREwithLineNum       :: Regex -> [String] -> [(Int,String)]
grepREwithLineNum       = G.grepREwithLineNum

-- ------------------------------------------------------------
