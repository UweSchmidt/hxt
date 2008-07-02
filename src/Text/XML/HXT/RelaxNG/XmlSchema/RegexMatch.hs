-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Convenient functions for W3C XML Schema Regular Expression Matcher.
   For internals see 'Text.XML.HXT.RelaxNG.XmlSchema.Regex'

   Grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>

-}

-- ------------------------------------------------------------

module Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch
    ( matchRE
    , splitRE
    , tokenizeRE
    )
where

import Data.Maybe

import Text.XML.HXT.RelaxNG.XmlSchema.Regex
import Text.XML.HXT.RelaxNG.XmlSchema.RegexParser

-- ------------------------------------------------------------


splitRegex		:: Regex -> String -> Maybe (String, String)
splitRegex re ""
    | nullable re	= Just ("", "")
    | otherwise		= Nothing

splitRegex re inp@(c : inp')
    | isZero   re	= Nothing
    | otherwise		= evalRes . splitRegex (delta re c) $ inp'
    where
    evalRes Nothing
	| nullable re	= Just ("", inp)
	| otherwise	= Nothing

    evalRes (Just (tok, rest))
	    		= Just (c : tok, rest)

-- | split a string by taking the longest prefix matching a regular expression
--
-- @Nothing@ is returned in case of a syntactically wrong regex string
-- or in case there is no matching prefix, else the pair of prefix and rest is returned
--
-- examples:
--
-- > splitRE "a*b" "abc" = Just ("ab","c")
-- > splitRE "a*"  "bc"  = Just ("", "bc)
-- > splitRE "["   "abc" = Nothing

splitRE	:: String -> String -> Maybe (String, String)
splitRE re input
    = either (const Nothing) (flip splitRegex input) . parseRegex $ re

-- | split a string into tokens (words) by giving a regular expression
-- which all tokens must match
--
-- This can be used for simple tokenizers.
-- The in the result list contain at least one char.
-- All none matching chars are discarded. If the given regex contains syntax errors,
-- @Nothing@ is returned
--
-- examples:
--
-- > tokenizeRE "a*b" ""         = Just []
-- > tokenizeRE "a*b" "abc"      = Just ["ab"]
-- > tokenizeRE "a*b" "abaab ab" = Just ["ab","aab","ab"]
-- >
-- > tokenizeRE "[a-z]{2,}|[0-9]{2,}|[0-9]+[.][0-9]+" "ab123 456.7abc"
-- >                                = Just ["ab","123","456.7","abc"]
-- >
-- > tokenizeRE "[a-z]*|[0-9]{2,}|[0-9]+[.][0-9]+" "cab123 456.7abc"
-- >                                = Just ["cab","123","456.7","abc"]
-- >
-- > tokenizeRE "[^ \t\n\r]*" "abc def\t\n\rxyz"
-- >                                = Just ["abc","def","xyz"]
-- >
-- > tokenizeRE "[^ \t\n\r]*"    = words

tokenizeRE	:: String -> String -> Maybe [String]
tokenizeRE regex input
    = either (const Nothing) (Just . flip tokenize input) $ parseRegex regex
    where
    tokenize	:: Regex -> String -> [String]
    tokenize re inp
	| null inp	= []
	| otherwise	= evalRes . splitRegex re $ inp
	where
	evalRes Nothing	= tokenize re (tail inp)	-- re does not match any prefix
	evalRes (Just (tok, rest))
	    | null tok	= tokenize re (tail rest)	-- re is nullable and only the empty prefix matches
	    | otherwise	= tok : tokenize re rest	-- token found, tokenize the rest


-- | match a string with a regular expression
--
-- First argument is the regex, second the input string,
-- if the regex is not well formed, @Nothing@ is returned,
-- else @Just@ the match result
--
-- Examples:
--
-- > matchRE "x*" "xxx" = Just True
-- > matchRE "x" "xxx"  = Just False
-- > matchRE "[" "xxx"  = Nothing

matchRE	:: String -> String -> Maybe Bool
matchRE regex input
    = either (const Nothing) (Just . isNothing . flip match input) $ parseRegex regex

-- ------------------------------------------------------------
