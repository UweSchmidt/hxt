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
    , sedRE
    , tokenizeRE
    , tokenizeRE'
    , match
    , tokenize
    , tokenize'
    , sed
    , split
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
-- ------------------------------------------------------------

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

-- | convenient function for splitRE
--
-- syntax errors in R.E. are interpreted as no matching prefix found

split		:: String -> String -> (String, String)
split re input
    = fromMaybe ("", input) . splitRE re $ input

-- ------------------------------------------------------------

-- | split a string into tokens (words) by giving a regular expression
-- which all tokens must match
--
-- This can be used for simple tokenizers.
-- The words in the result list contain at least one char.
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
    = either (const Nothing) (Just . flip token' input) $ parseRegex regex
    where
    token'	:: Regex -> String -> [String]
    token' re inp
	| null inp	= []
	| otherwise	= evalRes . splitRegex re $ inp
	where
	evalRes Nothing	= token' re (tail inp)	-- re does not match any prefix
	evalRes (Just (tok, rest))
	    | null tok	= token' re (tail rest)	-- re is nullable and only the empty prefix matches
	    | otherwise	= tok : token' re rest	-- token found, tokenize the rest

-- | convenient function for tokenizeRE a string
--
-- syntax errors in R.E. result in an empty list

tokenize	:: String -> String -> [String]
tokenize re
    = fromMaybe [] . tokenizeRE re

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
-- > concat . map (either id id) . fromJust . tokenizeRE' re == id

tokenizeRE'	:: String -> String -> Maybe [Either String String]
tokenizeRE' regex input
    = either (const Nothing) (Just . flip token' input) $ parseRegex regex
    where
    token'	:: Regex -> String -> [Either String String]
    token' re
	= tok2 ""
	where
	tok2 :: String -> String -> [Either String String]
	tok2 noMatchPrefix inp
	    | null inp	= addNoMatch []
	    | otherwise	= evalRes . splitRegex re $ inp
	    where
	    addNoMatch res
		| null noMatchPrefix	= res
		| otherwise		= (Left . reverse $ noMatchPrefix) : res

	    evalRes Nothing		= tok2 (head inp : noMatchPrefix) (tail inp)		-- re does not match any prefix
	    evalRes (Just (tok, rest))
		| null tok		= tok2 (head rest : noMatchPrefix) (tail rest)		-- re is nullable and only the empty prefix matches
		| otherwise		= addNoMatch . (Right tok :) . tok2 "" $ rest		-- token found, tokenize the rest

-- | convenient function for tokenizeRE'
--
-- When the regular expression contains errors @[Left input]@ is returned, that means tokens are found

tokenize'	:: String -> String -> [Either String String]
tokenize' regex input
    = fromMaybe [Left input] . tokenizeRE' regex $ input

-- ------------------------------------------------------------

-- | sed like editing function
--
-- All matching tokens are edited by the 1. argument, the editing function,
-- all other chars remain as they are
--
-- examples:
--
-- > sedRE (const "b") "a" "xaxax"       = Just "xbxbx"
-- > sedRE (\ x -> x ++ x) "a" "xax"     = Just "xaax"
-- > sedRE undefined       "[" undefined = Nothing

sedRE		:: (String -> String) -> String -> String -> Maybe String
sedRE edit regex input
    = maybe Nothing (Just . concatMap (either id edit)) $ tokenizeRE' regex input

-- | convenient function for sedRE
--
-- When the regular expression contains errors, sed is the identity, else
-- the funtionality is like 'sedRE'
--
-- > sed undefined "["  == id

sed		:: (String -> String) -> String -> String -> String
sed edit regex input
    = fromMaybe input . sedRE edit regex $ input

-- ------------------------------------------------------------

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
    = either (const Nothing) (Just . isNothing . flip matchWithRE input) $ parseRegex regex


-- | convenient function for matchRE
--
-- syntax errors in R.E. are interpreted as no match found

match		:: String -> String -> Bool
match re	= fromMaybe False . matchRE re

-- ------------------------------------------------------------

-- ------------------------------------------------------------
