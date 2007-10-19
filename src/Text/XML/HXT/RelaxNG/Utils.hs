-- |
-- Some helper functions

module Text.XML.HXT.RelaxNG.Utils
    ( isRelaxAnyURI
    , compareURI
    , normalizeURI
    , isNumber
    , isNmtoken
    , isName
    , formatStringList
    , formatStringListPatt
    , formatStringListId
    , formatStringListQuot
    , formatStringListPairs
    , formatStringListArr
    , qn2String
    )
where

import Text.XML.HXT.Arrow.DOMInterface

import Text.ParserCombinators.Parsec
import Text.XML.HXT.Parser.XmlParser
    ( skipS0
    , nmtoken
    , name
    )

import Network.URI
    ( isURI
    , isRelativeReference
    , parseURI
    , URI(..)
    )

import Data.Maybe
    ( fromMaybe
    )

import Data.Char
    ( toLower
    )


-- ------------------------------------------------------------


-- | Tests whether a URI matches the Relax NG anyURI symbol

isRelaxAnyURI :: String -> Bool
isRelaxAnyURI s 
    = s == "" ||
      ( isURI s && not (isRelativeReference s) &&
	( let (URI _ _ path _ frag) = fromMaybe (URI "" Nothing "" "" "") $ parseURI s
          in (frag == "" && path /= "")
	)
      )


-- | Tests whether two URIs are equal after 'normalizeURI' is performed

compareURI :: String -> String -> Bool
compareURI uri1 uri2
    = normalizeURI uri1 == normalizeURI uri2


-- |  Converts all letters to the corresponding lower-case letter 
-- and removes a trailing \"\/\" 

normalizeURI :: String -> String
normalizeURI ""
    = ""
normalizeURI uri
    = map toLower ( if last uri == '/'
		    then init uri
		    else uri
		  )

checkByParsing	:: Parser String -> String -> Bool
checkByParsing p s
    = either (const False) (const True) (parse p "" s)
                  
-- | Tests whether a string matches a number [-](0-9)*
isNumber :: String -> Bool
isNumber
    = checkByParsing parseNumber'
    where
    parseNumber' :: Parser String
    parseNumber'
	= do
	  skipS0
	  m <- option "" (string "-")
	  n <- many1 digit
	  skipS0
	  eof
	  return $ m ++ n

isNmtoken	:: String -> Bool
isNmtoken    = checkByParsing nmtoken

isName	:: String -> Bool
isName	= checkByParsing name

{- | 

Formats a list of strings into a single string.
The first parameter formats the elements, the 2. is inserted
between two elements.

example:

> formatStringList show ", " ["foo", "bar", "baz"] -> "foo", "bar", "baz"

-}

formatStringListPatt :: [String] -> String
formatStringListPatt
    = formatStringList (++ "-") ", "

formatStringListPairs :: [(String,String)] -> String
formatStringListPairs
    = formatStringList id ", "
      . map (\ (a, b) -> a ++ " = " ++ show b)

formatStringListQuot :: [String] -> String
formatStringListQuot
    = formatStringList show ", "

formatStringListId :: [String] -> String
formatStringListId
    = formatStringList id ", "

formatStringListArr :: [String] -> String
formatStringListArr
    = formatStringList show " -> "

formatStringList :: (String -> String) -> String -> [String] -> String
formatStringList _sf _sp []
    = ""
formatStringList sf spacer l
    = reverse $ drop (length spacer) $ reverse $ 
      foldr (\e -> ((if e /= "" then sf e ++ spacer else "") ++)) "" l


-- | Formats a qualified name, e.g. \"{namespace}localName\"

qn2String :: QName -> String
qn2String (QN _ lp ns)
    = if null ns
      then lp
      else "{" ++ ns ++ "}" ++ lp
