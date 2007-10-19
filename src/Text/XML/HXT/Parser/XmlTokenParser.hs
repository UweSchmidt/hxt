-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.XmlTokenParser
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: XmlTokenParser.hs,v 1.3 2005/09/02 17:09:39 hxml Exp $

   Parsec parser for XML tokens

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.XmlTokenParser
    ( allBut
    , allBut1
    , asciiLetter
    , attrValue
    , bar
    , charRef
    , comma
    , dq
    , encName
    , entityRef
    , entityValue
    , eq
    , gt
    , keyword
    , keywords
    , lpar
    , lt
    , name
    , names
    , ncName
    , nmtoken
    , nmtokens
    , peReference
    , pubidLiteral
    , qName
    , quoted
    , reference
    , rpar
    , semi
    , separator
    , singleChar
    , singleChars
    , skipS
    , skipS0
    , sPace
    , sPace0
    , sq
    , systemLiteral
    , versionNum

    , concRes
    , mkList

    , nameT
    , nmtokenT

    , entityValueT
    , entityTokensT
    , entityCharT

    , attrValueT
    , attrValueT'

    , referenceT
    , charRefT
    , entityRefT
    , peReferenceT

    , singleCharsT
    )
where

import Text.ParserCombinators.Parsec


import Text.XML.HXT.DOM.XmlTree hiding
    ( choice
    )

import Text.XML.HXT.DOM.Unicode
    ( isXmlChar
    , intToCharRef
    , intToCharRefHex
    )

import Text.XML.HXT.DOM.Util
    ( hexStringToInt
    , decimalStringToInt
    )

import Text.XML.HXT.Parser.XmlCharParser
    ( xmlNameChar
    , xmlNameStartChar
    , xmlNCNameChar
    , xmlNCNameStartChar
    , xmlSpaceChar
    )

-- ------------------------------------------------------------
--
-- Character (2.2) and White Space (2.3)
--
-- Unicode parsers in module XmlCharParser

-- ------------------------------------------------------------

sPace		:: GenParser Char state String
sPace
    = many1 xmlSpaceChar

sPace0		:: GenParser Char state String
sPace0
    = many xmlSpaceChar

skipS		:: GenParser Char state ()
skipS
    = skipMany1 xmlSpaceChar

skipS0		:: GenParser Char state ()
skipS0
    = skipMany xmlSpaceChar

-- ------------------------------------------------------------
--
-- Names and Tokens (2.3)

asciiLetter		:: GenParser Char state Char
asciiLetter
    = satisfy (\ c -> ( c >= 'A' && c <= 'Z' ||
			c >= 'a' && c <= 'z' )
	      )
      <?> "ASCII letter"

name		:: GenParser Char state String
name
    = try ( do
	    s1 <- xmlNameStartChar
	    sl <- many xmlNameChar
	    return (s1 : sl)
	  )
      <?> "Name"

-- Namespaces in XML: Rules [4-5] NCName:

ncName		:: GenParser Char state String
ncName
    = try ( do
	    s1 <- xmlNCNameStartChar
	    sl <- many xmlNCNameChar
	    return (s1 : sl)
	  )
      <?> "NCName"

-- Namespaces in XML: Rules [6-8] QName:

qName		:: GenParser Char state (String, String)
qName
    = do
      s1 <- ncName
      s2 <- option "" ( do
			char ':'
			ncName
		      )
      return ( if null s2 then (s2, s1) else (s1, s2) )

nmtoken		:: GenParser Char state String
nmtoken
    = try (many1 xmlNameChar)
      <?> "Nmtoken"

names		:: GenParser Char state [String]
names
    = sepBy1 name sPace

nmtokens	:: GenParser Char state [String]
nmtokens
    = sepBy1 nmtoken sPace

-- ------------------------------------------------------------
--
-- Literals (2.3)

singleChar		:: String -> GenParser Char state Char
singleChar notAllowed
    = satisfy (\ c -> isXmlChar c && not (c `elem` notAllowed))

singleChars		:: String -> GenParser Char state String
singleChars notAllowed
    = many1 (singleChar notAllowed)

entityValue	:: GenParser Char state String
entityValue
    = attrValue

attrValueDQ	:: GenParser Char state String
attrValueDQ
    = between dq dq (concRes $ many $ attrChar "<&\"")

attrValueSQ	:: GenParser Char state String
attrValueSQ
    = between sq sq (concRes $ many $ attrChar "<&\'")

attrValue	:: GenParser Char state String
attrValue
    = ( do
	v <- attrValueDQ
	return ("\"" ++ v ++ "\"")
      )
      <|>
      ( do
	v <- attrValueSQ
	return ("'" ++ v ++ "'")
      )
      <?> "attribute value (in quotes)"

attrChar	:: String -> GenParser Char state String
attrChar notAllowed
    = reference
      <|>
      mkList (singleChar notAllowed)
      <?> "legal attribute character or reference"


systemLiteral		:: GenParser Char state String
systemLiteral
    = between dq dq (many $ noneOf "\"")
      <|>
      between sq sq (many $ noneOf "\'")
      <?> "system literal (in quotes)"

pubidLiteral		:: GenParser Char state String
pubidLiteral
    = between dq dq (many $ pubidChar "\'")
      <|>
      between sq sq (many $ pubidChar "")
      <?> "pubid literal (in quotes)"
      where
      pubidChar		:: String -> GenParser Char state Char
      pubidChar quoteChars
	  = asciiLetter
	    <|>
	    digit
	    <|>
	    oneOf " \r\n"		-- no "\t" allowed, so xmlSpaceChar parser not used
	    <|>
	    oneOf "-()+,./:=?;!*#@$_%"
            <|>
	    oneOf quoteChars

-- ------------------------------------------------------------
--
-- Character and Entity References (4.1)

reference	:: GenParser Char state String
reference
    = ( do
	i <- charRef
	return ("&#" ++ show i ++ ";")
      )
      <|>
      ( do
        n <- entityRef
        return ("&" ++ n ++ ";")
      )
      

checkCharRef	:: Int -> GenParser Char state Int
checkCharRef i
    = if ( i <= fromEnum (maxBound::Char)
	   && isXmlChar (toEnum i)
	 )
        then return i
	else unexpected ("illegal value in character reference: " ++ intToCharRef i ++ " , in hex: " ++ intToCharRefHex i)

charRef		:: GenParser Char state Int
charRef
    = do
      try (string "&#x")
      d <- many1 hexDigit
      semi
      checkCharRef (hexStringToInt d)
      <|>
      do
      try (string "&#")
      d <- many1 digit
      semi
      checkCharRef (decimalStringToInt d)
      <?> "character reference"


entityRef	:: GenParser Char state String
entityRef
    = do
      char '&'
      n <- name
      semi
      return n
      <?> "entity reference"

peReference	:: GenParser Char state String
peReference
    = try ( do
	    char '%'
	    n <- name
	    semi
	    return n
	  )
      <?> "parameter-entity reference"

-- ------------------------------------------------------------
--
-- 4.3

encName		:: GenParser Char state String
encName
    = do
      c <- asciiLetter
      r <- many (asciiLetter <|> digit <|> oneOf "._-")
      return (c:r)

versionNum	:: GenParser Char state String
versionNum
    = many1 xmlNameChar


-- ------------------------------------------------------------
--
-- keywords

keyword		:: String -> GenParser Char state String
keyword kw
    = try ( do
	    n <- name
	    if n == kw
	      then return n
	      else unexpected n
	  )
      <?> kw

keywords	:: [String] -> GenParser Char state String
keywords
    = foldr1 (<|>) . map keyword

-- ------------------------------------------------------------
--
-- parser for quoted attribute values

quoted		:: GenParser Char state a -> GenParser Char state a
quoted p
    = between dq dq p
      <|>
      between sq sq p

-- ------------------------------------------------------------
--
-- simple char parsers

dq, sq, lt, gt, semi	:: GenParser Char state Char

dq	= char '\"'
sq	= char '\''
lt	= char '<'
gt	= char '>'
semi	= char ';'

separator	:: Char -> GenParser Char state ()
separator c
    = do
      try ( do
	    skipS0
	    char c
	  )
      skipS0
      <?> [c]

bar, comma, eq, lpar, rpar	:: GenParser Char state ()

bar	= separator '|'
comma	= separator ','
eq	= separator '='

lpar
    = do
      char '('
      skipS0

rpar
    = do
      skipS0
      char ')'
      return ()


-- ------------------------------------------------------------
--
-- all chars but not a special substring

allBut		:: (GenParser Char state Char -> GenParser Char state String) -> String -> GenParser Char state String
allBut p str
    = allBut1 p (const True) str

allBut1		:: (GenParser Char state Char -> GenParser Char state String) -> (Char -> Bool) -> String -> GenParser Char state String
allBut1 p prd (c:rest)
    = p ( satisfy (\ x -> isXmlChar x && prd x && not (x == c) )
	  <|>
	  try ( do
		char c
		notFollowedBy ( do
				try (string rest)
				return c
			      )
		return c
	      )
	)

allBut1 _p _prd str
    = error ("allBut1 _ _ " ++ show str ++ " is undefined")

-- ------------------------------------------------------------
--
-- concatenate parse results

concRes		:: GenParser Char state [[a]] -> GenParser Char state [a]
concRes p
    = do
      sl <- p
      return (concat sl)

mkList		:: GenParser Char state a -> GenParser Char state [a]
mkList p
    = do
      r <- p
      return [r]

-- ------------------------------------------------------------
--
-- token parsers returning XmlTrees
--
-- ------------------------------------------------------------
--
-- Literals (2.3)

nameT		:: GenParser Char state XmlTree
nameT
    = do
      n <- name
      return (mkXDTDTree NAME [(a_name, n)] [])

nmtokenT	:: GenParser Char state XmlTree
nmtokenT
    = do
      n <- nmtoken
      return (mkXDTDTree NAME [(a_name, n)] [])


entityValueT	:: GenParser Char state XmlTrees
entityValueT
    =  do
       sl <- between dq dq (entityTokensT "%&\"")
       return sl
       <|>
       do
       sl <- between sq sq (entityTokensT "%&\'")
       return sl
       <?> "entity value (in quotes)"

entityTokensT	:: String -> GenParser Char state XmlTrees
entityTokensT notAllowed
    = many (entityCharT notAllowed)

entityCharT	:: String -> GenParser Char state XmlTree
entityCharT notAllowed
    = peReferenceT
      <|>
      charRefT
      <|>
      bypassedEntityRefT
      <|>
      ( do
	cs <- many1 (singleChar notAllowed)
	return (mkXTextTree cs)
      )

attrValueT	:: GenParser Char state XmlTrees
attrValueT
    = between dq dq (attrValueT' "<&\"")
      <|>
      between sq sq (attrValueT' "<&\'")
      <?> "attribute value (in quotes)"

attrValueT'	:: String -> GenParser Char state XmlTrees
attrValueT' notAllowed
    = many ( referenceT <|> singleCharsT notAllowed)

singleCharsT	:: String -> GenParser Char state XmlTree
singleCharsT notAllowed
    = do
      cs <- singleChars notAllowed
      return (mkXTextTree cs)

-- ------------------------------------------------------------
--
-- Character and Entity References (4.1)

referenceT	:: GenParser Char state XmlTree
referenceT
    = charRefT
      <|>
      entityRefT

charRefT	:: GenParser Char state XmlTree
charRefT
    = do
      i <- charRef
      return (mkXCharRefTree $! i)

entityRefT	:: GenParser Char state XmlTree
entityRefT
    = do
      n <- entityRef
      return (mkXEntityRefTree $! n)

bypassedEntityRefT	:: GenParser Char state XmlTree
bypassedEntityRefT
    = do
      n <- entityRef
      return (mkXTextTree ("&" ++ n ++ ";"))

peReferenceT	:: GenParser Char state XmlTree
peReferenceT
    = do
      r <- peReference
      return (mkXPERefTree r)

-- ------------------------------------------------------------


