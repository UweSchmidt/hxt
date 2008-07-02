-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG.XmlSchema.RegexParser
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   W3C XML Schema Regular Expression Parser

   This parser supports the full W3C standard, the
   complete grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>

-}

-- ------------------------------------------------------------

module Text.XML.HXT.RelaxNG.XmlSchema.RegexParser
    ( parseRegex )
where

import Control.Applicative ( liftA2 )
import Data.Maybe
import Text.ParserCombinators.Parsec

import Text.XML.HXT.DOM.Unicode

import Text.XML.HXT.RelaxNG.Unicode.Blocks
import Text.XML.HXT.RelaxNG.Unicode.CharProps
import Text.XML.HXT.RelaxNG.XmlSchema.Regex

-- ------------------------------------------------------------

parseRegex :: String -> Either String Regex
parseRegex
    = either (Left . show) Right
      .
      parse ( do
	      r <- regExp
	      eof
	      return r
	    ) ""

-- ------------------------------------------------------------

regExp	:: Parser Regex
regExp
    = do
      r1 <- branch
      rs <- many branch1
      return (foldr1 mkAlt $ r1:rs)
    where
    branch1
	= do
	  char '|'
	  branch

branch	:: Parser Regex
branch
    = do
      rs <- many piece
      return $ foldr mkSeq mkUnit rs

piece	:: Parser Regex
piece
    = do
      r <- atom
      quantifier r

quantifier	:: Regex -> Parser Regex
quantifier r
    = ( do
	char '?'
	return $ mkOpt r )
      <|>
      ( do
	char '*'
	return $ mkStar r )
      <|>
      ( do
	char '+'
	return $ mkRep 1 r )
      <|>
      ( do
	char '{'
	res <- quantity r
	char '}'
	return res
      )
      <|>
      ( return r )

quantity	:: Regex -> Parser Regex
quantity r
    = do
      lb <- many1 digit
      quantityRest r (read lb)

quantityRest	:: Regex -> Int -> Parser Regex
quantityRest r lb
    = ( do
	char ','
	ub <- many digit
	return ( if null ub
		 then mkRep lb r
		 else mkRng lb (read ub) r
	       )
      )
      <|>
      ( return $ mkRng lb lb r)

atom	:: Parser Regex
atom
    = char1
      <|>
      charClass
      <|>
      between (char '(') (char ')') regExp

char1	:: Parser Regex
char1
    = do
      c <- satisfy $ (`notElem` ".\\?*+{}()|[]")
      return $ mkSym1 c

charClass	:: Parser Regex
charClass
    = charClassEsc
      <|>
      charClassExpr
      <|>
      wildCardEsc

charClassEsc	:: Parser Regex
charClassEsc
    = do
      char '\\'
      ( singleCharEsc
	<|>
	multiCharEsc
	<|>
	catEsc
	<|>
	complEsc )

singleCharEsc	:: Parser Regex
singleCharEsc
    = do
      c <- singleCharEsc'
      return $ mkSym1 c

singleCharEsc'	:: Parser Char
singleCharEsc'
    = do
      c <- satisfy (`elem` "nrt\\|.?*+(){}-[]^")
      return $ maybe c id . lookup c . zip "ntr" $ "\n\r\t"

multiCharEsc	:: Parser Regex
multiCharEsc
    = do
      c <- satisfy (`elem` es)
      return $ mkSym . fromJust . lookup c $ pm
    where
    es = map fst pm
    pm = [ ('s',       isXmlSpaceChar		)
	 , ('S', not . isXmlSpaceChar		)
	 , ('i',       isXmlNameStartChar	)
	 , ('I', not . isXmlNameStartChar	)
	 , ('c',       isXmlNameChar		)
	 , ('C', not . isXmlNameChar		)
	 , ('d',       isDigit			)
	 , ('D', not . isDigit			)
	 , ('w', not . isNotWord		)
	 , ('W',       isNotWord		)
	 ]
    isDigit   = ('0' <=) &&& (<= '9')
    isNotWord = isUnicodeP |||
		isUnicodeZ |||
		isUnicodeC

catEsc	:: Parser Regex
catEsc
    = do
      char 'p'
      s <- between (char '{') (char '}') charProp
      return $ mkSym s

charProp	:: Parser (Char -> Bool)
charProp
    = isCategory
      <|>
      isBlock

isBlock		:: Parser (Char -> Bool)
isBlock
    = do
      string "Is"
      name <- many1 (satisfy legalChar)
      let b = lookup name codeBlocks
      if isJust b
	 then return $ let
		       (lb, ub) = fromJust b
		       in
		       (lb <=) &&& (<= ub)
	 else fail   $ "unknown Unicode code block " ++ show name
    where
    legalChar c	 = 'A' <= c && c <= 'Z' ||
		   'a' <= c && c <= 'z' ||
	           '0' <= c && c <= '9' ||
		   '-' == c

isCategory	:: Parser (Char -> Bool)
isCategory
    = do
      pr <- isCategory'
      return $ fromJust (lookup pr categories)

categories	:: [(String, Char -> Bool)]
categories
    = [ ("C",  isUnicodeC )
      , ("Cc", isUnicodeCc)
      , ("Cf", isUnicodeCf)
      , ("Co", isUnicodeCo)
      , ("Cs", isUnicodeCs)
      , ("L",  isUnicodeL )
      , ("Ll", isUnicodeLl)
      , ("Lm", isUnicodeLm)
      , ("Lo", isUnicodeLo)
      , ("Lt", isUnicodeLt)
      , ("Lu", isUnicodeLu)
      , ("M",  isUnicodeM )
      , ("Mc", isUnicodeMc)
      , ("Me", isUnicodeMe)
      , ("Mn", isUnicodeMn)
      , ("N",  isUnicodeN )
      , ("Nd", isUnicodeNd)
      , ("Nl", isUnicodeNl)
      , ("No", isUnicodeNo)
      , ("P",  isUnicodeP )
      , ("Pc", isUnicodePc)
      , ("Pd", isUnicodePd)
      , ("Pe", isUnicodePe)
      , ("Pf", isUnicodePf)
      , ("Pi", isUnicodePi)
      , ("Po", isUnicodePo)
      , ("Ps", isUnicodePs)
      , ("S",  isUnicodeS )
      , ("Sc", isUnicodeSc)
      , ("Sk", isUnicodeSk)
      , ("Sm", isUnicodeSm)
      , ("So", isUnicodeSo)
      , ("Z",  isUnicodeZ )
      , ("Zl", isUnicodeZl)
      , ("Zp", isUnicodeZp)
      , ("Zs", isUnicodeZs)
      ]

isCategory'	:: Parser String
isCategory'
    = ( foldr1 (<|>) . map (uncurry prop) $
	[ ('L', "ultmo")
	, ('M', "nce")
	, ('N', "dlo")
	, ('P', "cdseifo")
	, ('Z', "slp")
	, ('S', "mcko")
	, ('C', "cfon")
	]
      ) <?> "illegal Unicode character property"
    where
    prop c1 cs2
	= do
	  char c1
	  s2 <- option ""
		( do
		  c2 <- satisfy (`elem` cs2)
		  return [c2] )
	  return $ c1:s2

complEsc	:: Parser Regex
complEsc
    = do
      char 'P'
      s <- between (char '{') (char '}') charProp
      return $ mkSym (not . s)

charClassExpr	:: Parser Regex
charClassExpr
    = between (char '[') (char ']') charGroup

charGroup	:: Parser Regex
charGroup
    = do
      r <- ( negCharGroup	-- a ^ at beginning denotes negation, not start of posCharGroup
	     <|>
	     posCharGroup
	   )
      s <- option (mkZero "")	-- charClassSub
	   ( do
	     char '-'
	     charClassExpr
	   )
      return $ mkDif r s

posCharGroup	:: Parser Regex
posCharGroup
    = do
      rs <- many1 (charRange <|> charClassEsc)
      return $ foldr1 mkAlt rs

charRange	:: Parser Regex
charRange
    = try seRange
      <|>
      xmlCharIncDash

seRange	:: Parser Regex
seRange
    = do
      c1 <- charOrEsc'
      char '-'
      c2 <- charOrEsc'
      return $ mkSymRng c1 c2

charOrEsc'	:: Parser Char
charOrEsc'
    = satisfy (`notElem` "\\-[]")
      <|>
      singleCharEsc'

xmlCharIncDash	:: Parser Regex
xmlCharIncDash
    = do
      c <- satisfy (`notElem` "\\[]")
      return $ mkSym1 c

negCharGroup	:: Parser Regex
negCharGroup
    = do
      char '^'
      r <- posCharGroup
      return $ mkCompl r

wildCardEsc	:: Parser Regex
wildCardEsc
    = do
      char '.'
      return $ mkSym (`notElem` "\n\r")


-- ------------------------------------------------------------

(&&&)	:: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool)
(&&&)	= liftA2 (&&)

(|||)	:: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool)
(|||)	= liftA2 (||)

-- ------------------------------------------------------------
