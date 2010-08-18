-- ------------------------------------------------------------

{- |
   Module     : Text.Regex.XMLSchema.String.RegexParser
   Copyright  : Copyright (C) 2010- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   W3C XML Schema Regular Expression Parser

   This parser supports the full W3C standard, the
   complete grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>
   and extensions for all missing set operations, intersection, difference, exclusive or, interleave, complement

-}

-- ------------------------------------------------------------

module Text.Regex.XMLSchema.String.RegexParser
    ( parseRegex
    , parseRegexExt
    )
where

import Data.Char.Properties.UnicodeBlocks
import Data.Char.Properties.UnicodeCharProps
import Data.Char.Properties.XMLCharProps

import Data.Maybe

import Data.Set.CharSet

import Text.ParserCombinators.Parsec

import Text.Regex.XMLSchema.String.Regex


-- ------------------------------------------------------------

-- | parse a standard W3C XML Schema regular expression

parseRegex 		:: String -> Regex
parseRegex 		= parseRegex' regExpStd

-- | parse an extended syntax W3C XML Schema regular expression
--
-- The Syntax of the W3C XML Schema spec is extended by
-- further useful set operations, like intersection, difference, exor.
-- Subexpression match becomes possible with \"named\" pairs of parentheses.
-- The multi char escape sequence \\a represents any Unicode char,
-- The multi char escape sequence \\A represents any Unicode word, (\\A = \\a*).
-- All syntactically wrong inputs are mapped to the Zero expression representing the
-- empty set of words. Zero contains as data field a string for an error message.
-- So error checking after parsing becomes possible by checking against Zero ('isZero' predicate)

parseRegexExt 	:: String -> Regex
parseRegexExt 	= parseRegex' regExpExt

parseRegex' 		:: Parser Regex -> String -> Regex
parseRegex' regExp'    	= either (mkZero . ("syntax error: " ++) . show) id
                          .
                          parse ( do
                                  r <- regExp'
                                  eof
                                  return r
                                ) ""

-- ------------------------------------------------------------

regExpExt  :: Parser Regex
regExpExt  = branchList orElseList

regExpStd  :: Parser Regex
regExpStd  = branchList seqListStd

branchList      :: Parser Regex -> Parser Regex
branchList exParser
    = do
      r1 <- exParser
      rs <- many branchList1
      return (foldr1 mkAlt $ r1:rs)     -- union is associative, so we use right ass.
                                        -- as with seq, alt and exor
    where
    branchList1
        = do
          _ <- char '|'
          exParser

orElseList      :: Parser Regex
orElseList
    = do
      r1 <- interleaveList
      rs <- many orElseList1
      return (foldr1 mkElse $ r1:rs)    -- orElse is associative, so we choose right ass.
                                        -- as with seq and alt ops
    where
    orElseList1
        = do
          _ <- try (string "{|}")
          interleaveList

interleaveList  :: Parser Regex
interleaveList
    = do
      r1 <- exorList
      rs <- many interleaveList1
      return (foldr1 mkInterleave $ r1:rs)      -- interleave is associative, so we choose right ass.
                                                -- as with seq and alt ops
    where
    interleaveList1
        = do
          _ <- try (string "{:}")
          exorList

exorList        :: Parser Regex
exorList
    = do
      r1 <- diffList
      rs <- many exorList1
      return (foldr1 mkExor $ r1:rs)    -- exor is associative, so we choose right ass.
    where
    exorList1
        = do
          _ <- try (string "{^}")
          diffList

diffList        :: Parser Regex
diffList
    = do
      r1 <- intersectList
      rs <- many diffList1
      return (foldl1 mkDiff $ r1:rs)    -- diff is not associative, so we choose left ass.
    where
    diffList1
        = do
          _ <- try (string "{\\}")
          intersectList

intersectList   :: Parser Regex
intersectList
    = do
      r1 <- seqListExt
      rs <- many intersectList1
      return (foldr1 mkIsect $ r1:rs)
    where
    intersectList1
        = do
          _ <- try (string "{&}")
          seqListExt

seqListExt	:: Parser Regex
seqListExt	= seqList' regExpLabel multiCharEscExt

seqListStd	:: Parser Regex
seqListStd	= seqList' regExpStd multiCharEsc

seqList'        :: Parser Regex -> Parser Regex -> Parser Regex
seqList' regExp' multiCharEsc'
    = do
      rs <- many piece
      return $ mkSeqs rs
    where
    piece           :: Parser Regex
    piece
        = do
          r <- atom
          quantifier r

    atom    :: Parser Regex
    atom
        = char1
          <|>
          charClass
          <|>
          between (char '(') (char ')') regExp'

    charClass       :: Parser Regex
    charClass
        = charClassEsc multiCharEsc'
          <|>
          charClassExpr multiCharEsc'
          <|>
          wildCardEsc



quantifier      :: Regex -> Parser Regex
quantifier r
    = ( do
        _ <- char '?'
        return $ mkOpt r )
      <|>
      ( do
        _ <- char '*'
        return $ mkStar r )
      <|>
      ( do
        _ <- char '+'
        return $ mkRep 1 r )
      <|>
      try ( do
            _ <- char '{'
            res <- quantity r
            _ <- char '}'
            return res
          )
      <|>
      ( return r )

quantity        :: Regex -> Parser Regex
quantity r
    = do
      lb <- many1 digit
      quantityRest r (read lb)

quantityRest    :: Regex -> Int -> Parser Regex
quantityRest r lb
    = ( do
        _ <- char ','
        ub <- many digit
        return ( if null ub
                 then mkRep lb r
                 else mkRng lb (read ub) r
               )
      )
      <|>
      ( return $ mkRng lb lb r)

regExpLabel :: Parser Regex
regExpLabel
    = do
      lab <- option id (between (char '{') (char '}') label')
      r    <- regExpExt
      return $ lab r
    where
    label'
        = do
          l <- many1 (satisfy isXmlNameChar)
          return $ mkBr l

char1   :: Parser Regex
char1
    = do
      c <- satisfy (`notElem` ".\\?*+{}()|[]")
      return $ mkSym1 c

charClassEsc    :: Parser Regex -> Parser Regex
charClassEsc multiCharEsc'
    = do
      _ <- char '\\'
      ( singleCharEsc
        <|>
        multiCharEsc'
        <|>
        catEsc
        <|>
        complEsc )

singleCharEsc   :: Parser Regex
singleCharEsc
    = do
      c <- singleCharEsc'
      return $ mkSym1 c

singleCharEsc'  :: Parser Char
singleCharEsc'
    = do
      c <- satisfy (`elem` "nrt\\|.?*+(){}-[]^")
      return $ maybe c id . lookup c . zip "ntr" $ "\n\r\t"

multiCharEscExt    :: Parser Regex
multiCharEscExt
    = multiCharEsc
      <|>
      ( do                      -- extension: \a represents the whole alphabet inclusive newline chars: \a == .|\n|\r
        _ <- char 'a'
        return mkDot )
      <|>
      ( do                      -- extension: \A represents all words: \A == \a* or \A == (.|\n|\r)*
        _ <- char 'A'
        return mkAll )

multiCharEsc    :: Parser Regex
multiCharEsc
    = ( do
        c <- satisfy (`elem` es)
        return $ mkSym . fromJust . lookup c $ pm )
    where
    es = map fst pm
    pm = [ ('s',        charPropXmlSpaceChar          )
         , ('S', compCS charPropXmlSpaceChar          )
         , ('i',        charPropXmlNameStartChar      )
         , ('I', compCS charPropXmlNameStartChar      )
         , ('c',        charPropXmlNameChar           )
         , ('C', compCS charPropXmlNameChar           )
         , ('d',        charPropDigit                 )
         , ('D', compCS charPropDigit                 )
         , ('w', compCS charPropNotWord               )
         , ('W',        charPropNotWord               )
         ]
    charPropDigit   = rangeCS '0' '9'
    charPropNotWord = charPropUnicodeP
                      `unionCS`
                      charPropUnicodeZ
                      `unionCS`
                      charPropUnicodeC

catEsc  :: Parser Regex
catEsc
    = do
      _ <- char 'p'
      s <- between (char '{') (char '}') charProp
      return $ mkSym s

charProp        :: Parser CharSet
charProp
    = isCategory
      <|>
      isBlock

isBlock         :: Parser CharSet
isBlock
    = do
      _ <- string "Is"
      name <- many1 (satisfy legalChar)
      case lookup name codeBlocks of
        Just b  -> return $ uncurry rangeCS b
        Nothing -> fail $ "unknown Unicode code block " ++ show name
    where
    legalChar c  = 'A' <= c && c <= 'Z' ||
                   'a' <= c && c <= 'z' ||
                   '0' <= c && c <= '9' ||
                   '-' == c

isCategory      :: Parser CharSet
isCategory
    = do
      pr <- isCategory'
      return $ fromJust (lookup pr categories)

categories      :: [(String, CharSet)]
categories
    = [ ("C",  charPropUnicodeC )
      , ("Cc", charPropUnicodeCc)
      , ("Cf", charPropUnicodeCf)
      , ("Co", charPropUnicodeCo)
      , ("Cs", charPropUnicodeCs)
      , ("L",  charPropUnicodeL )
      , ("Ll", charPropUnicodeLl)
      , ("Lm", charPropUnicodeLm)
      , ("Lo", charPropUnicodeLo)
      , ("Lt", charPropUnicodeLt)
      , ("Lu", charPropUnicodeLu)
      , ("M",  charPropUnicodeM )
      , ("Mc", charPropUnicodeMc)
      , ("Me", charPropUnicodeMe)
      , ("Mn", charPropUnicodeMn)
      , ("N",  charPropUnicodeN )
      , ("Nd", charPropUnicodeNd)
      , ("Nl", charPropUnicodeNl)
      , ("No", charPropUnicodeNo)
      , ("P",  charPropUnicodeP )
      , ("Pc", charPropUnicodePc)
      , ("Pd", charPropUnicodePd)
      , ("Pe", charPropUnicodePe)
      , ("Pf", charPropUnicodePf)
      , ("Pi", charPropUnicodePi)
      , ("Po", charPropUnicodePo)
      , ("Ps", charPropUnicodePs)
      , ("S",  charPropUnicodeS )
      , ("Sc", charPropUnicodeSc)
      , ("Sk", charPropUnicodeSk)
      , ("Sm", charPropUnicodeSm)
      , ("So", charPropUnicodeSo)
      , ("Z",  charPropUnicodeZ )
      , ("Zl", charPropUnicodeZl)
      , ("Zp", charPropUnicodeZp)
      , ("Zs", charPropUnicodeZs)
      ]

isCategory'     :: Parser String
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
          _ <- char c1
          s2 <- option ""
                ( do
                  c2 <- satisfy (`elem` cs2)
                  return [c2] )
          return $ c1:s2

complEsc        :: Parser Regex
complEsc
    = do
      _ <- char 'P'
      s <- between (char '{') (char '}') charProp
      return $ mkSym $ compCS s

charClassExpr   :: Parser Regex -> Parser Regex
charClassExpr multiCharEsc'
    = between (char '[') (char ']') charGroup
    where

    charGroup       :: Parser Regex
    charGroup
        = do
          r <- ( negCharGroup       -- a ^ at beginning denotes negation, not start of posCharGroup
                 <|>
                 posCharGroup
               )
          s <- option (mkZero "")   -- charClassSub
               ( do
                 _ <- char '-'
                 charClassExpr multiCharEsc'
               )
          return $ mkDiff r s

    posCharGroup    :: Parser Regex
    posCharGroup
        = do
          rs <- many1 (charRange <|> charClassEsc multiCharEsc')
          return $ foldr1 mkAlt rs

    negCharGroup    :: Parser Regex
    negCharGroup
        = do
          _ <- char '^'
          r <- posCharGroup
          return $ mkDiff mkDot r

charRange       :: Parser Regex
charRange
    = try seRange
      <|>
      xmlCharIncDash

seRange :: Parser Regex
seRange
    = do
      c1 <- charOrEsc'
      _ <- char '-'
      c2 <- charOrEsc'
      return $ mkSymRng c1 c2

charOrEsc'      :: Parser Char
charOrEsc'
    = ( do
        _ <- char '\\'
        singleCharEsc'
      )
      <|>
      satisfy (`notElem` "\\-[]")

xmlCharIncDash  :: Parser Regex
xmlCharIncDash
    = try ( do                          -- dash is only allowed if not followed by a [, else charGroup differences do not parse correctly
            _ <- char '-'
            notFollowedBy (char '[')
            return $ mkSym1 '-'
          )
      <|>
      ( do
        c <- satisfy (`notElem` "-\\[]")
        return $ mkSym1 c
      )

wildCardEsc     :: Parser Regex
wildCardEsc
    = do
      _ <- char '.'
      return . mkSym . compCS $ stringCS "\n\r"


-- ------------------------------------------------------------
