{-# LANGUAGE FlexibleContexts #-}

-- ------------------------------------------------------------

{- |
   Module     : Text.Regex.XMLSchema.RegexParser
   Copyright  : Copyright (C) 2014- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   W3C XML Schema Regular Expression Parser

   This parser supports the full W3C standard, the
   complete grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>
   and extensions for all missing set operations, intersection,
   difference, exclusive or, interleave, complement

-}

-- ------------------------------------------------------------

module Text.Regex.XMLSchema.Generic.RegexParser
    ( parseRegex
    , parseRegexExt
    , parseRegex'
    , parseRegexExt'
    , parseContextRegex
    )
where

import           Data.Char.Properties.UnicodeBlocks
import           Data.Char.Properties.UnicodeCharProps
import           Data.Char.Properties.XMLCharProps

import           Data.List                               (isPrefixOf,
                                                          isSuffixOf)
import           Data.Maybe
import           Data.Set.CharSet

import           Text.ParserCombinators.Parsec
import           Text.Regex.XMLSchema.Generic.Regex
import           Text.Regex.XMLSchema.Generic.StringLike

-- ------------------------------------------------------------

-- | parse a standard W3C XML Schema regular expression

parseRegex              :: StringLike s => s -> GenRegex s
parseRegex              = parseRegex' . toString

parseRegex'             :: StringLike s => String -> GenRegex s
parseRegex'             = parseRegex'' regExpStd

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

parseRegexExt   :: StringLike s => s -> GenRegex s
parseRegexExt   = parseRegexExt' . toString

parseRegexExt'  :: StringLike s => String -> GenRegex s
parseRegexExt'  = parseRegex'' regExpExt

parseRegex'' :: StringLike s => Parser (GenRegex s) -> String -> GenRegex s
parseRegex'' regExp'
  = either (mkZero' . ("syntax error: " ++) . show) id
    . parse ( do
                 r <- regExp'
                 eof
                 return r
            ) ""

-- ------------------------------------------------------------

-- | parse a regular expression surrounded by contenxt spec
--
-- a leading @^@ denotes start of text,
-- a trailing @$@ denotes end of text,
-- a leading @\\<@ denotes word start,
-- a trailing @\\>@ denotes word end.
--
-- The 1. param ist the regex parser ('parseRegex' or 'parseRegexExt')

parseContextRegex :: StringLike s => (String -> GenRegex s) -> s -> GenRegex s
parseContextRegex parseRe re0
    = re'
    where
      parseAW = parseRegexExt' "(\\A\\W)?"
      parseWA = parseRegexExt' "(\\W\\A)?"

      re  = toString re0
      re' = mkSeqs . concat $ [ startContext
                              , (:[]) . parseRe $ re2
                              , endContext
                              ]
      (startContext, re1)
          | "^"   `isPrefixOf` re   = ([],             tail   re)
          | "\\<" `isPrefixOf` re   = ([parseAW],      drop 2 re)
          | otherwise               = ([mkStar mkDot],        re)
      (endContext, re2)
          | "$"   `isSuffixOf` re1  = ([],             init          re1)
          | "\\>" `isSuffixOf` re1  = ([parseWA],      init . init $ re1)
          | otherwise               = ([mkStar mkDot],               re1)

-- ------------------------------------------------------------

regExpExt  :: StringLike s => Parser (GenRegex s)
regExpExt  = branchList orElseList

regExpStd  :: StringLike s => Parser (GenRegex s)
regExpStd  = branchList seqListStd

branchList :: StringLike s => Parser (GenRegex s) -> Parser (GenRegex s)
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

orElseList      :: StringLike s => Parser (GenRegex s)
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

interleaveList  :: StringLike s => Parser (GenRegex s)
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

exorList        :: StringLike s => Parser (GenRegex s)
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

diffList        :: StringLike s => Parser (GenRegex s)
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

intersectList   :: StringLike s => Parser (GenRegex s)
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

seqListExt      :: StringLike s => Parser (GenRegex s)
seqListExt      = seqList' regExpLabel multiCharEscExt

seqListStd      :: StringLike s => Parser (GenRegex s)
seqListStd      = seqList' regExpStd multiCharEsc

seqList'        :: StringLike s => Parser (GenRegex s) -> Parser (GenRegex s) -> Parser (GenRegex s)
seqList' regExp' multiCharEsc'
    = do
      rs <- many piece
      return $ mkSeqs rs
    where
    -- piece :: StringLike s => Parser (GenRegex s)
    piece
        = do
          r <- atom
          quantifier r

    -- atom :: StringLike s => Parser (GenRegex s)
    atom
        = char1
          <|>
          charClass
          <|>
          between (char '(') (char ')') regExp'

    -- charClass :: StringLike s => Parser (GenRegex s)
    charClass
        = charClassEsc multiCharEsc'
          <|>
          charClassExpr multiCharEsc'
          <|>
          wildCardEsc



quantifier      :: StringLike s => GenRegex s -> Parser (GenRegex s)
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

quantity        :: StringLike s => GenRegex s -> Parser (GenRegex s)
quantity r
    = do
      lb <- many1 digit
      quantityRest r (read lb)

quantityRest    :: StringLike s => GenRegex s -> Int -> Parser (GenRegex s)
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

regExpLabel :: StringLike s => Parser (GenRegex s)
regExpLabel
    = do
      lab <- option id (between (char '{') (char '}') label')
      r    <- regExpExt
      return $ lab r
    where
    label'
        = do
          l <- many1 (satisfy isXmlNameChar)
          return $ mkBr' l

char1   :: StringLike s => Parser (GenRegex s)
char1
    = do
      c <- satisfy (`notElem` ".\\?*+{}()|[]")
      return $ mkSym1 c

charClassEsc    :: StringLike s => Parser (GenRegex s) -> Parser (GenRegex s)
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

singleCharEsc   :: StringLike s => Parser (GenRegex s)
singleCharEsc
    = do
      c <- singleCharEsc'
      return $ mkSym1 c

singleCharEsc'  :: Parser Char
singleCharEsc'
    = do
      c <- satisfy (`elem` "nrt\\|.?*+(){}-[]^")
      return $ maybe c id . lookup c . zip "ntr" $ "\n\r\t"

multiCharEscExt    :: StringLike s => Parser (GenRegex s)
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

multiCharEsc    :: StringLike s => Parser (GenRegex s)
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

catEsc  :: StringLike s => Parser (GenRegex s)
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

complEsc        :: StringLike s => Parser (GenRegex s)
complEsc
    = do
      _ <- char 'P'
      s <- between (char '{') (char '}') charProp
      return $ mkSym $ compCS s

charClassExpr   :: StringLike s => Parser (GenRegex s) -> Parser (GenRegex s)
charClassExpr multiCharEsc'
    = between (char '[') (char ']') charGroup
    where

    -- charGroup       :: StringLike s => Parser (GenRegex s)
    charGroup
        = do
          r <- ( negCharGroup       -- a ^ at beginning denotes negation, not start of posCharGroup
                 <|>
                 posCharGroup
               )
          s <- option (mkZero' "")   -- charClassSub
               ( do
                 _ <- char '-'
                 charClassExpr multiCharEsc'
               )
          return $ mkDiff r s

    -- posCharGroup    :: StringLike s => Parser (GenRegex s)
    posCharGroup
        = do
          rs <- many1 (charRange <|> charClassEsc multiCharEsc')
          return $ foldr1 mkAlt rs

    -- negCharGroup    :: StringLike s => Parser (GenRegex s)
    negCharGroup
        = do
          _ <- char '^'
          r <- posCharGroup
          return $ mkDiff mkDot r

charRange       :: StringLike s => Parser (GenRegex s)
charRange
    = try seRange
      <|>
      xmlCharIncDash

seRange :: StringLike s => Parser (GenRegex s)
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

xmlCharIncDash  :: StringLike s => Parser (GenRegex s)
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

wildCardEsc     :: StringLike s => Parser (GenRegex s)
wildCardEsc
    = do
      _ <- char '.'
      return . mkSym . compCS $ stringCS "\n\r"


-- ------------------------------------------------------------
