-- ------------------------------------------------------------

{- |
   Module     : Text.Regex.Glob.String.RegexParser
   Copyright  : Copyright (C) 2010- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   csh style Glob Pattern Parser for Regular Expressions
-}

-- ------------------------------------------------------------

module Text.Regex.Glob.String.RegexParser
    ( parseRegex
    )
where

import Text.ParserCombinators.Parsec

import Text.Regex.XMLSchema.String.Regex


-- ------------------------------------------------------------

-- | parse a glob pattern

parseRegex :: String -> Regex
parseRegex
    = either (mkZero . ("syntax error: " ++) . show) id
      .
      parse ( do
              r <- pattern
              eof
              return r
            ) ""

-- ------------------------------------------------------------

pattern  :: Parser Regex
pattern
    = many part >>= return . mkSeqs

part :: Parser Regex
part
    = ( many1 (noneOf "\\?*[{") >>= return . mkWord )
      <|>
      ( char '?' >> return mkDot )
      <|>
      ( char '*' >> return mkAll )
      <|>
      ( between (char '{') (char '}') wordList )
      <|>
      ( between (char '[') (char ']') charSet )
      <|>
      ( char '\\' >> anyChar >>= return . mkSym1 )

wordList :: Parser Regex
wordList
    = sepBy (many1 (noneOf ",}")) (char ',') >>= return . foldr mkAlt (mkZero "") . map mkWord

charSet :: Parser Regex
charSet
    = ( do p1 <- charSet' anyChar
           ps <- many $ charSet' (noneOf "]")
           return $ foldr mkAlt (mkZero "") (p1 : ps)
      )
    where
      charSet' cp
          = do c1 <- cp
               c2 <- rest c1
               return $ mkSymRng c1 c2
      rest c1
          = option c1 (char '-' >> anyChar)

-- ------------------------------------------------------------
