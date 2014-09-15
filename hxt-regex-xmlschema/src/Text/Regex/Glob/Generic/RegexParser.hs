-- ------------------------------------------------------------

{- |
   Copyright  : Copyright (C) 2014- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   csh style Glob Pattern Parser for Regular Expressions
-}

-- ------------------------------------------------------------

module Text.Regex.Glob.Generic.RegexParser
    ( parseRegex
    , parseRegexNoCase
    )
where

import Data.Char                         ( isLower
                                         , isUpper
                                         , toLower
                                         , toUpper
                                         )

import Text.ParserCombinators.Parsec
import Text.Regex.XMLSchema.Generic.Regex
import Text.Regex.XMLSchema.Generic.StringLike

-- ------------------------------------------------------------

-- | parse a glob pattern

parseRegex :: StringLike s => s -> GenRegex s
parseRegex
    = parseRegex' mkSymRng . toString

parseRegexNoCase :: StringLike s => s -> GenRegex s
parseRegexNoCase
    = parseRegex' mkNoCaseSymRng . toString

parseRegex' :: StringLike s => (Char -> Char -> GenRegex s) -> String -> GenRegex s
parseRegex' mkS
    = either (mkZero' . ("syntax error: " ++) . show) id
      .
      parse ( do
              r <- pattern mkS
              eof
              return r
            ) ""

-- ------------------------------------------------------------

pattern  :: StringLike s => (Char -> Char -> GenRegex s) -> Parser (GenRegex s)
pattern mkS
    = many part >>= return . mkSeqs
    where
      -- part :: Parser (GenRegex s)
      part
          = ( many1 (noneOf "\\?*[{") >>= return . mkWord' )
            <|>
            ( char '?' >> return mkDot )
            <|>
            ( char '*' >> return mkAll )
            <|>
            ( between (char '{') (char '}') wordList )
            <|>
            ( between (char '[') (char ']') charSet )
            <|>
            ( do c <- char '\\' >> anyChar
                 return $ mkS c c
            )
      mkWord'
          = mkSeqs . map (\ c -> mkS c c)

      -- wordList :: Parser (GenRegex s)
      wordList
          = sepBy (many1 (noneOf ",}")) (char ',')
            >>= return . foldr mkAlt (mkZero' "") . map mkWord'

      -- charSet :: Parser (GenRegex s)
      charSet
          = ( do p1 <- charSet' anyChar
                 ps <- many $ charSet' (noneOf "]")
                 return $ foldr mkAlt (mkZero' "") (p1 : ps)
            )
          where
            charSet' cp
                = do c1 <- cp
                     c2 <- rest c1
                     return $ mkS c1 c2
            rest c1
                = option c1 (char '-' >> anyChar)

-- ------------------------------------------------------------

mkNoCaseSymRng :: StringLike s => Char -> Char -> GenRegex s
mkNoCaseSymRng c1 c2
    | isLower c1
      &&
      isLower c2
          = mkAlt (mkSymRng (toUpper c1) (toUpper c2)) (mkSymRng c1 c2)
    | isUpper c1
      &&
      isUpper c2
          = mkAlt (mkSymRng (toLower c1) (toLower c2)) (mkSymRng c1 c2)
    | otherwise
        = mkSymRng c1 c2

-- ------------------------------------------------------------
