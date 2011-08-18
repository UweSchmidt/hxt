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
    , parseRegexNoCase
    )
where

import Data.Char                         ( isLower
                                         , isUpper
                                         , toLower
                                         , toUpper
                                         )

import Text.ParserCombinators.Parsec

import Text.Regex.XMLSchema.String.Regex


-- ------------------------------------------------------------

-- | parse a glob pattern

parseRegex :: String -> Regex
parseRegex
    = parseRegex' mkSymRng

parseRegexNoCase :: String -> Regex
parseRegexNoCase
    = parseRegex' mkNoCaseSymRng

parseRegex' :: (Char -> Char -> Regex) -> String -> Regex
parseRegex' mkS
    = either (mkZero . ("syntax error: " ++) . show) id
      .
      parse ( do
              r <- pattern mkS
              eof
              return r
            ) ""

-- ------------------------------------------------------------

pattern  :: (Char -> Char -> Regex) -> Parser Regex
pattern mkS
    = many part >>= return . mkSeqs
    where
      part :: Parser Regex
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

      wordList :: Parser Regex
      wordList
          = sepBy (many1 (noneOf ",}")) (char ',') >>= return . foldr mkAlt (mkZero "") . map mkWord'

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
                     return $ mkS c1 c2
            rest c1
                = option c1 (char '-' >> anyChar)

-- ------------------------------------------------------------

mkNoCaseSymRng :: Char -> Char -> Regex
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
