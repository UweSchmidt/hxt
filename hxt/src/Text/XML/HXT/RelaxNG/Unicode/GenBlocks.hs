-- ------------------------------------------------------------

{- |
   Module     : GenBlocks
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Generator for Unicode Blocks

-}

-- ------------------------------------------------------------

module Main
where

import System.IO

main :: IO ()
main
    = do
      c <- readFile "Blocks.txt"
      putStrLn $ genBlocks c

genBlocks       :: String -> String
genBlocks
    = ( header ++)
      . (++ trailer)
      . processBlockDefs
      . filter isBlock
      . lines
    where
    isBlock l
        = take 1 l /= ['#']
          &&
          not (all (`elem` " \t") l)
    header
        = unlines
          [ cmt
          , ""
          , "{- |"
          , "   Module     : Text.XML.HXT.RelaxNG.Unicode.Blocks"
          , "   Copyright  : Copyright (C) 2005 Uwe Schmidt"
          , "   License    : MIT"
          , ""
          , "   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)"
          , "   Stability  : experimental"
          , "   Portability: portable"
          , "   Version    : $Id$"
          , ""
          , "   Unicode Code Blocks"
          , ""
          , "   don't edit this module"
          , "   it's generated from 'http:\\/\\/www.unicode.org\\/Public\\/UNIDATA\\/Blocks.txt'"
          , "-}"
          , ""
          , cmt
          , ""
          , "module Text.XML.HXT.RelaxNG.Unicode.Blocks"
          , "  ( codeBlocks )"
          , "where"
          , ""
          , cmt
          , ""
          , "codeBlocks        :: [(String, (Char, Char))]"
          , "codeBlocks ="
          ]
    trailer
        = unlines
          [ ""
          , cmt
          ]
    processBlockDefs ls
        = "    [ " ++ join "\n    , " (map entry ls) ++ "\n    ]\n"
        where
        join js
            = foldr1 (\ x y -> x ++ js ++ y)
        entry l
            = "( " ++ show name ++ ",\t( '\\x" ++ lb ++ "', '\\x" ++ ub ++ "') )"
            where
            (rng, name') = break (==';') l
            (lb,ub')     = break (=='.') rng
            ub           = drop 2 ub'
            name         = filter legalChar . drop 1 $ name'
            legalChar c  = 'A' <= c && c <= 'Z' ||
                           'a' <= c && c <= 'z' ||
                           '0' <= c && c <= '9' ||
                           '-' == c

cmt :: String
cmt = "-- " ++ replicate 60 '-'
