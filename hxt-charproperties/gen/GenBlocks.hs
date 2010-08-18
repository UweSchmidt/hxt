-- ------------------------------------------------------------

{- |
   Module     : GenBlocks
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Generator for Unicode Blocks

-}

-- ------------------------------------------------------------

module Main
where

import Control.Arrow

import Data.Char (isAlphaNum)

main :: IO ()
main
    = do
      c <- readFile "Blocks.txt"
      putStrLn $ genBlocks c

genBlocks       :: String -> String
genBlocks
    = lines
      >>> ( ( filter isBlock
              >>> map parseLine
            )
            &&&
            ( head
              >>> dropWhile (/= '-')
              >>> drop 1
              >>> reverse
              >>> drop 4
              >>> reverse
            )
          )
      >>> processBlockDefs
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
          , "   Module     : Data.Char.Properties.UnicodeBlocks"
          , "   Copyright  : Copyright (C) 2010- Uwe Schmidt"
          , "   License    : MIT"
          , ""
          , "   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)"
          , "   Stability  : stable"
          , "   Portability: portable"
          , ""
          , "   Unicode Code Blocks"
          , ""
          , "   don't edit this module"
          , "   it's generated from 'http:\\/\\/www.unicode.org\\/Public\\/UNIDATA\\/Blocks.txt'"
          , "-}"
          , ""
          , cmt
          , ""
          , "module Data.Char.Properties.UnicodeBlocks"
          , "  ( codeBlocks"
          , "  , elemCodeBlock"
          , "  , versionUnicode"
          ]

    header2
        = unlines
          [ "  )"
          , "where"
          , ""
          , cmt
          , ""
          ]
    header3
        = unlines
          [ "elemCodeBlock     :: Char -> String -> Bool"
          , "elemCodeBlock c b = maybe False (\\ (lb, ub) -> c >= lb && c <= ub) $ lookup b codeBlocks"
          , ""
          , "codeBlocks        :: [(String, (Char, Char))]"
          , "codeBlocks ="
          ]

    trailer
        = unlines
          [ cmt
          ]

    processBlockDefs (ls, vers)
        = header ++
          processExports (map (blockName . fst) ls) ++
          header2 ++
          processVersion vers ++
          processBlockTable ls ++
          ( "\n" ++ cmt ++ "\n\n" ) ++
          concatMap processBlockPredicate ls ++
          trailer

    processExports
        = join "\n  , "
          >>> (++ "\n")
          >>> ("  , " ++)

    processVersion vers
        = unlines
          [ "versionUnicode :: String"
          , "versionUnicode = " ++ show vers
          , ""
          ]

    processBlockTable ls
        = header3 ++ "    [ " ++ join "\n    , " (map entry ls) ++ "\n    ]\n"
        where
        entry (name, (lb, ub))
            = "( " ++ show name ++ ", ( " ++ hexChar lb ++ ", " ++ hexChar ub ++ ") )"

    processBlockPredicate (name', (lb, ub))
        = unlines
          [ name ++ "   :: Char -> Bool"
          , name ++ " c = c >= " ++ hexChar lb ++ " && c <= " ++ hexChar ub
          , ""
          ]
        where
        name = blockName name'

    parseLine l
            = (name, (lb, ub))
            where
            (rng, name') = break (==';') l
            (lb,ub')     = break (=='.') rng
            ub           = drop 2 ub'
            name         = filter legalChar . drop 1 $ name'
            legalChar c  = 'A' <= c && c <= 'Z' ||
                           'a' <= c && c <= 'z' ||
                           '0' <= c && c <= '9' ||
                           '-' == c
    blockName
        = ("is" ++) . filter isAlphaNum

join :: String -> [String] -> String
join js
    = foldr1 (\ x y -> x ++ js ++ y)

hexChar :: String -> String
hexChar x = "'\\x" ++ x ++ "'"

cmt :: String
cmt = "-- " ++ replicate 60 '-'
