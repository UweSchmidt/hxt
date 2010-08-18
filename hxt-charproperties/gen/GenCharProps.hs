-- ------------------------------------------------------------

{- |
   Module     : GenCharProps
   Copyright  : Copyright (C) 2010 - Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Generator for Unicode Character Properties

-}

-- ------------------------------------------------------------

module Main
where

import qualified Data.Map  as M
import qualified Data.List as L

main :: IO()
main
    = do
      c <- readFile "UnicodeData.txt"
      putStr $ genCharProps c

genCharProps    :: String -> String
genCharProps
    = unlines
      . processCharProps
      . concatMap parseCharDescr
      . lines
    where
    parseCharDescr      :: String -> [(String, Char)]
    parseCharDescr l
        | length cols < 3
            = []
        | otherwise
            =  [(nn,c),(n,c)]
        where
        cols = columns (== ';') l
        c    :: Char
        c    = read ("'\\x" ++ head  cols ++ "'")
        nn   = cols !! 2
        n    = take 1 nn
    processCharProps l
        = ( header1
            ++
            [ genExp . map fst $ l1 ]
            ++
            header2
            ++
            map (uncurry genPred) l1
          )
        where
        l1 = mkRng . M.toAscList . mkCharMap $ l

header1, header2 :: [String]
header1
    =  [ cmt
       , ""
       , "{- |"
       , "   Module     : Data.Char.Properties.UnicodeCharProps"
       , "   Copyright  : Copyright (C) 2010 - Uwe Schmidt"
       , ""
       , "   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)"
       , "   Stability  : stable"
       , "   Portability: portable"
       , ""
       , "   Unicode character properties"
       , ""
       , "   don't edit this module"
       , "   it's generated from 'http:\\/\\/www.unicode.org\\/Public\\/UNIDATA\\/UnicodeData.txt'"
       , ""
       , "-}"
       , ""
       , cmt
       , ""
       , "module Data.Char.Properties.UnicodeCharProps"
       ]

header2
    = [ "where"
      , ""
      , cmt
      , ""
      , "isInList        :: Char -> [(Char, Char)] -> Bool"
      , "isInList i      ="
      , "   foldr (\\ (lb, ub) b -> i >= lb && (i <= ub || b)) False"
      , ""
      , cmt
      , ""
      ]

cmt :: String
cmt = "-- " ++ replicate 60 '-'

columns :: (Char -> Bool) -> String -> [String]
columns _ [] = []
columns p xs
    = c : columns p (drop 1 r)
    where
    (c, r) = break p xs

type CharMap    = M.Map String [Char]

mkCharMap       :: [(String, Char)] -> CharMap
mkCharMap
    = foldl ins M.empty
    where
    ins :: CharMap -> (String, Char) -> CharMap
    ins m (k, v)
        = M.insertWith (++) k [v] m

mkRng   :: [(String,[Char])] -> [(String,[(Char, Char)])]
mkRng l
    = zip (map fst l) (map (charRngs . L.sort . snd) l)

charRngs        :: [Char] -> [(Char, Char)]
charRngs []     = []
charRngs (x:xs) = charRng x xs
                  where
                  charRng y []          = (x,y) : []
                  charRng y xs'@(x1:xs1)
                      | x1 == succ y    = charRng x1 xs1
                      | otherwise       = (x,y) : charRngs xs'

genPred :: String -> [(Char, Char)] -> String
genPred n rngs
    = unlines $
      [ "isUnicode" ++ n ++ " :: Char -> Bool"
      , "isUnicode" ++ n ++ " c"
      , "  = isInList c charProp" ++ n
      , ""
      , "charProp" ++ n ++ " :: [(Char, Char)]"
      , "charProp" ++ n
      , "  = [ " ++ (join "\n    , " . map show) rngs
      , "    ]"
      , ""
      , cmt
      ]

join    :: String -> [String] -> String
join js
    = foldr1 (\ x y -> x ++ js ++ y)

genExp  :: [String] -> String
genExp ns
    = "  ( " ++ join "\n  , " (map ("isUnicode" ++) ns ++ map ("charProp" ++) ns) ++ "\n  )"
