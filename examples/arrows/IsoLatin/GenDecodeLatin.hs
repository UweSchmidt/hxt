module Main
where

import Numeric
import Data.Maybe
import Data.List
import System.IO

iso_8859_2        :: String -> String
iso_8859_2
    = map charToUni
    where
    charToUni c
        = found . dropWhile ((< c) . fst) $ ct
        where
        found ((c1, r1) :_)
            | c == c1 = r1
            | otherwise = c
    ct = [ ('\129', '\129')
         ]

main        :: IO ()
main
    = do
      fctList <- sequence . map genDecodeFct $ ([2..11] ++ [13..16])
      writeFile "DecodeIsoLatin.hs" (mod ++ (concat fctList))
    where
    mod = unlines
	  [ "module Text.XML.HXT.DOM.IsoLatinTables"
	  , "where"
	  , ""
	  ]

genDecodeFct	:: Int -> IO String
genDecodeFct i
    = do
      c <- readFile fileName
      return $ genFct fctName c
    where
    fctName  = "iso_8859_" ++ show i
    fileName = "ftp.unicode.org/Public/MAPPINGS/ISO8859/8859-" ++ show i ++ ".TXT"

genFct	:: String -> String -> String
genFct n
    = (fct ++) .
      ("    = [ " ++) .
      (++ "\n      ]\n\n") .
      foldl1 (\ x y -> x ++ "\n      , " ++ y) .
      ct
    where
    fct = unlines
	  [ n ++ "\t:: [(Char, Char)]"
	  , n
	  ]
    ct	:: String -> [String]
    ct
	= map toPair .
	  filter ne .
	  map ( map (toEnum . hexToInt) . take 2 . words) .
	  filter ("0x" `isPrefixOf`) .
	  lines

    hexToInt :: String -> Int
    hexToInt
	= fst . head . readHex . drop 2

    toPair	:: [Char] -> String
    toPair [i,j]
	= "(" ++ show i ++ ", " ++ show j ++ " )"
    toPair _
	= ""

    ne [c1,c2]
	| c1 /= c2 = True
    ne _
	= False
