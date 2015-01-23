{-# LANGUAGE BangPatterns#-}

-- ----------------------------------------

module Main(main)
where

import Text.Regex.XMLSchema.Generic

import Control.Arrow

import Data.Maybe

import System.IO                        -- import the IO and commandline option stuff
import System.Environment

-- ----------------------------------------

main    :: IO ()
main
    = do
      p  <- getProgName
      al <- getArgs
      let i = if null al
              then 4
              else (read . head $ al)::Int
      main' p i
    where
    main' p' = fromMaybe main1 . lookup (pn p') $ mpt
    mpt = [ ("REtest",     main1)
          , ("Copy",       main2 "copy"     (:[]))
          , ("Lines",      main2 "lines"    lines)
          , ("RElines",    main2 "relines"  relines)
          , ("SElines",    main2 "selines'" relines')
          , ("Words",      main2 "words"    words)
          , ("REwords",    main2 "rewords"  rewords)
          , ("SEwords",    main2 "sewords"  rewords')
          ]

-- ----------------------------------------

-- generate a document containing a binary tree of 2^i leafs (= 2^(i-1) XML elements)

main1   :: Int -> IO ()
main1 i
    = do
      genDoc i "REtest.hs" (fn i)
      return ()

-- ----------------------------------------

-- read a document containing a binary tree of 2^i leafs

main2   :: String -> (String -> [String]) -> Int -> IO ()
main2 ext lines' i
    = do
      hPutStrLn stderr "start processing"
      h  <- openFile (fn i) ReadMode
      c  <- hGetContents h
      let ls = lines' c
      o  <- openFile (fn i ++ "." ++ ext) WriteMode
      mapM_ (hPutStrLn o) ls
      hClose o
      hClose h
      hPutStrLn stderr "end  processing"

relines         :: String -> [String]
relines         = tokenize ".*"

relines'        :: String -> [String]
relines'        = tokenizeSubex "({line}.*)" >>> map snd

rewords         :: String -> [String]
rewords         = tokenize "\\S+"

rewords'        :: String -> [String]
rewords'        = tokenizeSubex "({word}\\S+)" >>> map snd

-- ----------------------------------------

pn      :: String -> String
pn      = reverse . takeWhile (/= '/') . reverse

fn      :: Int -> String
fn      = ("lines-" ++) . (++ ".txt") . reverse . take 4 . reverse . ((replicate 4 '0') ++ ) . show

-- ----------------------------------------

genDoc          :: Int -> String -> String -> IO ()
genDoc d inp outp
                = do
                  s <- readFile inp
                  let s' = take (2^d) . concat . repeat $ s
                  writeFile outp s'

-- ----------------------------------------
