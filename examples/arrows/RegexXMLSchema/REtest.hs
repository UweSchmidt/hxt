{-# LANGUAGE BangPatterns#-}

-- ----------------------------------------

module Main(main)
where

import Text.XML.HXT.Arrow
import Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

import Text.XML.HXT.DOM.Unicode
    ( unicodeToXmlEntity
    )

import Control.Monad.State.Strict hiding (when)

import Data.List
import Data.Maybe

import System.IO			-- import the IO and commandline option stuff
import System.Environment

-- ----------------------------------------

main	:: IO ()
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
    mpt = [ ("REtest",	   main1)
	  , ("Copy",       main2 "copy"    (:[]))
	  , ("Lines",      main2 "lines"   lines)
	  , ("RElines",    main2 "relines" relines)
	  , ("Words",      main2 "words"   words)
	  , ("REwords",    main2 "rewords" rewords)
	  ]

-- ----------------------------------------

-- generate a document containing a binary tree of 2^i leafs (= 2^(i-1) XML elements)

main1	:: Int -> IO ()
main1 i
    = do
      runX (genDoc i (fn i))
      return ()

-- ----------------------------------------

-- read a document containing a binary tree of 2^i leafs

main2	:: String -> (String -> [String]) -> Int -> IO ()
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

relines		:: String -> [String]
relines		= tokenize "[^\n\r]*"

rewords		:: String -> [String]
rewords		= tokenize "[^ \t\n\r]+"

-- ----------------------------------------

pn	:: String -> String
pn	= reverse . takeWhile (/= '/') . reverse

fn	:: Int -> String
fn	= ("tree-" ++) . (++ ".xml") . reverse . take 4 . reverse . ((replicate 4 '0') ++ ) . show

-- ----------------------------------------

genDoc		:: Int -> String -> IOSArrow b XmlTree
genDoc d out    = constA (mkBTree d)
		  >>>
		  xpickleVal xpickle
		  >>>
		  indentDoc
		  >>>
		  putDoc out

-- ----------------------------------------

type Counter a	= State Int a

incr	:: Counter Int
incr	= do
	  modify (+1)
	  get

-- ----------------------------------------

data BTree	= Leaf Int
		| Fork BTree BTree
		  deriving (Show)

instance XmlPickler BTree where
    xpickle = xpAlt tag ps
	where
	tag (Leaf _	) = 0
	tag (Fork _ _	) = 1
	ps = [ xpWrap ( Leaf, \ (Leaf i) -> i)
	       ( xpElem "leaf" $ xpAttr "value" $ xpickle )

	     , xpWrap ( uncurry Fork, \ (Fork l r) -> (l, r))
	       ( xpElem "fork" $ xpPair xpickle xpickle )
	       ]

-- ----------------------------------------

mkBTree		:: Int -> BTree
mkBTree	depth	= evalState (mkT depth) 0

mkT	:: Int -> Counter BTree
mkT 0	= do
	  i <- incr
	  return (Leaf i)
mkT n	= do
	  l <- mkT (n-1)
	  r <- mkT (n-1)
	  return (Fork l r)

-- ----------------------------------------

-- output is done with low level ops to write the
-- document i a lasy manner
-- adding an xml pi and encoding is done "by hand"
-- latin1 decoding is the identity, so please generate the
-- docs with latin1 encoding. Here ist done even with ASCCI
-- every none ASCII char is represented by a char ref (&nnn;)

putDoc	:: String -> IOStateArrow s XmlTree XmlTree
putDoc dst
    = addXmlPi
      >>>
      addXmlPiEncoding isoLatin1
      >>>
      xshow getChildren
      >>>
      arr unicodeToXmlEntity
      >>>
      arrIO (\ s -> hPutDocument (\h -> hPutStrLn h s))
      >>>
      none
      where
      isStdout	= null dst || dst == "-"

      hPutDocument	:: (Handle -> IO()) -> IO()
      hPutDocument action
	  | isStdout
	      = action stdout
	  | otherwise
	      = do
		handle <- openFile dst WriteMode
		action handle
		hClose handle

-- ----------------------------------------
