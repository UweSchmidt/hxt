module Main
where

import Text.XML.HXT.Arrow

import Control.Monad.State.Strict

import Data.List

import System.IO			-- import the IO and commandline option stuff
import System.Environment

-- ----------------------------------------

main	:: IO ()
main
    = do
      pn <- getProgName
      (is : _) <- getArgs
      let i = (read is)::Int
      if "GenDoc" `isSuffixOf` pn
	 then main1 i
	 else if "ReadDoc" `isSuffixOf` pn
	      then main2 i
	      else main3 i

-- generate a document containing a binary tree of 2^i leafs (= 2^(i-1) XML elements)

main1	:: Int -> IO ()
main1 i
    = do
      runX (genDoc i (fn i))
      return ()

-- read a document containing a binary tree of 2^i leafs

main2	:: Int -> IO ()
main2 i
    = do
      [t] <- runX (readDoc (fn i))
      putStrLn ("maximum value in tree is " ++ show (foldT1 max t) ++ ", expected value was " ++ show ((2::Int)^i))

-- just to check how much memory is used for the tree

main3	:: Int -> IO ()
main3 i
    = do
      let t = mkBTree i
      let m = show . foldT1 max $ t
      putStrLn ("maximum value in tree is " ++ m ++ ", minimum value is " ++ show (foldT1 min t))
      return ()

fn	:: Int -> String
fn	= ("tree-" ++) . (++ ".xml") . reverse . take 4 . reverse . ((replicate 4 '0') ++ ) . show

genDoc	:: Int -> String -> IOSArrow b XmlTree
genDoc d out
    = constA (mkBTree d)
      >>>
      xpickleVal xpickle
      >>>
      putDoc out

readDoc	:: String -> IOSArrow b BTree
readDoc src
    = xunpickleDocument xpickle [ (a_tagsoup, v_1)
				, (a_remove_whitespace, v_1)
				, (a_encoding, isoLatin1)
				, (a_issue_warnings, v_0) ] src

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

foldT1	:: (Int -> Int -> Int) -> BTree -> Int
foldT1 _  (Leaf v)	= v
foldT1 op (Fork l r)	= foldT1 op l `op` foldT1 op r

-- ----------------------------------------

putDoc	:: String -> IOStateArrow s XmlTree XmlTree
putDoc dst
    = xshow getChildren
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