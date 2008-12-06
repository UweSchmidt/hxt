{-# LANGUAGE BangPatterns#-}

-- ----------------------------------------

module Main
where

import Text.XML.HXT.Arrow

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
      p <- getProgName
      (is : _) <- getArgs
      let i = (read is)::Int
      main' p i
    where
    main' p' = fromMaybe main0 . lookup (pn p') $ mpt
    mpt = [ ("GenDoc",	   main1)
	  , ("ReadDoc",    main2)
	  , ("PruneRight", main3 False)
	  , ("PruneLeft",  main3 True)
	  , ("MemTest",    main4)
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

main2	:: Int -> IO ()
main2 i
    = do
      [x] <- runX (readDoc (fn i)
		   >>>
		   traceMsg 1 "start unpickle"
		   >>>
		   unpickleTree
		   >>>
		   traceMsg 1 "start fold"
		   >>>
		   arr (foldT1 max)
		  )
      putStrLn ( "maximum value in tree is " ++ show x ++
		 ", expected value was " ++ show ((2::Int)^i)
	       )

-- ----------------------------------------

-- test on lasyness, is the whole tree read or only the first child of every child node?

main3	:: Bool -> Int -> IO ()
main3 l i
    = do
      [t] <- runX ( readDoc (fn i)
		    >>>
		    fromLA (xshow ( getChildren
				    >>>
				    if l then pruneForkLeft else pruneForkRight
				  )
			   )
		  )
      putStrLn ("pruned binary tree is : " ++ show t)

-- ----------------------------------------

main4	:: Int -> IO ()
main4 i
    = do
      [x] <- runX ( setTraceLevel 1
		    >>>
		    traceMsg 1 ("generate tree of depth " ++ show i)
		    >>>
		    fromLA (genTree 0 i)
		    >>>
		    traceMsg 1 ("compute maximum and minimum")
		    >>>
		    fromLA ( foldBTree maximum &&& foldBTree minimum )	-- 2 traversals: complete tree in mem
		    >>>
		    arr2 (\ ma mi -> "maximum value = " ++ show ma ++ ", minimum = " ++ show mi )
		    >>>
		    traceValue 1 id
		  )
      putStrLn x

foldBTree	:: ([Int] -> Int) -> LA XmlTree Int
foldBTree f
    = choiceA
      [ hasName "leaf" :-> ( getAttrValue "value" >>^ read )
      , hasName "fork" :-> ( (getChildren >>> foldBTree f) >. f )
      , this           :-> none
      ]

-- ----------------------------------------

-- just to check how much memory is used for the tree

main0	:: Int -> IO ()
main0 i
    = do
      let t = mkBTree i
      let m = show . foldT1 max $ t
      putStrLn ("maximum value in tree is " ++ m ++ ", minimum value is " ++ show (foldT1 min t))
      return ()

-- ----------------------------------------

pn	:: String -> String
pn	= reverse . takeWhile (/= '/') . reverse

fn	:: Int -> String
fn	= ("tree-" ++) . (++ ".xml") . reverse . take 4 . reverse . ((replicate 4 '0') ++ ) . show

-- ----------------------------------------

genTree	:: Int -> Int -> LA XmlTree XmlTree
genTree !n !d
    | d == 0	= aelem "leaf" [sattr "value" (show (n + 1))]
    | otherwise	= selem "fork" [ genTree (2*n)   (d-1)
			       , genTree (2*n+1) (d-1)
			       ]

-- ----------------------------------------

genDoc		:: Int -> String -> IOSArrow b XmlTree
genDoc d out    = constA (mkBTree d)
		  >>>
		  xpickleVal xpickle
		  >>>
		  putDoc out

-- ----------------------------------------

readDoc	:: String -> IOSArrow b XmlTree
readDoc src
    = readDocument [ (a_tagsoup, v_1)
		   , (a_parse_xml, v_1)
		   , (a_remove_whitespace, v_1)
		   , (a_encoding, isoLatin1)
		   , (a_issue_warnings, v_0)
		   , (a_trace, v_1)
		   , (a_strict_input, v_0)
		   ] src
      >>>
      processChildren (isElem `guards` this)

-- ----------------------------------------

unpickleTree	:: ArrowXml a => a XmlTree BTree
unpickleTree	= xunpickleVal xpickle

-- ----------------------------------------

pruneForkRight	:: LA XmlTree XmlTree
pruneForkRight
    = ( replaceChildren
	( ( getChildren >>. take 1 )
	  >>>
	  pruneForkRight
	)
      ) `when` (hasName "fork")


pruneForkLeft	:: LA XmlTree XmlTree
pruneForkLeft
    = ( replaceChildren
	( ( getChildren >>. drop 1 )
	  >>>
	  pruneForkLeft
	)
      ) `when` (hasName "fork")

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
