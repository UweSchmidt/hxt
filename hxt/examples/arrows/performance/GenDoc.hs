{-# LANGUAGE BangPatterns#-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- ----------------------------------------

module Main
where

import Text.XML.HXT.Core hiding (trace)
-- import Text.XML.HXT.TagSoup
-- import Text.XML.HXT.Expat

import Data.Char (isDigit)
import Data.List (foldl')

import Data.String.Unicode
    ( unicodeToXmlEntity
    )

import Control.Monad.State.Strict hiding (when)

import Control.DeepSeq
import Control.FlatSeq

import Data.Maybe

import System.IO  hiding (utf8)                      -- import the IO and commandline option stuff
import System.Environment

import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Data.Tree.Class as T
import Data.Tree.NTree.TypeDefs -- as T

import           Debug.Trace

-- ------------------------------------------------------------

main    :: IO ()
main
    = do
      p <- getProgName
      (is : _) <- getArgs
      let i = (read is)::Int
      main' p i
    where
    main' p' = fromMaybe main0 . lookup (pn p') $ mpt
    mpt = [ ("GenDoc",     main1)
          , ("ReadDoc",    main2)
          , ("PruneRight", main3 False)
          , ("PruneLeft",  main3 True)
          , ("MemTest",    main4 True)
          , ("MemTest1",   main4 False)
         ]

-- ----------------------------------------

-- generate a document containing a binary tree of 2^i leafs (= 2^(i-1) XML elements)

main1   :: Int -> IO ()
main1 i
    = runX (genDoc i (fn i))
      >> return ()

-- ----------------------------------------

-- read a document containing a binary tree of 2^i leafs

main2   :: Int -> IO ()
main2 i
    = do
      [x] <- runX (setTraceLevel 2
                   >>>
                   readDoc (fn i)
                   >>>
-- {-
                   traceMsg 1 "start rnfA"
                   >>>
                   rnfA this
                   >>>
-- -}
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

-- test on lazyness, is the whole tree read or only the first child of every child node?

main3   :: Bool -> Int -> IO ()
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

main4   :: Bool -> Int -> IO ()
main4 wnf i
    = do
      [x] <- runX ( setTraceLevel 1
                    >>>
                    traceMsg 1 ("generate tree of depth " ++ show i)
                    >>>
                    ( if wnf
                      then fromLA (genTree'' 0 i)
                      else fromLA (rnfA $ genTree   0 i)
                    )
                    >>>
                    perform ( traceMsg 1 ("deep hasAttrValue")
                              >>>
                              deep (hasName "leaf" >>> hasAttrValue "value" (== "1"))
                              >>>
                              getAttrValue "value"
                              >>>
                              arrIO putStrLn
                            )
                    >>>
                    perform ( traceMsg 1 ("deep hasAttrValue")
                              >>>
                              deep (hasName "leaf" >>> hasAttrValue "value" (== "1"))
                              >>>
                              getAttrValue "value"
                              >>>
                              arrIO putStrLn
                            )
                    {-
                    perform (traceMsg 1 ("write doc")
                             >>>
                             putDoc "./tmp.xml"
                            )
                    >>>
                    traceMsg 1 ("compute maximum and minimum")
                    >>>
                    fromLA ( foldBTree maximum &&& foldBTree minimum )  -- 2 traversals: complete tree in mem
                    >>>
                    arr2 (\ ma mi -> "maximum value = " ++ show ma ++ ", minimum = " ++ show mi )
                    >>>
                    traceValue 1 id
                     -}
                    >>>
                    traceMsg 1 "done"
                    >>>
                    constA "0"
                  )
      putStrLn x

foldBTree       :: ([Int] -> Int) -> LA XmlTree Int
foldBTree f
    = choiceA
      [ hasName "leaf" :-> ( getAttrValue "value" >>^ read )
      , hasName "fork" :-> ( (getChildren >>> foldBTree f) >. f )
      , this           :-> none
      ]

-- ----------------------------------------

-- just to check how much memory is used for the tree

main0   :: Int -> IO ()
main0 i
    = do
      let t = mkBTree i
      let m = show . foldT1 max $ t
      putStrLn ("maximum value in tree is " ++ m ++ ", minimum value is " ++ show (foldT1 min t))
      return ()

-- ----------------------------------------

pn      :: String -> String
pn      = reverse . takeWhile (/= '/') . reverse

fn      :: Int -> String
fn      = ("tree-" ++) . (++ ".xml") . reverse . take 4 . reverse . ((replicate 4 '0') ++ ) . show

-- ----------------------------------------

genTree :: Int -> Int -> LA XmlTree XmlTree
genTree !n !d
    | d == 0    = aelem "leaf" [sattr "value" (show (n + 1))]
    | otherwise = selem "fork" [ genTree (2*n)   (d-1)
                               , genTree (2*n+1) (d-1)
                               ]

-- ----------------------------------------

genTree'        :: Int -> Int -> LA XmlTree XmlTree
genTree' !n !d
    | d == 0    = aelem' "leaf" [sattr' "value" (show (n + 1))]
    | otherwise = selem' "fork" [ genTree' (2*n)   (d-1)
                                , genTree' (2*n+1) (d-1)
                                ]

-- ----------------------------------------

genTree''       :: Int -> Int -> LA XmlTree XmlTree
genTree'' !n !d
    | d == 0    = rwnfA $
                  -- trace ("+leaf " ++ show (n+1)) $
                  aelem "leaf" $
                  -- trace ("++leaf " ++ show (n+1)) $
                  [rwnf2A $
                   -- trace ("+valu " ++ show (n+1)) $
                   sattr "value" $
                   show $
                   -- trace ("++valu " ++ show (n+1)) $
                   (n + 1)
                  ]
    | otherwise = rwnfA $
                  -- trace ("+fork " ++ show (n+1)) $
                  selem "fork" $
                  -- trace ("++fork " ++ show (n+1)) $
                  [ genTree'' (2*n)   (d-1)
                  , genTree'' (2*n+1) (d-1)
                  ]

-- ----------------------------------------

-- (hopefully) strict constructors

mkText' s               = rwnf s `seq`
                          T.mkLeaf tn
    where
    tn = XN.mkText s

mkAttr' n al            = n        `seq`
                          rwnf al `seq`
                          T.mkTree an al
    where
    an = XN.mkAttrNode n

mkElem' n al cl         = n        `seq`
                          en       `seq`
                          rwnf al `seq`
                          rwnf cl `seq`
                          T.mkTree en cl
    where
    en = XN.mkElementNode n al

-- ----------------------------------------
-- strict arrows

mkElement'              :: String -> LA n XmlTree -> LA n XmlTree -> LA n XmlTree
mkElement' n af cf      = (listA af &&& listA cf)
                          >>>
                          arr2 (mkElem' (mkName n))

selem'                  :: String -> [LA n XmlTree] -> LA n XmlTree
selem' n cfs            = mkElement' n none (catA cfs)

aelem'                  :: String -> [LA n XmlTree] -> LA n XmlTree
aelem' n afs            = mkElement' n (catA afs) none

sattr'                  :: String -> String -> LA n XmlTree
sattr' an av            = constA (mkAttr' (mkName an) [mkText' av])

-- ----------------------------------------

genDoc          :: Int -> String -> IOSArrow b XmlTree
genDoc d out    = constA (let t = mkBTree d in rnf t `seq` t)
                  >>>
                  xpickleVal xpickle
                  >>>
{-
                  strictA
                  >>>
                  perform (writeBinaryValue (out ++ ".bin"))
                  >>>
                  readBinaryValue (out ++ ".bin")
                  >>>
                  strictA
                  >>>
-}
                  putDoc out

-- ----------------------------------------

readDoc :: String -> IOSArrow b XmlTree
readDoc src
    = readDocument [ withParseHTML yes
                   , withTrace 2
                   , withValidate no
                   , withInputEncoding isoLatin1
                   , withWarnings yes
                   , withStrictInput no
                   , withCanonicalize yes
                   , withRemoveWS no
                   -- , withExpat yes
                   -- , withTagSoup
                   ] src
{-
      >>>
      perform ( writeDocument [ withShowTree yes
                              , withOutputHTML
                              ] ""
              )
      >>>
      perform ( writeDocument [ withShowTree no
                              , withOutputHTML
                              ] ""
              )
-}
-- ----------------------------------------

unpickleTree    :: ArrowXml a => a XmlTree BTree
unpickleTree    = xunpickleVal xpickle

-- ----------------------------------------

pruneForkRight  :: LA XmlTree XmlTree
pruneForkRight
    = ( replaceChildren
        ( ( getChildren >>. take 1 )
          >>>
          pruneForkRight
        )
      ) `when` (hasName "fork")


pruneForkLeft   :: LA XmlTree XmlTree
pruneForkLeft
    = ( replaceChildren
        ( ( getChildren >>. drop 1 )
          >>>
          pruneForkLeft
        )
      ) `when` (hasName "fork")

-- ----------------------------------------

type Counter a  = State Int a

incr    :: Counter Int
incr    = do
          modify (+1)
          get

-- ----------------------------------------

data BTree      = Leaf Int
                | Fork BTree BTree
                  deriving (Show)

instance NFData BTree where
    rnf (Leaf i)	= rnf i
    rnf (Fork t1 t2)	= rnf t1 `seq` rnf t2

instance XmlPickler BTree where
    xpickle = xpAlt tag ps
        where
        tag (Leaf _     ) = 0
        tag (Fork _ _   ) = 1

        ps = [ xpWrap ( Leaf, \ (Leaf i) -> i)
               ( xpElem "leaf" $ xpAttr "value" $ xpInt ) -- xpWrap (const 0, const ">&\"äöü") $ xpText )
             , xpWrap ( uncurry Fork, \ (Fork l r) -> (l, r))
               ( xpElem "fork" $ xpPair xpickle xpickle )
             ]

-- ----------------------------------------

mkBTree         :: Int -> BTree
mkBTree depth   = evalState (mkT depth) 0
    where
    mkT :: Int -> Counter BTree
    mkT 0 = do
            i <- incr
            return (Leaf i)
    mkT n = do
            l <- mkT (n-1)
            r <- mkT (n-1)
            return (Fork l r)

bTreeToNTree	:: BTree -> NTree Int
bTreeToNTree (Leaf i)	= NTree i []
bTreeToNTree (Fork l r) = NTree j [l',r']
    where
    l' = bTreeToNTree l
    r' = bTreeToNTree r
    j  = T.getNode l' + T.getNode r'

mkNTree = bTreeToNTree . mkBTree

-- ----------------------------------------

foldT1  :: (Int -> Int -> Int) -> BTree -> Int
foldT1 _  (Leaf v)      = v
foldT1 op (Fork l r)    = foldT1 op l `op` foldT1 op r

-- ----------------------------------------

-- output is done with low level ops to write the
-- document i a lazy manner
-- adding an xml pi and encoding is done "by hand"
-- latin1 decoding is the identity, so please generate the
-- docs with latin1 encoding. Here ist done even with ASCCI
-- every none ASCII char is represented by a char ref (&nnn;)

putDoc  :: String -> IOStateArrow s XmlTree XmlTree
putDoc dst
    = writeDocument [ withOutputEncoding isoLatin1
                    , withOutputXML
                    ] dst

-- ----------------------------------------
{-

putDoc  :: String -> IOStateArrow s XmlTree XmlTree
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
      isStdout  = null dst || dst == "-"

      hPutDocument      :: (Handle -> IO()) -> IO()
      hPutDocument action
          | isStdout
              = action stdout
          | otherwise
              = do
                handle <- openBinaryFile dst WriteMode
                action handle
                hClose handle
-}
-- ----------------------------------------
