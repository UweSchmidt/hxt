module Zipper
where

-- import Control.Arrow.ListArrows

import Data.Tree.Class                 ( formatTree )
import Data.Tree.NTree.TypeDefs        ( NTree(..) )
import Data.Tree.NTree.Zipper.TypeDefs ( NTZipper )

import Text.XML.HXT.Core

-- ------------------------------------------------------------

type TreeOfInt = NTree Int
type NavTreeOfInt = NTZipper Int
type NavArrow = LA NavTreeOfInt NavTreeOfInt

t0 :: TreeOfInt
t0 = NTree 1 [NTree 11 []
	     ,NTree 12 [NTree 121 [NTree 1211 []
                                  ,NTree 1212 []
                                  ]
		       ,NTree 122 [NTree 1221 []]
		       ]
             ,NTree 13 []
             ,NTree 14 [NTree 141 []]
	    ]

t2 :: NavArrow
t2 = processTopDown (changeNode (+1))

t3 :: NavArrow
t3 = moveDown >>> moveRight >>> (change $< parNode) >>> moveUp
    where
    parNode  = moveUp >>> getNode
    change x = changeNode (+(5000 * x)) 

t4  ::  Int -> NavArrow
t4 n = ( descendantOrSelfAxis `moveTo` hasNode (== n)
         >>>
         setNode 42
         >>>
         moveToRoot
       )
       `orElse` this

nn :: Int -> NavArrow
nn n
    = setNode n
      >>> ( ( moveOn descendantOrFollowingAxis >>> nn (n+1) )
            `orElse`
            moveToRoot
          )
       
nnn :: Int -> NavArrow
nnn n
    = ( moveTo descendantOrFollowingAxis (hasNode odd)
        >>>
        setNode n
        >>>
        nnn (n + 1)
      )
      `orElse` moveToRoot

nnn' = loop' (\ n -> (descendantOrFollowingAxis >>> hasNode odd >>> setNode n))
            (1+)
            (1::Int)

loop'	:: (a -> NavArrow) -> (a -> a) -> a -> NavArrow
loop' axis inc = cont
    where
    cont acc = ( moveOn (axis acc)
                 >>>
                 cont (inc acc)
               )
               `orElse` moveToRoot

tt :: NavArrow -> TreeOfInt -> [TreeOfInt]
tt f = runLA $ withNavi f

ex :: NavArrow -> TreeOfInt -> IO ()
ex f = putStrLn . formatTree show . head . tt f

e :: (Show a ) => [NTree a] -> IO ()
e = putStrLn . formatTree show . head

tab1 :: String -> String -> Int -> Int -> String
tab1 capt name rows cols = "<table id=" ++ show name ++ ">" ++ cap ++ trs ++ "</table>"
    where
    cap
        | null capt	= ""
        | otherwise	= "<caption>" ++ capt ++ "</caption>"

    trs   = concat ["<tr>" ++ tds i ++ "</tr>" | i <- [1..rows]]
    tds i = concat ["<td>" ++ show i ++ "." ++ show j ++ "</td>" | j <- [1..cols]]

html1 :: String -> String -> String
html1 t b
    = unlines $
      [ "<html>"
      , "  <head><title>" ++ t ++ "</title></head>"
      , "  <body>"
      , b
      , "  </body>"
      , "</html>"
      ]

doc1 :: ArrowXml a => a b XmlTree
doc1 = root [] [constA (html1 "example1" (tab1 "1. Table" "table1" 5 3)) >>> xread]

colorizeRows :: String -> String -> String -> LA XmlNavTree XmlNavTree
colorizeRows name dark light
    = ( moveTo descendantAxis (remNavi >>> hasAttrValue "id" (== name))
        >>>
        colorize dark light childAxis
        >>>
        moveToRoot
      )
      `orElse` this
    where
    colorize c1 c2 axis
        = ( moveTo axis (remNavi >>> hasName "tr")
            >>>
            ( withoutNavi $ addAttr "class" c1 )
            >>>
            colorize c2 c1 followingSiblingAxis
          )
          `orElse` this

numberH1 :: LA XmlNavTree XmlNavTree
numberH1
    = number (0::Int) descendantAxis
    where
    number cnt axis
        =   moveTo axis (remNavi >>> hasName "td")
            >>>
            ( withoutNavi $ addNumber (cnt + 1) )
            >>>
            ( number (cnt + 1) followingAxis
              `orElse`
              moveToRoot
            )
    addNumber i
        = insertChildrenAt 0 (txt (show i ++ ". "))

runTest	:: LA XmlNavTree XmlNavTree -> IOSArrow XmlTree XmlTree -> IO XmlTrees
runTest transf doc
    = runX (doc >>> fromLA (withNavi transf) >>> writeDocument [withIndent yes, withXmlPi no] "")


transformDoc cfg rules src dst =
    configSysVars (withTrace 4 : cfg) >>>
    readDocument  [] src >>>
    perform (getErrStatus >>> arrIO print) >>>
    rules >>> -- some transformations
    -- writeDocument [] dst >>>
    getErrStatus
