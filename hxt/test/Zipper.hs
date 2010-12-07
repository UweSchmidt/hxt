module Zipper
where

-- import Control.Arrow.ListArrows

import Data.Tree.Class                 ( formatTree )
import Data.Tree.NTree.TypeDefs        ( NTree(..) )
import Data.Tree.NTree.Zipper.TypeDefs ( NTZipper )

import qualified Data.Tree.NavigatableTree.Class        as T
import           Data.Tree.NavigatableTree.Class        ( NavigatableTree
                                                        , TreeToNavigatableTree
                                                        )

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
t4 n = ( moveOn (descendantOrSelfAxis >>> filterAxis (hasNode (== n)))
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
    = ( moveOn (descendantOrFollowingAxis >>> filterAxis (hasNode odd))
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
    = ( moveOn (descendantAxis >>> filterAxis (hasAttrValue "id" (== name)))
        >>>
        colorize dark light childAxis
        >>>
        moveToRoot
      )
      `orElse` this
    where
    colorize c1 c2 axis
        = ( moveOn (axis >>> filterAxis (hasName "tr"))
            >>>
            ( withoutNavi $ addAttr "class" c1 )
            >>>
            colorize c2 c1 followingSiblingAxis
          )
          `orElse` this

numberH1 :: LA XmlNavTree XmlNavTree
numberH1
    = number (0::Int) descendantAxis `orElse` this
    where
    number cnt axis
        =   moveOn (axis >>> filterAxis (hasName "td"))
            >>>
            ( withoutNavi $ addNumber (cnt + 1) )
            >>>
            ( number (cnt + 1) followingAxis
              `orElse`
              moveToRoot
            )
    addNumber i
        = insertChildrenAt 0 (txt (show i ++ ". "))

processTable p = visit
                 (descendantAxis >>> filterAxis (hasAttrValue "id" (=="table1")))
                 none
                 ()
                 (const ())
                 (const p)

numberH1' = visit
            (descendantAxis >>> filterAxis (hasAttrValue "id" (=="table1")) >>>
             descendantAxis >>> filterAxis (hasName "td")
            )
            (followingAxis  >>> filterAxis (hasName "td"))
            (1::Int)
            (1+)
            (\ i -> insertChildrenAt 0 (txt (show i ++ ". ")))


numberH1'' = visit
            (descendantAxis >>> filterAxis (hasName "td"))
            (followingAxis  >>> filterAxis (hasName "td"))
            (1::Int)
            (1+)
            (\ i -> insertChildrenAt 0 (txt (show i ++ ". ")))

visit	:: LA XmlNavTree XmlNavTree ->
           LA XmlNavTree XmlNavTree ->
           a  ->
          (a -> a) ->
          (a -> LA XmlTree XmlTree) ->
           LA XmlTree XmlTree
visit initAxis nextAxis initState nextState nodeTransf
    = withNavi (move initState initAxis)
      `orElse`
      this
    where
    move state axis
        = moveOn axis
          >>>
          changeTree (nodeTransf state)
          >>>
          ( move (nextState state) nextAxis	-- and go to the next node
            `orElse`
            moveToRoot			--
          )

runTest	:: LA XmlNavTree XmlNavTree -> IOSArrow XmlTree XmlTree -> IO XmlTrees
runTest transf doc
    = runX (doc >>> fromLA (withNavi transf) >>> writeDocument [withIndent yes, withXmlPi no] "")

r1 = runX (doc1 >>> fromLA (numberH1') >>> writeDocument [withIndent yes, withXmlPi no] "")
r2 = runX (doc1 >>> fromLA (processTable numberH1'') >>> writeDocument [withIndent yes, withXmlPi no] "")
r3 = runX (doc1 >>> root [] [fromLA (withNavi (descendantAxis >>> filterAxis (hasName "table") >>> descendantAxis >>> (selfAxis <+> followingAxis) >>> filterAxis (hasName "td")))] >>> writeDocument [withIndent yes, withXmlPi no] "")

changeTree		:: ( ArrowList a
                           , ArrowIf a
                           , TreeToNavigatableTree t nt
                           ) =>
                           a (t b) (t b) -> a (nt b) (nt b)
changeTree cf		= withoutNavi $ single cf `orElse` this
