module Zipper
where

import Data.Tree.Class (formatTree)
import Data.Tree.NTree.TypeDefs
import Data.Tree.NTree.Zipper.TypeDefs
-- import Data.Tree.NavigatableTree.Class hiding (fromTree, toTree)

import Control.Arrow.ListArrows
-- import Control.Arrow.ArrowNavigatableTree

-- import Text.XML.HXT.DOM.XmlNode

-- import Text.XML.HXT.Core

-- import Data.Maybe


-- ------------------------------------------------------------

t0 :: NTree Int
t0 = NTree 1 [NTree 11 []
	     ,NTree 12 [NTree 121 [NTree 1211 []
                                  ,NTree 1212 []
                                  ]
		       ,NTree 122 [NTree 1221 []]
		       ]
             ,NTree 13 []
             ,NTree 14 [NTree 141 []]
	    ]

-- nt0, nt1 :: NTZipper Int
-- nt0 = fromJust . mvRight . fromJust . mvDown $ toNTZipper t0
-- nt1 = fromJust . mvRight . fromJust . mvDown $ nt0

{-
po t = map (NT.getNode . ntree) $ pathStar next $ NT.changeChildren reverse $ (toNTZipper t)
-}

instance ArrowNavigatableTree LA

t2 :: NTree Int -> [NTree Int]
t2 = tt (processTopDown (changeNode (+1)))

t3 ::  NTree Int -> [NTree Int]
t3 = tt $ 
     moveDown >>> moveRight >>> (change $< parNode) >>> moveUp
    where
    parNode = moveUp >>> toTree >>> getNode
    change x = changeNode (+(5000 * x)) 

t4  ::  Int -> NTree Int -> [NTree Int]
t4 n = tt $
       descendantOrSelfAxis `moveUntil` (getNode >>> isA (== n))
       >>>
       changeNode (+ 5555)
       >>>
       moveToRoot

t5 :: NTree Int -> [NTree Int]
t5 = tt $ nn (0::Int)

nn n = ( setNode n
         >>> ( (single (descendantOrFollowingAxis) >>> nn (n+1))
               `orElse`
               moveToRoot
             )
       )
       
tt :: LA (NTZipper a) (NTZipper b) -> NTree a -> [NTree b]
tt f = runLA $ withNavTree f

ex f = putStrLn . formatTree show . head . tt f

e :: (Show a ) => [NTree a] -> IO ()
e = putStrLn . formatTree show . head
 
