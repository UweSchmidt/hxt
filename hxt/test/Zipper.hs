module Zipper
where

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.XmlNode

import Data_Tree_NavigatableTree_Class
import Data_Tree_NTree_Zipper_TypeDefs

-- import Text.XML.HXT.Core

import Data.Maybe

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

nt0, nt1 :: NTZipper Int
nt0 = fromJust . mvRight . fromJust . mvDown $ toNTZipper t0
nt1 = fromJust . mvRight . fromJust . mvDown $ nt0

{-
po t = map (NT.getNode . ntree) $ pathStar next $ NT.changeChildren reverse $ (toNTZipper t)

t2 = runLA (arr toNTZipper >>> processTopDown (changeNode (+1)) >>> arr fromNTZipper) t
t3 = runLA
     ( arr toNTZipper
       >>>
       arrL down >>> arrL toTheRight >>> (change $< parNode) >>> arrL up
       >>>
       arr fromNTZipper
     ) t
    where
    parNode = arrL up >>> arr fromNTZipper >>> getNode
    change x = changeNode (+(1000 * x)) 
-}