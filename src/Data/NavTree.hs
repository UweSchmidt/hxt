-- |
-- Navigable tree structure which allow a program to traverse
-- up the tree as well as down.
-- copied and modified from HXML (<http://www.flightlab.com/~joe/hxml/>)
--

module Data.NavTree
    ( module Data.NavTree
    , module Data.Tree.NTree.Filter
    )
where

import Data.Tree.NTree.Filter
import Data.Maybe

-- -----------------------------------------------------------------------------

-- NavTree
--
-- | navigable tree with nodes of type node
--
-- a navigable tree consists of a n-ary tree for the current fragment tree,
-- a navigable tree for all ancestors, and two n-ary trees for
-- the previous- and following siblings

data NavTree a = NT
    (NTree a)		-- self
    [NavTree a]		-- ancestors
    [NTree a]		-- previous siblings (in reverse order)
    [NTree a]		-- following siblings
	deriving (Show, Eq, Ord)


-- -----------------------------------------------------------------------------


-- |
-- converts a n-ary tree in a navigable tree

ntree	:: NTree a -> NavTree a
ntree nd
    = NT nd [] [] []

-- |
-- converts a navigable tree in a n-ary tree

subtreeNT	:: NavTree a -> NTree a
subtreeNT (NT nd _ _ _)
    = nd


-- |
-- function for selecting the value of the current fragment tree

dataNT	:: NavTree a -> a
dataNT (NT (NTree a _) _ _ _)
    = a


-- -----------------------------------------------------------------------------

-- functions for traversing up, down, left and right in a navigable tree

upNT
 , downNT
 , leftNT
 , rightNT :: NavTree a -> Maybe (NavTree a)

upNT	  (NT _ (p:_) _ _)		= Just p
upNT	  (NT _ [] _ _) 		= Nothing
downNT	t@(NT (NTree _ (c:cs)) u _ _)	= Just (NT c (t:u) [] cs)
downNT 	  (NT (NTree _ []    ) _ _ _)	= Nothing
leftNT	  (NT s u (l:ls) r)		= Just (NT l u ls (s:r))
leftNT	  (NT _ _ []     _)		= Nothing
rightNT   (NT s u l (r:rs))		= Just (NT r u (s:l) rs)
rightNT   (NT _ _ _ []    ) 		= Nothing


-- preorderNT t = t : concatMap preorderNT (children t)
-- where children = maybe [] (maybeStar rightNT) . downNT

preorderNT	:: NavTree a -> [NavTree a]
preorderNT
    = visit []
    where
    visit  k t = t : maybe k (visit' k) (downNT t)
    visit' k t = visit (maybe k (visit' k) (rightNT t)) t

revPreorderNT	:: NavTree a -> [NavTree a]
revPreorderNT t
    = t : concatMap revPreorderNT (reverse (children t))
      where
      children = maybe [] (maybeStar rightNT) . downNT

getChildrenNT	:: NavTree a -> [NavTree a]
getChildrenNT node
    = maybe [] follow (downNT node)
      where
      follow n = n : maybe [] follow (rightNT n)

-- -----------------------------------------------------------------------------
-- Miscellaneous useful combinators

-- |
-- Kleisli composition:

o' :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f `o'` g = \x -> g x >>= f


-- Some useful anamorphisms:

maybeStar, maybePlus :: (a -> Maybe a) -> a -> [a]
maybeStar f a = a : maybe [] (maybeStar f) (f a)
maybePlus f a =     maybe [] (maybeStar f) (f a)

-- -----------------------------------------------------------------------------
