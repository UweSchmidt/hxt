module Main
where

import Data.Maybe

import Data.Tree.NTree.TypeDefs

class Functor f => FunctorMaybe f where
    fmapMaybe 			:: (a -> Maybe a) -> f a -> f a
    fmapMaybe f 		= fmap (\ x -> fromMaybe x . f $ x)

instance FunctorMaybe NTree where
    fmapMaybe f t               = fromMaybe t . fmapNTree f $ t

-- | Auxiliary function for implementing a space efficient fmapMaybe
--
-- When editing just a few nodes in a tree, only these nodes that are
-- really changed will be constructed, those nodes and subtrees, that
-- do not change are shared between the old and the new tree.

fmapNTree			:: (a -> Maybe a) -> (NTree a -> Maybe (NTree a))
fmapNTree f (NTree x cs)
    | isNothing x'
      &&
      all isNothing cs'		= Nothing
    | otherwise			= Just (NTree x'' cs'')
    where
    x'				= f x
    x''				= fromMaybe x x'
    cs'                         = map (fmapNTree f) cs
    cs''                        = zipWith fromMaybe cs cs'

-- ------------------------------------------------------------

type T = NTree Int

t0 = NTree 1 [NTree 2 [], NTree 3 []]

f0 2 = Just 3
f0 _ = Nothing

t1 = fmapMaybe f0 t0
t2 = fmapMaybe (const Nothing) t0

-- ------------------------------------------------------------
