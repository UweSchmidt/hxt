module Main
where

import Data.Maybe

import Data.Tree.NTree.TypeDefs		( NTree(..) )
import qualified
       Data.Tree.NTree.TypeDefs		as NT

import Text.XML.HXT.Core

-- ------------------------------------------------------------

class Functor f => FunctorMaybe f where
    fmapMaybe 			:: (a -> Maybe a) -> f a -> f a
    fmapMaybe f 		= fmap (\ x -> fromMaybe x . f $ x)

instance FunctorMaybe NTree where
    fmapMaybe f t               = fromMaybe t . fmapNTree f $ t

-- ------------------------------------------------------------

-- | Auxiliary function for implementing a space efficient fmapMaybe
--
-- When editing just a few nodes in a tree, only these nodes that are
-- really changed will be constructed, those nodes and subtrees, that
-- do not change are shared between the old and the new tree.

fmapNTree			:: (a -> Maybe a) -> (NTree a -> Maybe (NTree a))
fmapNTree f (NTree x cs)
    | isNothing x'
      &&
      isNothing cs'		= Nothing
    | otherwise			= Just (NTree x'' cs'')
    where
    x'				= f x
    x''				= fromMaybe x x'
    cs'                         = map' (fmapNTree f) cs
    cs''                        = fromMaybe cs cs'

map'                            :: (NTree a -> Maybe (NTree a)) -> [NTree a] -> Maybe [NTree a]
map' f' []                      = Nothing
map' f' (x:xs)
    | isNothing x'
      &&
      isNothing xs'             = Nothing
    | otherwise                 = Just (fromMaybe x x' : fromMaybe xs xs')
    where
    x'                          = f' x
    xs'                         = map' f' xs

-- ------------------------------------------------------------

-- | Auxiliary function for implementing a space efficient fmapMaybe
--
-- When editing just a few nodes in a tree, only these nodes that are
-- really changed will be constructed, those nodes and subtrees, that
-- do not change are shared between the old and the new tree.

fmapNTreeA			:: (NTree a -> Maybe a) -> (NTree a -> Maybe (NTree a))
fmapNTreeA f t@(NTree x cs)
    | isNothing x'
      &&
      isNothing cs'		= Nothing
    | otherwise			= Just (NTree x'' cs'')
    where
    x'				= f t
    x''				= fromMaybe x x'
    cs'                         = map' (fmapNTreeA f) cs
    cs''                        = fromMaybe cs cs'

{--
map'                            :: (NTree a -> Maybe (NTree a)) -> [NTree a] -> Maybe [NTree a]
map' f' []                      = Nothing
map' f' (x:xs)
    | isNothing x'
      &&
      isNothing xs'             = Nothing
    | otherwise                 = Just (fromMaybe x x' : fromMaybe xs xs')
    where
    x'                          = f' x
    xs'                         = map' f' xs
--}
-- ------------------------------------------------------------

mapA				:: FunctorMaybe t => LA a a -> LA (t a) (t a)
mapA f                          = LA $ (:[]) . fmapMaybe f'
    where
    f'                          = listToMaybe . runLA f

fmapA				:: Functor t => (a -> a) -> LA (t a) (t a)
fmapA f                         = LA $ (:[]) . fmap f

fmapA'				:: FunctorMaybe t => (a -> a) -> LA (t a) (t a)		-- it isn't worth doing this with fmapMaybe
fmapA' f                        = LA $ (:[]) . fmapMaybe (Just . f)			-- because of this Just

mapA2				:: (Tree t, FunctorMaybe t) => LA (t a) (t a) -> LA (t a) (t a)
mapA2 f                         = LA $ (:[]) . fmapMaybe f'
    where
    f'                          = fmap NT.getNode . listToMaybe . runLA (flip NT.mkTree [] ^>> f)


-- ------------------------------------------------------------

type T = NTree Int

t0 = NTree 1 [NTree 2 [], NTree 3 []]

f0 2 = Just 3
f0 _ = Nothing

t1 = fmapMaybe f0 t0
t2 = fmapMaybe (const Nothing) t0

-- ------------------------------------------------------------
