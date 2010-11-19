module Data_Tree_NTree_Zipper_TypeDefs
where

import Control.Monad

import Data.Tree.NTree.TypeDefs

data NTZipper a 	= NTZ
                          { ntree   :: (NTree a)
                          , context :: (NTBreadCrumbs a)
                          }
			  deriving (Show)

type NTBreadCrumbs a 	= [NTCrumb a]

data NTCrumb a 		= NTC
                          { leftSide  :: (NTrees a)
                          , node      :: a
                          , rightSide :: (NTrees a)
                          }
			  deriving (Show)

toNTZipper		:: NTree a -> NTZipper a
toNTZipper t            = NTZ t []

{-# INLINE toNTZipper #-}

fromNTZipper            :: NTZipper a -> NTree a
fromNTZipper            = ntree . head . up

{-# INLINE fromNTZipper #-}

-- ------------------------------------------------------------

up                     	:: NTZipper a -> [NTZipper a]
up z
    | isTop z		= []
    | otherwise         = [NTZ (up1 t bc) bcs]
    where
    NTZ t (bc : bcs)    = z

{-# INLINE up #-}

down                    :: NTZipper a -> [NTZipper a]
down (NTZ (NTree n cs) bcs)
          | null cs	= []
          | otherwise	= [NTZ (head cs) (NTC [] n (tail cs) : bcs)]

{-# INLINE down #-}

right                   :: NTZipper a -> [NTZipper a]
right z
    | isTop z		= []
    | null rs           = []
    | otherwise         = [NTZ t' (bc' : bcs)]
    where
    (NTZ t (bc : bcs))	= z
    (NTC ls n rs)       = bc
    t'                  = head rs
    bc'                 = NTC (t : ls) n (tail rs)

{-# INLINE right #-}

left                    :: NTZipper a -> [NTZipper a]
left z
    | isTop z		= []
    | null ls           = []
    | otherwise         = [NTZ t' (bc' : bcs)]
    where
    (NTZ t (bc : bcs))	= z
    (NTC ls n rs)       = bc
    t'                  = head ls
    bc'                 = NTC (tail ls) n (t : rs)

{-# INLINE left #-}

next                    :: NTZipper a -> [NTZipper a]
next z                  = take 1 (down z ++ (concatMap right . pathStar up) z)

top                     :: NTZipper a -> NTZipper a
top z
    | isTop z		= z
    | otherwise         = top $
                          NTZ (up1 t bc) bcs
    where
    NTZ t (bc : bcs)    = z

isTop                   :: NTZipper a -> Bool
isTop (NTZ _ [])        = True
isTop _                 = False

{-# INLINE isTop #-}

up1			:: NTree a -> NTCrumb a -> NTree a
up1 t (NTC ls n rs)	= NTree n (foldl (flip (:)) (t : rs) ls)

{-# INLINE up1 #-}

pathStar		:: (a -> [a]) -> (a -> [a])
pathStar f x            = x : concatMap (pathStar f) (f x)

pathPlus                :: (a -> [a]) -> (a -> [a])
pathPlus f x            = concatMap (pathStar f) (f x)

{-# INLINE pathPlus #-}

-- ------------------------------------------------------------
-- XPath axis

parentAxis              :: NTZipper a -> [NTZipper a]
parentAxis              = up

ancestorAxis            :: NTZipper a -> [NTZipper a]
ancestorAxis            = pathPlus up

ancestorOrSelfAxis      :: NTZipper a -> [NTZipper a]
ancestorOrSelfAxis      = pathStar up

childAxis               :: NTZipper a -> [NTZipper a]
childAxis               = down >=> pathStar right

descendantAxis          :: NTZipper a -> [NTZipper a]
descendantAxis          = undefined -- tail . preorderNT             -- concatMap preorderNT . childAxis

descendantOrSelfAxis    :: NTZipper a -> [NTZipper a]
descendantOrSelfAxis    = undefined -- preorderNT

followingSiblingAxis    :: NTZipper a -> [NTZipper a]
followingSiblingAxis    = pathPlus right

precedingSiblingAxis    :: NTZipper a -> [NTZipper a]
precedingSiblingAxis    = pathPlus left                               -- reverse . pathPlus left

selfAxis                :: NTZipper a -> [NTZipper a]
selfAxis                = (:[])

followingAxis           :: NTZipper a -> [NTZipper a]
followingAxis           = undefined -- preorderNT     `o'` followingSiblingAxis `o'` ancestorOrSelfAxis

precedingAxis           :: NTZipper a -> [NTZipper a]
precedingAxis           = undefined -- revPreorderNT  `o'` precedingSiblingAxis `o'` ancestorOrSelfAxis


-- ------------------------------------------------------------

instance Functor NTZipper where
    fmap f (NTZ t xs)	= NTZ (fmap f t) (map (fmap f) xs)

instance Functor NTCrumb where
    fmap f (NTC xs x ys)= NTC (map (fmap f) xs) (f x) (map (fmap f) ys)

-- ------------------------------------------------------------

t :: NTree Int
t = NTree 1 [NTree 11 []
	    ,NTree 12 [NTree 121 []
		      ,NTree 122 []
		      ]
	    ]

(>>>) f g x = do
	      y <- f x
	      z <- g y
	      return z

po t = map (getNode . ntree) $ pathStar next $ (toNTZipper t)