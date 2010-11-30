module Data_Tree_NTree_Zipper_TypeDefs
    ( NTZipper
    , toNTZipper
    , fromNTZipper
    )
where

import Data_Tree_NavigatableTree_Class

import Data.Tree.NTree.TypeDefs

-- ------------------------------------------------------------

-- | Zipper for rose trees
--
-- A zipper consist of the current tree and the branches on the way back to the root

data NTZipper a 	= NTZ
                          { ntree   :: (NTree a)
                          , context :: (NTBreadCrumbs a)
                          }
			  deriving (Show)

-- | The list of unzipped nodes from a current tree back to the root

type NTBreadCrumbs a 	= [NTCrumb a]

-- | One unzipped step consists of the left siblings, the node info and the right siblings

data NTCrumb a 		= NTC
                          (NTrees a)		-- left side
                          a			-- node
                          (NTrees a)		-- right side
			  deriving (Show)

-- ------------------------------------------------------------

-- | Conversion of a rose tree into a navigatable rose tree

toNTZipper		:: NTree a -> NTZipper a
toNTZipper t            = NTZ t []

{-# INLINE toNTZipper #-}

-- | Conversion of a navigatable rose tree into an ordinary rose tree.
--
-- The context, the parts for moving up to the root are just removed from the tree.
-- So when transforming a navigatable tree by moving around and by changing some nodes,
-- one has to navigate back
-- to the root, else that parts are removed from the result

fromNTZipper            :: NTZipper a -> NTree a
fromNTZipper            = ntree

{-# INLINE fromNTZipper #-}

-- ------------------------------------------------------------

up                     	:: NTZipper a -> Maybe (NTZipper a)
up z
    | isTop z		= Nothing
    | otherwise         = Just $ NTZ (up1 t bc) bcs
    where
    NTZ t (bc : bcs)    = z

{-# INLINE up #-}

down                    :: NTZipper a -> Maybe (NTZipper a)
down (NTZ (NTree n cs) bcs)
          | null cs	= Nothing
          | otherwise	= Just $ NTZ (head cs) (NTC [] n (tail cs) : bcs)

{-# INLINE down #-}

toTheRight                   :: NTZipper a -> Maybe (NTZipper a)
toTheRight z
    | isTop z
      ||
      null rs           = Nothing
    | otherwise         = Just $ NTZ t' (bc' : bcs)
    where
    (NTZ t (bc : bcs))	= z
    (NTC ls n rs)       = bc
    t'                  = head rs
    bc'                 = NTC (t : ls) n (tail rs)

{-# INLINE toTheRight #-}

toTheLeft                    :: NTZipper a -> Maybe (NTZipper a)
toTheLeft z
    | isTop z
      ||
      null ls           = Nothing
    | otherwise         = Just $ NTZ t' (bc' : bcs)
    where
    (NTZ t (bc : bcs))	= z
    (NTC ls n rs)       = bc
    t'                  = head ls
    bc'                 = NTC (tail ls) n (t : rs)

{-# INLINE toTheLeft #-}

isTop                   :: NTZipper a -> Bool
isTop                   = null . context

{-# INLINE isTop #-}

up1			:: NTree a -> NTCrumb a -> NTree a
up1 t (NTC ls n rs)	= NTree n (foldl (flip (:)) (t : rs) ls)

{-# INLINE up1 #-}

-- ------------------------------------------------------------

instance Functor NTZipper where
    fmap f (NTZ t xs)	= NTZ (fmap f t) (map (fmap f) xs)

instance Functor NTCrumb where
    fmap f (NTC xs x ys)= NTC (map (fmap f) xs) (f x) (map (fmap f) ys)

instance Tree NTZipper where
    mkTree n cl		= toNTZipper . mkTree n $ map ntree cl

    getNode             = getNode . ntree
    getChildren         = childAxis

    changeNode     cf t = t { ntree = changeNode cf (ntree t) }
    changeChildren cf t = t { ntree = setChildren (map ntree . cf . childAxis $ t) (ntree t) }

    foldTree f          = foldTree f . ntree


instance NavigatableTree NTZipper where
    mvDown 		= down
    mvUp   		= up
    mvLeft 		= toTheLeft
    mvRight 		= toTheRight

-- ------------------------------------------------------------