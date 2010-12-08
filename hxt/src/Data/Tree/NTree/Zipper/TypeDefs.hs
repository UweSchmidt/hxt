{-# OPTIONS -fno-warn-orphans #-}

-- ------------------------------------------------------------

{- |
   Module     : Data.Tree.NTree.Zipper.TypeDefs
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Implementation of navigateble trees for
   rose trees. The implementation is done with zippers.
   A description and introductory tutorial about zippers
   can be found in <http://learnyouahaskell.com/zippers>
-}

-- ------------------------------------------------------------

module Data.Tree.NTree.Zipper.TypeDefs
{-
    ( NTZipper
    , NTree
    , toNTZipper
    , fromNTZipper
    )
-}
where

import Data.Tree.Class

import Data.Tree.NavigatableTree.Class
import Data.Tree.NavigatableTree.XPathAxis      ( childAxis )

import Data.Tree.NTree.TypeDefs

-- ------------------------------------------------------------

-- | Zipper for rose trees
--
-- A zipper consist of the current tree and the branches on the way back to the root

data NTZipper a         = NTZ
                          { ntree   :: (NTree a)
                          , context :: (NTBreadCrumbs a)
                          }
                          deriving (Show)

-- | The list of unzipped nodes from a current tree back to the root

type NTBreadCrumbs a    = [NTCrumb a]

-- | One unzipped step consists of the left siblings, the node info and the right siblings

data NTCrumb a          = NTC
                          (NTrees a)            -- left side
                          a                     -- node
                          (NTrees a)            -- right side
                          deriving (Show)

-- ------------------------------------------------------------

-- | Conversion of a rose tree into a navigatable rose tree

toNTZipper              :: NTree a -> NTZipper a
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

up                      :: NTZipper a -> Maybe (NTZipper a)
up z
    | isTop z           = Nothing
    | otherwise         = Just $ NTZ (up1 t bc) bcs
    where
    NTZ t (bc : bcs)    = z

{-# INLINE up #-}

down                    :: NTZipper a -> Maybe (NTZipper a)
down (NTZ (NTree n cs) bcs)
          | null cs     = Nothing
          | otherwise   = Just $ NTZ (head cs) (NTC [] n (tail cs) : bcs)

{-# INLINE down #-}

toTheRight                   :: NTZipper a -> Maybe (NTZipper a)
toTheRight z
    | isTop z
      ||
      null rs           = Nothing
    | otherwise         = Just $ NTZ t' (bc' : bcs)
    where
    (NTZ t (bc : bcs))  = z
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
    (NTZ t (bc : bcs))  = z
    (NTC ls n rs)       = bc
    t'                  = head ls
    bc'                 = NTC (tail ls) n (t : rs)

{-# INLINE toTheLeft #-}

addToTheLeft            :: NTree a -> NTZipper a -> Maybe (NTZipper a)
addToTheLeft t z
    | isTop z           = Nothing
    | otherwise         = Just $ NTZ t' (NTC (t:ls) n rs : bcs)
    where
    (NTZ t' (bc : bcs)) = z
    (NTC ls n rs)       = bc
{-# INLINE addToTheLeft #-}

addToTheRight            :: NTree a -> NTZipper a -> Maybe (NTZipper a)
addToTheRight t z
    | isTop z           = Nothing
    | otherwise         = Just $ NTZ t' (NTC ls n (t:rs) : bcs)
    where
    (NTZ t' (bc : bcs)) = z
    (NTC ls n rs)       = bc
{-# INLINE addToTheRight #-}

dropFromTheLeft            :: NTZipper a -> Maybe (NTZipper a)
dropFromTheLeft z
    | isTop z           = Nothing
    | null ls           = Nothing
    | otherwise         = Just $ NTZ t' (NTC (tail ls) n rs : bcs)
    where
    (NTZ t' (bc : bcs)) = z
    (NTC ls n rs)       = bc
{-# INLINE dropFromTheLeft #-}

dropFromTheRight        :: NTZipper a -> Maybe (NTZipper a)
dropFromTheRight z
    | isTop z           = Nothing
    | null rs           = Nothing
    | otherwise         = Just $ NTZ t' (NTC ls n (tail rs) : bcs)
    where
    (NTZ t' (bc : bcs)) = z
    (NTC ls n rs)       = bc
{-# INLINE dropFromTheRight #-}

-- ------------------------------------------------------------

isTop                   :: NTZipper a -> Bool
isTop                   = null . context

{-# INLINE isTop #-}

up1                     :: NTree a -> NTCrumb a -> NTree a
up1 t (NTC ls n rs)     = NTree n (foldl (flip (:)) (t : rs) ls)

{-# INLINE up1 #-}

-- ------------------------------------------------------------

instance Functor NTZipper where
    fmap f (NTZ t xs)   = NTZ (fmap f t) (map (fmap f) xs)
    {-# INLINE fmap #-}

instance Functor NTCrumb where
    fmap f (NTC xs x ys)= NTC (map (fmap f) xs) (f x) (map (fmap f) ys)
    {-# INLINE fmap #-}

instance Tree NTZipper where
    mkTree n cl         = toNTZipper . mkTree n $ map ntree cl

    getNode             = getNode . ntree
    {-# INLINE getNode #-}
    getChildren         = childAxis
    {-# INLINE getChildren #-}

    changeNode     cf t = t { ntree = changeNode cf (ntree t) }
    changeChildren cf t = t { ntree = setChildren (map ntree . cf . childAxis $ t) (ntree t) }

    foldTree f          = foldTree f . ntree
    {-# INLINE foldTree #-}

instance NavigatableTree NTZipper where
    mvDown              = down
    {-# INLINE mvDown #-}

    mvUp                = up
    {-# INLINE mvUp #-}

    mvLeft              = toTheLeft
    {-# INLINE mvLeft #-}

    mvRight             = toTheRight
    {-# INLINE mvRight #-}

instance NavigatableTreeToTree NTZipper NTree where
    fromTree            = toNTZipper
    {-# INLINE fromTree #-}

    toTree              = fromNTZipper
    {-# INLINE toTree #-}

instance NavigatableTreeModify NTZipper NTree where
    addTreeLeft         = addToTheLeft
    {-# INLINE addTreeLeft #-}

    addTreeRight        = addToTheRight
    {-# INLINE addTreeRight #-}

    dropTreeLeft        = dropFromTheLeft
    {-# INLINE dropTreeLeft #-}

    dropTreeRight       = dropFromTheRight
    {-# INLINE dropTreeRight #-}

    substThisTree t nt  = nt { ntree = t }
    {-# INLINE substThisTree #-}

-- ------------------------------------------------------------
