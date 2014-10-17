{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : Data.Tree.NTree.TypeDefs
   Copyright  : Copyright (C) 2005-2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Interface definition for trees

   n-ary tree structure (rose trees)

-}

-- ------------------------------------------------------------

module Data.Tree.NTree.TypeDefs
where

import           Control.Applicative (Applicative (..), (<$>))
import           Control.DeepSeq     (NFData (..))
import           Control.FlatSeq     (WNFData (..), rlnf)

import           Data.Binary
import           Data.Foldable       (Foldable (..))
import           Data.Monoid         (Monoid (..), (<>))
import           Data.Traversable    (Traversable (..), sequenceA)
import           Data.Tree.Class     (Tree (..))
import           Data.Typeable       (Typeable)

-- ------------------------------------------------------------

-- | n-ary ordered tree (rose trees)
--
-- a tree consists of a node and a possible empty list of children.
-- If the list of children is empty, the node is a leaf, else it's
-- an inner node.
--
-- NTree implements Eq, Ord, Show and Read

data NTree  a   = NTree a (NTrees a)
    deriving
    (Eq, Ord, Show, Read, Typeable)

-- | shortcut for a sequence of n-ary trees

type NTrees   a = [NTree a]

-- ------------------------------------------------------------

instance (NFData a) => NFData (NTree a) where
    rnf (NTree n cl)                    = rnf n `seq` rnf cl
    {-# INLINE rnf #-}

instance (WNFData a) => WNFData (NTree a) where
    rwnf (NTree n cl)           	= rwnf n `seq` rwnf cl
    {-# INLINE rwnf #-}

    -- | Evaluate a tree 2 steps deep, the top node and all children are evaluated with rwnf
    rwnf2 (NTree n cl)              	= rwnf n `seq` rlnf rwnf cl
    {-# INLINE rwnf2 #-}

-- ------------------------------------------------------------

instance (Binary a) => Binary (NTree a) where
    put (NTree n cs)    = put n >> put cs
    get                 = do
                          n  <- get
                          cs <- get
                          return (NTree n cs)

-- ------------------------------------------------------------

-- | NTree implements class Functor

instance Functor NTree where
    fmap f (NTree n cl)                 = NTree (f n) (map (fmap f) cl)
    {-# INLINE fmap #-}

-- ------------------------------------------------------------

-- | NTree implements class Foldable

instance Foldable NTree where
    foldMap f (NTree n cl)              = f n <> mconcat (map (foldMap f) cl)
    {-# INLINE foldMap #-}


-- ------------------------------------------------------------

-- | NTree implements class Taversable

instance Traversable NTree where
    traverse f (NTree n cl)             = NTree <$> f n <*> sequenceA (map (traverse f) cl)
    {-# INLINE traverse #-}

-- ------------------------------------------------------------

-- | Implementation of "Data.Tree.Class" interface for rose trees

instance Tree NTree where
    mkTree n cl                         = NTree n cl
    {-# INLINE mkTree #-}

    getNode           ~(NTree n _ )     = n
    {-# INLINE getNode #-}
    getChildren       ~(NTree _ cl)     = cl
    {-# INLINE getChildren #-}

    changeNode     cf ~(NTree n cl)     = NTree (cf n) cl
    {-# INLINE changeNode #-}
    changeChildren cf ~(NTree n cl)     = NTree n (cf cl)
    {-# INLINE changeChildren #-}

    foldTree        f ~(NTree n cs)     = f n (map (foldTree f) cs)
    {-# INLINE foldTree #-}

-- eof ------------------------------------------------------------
