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

import Control.DeepSeq

import Data.Binary
import Data.Tree.Class
import Data.Typeable

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

instance (Binary a) => Binary (NTree a) where
    put (NTree n cs)    = put n >> put cs
    get                 = do
                          n  <- get
                          cs <- get
                          return (NTree n cs)

-- | NTree implements class Functor

instance Functor NTree where
    fmap f ~(NTree n cl)                = NTree (f n) (map (fmap f) cl)
    {-# INLINE fmap #-}


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
