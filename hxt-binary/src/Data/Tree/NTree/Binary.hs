{-# OPTIONS -fno-warn-orphans #-}

-- ------------------------------------------------------------

{- |
   Module     : Data.Tree.NTree.Binary
   Copyright  : Copyright (C) 2009 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   De-/Serialisation for NTrees

-}

-- ------------------------------------------------------------

module Data.Tree.NTree.Binary
where


import Data.Binary
import Data.Tree.NTree.TypeDefs

instance (Binary a) => Binary (NTree a) where
    put (NTree n cs)    = put n >> put cs
    get                 = do
                          n  <- get
                          cs <- get
                          return (NTree n cs)

-- ------------------------------------------------------------
