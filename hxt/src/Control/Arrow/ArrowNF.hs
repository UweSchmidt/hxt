-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowNF
   Copyright  : Copyright (C) 2011 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: non-portable

   Arrows for evaluation of normal form results

-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowNF
where

import           Control.Arrow
import           Control.Arrow.ArrowList

import           Control.DeepSeq
import           Control.FlatSeq

-- |
-- complete evaluation of an arrow result using 'Control.DeepSeq'
--
-- this is sometimes useful for preventing space leaks, especially after reading
-- and validation of a document, all DTD stuff is not longer in use and can be
-- recycled by the GC.

strictA :: (Arrow a, NFData b) => a b b
strictA = arr $ \ x -> deepseq x x

class (Arrow a) => ArrowNF a where
    rnfA                        :: (NFData c) => a b c -> a b c
    rnfA f                      = f >>^ (\ x -> deepseq x x)
    {-# INLINE rnfA #-}

-- |
-- partial evaluation of an arrow result using 'Control.FlatSeq'
--
-- There are two arrows with force the partial evaluation. By convention
-- the 2. should be less lazy than the 1.
--
-- These arrows are sometimes useful for preventing space leaks, especially when parsing
-- complex data structures. In many cases the evaluated AST is more space efficient
-- than the unevaluaded with a lot of closures.

class (Arrow a, ArrowList a) => ArrowWNF a where
    rwnfA                       :: (WNFData c) => a b c -> a b c
    rwnfA f                     = f >>. \ x -> rlnf rwnf x `seq` x
    {-# INLINE rwnfA #-}

    rwnf2A                      :: (WNFData c) => a b c -> a b c
    rwnf2A f                    = f >>. \ x -> rlnf rwnf2 x `seq` x
    {-# INLINE rwnf2A #-}

-- ------------------------------------------------------------
