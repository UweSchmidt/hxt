-- ------------------------------------------------------------

{- |
   Module     : Control.FlatSeq
   Copyright  : Copyright (C) 2011 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Force evaluation like deepseq in Control.DeepSeq,
   but control the depth of evaluation.
   flatseq may evaluate more than seq but less than deepseq

-}

-- ------------------------------------------------------------

module Control.FlatSeq
where

import Data.Word

-- ------------------------------------------------------------

flatseq                         :: WNFData a => a -> b -> b
flatseq a b                     = rwnf a `seq` b

rlnf                            :: (a -> ()) -> [a] -> ()
rlnf _ []                       = ()
rlnf r (x:xs)                   = r x `seq` rlnf r xs
{-# INLINE rlnf #-}

-- | A class of types that can be partially evaluated, but evaluation can be propagated deeper than WHNF

class WNFData a where
    -- | Default for rwnf is reduction to WHNF
    rwnf                        :: a -> ()
    rwnf a                      = a `seq` ()
    {-# INLINE rwnf #-}

    -- | Default for rwnf2 is rwnf
    rwnf2                       :: a -> ()
    rwnf2                       = rwnf
    {-# INLINE rwnf2 #-}

instance WNFData Int 
instance WNFData Integer
instance WNFData Float
instance WNFData Double

instance WNFData Char
instance WNFData Bool
instance WNFData ()

instance WNFData Word
instance WNFData Word8
instance WNFData Word16
instance WNFData Word32
instance WNFData Word64

instance WNFData a => WNFData [a] where
    rwnf []                     = ()
    rwnf (x:xs)                 = x `seq` rwnf xs
    {-# INLINE rwnf #-}

instance (WNFData a, WNFData b) => WNFData (a,b) where
    rwnf (x,y)                  = rwnf x `seq` rwnf y
    {-# INLINE rwnf #-}

instance (WNFData a, WNFData b, WNFData c) => WNFData (a,b,c) where
    rwnf (x,y,z)                = rwnf x `seq` rwnf y `seq` rwnf z 
    {-# INLINE rwnf #-}

instance (WNFData a, WNFData b, WNFData c, WNFData d) => WNFData (a,b,c,d) where
    rwnf (x1,x2,x3,x4)          = rwnf x1 `seq` 
                                  rwnf x2 `seq` 
                                  rwnf x3 `seq` 
                                  rwnf x4 
    {-# INLINE rwnf #-}

-- ------------------------------------------------------------
