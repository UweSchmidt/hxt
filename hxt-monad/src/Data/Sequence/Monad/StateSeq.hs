{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- ----------------------------------------
{- |

   Lifting of sequences (lists, trees, ...)
   into the state monad

-}
-- ----------------------------------------

module Data.Sequence.Monad.StateSeq
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSeq
import           Control.Monad.State

import           Data.Sequence.ErrorSequence
import           Data.Sequence.Seq
import           Data.Sequence.Sequence

-- ----------------------------------------

newtype StateSeq st a = STS {unSTS :: st -> (Seq a, st)}

-- ----------------------------------------

instance Functor (StateSeq st) where
    fmap f (STS a) = STS $ \ s0 ->
                     let (xs, s1) = a s0 in (fmap f xs, s1)

    {-# INLINE fmap #-}

instance Applicative (StateSeq st) where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance Monad (StateSeq st) where
    return x      = STS $ \ s0 -> (return x, s0)
    (STS a) >>= f = STS ( \ s0 ->
                          let (xs, s1) = a s0 in
                          runState (substS xs f') s1
                        )
                    where
                      f' x = do s' <- get
                                let (xs, s'') = unSTS (f x) s'
                                put s''
                                return xs

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}


instance MonadPlus (StateSeq st) where
    mzero                   = STS $ \ s0 -> (mzero, s0)
    (STS x) `mplus` (STS y) = STS $ \ s0 ->
                              let (xs, s1) = x s0
                                  (ys, s2) = y s1
                              in (xs `mplus` ys, s2)

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}


instance (MonadError e Seq, ErrorSequence e Seq) => MonadError e (StateSeq st) where
    throwError x         = STS $ \ s0 -> (throwError x, s0)

    catchError (STS a) h = STS $ \ s0 ->
                                 let (xs, s1) = a s0 in
                                 case failS xs of
                                   Left e -> unSTS (h e) $ s1
                                   _      -> (xs, s1)

    {-# INLINE throwError #-}

instance MonadState st (StateSeq st) where
    get    = STS $ \  s0 -> (return s0, s0)
    put s1 = STS $ \ _s0 -> (return (), s1)

    {-# INLINE get #-}
    {-# INLINE put #-}

instance MonadSeq (StateSeq st) where
    returnS xs      = STS $ \ s0 -> (xs, s0)
    (STS m) >>=* f  = STS $ \ s0 -> let (xs, s1) = m s0 in
                                        (unSTS (f xs)) s1

    {-# INLINE returnS #-}
    {-# INLINE (>>=*)  #-}

-- ----------------------------------------
