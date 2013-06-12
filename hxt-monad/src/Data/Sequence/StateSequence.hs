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

module Data.Sequence.StateSequence
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence
import           Control.Monad.State

-- ----------------------------------------

newtype StateSequence s st a = STS {unSTS :: st -> (s a, st)}

-- ----------------------------------------

instance (Sequence s) => Functor (StateSequence s st) where
    fmap f (STS a) = STS $ \ s0 ->
                     let (xs, s1) = a s0 in (fmap f xs, s1)

    {-# INLINE fmap #-}

instance (Sequence s) => Applicative (StateSequence s st) where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance (Sequence s) => Monad (StateSequence s st) where
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
    fail x        = STS $ \ s0 -> (fail x, s0)

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}


instance (Sequence s) => MonadPlus (StateSequence s st) where
    mzero                   = STS $ \ s0 -> (mzero, s0)
    (STS x) `mplus` (STS y) = STS $ \ s0 ->
                              let (xs, s1) = x s0
                                  (ys, s2) = y s1
                              in (xs `mplus` ys, s2)

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}


instance (Sequence s, MonadError e s, ErrSeq e s) => MonadError e (StateSequence s st) where
    throwError x         = STS $ \ s0 -> (throwError x, s0)

    catchError (STS a) h = STS $ \ s0 ->
                                 let (xs, s1) = a s0 in
                                 case failS xs of
                                   Left e -> unSTS (h e) $ s1
                                   _      -> (xs, s1)

    {-# INLINE throwError #-}

instance (Sequence s) => MonadState st (StateSequence s st) where
    get    = STS $ \  s0 -> (return s0, s0)
    put s1 = STS $ \ _s0 -> (return (), s1)

    {-# INLINE get #-}
    {-# INLINE put #-}

instance (Sequence s) => MonadList s (StateSequence s st) where
    returnS xs      = STS $ \ s0 -> (xs, s0)
    (STS m) >>=* f  = STS $ \ s0 -> let (xs, s1) = m s0 in
                                        (unSTS (f xs)) s1

    {-# INLINE returnS #-}
    {-# INLINE (>>=*)  #-}

-- ----------------------------------------
