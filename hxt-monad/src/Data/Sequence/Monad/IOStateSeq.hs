{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- ----------------------------------------
{- |

   Lifting of sequences (lists, trees, ...)
   into the IO-State monad

-}
-- ----------------------------------------

module Data.Sequence.Monad.IOStateSeq
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSeq
import           Control.Monad.MonadTry
import           Control.Monad.State

import           Data.Sequence.ErrorSequence
import           Data.Sequence.Seq
import           Data.Sequence.Sequence

-- ----------------------------------------

newtype IOStateSeq st a = ISS {unISS :: st -> IO (Seq a, st)}

-- ----------------------------------------

instance Functor (IOStateSeq st) where
    fmap f (ISS a) = ISS $ \ s0 ->
                     do (xs, s1) <- a s0
                        return (fmap f xs, s1)

    {-# INLINE fmap #-}

instance Applicative (IOStateSeq st) where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance Monad (IOStateSeq st) where
    return x      = ISS $ \ s0 ->
                    return (return x, s0)

    (ISS a) >>= f = ISS $ \ s0 ->
                    do (xs, s1) <- a s0
                       runStateT (substS xs f') s1
                    where
                      f' x = do s' <- get
                                (xs, s'') <- liftIO $ unISS (f x) s'
                                put s''
                                return xs

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}

instance MonadPlus (IOStateSeq st) where
    mzero                   = ISS $ \ s0 ->
                              return (mzero, s0)

    (ISS x) `mplus` (ISS y) = ISS $ \ s0 ->
                              do (xs, s1) <- x s0
                                 (ys, s2) <- y s1
                                 return (xs `mplus` ys, s2)

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}


instance (ErrorSequence e Seq, MonadError e Seq) => MonadError e (IOStateSeq st) where
    throwError x         = ISS $ \ s0 ->
                           return (throwError x, s0)

    catchError (ISS a) h = ISS $ \ s0 ->
                           do (xs, s1) <- a s0
                              case failS xs of
                                Left  e -> unISS (h e) $ s1
                                Right _ -> return (xs, s1)

    {-# INLINE throwError #-}

instance MonadState st (IOStateSeq st) where
    get    = ISS $ \  s0 -> return (return s0, s0)
    put s1 = ISS $ \ _s0 -> return (return (), s1)

    {-# INLINE get #-}
    {-# INLINE put #-}

instance MonadIO (IOStateSeq st) where
    liftIO x = ISS $ \ s0 ->
               do res <- x
                  return (return res, s0)

    {-# INLINE liftIO #-}

instance MonadSeq (IOStateSeq st) where
    returnS xs      = ISS $ \ s0 -> return (xs, s0)
    (ISS m) >>=* f  = ISS $ \ s0 -> do (xs, s1) <- m s0
                                       (unISS (f xs)) s1

    {-# INLINE returnS #-}
    {-# INLINE (>>=*)  #-}

instance MonadTry (IOStateSeq st) where
    tryM (ISS a) = ISS $ \ s0 ->
                   do res <- try' (a s0)
                      return $ case res of
                                 Left er        -> (return (Left  er), s0)
                                 Right (xs, s1) -> (fmap    Right xs,  s1)
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
