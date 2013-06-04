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

module Data.Sequence.IOStateSequence
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence
import           Control.Monad.State

-- ----------------------------------------

newtype IOStateSequence s st a = ISS {unISS :: st -> IO (s a, st)}

-- ----------------------------------------

instance (Sequence s) => Functor (IOStateSequence s st) where
    fmap f (ISS a) = ISS $ \ s0 ->
                     do (xs, s1) <- a s0
                        return (fmap f xs, s1)

    {-# INLINE fmap #-}

instance (Sequence s) => Applicative (IOStateSequence s st) where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance (Sequence s) => Monad (IOStateSequence s st) where
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

    fail x        = ISS $ \ s0 ->
                    return (fail x, s0)

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}

instance (Sequence s) => MonadPlus (IOStateSequence s st) where
    mzero                   = ISS $ \ s0 ->
                              return (mzero, s0)

    (ISS x) `mplus` (ISS y) = ISS $ \ s0 ->
                              do (xs, s1) <- x s0
                                 (ys, s2) <- y s1
                                 return (xs `mplus` ys, s2)

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}


instance (Sequence s, ErrSeq e s, MonadError e s) => MonadError e (IOStateSequence s st) where
    throwError x         = ISS $ \ s0 ->
                           return (throwError x, s0)

    catchError (ISS a) h = ISS $ \ s0 ->
                           do (xs, s1) <- a s0
                              case failS xs of
                                Left  e -> unISS (h e) $ s1
                                Right _ -> return (xs, s1)

    {-# INLINE throwError #-}

instance (Sequence s) => MonadState st (IOStateSequence s st) where
    get    = ISS $ \  s0 -> return (return s0, s0)
    put s1 = ISS $ \ _s0 -> return (return (), s1)

    {-# INLINE get #-}
    {-# INLINE put #-}

instance (Sequence s) => MonadIO (IOStateSequence s st) where
    liftIO x = ISS $ \ s0 ->
               do res <- x
                  return (return res, s0)

    {-# INLINE liftIO #-}

instance (Sequence s) => MonadSeq (IOStateSequence s st) where
    fromList xs    = ISS $ \ s0 ->
                     return (fromList xs, s0)

    toList (ISS a) = ISS $ \ s0 ->
                     do (xs, s1) <- a s0
                        return (toList xs, s1)

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance (Sequence s) => MonadConv (IOStateSequence s st) s where
    convFrom xs    = ISS $ \ s0 -> return (xs, s0)
    convTo (ISS a) = ISS $ \ s0 ->
                     do (xs, s1) <- a s0
                        return (return xs, s1)

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}


instance (Sequence s) => MonadCond (IOStateSequence s st) s where
    ifM (ISS a) (ISS t) (ISS e)
        = ISS $ \ s0 ->
          do (xs, s1) <- a s0
             if nullS xs
                then e s1
                else t s1

    orElseM (ISS t) (ISS e)
        = ISS $ \ s0 ->
          do res@(xs, s1) <- t s0
             if nullS xs
                then e s1
                else return res

    {-# INLINE ifM     #-}
    {-# INLINE orElseM #-}

instance (Sequence s) => MonadTry (IOStateSequence s st) where
    tryM (ISS a) = ISS $ \ s0 ->
                   do res <- try' (a s0)
                      return $ case res of
                                 Left er        -> (return (Left  er), s0)
                                 Right (xs, s1) -> (fmap    Right xs,  s1)
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
