{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- ----------------------------------------
{- |

   Lifting of sequences (lists, trees, ...)
   into the IO monad

-}
-- ----------------------------------------

module Data.Sequence.IOSequence
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence

-- ----------------------------------------

newtype IOSequence s a = IOS {unIOS :: IO (s a)}

instance (Sequence s) => Functor (IOSequence s) where
    fmap f (IOS a) = IOS $ a >>= return . fmap f

    {-# INLINE fmap #-}

instance (Sequence s) => Applicative (IOSequence s) where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance (Sequence s) => Monad (IOSequence s) where
    return        = IOS . return . return
    (IOS a) >>= f = IOS $ a >>= \ x -> substS x (unIOS . f)
    fail          = IOS . return . fail

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}

instance (Sequence s) => MonadPlus (IOSequence s) where
    mzero                   = IOS $ return mzero
    (IOS x) `mplus` (IOS y) = IOS $ liftM2 mplus x y

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

-- MonadPlus laws violated !!!
-- (IOS $ print 42) >> mzero /= mzero

instance (Sequence s, ErrSeq e s, MonadError e s) => MonadError e (IOSequence s) where
    throwError           = IOS . return . throwError

    catchError (IOS a) h = IOS $
                           do t <- a
                              case failS t of
                                Left  s -> (unIOS . h) s
                                Right _ -> return t

    {-# INLINE throwError #-}

instance (Sequence s) => MonadIO (IOSequence s) where
    liftIO x = IOS $ x >>= return . return

    {-# INLINE liftIO #-}

instance (Sequence s) => MonadSequence (IOSequence s) where
    fromList       = IOS . return . fromList
    toList (IOS a) = IOS $ a >>= return . toList

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance (Sequence s) => MonadConv (IOSequence s) s where
    convFrom       = IOS . return
    convTo (IOS a) = IOS $ a >>= return . return

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}


instance (Sequence s) => MonadCond (IOSequence s) s where
    ifM (IOS a) (IOS t) (IOS e)
        = IOS $
          do x <- a
             if nullS x
                then e
                else t

    orElseM (IOS t) (IOS e)
        = IOS $
          do x <- t
             if nullS x
                then e
                else return x

    {-# INLINE ifM     #-}
    {-# INLINE orElseM #-}

instance (Sequence s) => MonadTry (IOSequence s) where
    tryM (IOS a) = IOS $
                   do res <- try' a
                      return $ case res of
                                 Left  er -> return (Left  er)
                                 Right xs -> fmap    Right xs
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
