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

module Data.List.IOSeq
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence

import           Data.List.List
import           Data.List.Tree

-- ----------------------------------------

type IOLA' s a b = a -> IOSeq s b

type IOLAt a b = IOLA' Tree a b
type IOLAs a b = IOLA' List a b

-- ----------------------------------------

newtype IOSeq s a = IOS {unIOS :: IO (s a)}

instance (Functor s) => Functor (IOSeq s) where
    fmap f (IOS a) = IOS $ a >>= return . fmap f

    {-# INLINE fmap #-}

instance (Monad s, Functor s, Sequence s) => Applicative (IOSeq s) where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance (Monad s, Sequence s) => Monad (IOSeq s) where
    return        = IOS . return . return
    (IOS a) >>= f = IOS $ a >>= \ x -> substS x (unIOS . f)
    fail          = IOS . return . fail

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}

instance (MonadPlus s, Sequence s) => MonadPlus (IOSeq s) where
    mzero                   = IOS $ return mzero
    (IOS x) `mplus` (IOS y) = IOS $ liftM2 mplus x y

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance (Monad s, MonadError e s, Sequence s, ErrSeq e s) => MonadError e (IOSeq s) where
    throwError = IOS . return . throwError

    catchError (IOS a) h = IOS $
                           do t <- a
                              case failS t of
                                Left s -> (unIOS . h) s
                                _      -> return t

    {-# INLINE throwError #-}

instance (Monad s, Sequence s) => MonadIO (IOSeq s) where
    liftIO x = IOS $ x >>= return . return

    {-# INLINE liftIO #-}

instance (MonadSequence s, Sequence s) => MonadSequence (IOSeq s) where
    fromList = IOS . return . fromList
    toList (IOS a) = IOS $ a >>= return . toList

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance (Monad s, Sequence s) => MonadConv (IOSeq s) s where
    convFrom       = IOS . return
    convTo (IOS a) = IOS $ a >>= return . return

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}

instance (MonadPlus s, Sequence s) => MonadCond (IOSeq s) s where
    ifM (IOS a) (IOS t) (IOS e)
        = IOS $ do x <- a
                   if nullS x
                      then e
                      else t

    orElseM (IOS t) (IOS e)
        = IOS $ do x <- t
                   if nullS x
                      then e
                      else return x

instance (Monad s, Functor s, Sequence s) => MonadTry (IOSeq s) where
    tryM (IOS a) = IOS $
                   do x <- try' a
                      return $ case x of
                                 Left er -> return $ Left er
                                 Right t -> fmap Right t
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
