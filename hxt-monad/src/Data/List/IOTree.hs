{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.List.IOTree
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence
import           Data.List.Tree

-- ----------------------------------------

newtype IOTree a = IOT {unIOT :: IO (Tree a)}

instance Functor IOTree where
    fmap f (IOT a) = IOT $ a >>= return . fmap f

    {-# INLINE fmap #-}

instance Applicative IOTree where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance Monad IOTree where
    return        = IOT . return . return
    (IOT a) >>= f = IOT $ a >>= \ x -> substS x (unIOT . f)
    fail          = IOT . return . fail

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}

instance MonadPlus IOTree where
    mzero                   = IOT $ return mzero
    (IOT x) `mplus` (IOT y) = IOT $ liftM2 mplus x y

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadError (Tree String) IOTree where
    throwError = IOT . return . throwError

    catchError (IOT a) h = IOT $
                           do t <- a
                              case t of
                                Fail s -> (unIOT . h) s
                                _      -> return t

    {-# INLINE throwError #-}

instance MonadIO IOTree where
    liftIO x = IOT $ x >>= return . return

    {-# INLINE liftIO #-}

instance MonadSequence IOTree where
    fromList = IOT . return . fromList
    toList (IOT a) = IOT $ a >>= return . toList

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance MonadConv IOTree Tree where
    convFrom       = IOT . return
    convTo (IOT a) = IOT $ a >>= return . return

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}

instance MonadCond IOTree Tree where
    ifM (IOT a) (IOT t) (IOT e)
        = IOT $ do x <- a
                   if nullS x
                      then e
                      else t

    orElseM (IOT t) (IOT e)
        = IOT $ do x <- t
                   if nullS x
                      then e
                      else return x

instance MonadTry IOTree where
    tryM (IOT a) = IOT $
                   do x <- try' a
                      return $ case x of
                                 Left er -> return $ Left er
                                 Right t -> fmap Right t
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
