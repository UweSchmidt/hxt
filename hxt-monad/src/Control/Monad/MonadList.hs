{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.MonadList
where

import           Control.Exception   (SomeException)
import           Control.Monad
import           Control.Monad.Error

-- ----------------------------------------

class Monad m => MonadList m where
    fromList :: [a] -> m a
    toList   :: m a -> m [a]

class (Monad m, Monad m1) => MonadConv m m1 where
    convFrom :: m1 a -> m a
    convTo   :: m a -> m (m1 a)

class MonadPlus m => MonadCond m where
    ifM     :: m b -> m c -> m c -> m c
    orElseM :: m c -> m c -> m c

-- | catch exceptions from the IO monad

class MonadIO m => MonadTry m where
    tryM :: m c -> m (Either SomeException c)

-- ----------------------------------------
