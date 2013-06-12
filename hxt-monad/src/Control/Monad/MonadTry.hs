-- ----------------------------------------

module Control.Monad.MonadTry
where

import           Control.Exception   (SomeException)
import           Control.Monad.Error

-- ----------------------------------------

-- | catch exceptions from the IO monad

class MonadIO m => MonadTry m where
    tryM :: m s -> m (Either SomeException s)

-- ----------------------------------------
