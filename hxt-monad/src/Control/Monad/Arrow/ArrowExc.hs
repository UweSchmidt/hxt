
module Control.Monad.Arrow.ArrowExc
where

import           Control.Exception                   (SomeException)
import           Control.Monad.Arrow.ArrowSubstitute
import           Control.Monad.MonadTry

-- ----------------------------------------

-- simulating Control.ArrowExc with monads

tryA :: MonadTry m => (b -> m c) -> (b -> m (Either SomeException c))
tryA a = tryM . a

catchA      :: MonadTry m => (b -> m c) -> (SomeException -> m c) -> (b -> m c)
catchA f h  = tryA f
              >=>
              ( h |=| return )

{-# INLINE tryA #-}
{-# INLINE catchA #-}

-- ----------------------------------------
