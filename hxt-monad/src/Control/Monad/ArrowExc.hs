{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.ArrowExc
where

import           Control.Exception           (SomeException)
import           Control.Monad.Arrow
import           Control.Monad.MonadSequence

-- ----------------------------------------

-- simulating Control.ArrowExc with monads

tryA :: MonadTry m => (b -> m c) -> (b -> m (Either SomeException c))
tryA a = tryM . a

catchA      :: MonadTry m => (b -> m c) -> (SomeException -> m c) -> (b -> m c)
catchA f h  = tryA f
              >>>
              ( h ||| returnA )

{-# INLINE tryA #-}
{-# INLINE catchA #-}

-- ----------------------------------------
