{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.ArrowIO
where

import           Control.Monad
import           Control.Monad.Error

-- ----------------------------------------

-- simulating Control.ArrowIO with monads

arrIO :: MonadIO m => (b -> IO c) -> (b -> m c)
arrIO f = liftIO . f

arrIO0 :: MonadIO m => IO c -> (b -> m c)
arrIO0 f = arrIO (const f)

arrIO2 :: MonadIO m => (b1 -> b2 -> IO c) -> ((b1, b2) -> m c)
arrIO2 f = arrIO (\ ~(x1, x2) -> f x1 x2)

arrIO3 :: MonadIO m => (b1 -> b2 -> b3 -> IO c) -> ((b1, (b2, b3)) -> m c)
arrIO3 f = arrIO (\ ~(x1, ~(x2, x3)) -> f x1 x2 x3)

arrIO4 :: MonadIO m => (b1 -> b2 -> b3 -> b4 -> IO c) -> ((b1, (b2, (b3, b4))) -> m c)
arrIO4 f            = arrIO (\ ~(x1, ~(x2, ~(x3, x4))) -> f x1 x2 x3 x4)

isIOA :: (MonadPlus m, MonadIO m) => (b -> IO Bool) -> (b -> m b)
isIOA p = \ x -> liftIO (p x) >>= \ b -> if b then return x else mzero

{-# INLINE arrIO #-}
{-# INLINE arrIO0 #-}
{-# INLINE arrIO2 #-}
{-# INLINE arrIO3 #-}
{-# INLINE arrIO4 #-}
{-# INLINE isIOA #-}

-- ----------------------------------------
