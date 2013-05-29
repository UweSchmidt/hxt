{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.ArrowNF
where

import           Control.DeepSeq
import           Control.Monad.MonadSequence

-- ----------------------------------------

-- simulating Control.ArrowNF with monads

rnfA :: (MonadConv m s, Sequence s, NFData (s a)) =>
        (b -> m a) -> (b -> m a)
rnfA f = \ x -> convTo (f x) >>= (\ res -> return $!! res) >>= convFrom

-- ----------------------------------------
