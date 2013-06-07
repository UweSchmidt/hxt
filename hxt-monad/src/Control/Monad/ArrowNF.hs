{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.ArrowNF
where

import           Control.DeepSeq
import           Control.Monad.MonadSequence

-- ----------------------------------------

-- simulating Control.ArrowNF with monads

rnfA :: (MonadList s m, Sequence s, NFData (s a)) =>
        (b -> m a) -> (b -> m a)
rnfA f = \ x -> f x >>=* (\ res -> returnS $!! res)

-- ----------------------------------------
