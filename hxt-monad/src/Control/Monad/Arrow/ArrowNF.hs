module Control.Monad.Arrow.ArrowNF
where

import           Control.DeepSeq
import           Control.Monad.MonadSeq

-- ----------------------------------------

-- simulating Control.ArrowNF with monads

rnfA :: (MonadSeq m, NFData a) =>
        (b -> m a) -> (b -> m a)
rnfA f = \ x -> f x >>=* (\ res -> returnS $!! res)

-- ----------------------------------------
