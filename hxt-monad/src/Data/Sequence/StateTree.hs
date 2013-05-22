{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Sequence.StateTree
where

import           Control.Monad.State.Strict
import           Data.Sequence.Tree

type StateTree s a = StateT s Tree a

type SLA s a b = a -> StateTree s b

data Hole x = Hole

runSLA :: SLA s a b -> s -> a -> (s, x)
runSLA action s0 x = undefined
    -- = runState action s0 undefined -- (action x)


