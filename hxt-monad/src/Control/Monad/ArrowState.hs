{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.ArrowState
where

import           Control.Monad.Arrow
import           Control.Monad.State.Strict

-- ----------------------------------------

-- simulate ArrowState with monads

changeState :: (MonadState s m) => (s -> b -> s) -> (b -> m b)
changeState f = \ x -> modify (flip f x) >> return x

accessState :: (MonadState s m) => (s -> b -> c) -> (b -> m c)
accessState f = \ x -> gets (flip f x)

getState :: (MonadState s m) => b -> m s
getState = accessState (\ s _x -> s)

setState :: (MonadState s m) => s -> m s
setState = changeState (\ _s x -> x)     -- changeState (const id)

nextState :: (MonadState s m) => (s -> s) -> (b -> m s)
nextState sf = changeState (\s -> const (sf s))
               >>>
               getState

-- ----------------------------------------
