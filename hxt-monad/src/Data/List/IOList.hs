{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.List.IOList
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.MonadList

-- ----------------------------------------

-- TODO: error case not handled

newtype IOList a = IOL {unIOL :: IO [a]}

instance Functor IOList where
    fmap f (IOL a) = IOL $ a >>= return . fmap f

instance Applicative IOList where
    pure = return
    (<*>) = ap

instance Monad IOList where
    return        = IOL . return . return
    (IOL a) >>= f = IOL $ a >>= fmap concat . sequence . fmap (unIOL . f)
    fail _        = IOL $ return []

instance MonadList IOList where
    fromList = IOL . return
    toList (IOL a) = IOL $ a >>= return . return

-- ----------------------------------------
