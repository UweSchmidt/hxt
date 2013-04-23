{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.List.IOTree
where

import           Control.Exception

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadList
import           Data.List.Tree

-- ----------------------------------------

type IOLA a b = a -> IOTree b

newtype IOTree a = IOT {unIOT :: IO (Tree a)}

instance Functor IOTree where
    fmap f (IOT a) = IOT $ a >>= return . fmap f

instance Applicative IOTree where
    pure  = return
    (<*>) = ap

instance Monad IOTree where
    return        = IOT . return . return
    (IOT a) >>= f = IOT $ a >>= \ x -> substTreeM x (unIOT . f)
    fail          = IOT . return . fail

instance MonadPlus IOTree where
    mzero                   = IOT $ return mzero
    (IOT x) `mplus` (IOT y) = IOT $ liftM2 mplus x y

instance MonadError String IOTree where
    throwError = IOT . return . throwError

    catchError (IOT a) h = IOT $
                           do t <- a
                              case t of
                                Fail s -> (unIOT . h) s
                                _      -> return t

instance MonadIO IOTree where
    liftIO x = IOT $ x >>= return . return

instance MonadList IOTree where
    fromList = IOT . return . fromList
    toList (IOT a) = IOT $ a >>= return . toList

instance MonadConv IOTree [] where
    convFrom   = fromList
    convTo     = toList

instance MonadConv IOTree Tree where
    convFrom = IOT . return
    convTo (IOT a)   = IOT $ a >>= return . return

instance MonadCond IOTree where
    ifM (IOT a) (IOT t) (IOT e) = IOT $
                                  do x <- a
                                     case x of
                                       Empty  -> e
                                       Fail s -> return (Fail s)
                                       _      -> t

    orElseM (IOT t) (IOT e) = IOT $
                              do x <- t
                                 case x of
                                   Empty  -> e
                                   Fail s -> return (Fail s)
                                   _      -> t

instance MonadTry IOTree where
    tryM (IOT a) = IOT $
                   do x <- try' a
                      return $ case x of
                                 Left er -> return $ Left er
                                 Right t -> fmap Right t
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
