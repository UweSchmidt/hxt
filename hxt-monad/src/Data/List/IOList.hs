{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.List.IOList
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadList
import           Data.List.List

-- ----------------------------------------

type IOLA a b = a -> IOList b

newtype IOList a = IOL {unIOL :: IO (List a)}

instance Functor IOList where
    fmap f (IOL a) = IOL $ a >>= return . fmap f

    {-# INLINE fmap #-}

instance Applicative IOList where
    pure = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance Monad IOList where
    return        = IOL . return . return
    (IOL a) >>= f = IOL $ a >>= \ x -> substListM x (unIOL . f)
    fail          = IOL . return . fail

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}

instance MonadPlus IOList where
    mzero                   = IOL $ return mzero
    (IOL x) `mplus` (IOL y) = IOL $ liftM2 mplus x y

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}


instance MonadError String IOList where
    throwError = IOL . return . throwError

    catchError (IOL a) h = IOL $
                           do t <- a
                              case t of
                                Fail s -> (unIOL . h) s
                                _      -> return t

    {-# INLINE throwError #-}

instance MonadIO IOList where
    liftIO x = IOL $ x >>= return . return

    {-# INLINE liftIO #-}

instance MonadList IOList where
    fromList = IOL . return . fromList
    toList (IOL a) = IOL $ a >>= return . toList

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance MonadConv IOList [] where
    convFrom   = fromList
    convTo     = toList

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}

instance MonadCond IOList where
    ifM (IOL a) (IOL t) (IOL e) = IOL $
                                  do x <- a
                                     case x of
                                       List [] -> e
                                       Fail s  -> return (Fail s)
                                       _       -> t

    orElseM (IOL t) (IOL e) = IOL $
                              do x <- t
                                 case x of
                                   List [] -> e
                                   Fail s  -> return (Fail s)
                                   _       -> t


instance MonadTry IOList where
    tryM (IOL a) = IOL $
                   do x <- try' a
                      return $ case x of
                                 Left er -> return $ Left er
                                 Right t -> fmap Right t
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
