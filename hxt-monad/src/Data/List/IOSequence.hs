{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.List.IOSequence
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence
import           Data.List.List

-- ----------------------------------------

newtype IOSeq s a = IOL {unIOL :: IO (s a)}

instance (Functor s) => Functor (IOSeq s) where
    fmap f (IOL a) = IOL $ a >>= return . fmap f

    {-# INLINE fmap #-}

instance (Functor s, Monad s, Sequence s) => Applicative (IOSeq s) where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance (Monad s, Sequence s) => Monad (IOSeq s) where
    return        = IOL . return . return
    (IOL a) >>= f = IOL $ a >>= \ x -> substS x (unIOL . f)
    fail          = IOL . return . fail

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}

instance (MonadPlus s, Sequence s) => MonadPlus (IOSeq s) where
    mzero                   = IOL $ return mzero
    (IOL x) `mplus` (IOL y) = IOL $ liftM2 mplus x y

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}


instance (Monad s, Sequence s, ErrSeq [String] s, MonadError [String] s) => MonadError [String] (IOSeq s) where
    throwError = IOL . return . throwError

    catchError (IOL a) h = IOL $
                           do t <- a
                              case failS t of
                                Left  s -> (unIOL . h) s
                                Right _ -> return t

    {-# INLINE throwError #-}

instance (Monad s, Sequence s) => MonadIO (IOSeq s) where
    liftIO x = IOL $ x >>= return . return

    {-# INLINE liftIO #-}

instance (MonadSequence s, Sequence s) => MonadSequence (IOSeq s) where
    fromList = IOL . return . fromList
    toList (IOL a) = IOL $ a >>= return . toList

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance (MonadSequence s, Sequence s) => MonadConv (IOSeq s) s where
    convFrom   = IOL . return
    convTo (IOL a) = IOL $ a >>= return . return

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}


instance (MonadPlus s, MonadSequence s, Sequence s) => MonadCond (IOSeq s) s where
    ifM (IOL a) (IOL t) (IOL e)
        = IOL $ do x <- a
                   if nullS x
                      then e
                      else t

    orElseM (IOL t) (IOL e)
        = IOL $ do x <- t
                   if nullS x
                      then e
                      else return x

    {-# INLINE ifM     #-}
    {-# INLINE orElseM #-}

instance (Functor s, Monad s, Sequence s) => MonadTry (IOSeq s) where
    tryM (IOL a) = IOL $
                   do x <- try' a
                      return $ case x of
                                 Left er -> return $ Left er
                                 Right t -> fmap Right t
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
