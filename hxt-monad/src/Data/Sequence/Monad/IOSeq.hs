{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- ----------------------------------------

module Data.Sequence.Monad.IOSeq
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSeq
import           Control.Monad.MonadTry

import           Data.Sequence.ErrorSequence
import           Data.Sequence.Seq
import           Data.Sequence.Sequence

-- ----------------------------------------

newtype IOSeq a = IOS {unIOS :: IO (Seq a)}

instance Functor IOSeq where
    fmap f (IOS a) = IOS $ a >>= return . fmap f

    {-# INLINE fmap #-}

instance Applicative IOSeq where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance Monad IOSeq where
    return        = IOS . return . return
    (IOS a) >>= f = IOS $ a >>= \ x -> substS x (unIOS . f)

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}

instance MonadPlus IOSeq where
    mzero                   = IOS $ return mzero
    (IOS x) `mplus` (IOS y) = IOS $ liftM2 mplus x y

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

-- MonadPlus laws violated !!!
-- (IOS $ print 42) >> mzero /= mzero

instance (ErrorSequence e Seq, MonadError e Seq) => MonadError e IOSeq where
    throwError           = IOS . return . throwError

    catchError (IOS a) h = IOS $
                           do t <- a
                              case failS t of
                                Left  s -> (unIOS . h) s
                                Right _ -> return t

    {-# INLINE throwError #-}

instance MonadIO IOSeq where
    liftIO x = IOS $ x >>= return . return

    {-# INLINE liftIO #-}

instance MonadSeq IOSeq where
    returnS xs      = IOS $ return xs
    (IOS m) >>=* f  = IOS $ m >>= unIOS . f

    {-# INLINE returnS #-}
    {-# INLINE (>>=*)  #-}

instance MonadTry IOSeq where
    tryM (IOS a) = IOS $
                   do res <- try' a
                      return $ case res of
                                 Left  er -> return (Left  er)
                                 Right xs -> fmap    Right xs
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try

-- ----------------------------------------
