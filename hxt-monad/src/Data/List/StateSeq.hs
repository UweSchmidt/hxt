{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- ----------------------------------------
{- |

   Lifting of sequences (lists, trees, ...)
   into the IO monad

-}
-- ----------------------------------------

module Data.List.StateSeq
where

import           Control.Applicative
-- import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence
import           Control.Monad.State.Strict
import           Data.List.List
import           Data.List.Tree

-- ----------------------------------------

type SLA' s st a b = a -> StateSeq s st b

type SLAt st a b = SLA' Tree st a b
type SLAs st a b = SLA' List st a b

-- ----------------------------------------

newtype StateSeq s st a = STS {unSTS :: st -> (s a, st)}

instance (Functor s) => Functor (StateSeq s st) where
    fmap f (STS a) = STS $ \ s0 ->
                     let (xs, s1) = a s0 in (fmap f xs, s1)

    {-# INLINE fmap #-}

instance (Monad s, Functor s, Sequence s) => Applicative (StateSeq s st) where
    pure  = return
    (<*>) = ap

    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}

instance (Monad s, Sequence s) => Monad (StateSeq s st) where
    return x      = STS $ \ s0 -> (return x, s0)
    (STS a) >>= f = STS ( \ s0 ->
                          let (xs, s1) = a s0 in
                          runState (substS xs f') s1
                        )
                    where
                      f' x = do s' <- get
                                let (xs, s'') = unSTS (f x) s'
                                put s''
                                return xs
    fail x        = STS $ \ s0 -> (fail x, s0)

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}


instance (MonadPlus s, Sequence s) => MonadPlus (StateSeq s st) where
    mzero                   = STS $ \ s0 -> (mzero, s0)
    (STS x) `mplus` (STS y) = STS $ \ s0 ->
                              let (xs, s1) = x s0
                                  (ys, s2) = y s1
                              in (xs `mplus` ys, s2)

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}


instance (Monad s, MonadError e s, Sequence s, ErrSeq e s) => MonadError e (StateSeq s st) where
    throwError x         = STS $ \ s0 -> (throwError x, s0)

    catchError (STS a) h = STS $ \ s0 ->
                                 let (xs, s1) = a s0 in
                                 case failS xs of
                                   Left e -> unSTS (h e) $ s1
                                   _      -> (xs, s1)

    {-# INLINE throwError #-}

instance(Monad s, Sequence s) => MonadState st (StateSeq s st) where
    get    = STS $ \  s0 -> (return s0, s0)
    put s1 = STS $ \ _s0 -> (return (), s1)

    {-# INLINE get #-}
    {-# INLINE put #-}

{-
instance (Monad s, Sequence s) => MonadIO (StateSeq s st) where
    liftIO x = STS $ x >>= return . return

    {-# INLINE liftIO #-}
-- -}

instance (MonadSequence s, Sequence s) => MonadSequence (StateSeq s st) where
    fromList xs    = STS $ \ s0 -> (fromList xs, s0)
    toList (STS a) = STS $ \ s0 -> let (xs, s1) = a s0 in (toList xs, s1)

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance (Monad s, Sequence s) => MonadConv (StateSeq s st) s where
    convFrom xs    = STS $ \ s0 -> (xs, s0)
    convTo (STS a) = STS $ \ s0 -> let (xs, s1) = a s0 in (return xs, s1)

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}

instance (MonadPlus s, Sequence s) => MonadCond (StateSeq s st) s where
    ifM (STS a) (STS t) (STS e)
        = STS $ \ s0 ->
                let (xs, s1) = a s0 in
                if nullS xs
                   then e s1
                   else t s1

    orElseM (STS t) (STS e)
        = STS $ \ s0 ->
                let (xs, s1) = t s0 in
                if nullS xs
                   then e s1
                   else (xs, s1)
{-
instance (Monad s, Functor s, Sequence s) => MonadTry (StateSeq s st) where
    tryM (STS a) = STS $
                   do x <- try' a
                      return $ case x of
                                 Left er -> return $ Left er
                                 Right t -> fmap Right t
        where
          try' :: IO a -> IO (Either SomeException a)
          try' = try
-- -}
-- ----------------------------------------
