{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- ----------------------------------------

module Data.Sequence.IdSeq
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSeq

import           Data.Sequence.ErrorSequence
import           Data.Sequence.Seq

-- ----------------------------------------

newtype IdSeq a = IdS {unIdS :: Seq a}
    deriving (Functor, Applicative, Monad, MonadPlus)


instance (ErrorSequence e Seq, MonadError e Seq) => MonadError e IdSeq where
    throwError e         = IdS $ throwError e

    catchError (IdS a) h = IdS $
                              case failS a of
                                Left  s -> (unIdS . h) s
                                Right _ -> a

    {-# INLINE throwError #-}

instance MonadSeq IdSeq where
    returnS xs      = IdS $ xs
    (IdS xs) >>=* f = IdS $ unIdS $ f xs

    {-# INLINE returnS #-}
    {-# INLINE (>>=*)  #-}

-- ----------------------------------------
