{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Sequence.Types where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.MonadSequence

import           Data.Foldable                 (Foldable)
import           Data.Monoid
import           Data.Sequence.IOSequence
import           Data.Sequence.IOStateSequence
import qualified Data.Sequence.List            as SL
import qualified Data.Sequence.ListWithFailure as FL
import           Data.Sequence.StateSequence
import qualified Data.Sequence.Tree            as ST
import qualified Data.Sequence.TreeWithFailure as FT

-- ----------------------------------------

type LA0       a b = a ->                 SL.Seq   b
type SLA0    s a b = a -> StateSequence   SL.Seq s b
type IOLA0     a b = a -> IOSequence      SL.Seq   b
type IOSLA0  s a b = a -> IOStateSequence SL.Seq s b

type LA1      a b = a ->                 FL.Seq   b
type SLA1   s a b = a -> StateSequence   FL.Seq s b
type IOLA1    a b = a -> IOSequence      FL.Seq   b
type IOSLA1 s a b = a -> IOStateSequence FL.Seq s b

type LA2      a b = a ->                 ST.Tree   b
type SLA2   s a b = a -> StateSequence   ST.Tree s b
type IOLA2    a b = a -> IOSequence      ST.Tree   b
type IOSLA2 s a b = a -> IOStateSequence ST.Tree s b

type LA3      a b = a ->                 FT.Tree   b
type SLA3   s a b = a -> StateSequence   FT.Tree s b
type IOLA3    a b = a -> IOSequence      FT.Tree   b
type IOSLA3 s a b = a -> IOStateSequence FT.Tree s b

-- | The newtype wrapper enables us to change the underlying sequence implementation
-- by substituting SL.Seq by one of the other 3 possibilities: FL.Seq, ST.Tree, FT.Tree
-- or something else

newtype Seq a = Seq {unSeq :: (FT.Tree a)}
    deriving (Functor, Applicative, Monad, MonadPlus, Sequence, NFData, Monoid, Foldable)

instance MonadList Seq Seq where
    returnS = id
    xs >>=* f = f xs

    {-# INLINE returnS #-}
    {-# INLINE (>>=*) #-}

{- old stuff
instance MonadSequence Seq Seq where

instance MonadConv Seq Seq where
    convFrom = id
    convTo   = return

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}

instance MonadCond Seq Seq where
    ifM     (Seq c) (Seq t) (Seq e) = Seq $ ifM c t e
    orElseM         (Seq t) (Seq e) = Seq $ (t `orElseM` e)

    {-# INLINE ifM     #-}
    {-# INLINE orElseM #-}
-- -}

type LA       a b = a ->                 Seq   b
type SLA    s a b = a -> StateSequence   Seq s b
type IOLA     a b = a -> IOSequence      Seq   b
type IOSLA  s a b = a -> IOStateSequence Seq s b

type SeqA   m a b = (MonadList Seq m) => a -> m b

-- ----------------------------------------

-- | run a sequence (list) arrow

runLA :: (a -> Seq b) -> (a -> [b])
runLA f = fromS . f

fromLA :: (MonadList Seq m) =>
          (a -> Seq b) -> (a -> m b)
fromLA f = convFrom . f

-- ----------------------------------------

-- | run a state sequence (list) arrow

runSLA :: (a -> StateSequence Seq st b) -> (st -> a -> (st, [b]))
runSLA f = \ s0 x ->
           let (xs, s1) = unSTS (f x) s0
           in (s1, fromS xs)

{-# INLINE runSLA #-}


fromSLA :: (MonadList s m) =>
           st -> (a -> StateSequence Seq st b) -> (a -> m b)
fromSLA s f =  fromList . (snd . runSLA f s)

{-# INLINE fromSLA #-}

-- ----------------------------------------

runSt :: st2 -> (a -> IOStateSequence Seq (st, st2) b) -> (a -> IOStateSequence Seq st b)
runSt s2 a
    = \ x -> ISS $ \ s1 ->
             do (ys, (s1', _s2')) <- unISS (a x) (s1, s2)
                return (ys, s1')

{-# INLINE runSt #-}


liftSt :: (a -> IOStateSequence Seq st b) -> (a -> IOStateSequence Seq (st, st2) b)
liftSt a
    = \ x -> ISS $ \ (s1, s2) ->
             do (ys, s1') <- unISS(a x) s1
                return (ys, (s1', s2))

{-# INLINE liftSt #-}


-- | run a state sequence (list) arrow in the IO monad

runIOSLA :: (a -> IOStateSequence Seq st b) -> (st -> a -> IO (st, [b]))
runIOSLA a
    = \ s0 x ->
      do (xs, s1) <- unISS (a x) s0
         return (s1, fromS xs)

{-# INLINE runIOSLA #-}

-- ----------------------------------------
