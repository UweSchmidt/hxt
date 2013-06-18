{-# LANGUAGE RankNTypes #-}

module Data.Sequence.ArrowTypes where

import           Control.Monad.MonadSeq

import           Data.Sequence.Monad.IdSeq
import           Data.Sequence.Monad.IOSeq
import           Data.Sequence.Monad.IOStateSeq
import           Data.Sequence.Monad.StateSeq
import           Data.Sequence.Seq
import           Data.Sequence.Sequence

-- ----------------------------------------

type LA       a b = a -> IdSeq        b
type SLA    s a b = a -> StateSeq   s b
type IOLA     a b = a -> IOSeq        b
type IOSLA  s a b = a -> IOStateSeq s b

-- this type needs RankNTypes, do we really need this ?

type SeqA   m a b = (MonadSeq m) => a -> m b

-- ----------------------------------------

-- | run a sequence (list) arrow

runLA :: (a -> IdSeq b) -> (a -> [b])
runLA f = \ x ->  fromS . unIdS $ f x

fromLA :: (MonadSeq m) =>
          (a -> IdSeq b) -> (a -> m b)
fromLA f = \ x -> convFrom . unIdS $ f x

-- ----------------------------------------

-- | run a state sequence (list) arrow

runSLA :: (a -> StateSeq st b) -> (st -> a -> (st, [b]))
runSLA f = \ s0 x ->
           let (xs, s1) = unSTS (f x) s0
           in (s1, fromS xs)

{-# INLINE runSLA #-}


fromSLA :: (MonadSeq m) =>
           st -> (a -> StateSeq st b) -> (a -> m b)
fromSLA s f =  fromList . (snd . runSLA f s)

{-# INLINE fromSLA #-}

-- ----------------------------------------

runSt :: st2 -> (a -> IOStateSeq (st, st2) b) -> (a -> IOStateSeq st b)
runSt s2 a
    = \ x -> ISS $ \ s1 ->
             do (ys, (s1', _s2')) <- unISS (a x) (s1, s2)
                return (ys, s1')

{-# INLINE runSt #-}


liftSt :: (a -> IOStateSeq st b) -> (a -> IOStateSeq (st, st2) b)
liftSt a
    = \ x -> ISS $ \ (s1, s2) ->
             do (ys, s1') <- unISS(a x) s1
                return (ys, (s1', s2))

{-# INLINE liftSt #-}


-- | run a state sequence (list) arrow in the IO monad

-- runIOSLA :: (a -> IOStateSeq st b) -> (st -> a -> IO (st, [b]))
runIOSLA :: IOSLA st a b -> (st -> a -> IO (st, [b]))
runIOSLA a
    = \ s0 x ->
      do (xs, s1) <- unISS (a x) s0
         return (s1, fromS xs)

{-# INLINE runIOSLA #-}

runIOSLA' :: IOSLA st a b -> (st -> a -> IO (st, Seq b))
runIOSLA' a
    = \ s0 x ->
      do (xs, s1) <- unISS (a x) s0
         return (s1, xs)

{-# INLINE runIOSLA' #-}

-- ----------------------------------------
