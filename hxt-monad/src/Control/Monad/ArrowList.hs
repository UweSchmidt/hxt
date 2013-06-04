{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.ArrowList
where

import           Control.Monad
import           Control.Monad.Arrow
import           Control.Monad.MonadSequence

-- ----------------------------------------

-- simulating Control.ArrowList with monads

infixl 8 >>., >.

infixl 2 $<, $<<, $<<<, $<<<<
infixl 2 $<$

arr2 :: Monad m => (b1 -> b2 -> c) -> (b1, b2) -> m c
arr2 = arr . uncurry

arr3 :: Monad m => (b1 -> b2 -> b3 -> c) -> (b1, (b2, b3)) -> m c
arr3 f = arr (\ ~(x1, ~(x2, x3)) -> f x1 x2 x3)

arr4 :: Monad m => (b1 -> b2 -> b3 -> b4 -> c) -> (b1, (b2, (b3, b4))) -> m c
arr4 f = arr (\ ~(x1, ~(x2, ~(x3, x4))) -> f x1 x2 x3 x4)

arr2A :: Monad m => (b -> c -> m d) -> (b, c) -> m d
arr2A f = first (arr f) >>> app

arrL :: MonadSeq m => (b -> [c]) -> b -> m c
arrL f = fromList . f

arr2L :: MonadSeq m => (b -> c -> [d]) -> (b, c) -> m d
arr2L = arrL . uncurry

constA :: Monad m => c -> b -> m c
constA = arr . const

constL :: MonadSeq m => [c] -> b -> m c
constL = arrL . const

isA :: MonadPlus m => (b -> Bool) -> b -> m b
isA p x
    | p x       = return x
    | otherwise = mzero

(>>.) :: MonadSeq m => (b -> m c) -> ([c] -> [d]) -> (b -> m d)
f >>. g = \ x -> toList (f x) >>= return . g >>= fromList

(>.) :: MonadSeq m => (b -> m c) -> ([c] ->  d ) -> b -> m d
af >. f = af >>. ((:[]) . f)

listA :: MonadSeq m => (b -> m c) -> b -> m [c]
listA af = af >>.  (:[])

unlistA :: MonadSeq m => [b] -> m b
unlistA = arrL id

this :: Monad m => b -> m b
this = arr id

none :: MonadPlus m => b -> m c
none = zeroArrow

-- is defined in ArrowIf

-- withDefault :: MonadCond m => (b -> m c) -> c -> (b -> m c)
-- withDefault a d = a `orElse` constA d

single :: MonadSeq m => (b -> m c) -> b -> m c
single f = f >>. take 1

applyA :: Monad m => (b -> m (b -> m c)) -> (b -> m c)
applyA f = (f &&& this) >>> app

($<) :: Monad m => (c -> b -> m d) -> (b -> m c) -> (b -> m d)
g $< f = applyA (f >>> arr g)

($<<) :: Monad m => (c1 -> c2 -> b -> m d) -> (b -> m (c1, c2)) -> (b -> m d)
f $<< g = applyA (g >>> arr2 f)

($<<<) :: Monad m => (c1 -> c2 -> c3 -> b -> m d) -> (b -> m (c1, (c2, c3))) -> (b -> m d)
f $<<< g = applyA (g >>> arr3 f)

($<<<<) :: Monad m =>
           (c1 -> c2 -> c3 -> c4 -> b -> m d) ->
           (b -> m (c1, (c2, (c3, c4)))) ->
           (b -> m d)
f $<<<< g           = applyA (g >>> arr4 f)

($<$) :: MonadSeq m =>
         (c -> (b -> m b)) -> (b -> m c) -> (b -> m b)
g $<$ f = applyA (listA (f >>> arr g) >>> arr seqA)

mergeA :: Monad m =>
          (((a1, b1) -> m a1) -> ((a1, b1) -> m b1) -> ((a1, b1) -> m c)) ->
          ((a1, b1) -> m c)
mergeA op = (\ x -> arr fst `op` constA (snd x)) $< this

perform :: MonadSeq m => (b -> m c) -> (b -> m b)
perform f = listA f &&& this >>> arr snd

catA :: MonadPlus m => [b -> m c] -> b -> m c
catA = foldl (<+>) none

seqA :: Monad m => [b -> m b] -> b -> m b
seqA = foldl (>>>) this

{-# INLINE (>.) #-}
{-# INLINE ($<<<<) #-}
{-# INLINE ($<<<) #-}
{-# INLINE ($<<) #-}
{-# INLINE ($<) #-}
{-# INLINE applyA #-}
{-# INLINE arr2 #-}
{-# INLINE arr2A #-}
{-# INLINE arr2L #-}
{-# INLINE arr3 #-}
{-# INLINE arr4 #-}
{-# INLINE catA #-}
{-# INLINE constA #-}
{-# INLINE constL #-}
{-# INLINE listA #-}
{-# INLINE none #-}
{-# INLINE perform #-}
{-# INLINE seqA #-}
{-# INLINE single #-}
{-# INLINE this #-}
{-# INLINE unlistA #-}

-- ----------------------------------------
