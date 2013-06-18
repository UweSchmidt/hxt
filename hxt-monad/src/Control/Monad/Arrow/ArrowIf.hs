module Control.Monad.Arrow.ArrowIf
where

import           Control.Monad                       ()
import           Control.Monad.Arrow.ArrowList
import           Control.Monad.Arrow.ArrowSubstitute
import           Control.Monad.MonadSeq

import           Data.List                           (partition)
import           Data.Sequence.Sequence

-- ----------------------------------------

-- simulating Control.ArrowIf with monads

ifA :: (MonadSeq m) => (a -> m b) -> (a -> m c) -> (a -> m c) -> (a -> m c)
ifA c t e = \ x -> c x >>=* (\ s -> if nullS s
                                    then e x
                                    else t x
                            )

ifP :: Monad m => (b -> Bool) -> (b -> m d) -> (b -> m d) -> (b -> m d)
ifP c t e = \ x -> if c x
                   then t x
                   else e x

neg :: (MonadSeq m) => (b -> m a) -> (b -> m b)
neg f = ifA f none this

whenA :: MonadSeq m => (b -> m b) -> (b -> m a) -> (b -> m b)
f `whenA` g = ifA g f this

whenP :: Monad m => (b -> m b) -> (b -> Bool) -> (b -> m b)
f `whenP` g         = ifP g f this

whenNot :: MonadSeq m => (b -> m b) -> (b -> m a) -> (b -> m b)
f `whenNot` g       = ifA g this f

whenNotP :: Monad m => (b -> m b) -> (b -> Bool) -> (b -> m b)
f `whenNotP` g = ifP g this f

-- | @ g \`guards\` f @ : when the predicate g holds, f is applied, else none

guards :: (MonadSeq m) => (b -> m a) -> (b -> m d) -> (b -> m d)
f `guards` g        = ifA f g none

guardsP :: (MonadSeq m) => (b -> Bool) -> (b -> m d) -> (b -> m d)
f `guardsP` g       = ifP f g none

filterA :: (MonadSeq m) => (b -> m a) -> (b -> m b)
filterA f           = ifA f this none

containing :: (MonadSeq m) => (b -> m a) -> (a -> m d) -> (b -> m a)
f `containing` g    = f >=> g `guards` this

notContaining :: (MonadSeq m) => (b -> m a) -> (a -> m d) -> (b -> m a)
f `notContaining` g = f >=> ifA g none this

orElse :: (MonadSeq m) => (a -> m b) -> (a -> m b) -> (a -> m b)
f `orElse` g = \ x -> f x >>=* (\ s -> if nullS s then g x else returnS s)

data IfThen a b = a :-> b

choiceA :: (MonadSeq m) => [IfThen (b -> m c) (b -> m d)] -> (b -> m d)
choiceA = foldr ifA' none
    where
      ifA' :: (MonadSeq m) => IfThen (a -> m b) (a -> m c) -> (a -> m c) -> a -> m c
      ifA' (g :-> f) = ifA g f

withDefault :: MonadSeq m => (b -> m c) -> c -> (b -> m c)
withDefault a d = a `orElse` constA d

tagA :: MonadSeq m => (b -> m c) -> (b -> m (Either b b))
tagA p = ifA p (return . Left) (return . Right)

spanA :: (MonadSeq m) => (b -> m b) -> ([b] -> m ([b],[b]))
spanA p = ifA ( arrL (take 1) >=> p )
              ( return . head &=& (return . tail >=> spanA p)
                >=>
                return . (\ ~(x, ~(xs,ys)) -> (x : xs, ys))
              )
              ( return . (\ l -> ([],l)) )

partitionA :: (MonadSeq m) => (b -> m b) -> ([b] -> m ([b],[b]))
partitionA  p = listA ( arrL id >=> tagA p )
                >=^
                ( (\ ~(l1, l2) -> (unTag l1, unTag l2) ) . partition (isLeft) )
    where
      isLeft (Left _) = True
      isLeft _        = False
      unTag = map (either id id)

{-# INLINE containing #-}
{-# INLINE filterA #-}
{-# INLINE guards #-}
{-# INLINE guardsP #-}
{-# INLINE ifA #-}
{-# INLINE ifP #-}
{-# INLINE neg #-}
{-# INLINE notContaining #-}
{-# INLINE orElse #-}
{-# INLINE whenA #-}
{-# INLINE whenNot #-}
{-# INLINE whenNotP #-}
{-# INLINE whenP #-}
{-# INLINE withDefault #-}

-- ----------------------------------------

