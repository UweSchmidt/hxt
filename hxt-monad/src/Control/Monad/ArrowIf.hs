{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.ArrowIf
where

import           Control.Monad
import           Control.Monad.Arrow
import           Control.Monad.ArrowList
import           Control.Monad.MonadSequence
import           Data.List                   (partition)

-- ----------------------------------------

-- simulating Control.ArrowIf with monads

ifA :: MonadCond m c => (b -> m a) -> (b -> m d) -> (b -> m d) -> (b -> m d)
ifA c t e = \ x -> ifM (convTo (c x)) (t x) (e x)

ifP :: Monad m => (b -> Bool) -> (b -> m d) -> (b -> m d) -> (b -> m d)
ifP c t e = \ x -> if c x
                   then t x
                   else e x
{-
neg :: MonadCond m => (b -> m c) -> (b -> m b)
neg f = ifA f none this

when :: MonadCond m => (b -> m b) -> (b -> m c) -> (b -> m b)
f `when` g = ifA g f this

whenP :: Monad m => (b -> m b) -> (b -> Bool) -> (b -> m b)
f `whenP` g         = ifP g f this

whenNot :: MonadCond m => (b -> m b) -> (b -> m c) -> (b -> m b)
f `whenNot` g       = ifA g this f

whenNotP :: Monad m => (b -> m b) -> (b -> Bool) -> (b -> m b)
f `whenNotP` g = ifP g this f

    -- | @ g \`guards\` f @ : when the predicate g holds, f is applied, else none

guards :: MonadCond m => (b -> m c) -> (b -> m d) -> (b -> m d)
f `guards` g        = ifA f g none

guardsP :: MonadPlus m => (b -> Bool) -> (b -> m d) -> (b -> m d)
f `guardsP` g       = ifP f g none

filterA :: MonadCond m => (b -> m c) -> (b -> m b)
filterA f           = ifA f this none

containing :: MonadCond m => (b -> m c) -> (c -> m d) -> (b -> m c)
f `containing` g    = f >>> g `guards` this

notContaining :: MonadCond m => (b -> m c) -> (c -> m d) -> (b -> m c)
f `notContaining` g = f >>> ifA g none this
-- -}

orElse :: MonadCond m a => (b -> m c) -> (b -> m c) -> (b -> m c)
orElse t e = \ x -> orElseM (t x) (e x)

{-
data IfThen a b = a :-> b

choiceA :: MonadCond m => [IfThen (b -> m c) (b -> m d)] -> (b -> m d)
choiceA = foldr ifA' none
    where
      ifA' (g :-> f) = ifA g f

withDefault :: MonadCond m => (b -> m c) -> c -> (b -> m c)
withDefault a d = a `orElse` constA d

tagA :: MonadCond m => (b -> m c) -> (b -> m (Either b b))
tagA p = ifA p (arr Left) (arr Right)

spanA :: (MonadSequence m, MonadCond m) => (b -> m b) -> ([b] -> m ([b],[b]))
spanA p = ifA ( arrL (take 1) >>> p )
              ( arr head &&& (arr tail >>> spanA p)
                >>>
                arr (\ ~(x, ~(xs,ys)) -> (x : xs, ys))
              )
              ( arr (\ l -> ([],l)) )

partitionA :: (MonadSequence m, MonadCond m) => (b -> m b) -> ([b] -> m ([b],[b]))
partitionA  p = listA ( arrL id >>> tagA p )
                >>^
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
{-# INLINE when #-}
{-# INLINE whenNot #-}
{-# INLINE whenNotP #-}
{-# INLINE whenP #-}
{-# INLINE withDefault #-}

-- ----------------------------------------
-- -}
