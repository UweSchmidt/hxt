{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Control.Monad.Arrow
where

import           Control.Monad

-- ----------------------------------------

-- simulating Control.Arrow with monad oprations

infixr 5 <+>
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 >>>, ^>>, >>^
infixr 1 <<<, ^<<, <<^

returnA :: Monad m => b -> m b
returnA = arr id

arr :: Monad m => (b -> c) -> b -> m c
arr f = return . f

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>>>) = (>=>)

(^>>) :: Monad m => (a -> b) -> (b -> m c) -> a -> m c
f ^>> g = arr f >>> g

(>>^) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
f >>^ g = f >>> arr g

(<<<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<<<) = flip (>>>)

(^<<) :: Monad m => (c -> d) -> (b -> m c) -> b -> m d
f ^<< a = arr f <<< a

(<<^) :: Monad m => (c -> m d) -> (b -> c) -> b -> m d
a <<^ f = a <<< arr f

first :: Monad m => (b -> m c) -> (b,d) -> m (c,d)
first f (x, y) = do r <- f x
                    return (r, y)

second :: Monad m => (b -> m c) -> (d,b) -> m (d,c)
second f (x, y) = do r <- f y
                     return (x, r)

(&&&) :: Monad m => (b -> m c) -> (b -> m c') -> (b -> m (c,c'))
(f &&& g) x = do r1 <- f x
                 r2 <- g x
                 return (r1, r2)

(***) :: Monad m => (b -> m c) -> (b' -> m c') -> ((b,b') -> m (c,c'))
(f *** g) (x, y) = do r1 <- f x
                      r2 <- g y
                      return (r1, r2)

zeroArrow :: MonadPlus m => a -> m b
zeroArrow = const $ mzero

(<+>) :: MonadPlus m => (a -> m b) -> (a -> m b) -> (a -> m b)
f <+> g = \ x -> f x `mplus` g x

left :: Monad m => (b -> m c) -> (Either b d) -> m (Either c d)
left f (Left  l) = f l >>= return . Left
left _ (Right r) = return $ Right r

right :: Monad m => (b -> m c) -> (Either d b) -> m (Either d c)
right f (Right r) = f r >>= return . Right
right _ (Left  l) = return $ Left l

(+++) :: Monad m => (b -> m c) -> (b' -> m c') -> (Either b b') -> m (Either c c')
(f +++ _) (Left  l) = f l >>= return . Left
(_ +++ g) (Right r) = g r >>= return . Right

(|||) :: Monad m => (b -> m d) -> (c -> m d) -> (Either b c) -> m d
f ||| g = f +++ g >>> arr untag
    where
      untag (Left  x) = x
      untag (Right y) = y

app :: Monad m => (b -> m c, b) -> m c
app (f, x) = f x

{-# INLINE (^<<) #-}
{-# INLINE (^>>) #-}
{-# INLINE (<<^) #-}
{-# INLINE (<<<) #-}
{-# INLINE (<+>) #-}
{-# INLINE (>>^) #-}
{-# INLINE (>>>) #-}
{-# INLINE (|||) #-}
{-# INLINE (***) #-}
{-# INLINE (&&&) #-}
{-# INLINE (+++) #-}
{-# INLINE app #-}
{-# INLINE arr #-}
{-# INLINE first #-}
{-# INLINE left #-}
{-# INLINE returnA #-}
{-# INLINE right #-}
{-# INLINE second #-}
{-# INLINE zeroArrow #-}

-- ----------------------------------------
