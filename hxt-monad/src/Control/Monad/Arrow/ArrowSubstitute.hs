
module Control.Monad.Arrow.ArrowSubstitute
    ( module Control.Monad.Arrow.ArrowSubstitute
    , module Control.Monad
    )
where

import           Control.Monad (MonadPlus, mplus, mzero, (<=<), (>=>))

-- ----------------------------------------

-- simulating Control.Arrow with monad oprations

infixr 5 <++>
infixr 3 *=*
infixr 3 &=&
infixr 2 +=+
infixr 2 |=|
infixr 1 ^=>, >=^
infixr 1 ^=<, <=^

(^=>) :: Monad m => (a -> b) -> (b -> m c) -> a -> m c
f ^=> g = (return . f) >=> g

(>=^) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
f >=^ g = f >=> (return . g)

(^=<) :: Monad m => (c -> d) -> (b -> m c) -> b -> m d
f ^=< a = (return . f) <=< a

(<=^) :: Monad m => (c -> m d) -> (b -> c) -> b -> m d
a <=^ f = a <=< (return . f)

firstA :: Monad m => (b -> m c) -> (b,d) -> m (c,d)
firstA f (x, y) = do r <- f x
                     return (r, y)

secondA :: Monad m => (b -> m c) -> (d,b) -> m (d,c)
secondA f (x, y) = do r <- f y
                      return (x, r)

(&=&) :: Monad m => (b -> m c) -> (b -> m c') -> (b -> m (c,c'))
(f &=& g) x = do r1 <- f x
                 r2 <- g x
                 return (r1, r2)

(*=*) :: Monad m => (b -> m c) -> (b' -> m c') -> ((b,b') -> m (c,c'))
(f *=* g) (x, y) = do r1 <- f x
                      r2 <- g y
                      return (r1, r2)

zeroA :: MonadPlus m => a -> m b
zeroA = const mzero

(<++>) :: MonadPlus m => (a -> m b) -> (a -> m b) -> (a -> m b)
f <++> g = \ x -> f x `mplus` g x

leftA :: Monad m => (b -> m c) -> (Either b d) -> m (Either c d)
leftA f (Left  l) = f l >>= return . Left
leftA _ (Right r) = return $ Right r

rightA :: Monad m => (b -> m c) -> (Either d b) -> m (Either d c)
rightA f (Right r) = f r >>= return . Right
rightA _ (Left  l) = return $ Left l

(+=+) :: Monad m => (b -> m c) -> (b' -> m c') -> (Either b b') -> m (Either c c')
(f +=+ _) (Left  l) = f l >>= return . Left
(_ +=+ g) (Right r) = g r >>= return . Right

(|=|) :: Monad m => (b -> m d) -> (c -> m d) -> (Either b c) -> m d
f |=| g = f +=+ g >=> (return . untag)
    where
      untag (Left  x) = x
      untag (Right y) = y

{-# INLINE (^=<) #-}
{-# INLINE (^=>) #-}
{-# INLINE (<=^) #-}
{-# INLINE (<++>) #-}
{-# INLINE (>=^) #-}
{-# INLINE (|=|) #-}
{-# INLINE (*=*) #-}
{-# INLINE (&=&) #-}
{-# INLINE (+=+) #-}
{-# INLINE firstA #-}
{-# INLINE leftA #-}
{-# INLINE rightA #-}
{-# INLINE secondA #-}
{-# INLINE zeroA #-}

-- ----------------------------------------
