
module Control.Monad.Arrow.ArrowCompatible
where

import           Control.Monad.Arrow.ArrowSubstitute

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
returnA = return

arr :: Monad m => (b -> c) -> b -> m c
arr f = return . f

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>>>) = (>=>)

(^>>) :: Monad m => (a -> b) -> (b -> m c) -> a -> m c
(^>>) = (^=>)

(>>^) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(>>^) = (>=^)

(<<<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<<<) = (<=<)

(^<<) :: Monad m => (c -> d) -> (b -> m c) -> b -> m d
(^<<) = (^=<)

(<<^) :: Monad m => (c -> m d) -> (b -> c) -> b -> m d
(<<^) = (<=^)

first :: Monad m => (b -> m c) -> (b,d) -> m (c,d)
first = firstA

second :: Monad m => (b -> m c) -> (d,b) -> m (d,c)
second = secondA

(&&&) :: Monad m => (b -> m c) -> (b -> m c') -> (b -> m (c,c'))
(&&&) = (&=&)

(***) :: Monad m => (b -> m c) -> (b' -> m c') -> ((b,b') -> m (c,c'))
(***) = (*=*)

zeroArrow :: MonadPlus m => a -> m b
zeroArrow = zeroA

(<+>) :: MonadPlus m => (a -> m b) -> (a -> m b) -> (a -> m b)
(<+>) = (<++>)

left :: Monad m => (b -> m c) -> (Either b d) -> m (Either c d)
left = leftA

right :: Monad m => (b -> m c) -> (Either d b) -> m (Either d c)
right = rightA

(+++) :: Monad m => (b -> m c) -> (b' -> m c') -> (Either b b') -> m (Either c c')
(+++) = (+=+)

(|||) :: Monad m => (b -> m d) -> (c -> m d) -> (Either b c) -> m d
(|||) = (|=|)

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
