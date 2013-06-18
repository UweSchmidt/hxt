-- ------------------------------------------------------------

{- |
   The HXT interface for compatibility to HXT.9 based
   arrow implementation.

   The names defined in this module simulate the
   arrow operators and function with a monadic implementation.

   To prevent nameing conflicts, do not import
   Control.Arrow (unqualified), and do not import
   Control.Monad (when) unqualified

   When importing this module, instead of @Text.XML.HXT.Monad@,
   there are only few points, where refactoring of old HXT code
   becomes necessary.
-}

-- ------------------------------------------------------------

module Text.XML.HXT.ArrowCompat
    ( module Text.XML.HXT.Monad
    , module Text.XML.HXT.ArrowCompat
    )
where

import           Text.XML.HXT.Monad hiding (when)

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

when :: MonadSeq m => (b -> m b) -> (b -> m a) -> (b -> m b)
when = whenA

-- ----------------------------------------
