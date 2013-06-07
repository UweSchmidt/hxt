{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- ----------------------------------------
{- |

   Common features of sequences (lists, trees, ...)

-}
-- ----------------------------------------

module Control.Monad.MonadSequence
    ( module Control.Monad.MonadSequence
    , module Control.Monad
    , module Control.Applicative
    )
where

import           Control.Applicative
import           Control.Exception   (SomeException)
import           Control.Monad       hiding (when)
import           Control.Monad.Error

import           Data.Foldable       (Foldable)
import           Data.Maybe

-- ----------------------------------------

{- old stuff
-- | Conversion between list like containers and lists
--
-- Law: @toList . fromList = id@

class Monad m => MonadSeq m where
    fromList :: [a] -> m a
    toList   :: m a -> m [a]

-- | constructing monadic actions from sequences
-- and extracting sequences out of monadic actions

class (Monad m, Sequence s) => MonadConv m s | m -> s where
    convFrom :: s a -> m a
    convTo   :: m a -> m (s a)

-- | Monadic branching

class (MonadPlus m, MonadConv m s) => MonadCond m s | m -> s where
    ifM      :: m a -> m b -> m b -> m b
    orElseM  :: m a -> m a -> m a
-- -}
-- | catch exceptions from the IO monad

class MonadIO m => MonadTry m where
    tryM :: m s -> m (Either SomeException s)

-- | Common features of monadic computations with sequences of values.
--
-- The constaint list bundles the necessary features to simplify constraints
-- in function signatures

-- class (MonadList s m, Sequence s) => MonadSequence m s | m -> s where

infixl 1 >>=*
infixr 1 >=>*

-- MonadPlus m is added to simplify context specs,
-- else the context (MonadPlus m, MonadList s m) => is necessary at many places

class (Monad m, MonadPlus m, Sequence s) => MonadList s m | m -> s where
    returnS :: s a -> m a
    (>>=*)  :: m a -> (s a -> m b) -> m b

{- old stuff : results in funDep conflict
instance (Sequence s) => MonadList s s where
    returnS = id
    x >>=* f = f x

    {-# INLINE returnS #-}
    {-# INLINE (>>=*)  #-}
-- -}

(>=>*) :: (MonadList s m) => (a -> m b) -> (s b -> m c) -> (a -> m c)
f >=>* g = \ x -> f x >>=* g

convFrom :: (MonadList s m) => s a -> m a
convFrom = returnS

fromList :: (MonadList s m) => [a] -> m a
fromList = returnS . toS

toList :: (MonadList s m) => m a -> m [a]
toList m = m >>=* return . fromS

{-# INLINE (>=>*)   #-}
{-# INLINE fromList #-}
{-# INLINE toList   #-}

-- ----------------------------------------

-- | Common features of a sequence datatype
--
-- The long list of constraints bundles the necessary list of constraints
-- in function signatures into the only constraint @(Sequence s) => ...@

class (Functor s, Applicative s, Foldable s, Monad s, MonadPlus s) => Sequence s where
    emptyS  :: s a
    consS   :: a -> s a -> s a
    unconsS :: s a -> Maybe (a, s a)

    nullS   :: s a -> Bool
    fromS   :: s a -> [a]
    toS     :: [a] -> s a

    -- | The important operation for lifting nonderministic computations

    substS  :: Monad m => s a -> (a -> m (s b)) -> m (s b)

    nullS   = isNothing . unconsS
    fromS   = maybe [] (\ (x, xs) -> x : fromS xs) . unconsS
    toS     = foldr consS emptyS

    {-# INLINE nullS #-}
    {-# INLINE toS   #-}
    {-# INLINE fromS #-}

-- | Extraction of errors out of sequence datatypes

class ErrSeq e s | s -> e where
    failS :: s a -> Either e (s a)

    errS  :: s a -> Bool
    errS  = either (const True) (const False) . failS

    {-# INLINE errS #-}

-- ----------------------------------------

-- | Example instance for pure lists

instance Sequence [] where
    emptyS           = []
    consS            = (:)
    unconsS []       = Nothing
    unconsS (x : xs) = Just (x, xs)
    nullS            = null
    toS              = id
    fromS            = id
    substS xs k      = (sequence . map k $ xs) >>= return . concat

    {-# INLINE emptyS  #-}
    {-# INLINE consS   #-}
    {-# INLINE unconsS #-}
    {-# INLINE nullS   #-}
    {-# INLINE toS     #-}
    {-# INLINE fromS   #-}

{-
instance MonadList [] [] where
    returnS = id
    xs >>=* f = f xs


instance MonadSeq [] where
    fromList = id
    toList   = return
-- -}

-- | Pure lists never contain an error value

instance ErrSeq () [] where
    failS = Right

-- ----------------------------------------

