-- {-# LANGUAGE FlexibleInstances      #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses  #-}

-- ----------------------------------------

module Data.Sequence.Sequence
where

import           Data.Maybe

-- ----------------------------------------

-- | Common features of a sequence datatype
--
-- The long list of constraints bundles the necessary list of constraints
-- in function signatures into the only constraint @(Sequence s) => ...@

class Sequence s where
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

-- ----------------------------------------
