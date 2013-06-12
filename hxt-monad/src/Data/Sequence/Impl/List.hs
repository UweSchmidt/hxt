{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- ----------------------------------------
{- |

   A Sequence implementation with buildin Haskell lists

-}
-- ----------------------------------------

module Data.Sequence.Impl.List
    ( Seq )
where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad

import           Data.Foldable               (Foldable)
import qualified Data.Foldable               as F
import           Data.Monoid
import           Data.Sequence.ErrorSequence
import           Data.Sequence.Sequence

-- ----------------------------------------

newtype Seq a
    = List { unList :: [a] }
      deriving (Eq, Show)

-- ----------------------------------------

instance Sequence Seq where
    emptyS                  = List []
    consS x xs              = List (x : unList xs)
    unconsS (List [])       = Nothing
    unconsS (List (x : xs)) = Just (x, List xs)
    nullS                   = null . unList
    toS                     = List
    fromS                   = unList
    substS                  = substListM

    {-# INLINE emptyS  #-}
    {-# INLINE consS   #-}
    {-# INLINE unconsS #-}
    {-# INLINE nullS   #-}
    {-# INLINE toS     #-}
    {-# INLINE fromS   #-}
    {-# INLINE substS  #-}

instance ErrorSequence e Seq where
    failS s         = Right s

-- ----------------------------------------

instance NFData a => NFData (Seq a) where
    rnf (List x) = rnf x

instance Functor Seq where
    fmap f (List xs) = List $ map f xs

    {-# INLINE fmap #-}

instance Applicative Seq where
    pure  = return
    (<*>) = ap

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monad Seq where
    return x = List [x]
    (List xs) >>= f = foldr mappend mempty . map f $ xs

    {-# INLINE return #-}
    {-# INLINE (>>=) #-}

instance MonadPlus Seq where
    mzero = List []
    (List xs) `mplus` (List ys) = List $ xs ++ ys

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance Monoid (Seq a) where
    mempty  = List []
    (List xs) `mappend` (List ys) = List $ xs ++ ys

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance Foldable Seq where
    foldr op z (List xs) = foldr op z xs

-- ----------------------------------------

substListM :: Monad m => Seq a -> (a -> m (Seq b)) -> m (Seq b)
substListM (List xs) k = do rs <- sequence . map k $ xs
                            return $ foldr mappend mempty rs

-- ----------------------------------------


