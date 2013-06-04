{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- ----------------------------------------
{- |

   A Sequence implementation with lists and failure

-}
-- ----------------------------------------

module Data.Sequence.ListWithFailure
{- just for testing
    ( List(..)
    , LA
    )
-- -}
where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence

import           Data.Foldable               (Foldable)
import qualified Data.Foldable               as F
import           Data.Monoid

-- ----------------------------------------

data Seq a
    = List { unList :: [a] }
    | Fail { unFail :: [String] }
      deriving (Eq, Show)

-- ----------------------------------------

instance Sequence Seq where
    emptyS                  = List []
    consS x xs              = List (x : unList xs)
    unconsS (List (x : xs)) = Just (x, List xs)
    unconsS _               = Nothing
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

instance ErrSeq [String] Seq where
    failS (Fail xs) = Left xs
    failS s         = Right s

-- ----------------------------------------

instance NFData a => NFData (Seq a) where
    rnf (List x) = rnf x
    rnf (Fail e) = rnf e

instance Functor Seq where
    fmap f (List xs) = List $ map f xs
    fmap _ (Fail s)  = Fail s

instance Applicative Seq where
    pure  = return
    (<*>) = ap

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monad Seq where
    return x = List [x]
    (List xs) >>= f = foldr mappend mempty . map f $ xs
    (Fail s)  >>= _ = Fail s
    fail   = Fail . (:[])

    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    {-# INLINE fail #-}

instance MonadPlus Seq where
    mzero = List []
    (List xs) `mplus` (List ys) = List $ xs ++ ys
    (Fail xs) `mplus` (Fail ys) = Fail $ xs ++ ys
    (Fail s ) `mplus` _         = Fail s
    _         `mplus` (Fail s ) = Fail s

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadError [String] Seq where
    throwError = Fail

    catchError (Fail s) h = h s
    catchError t        _ = t

    {-# INLINE throwError #-}

instance MonadSeq Seq where
    fromList = List
    toList (List xs)   = return xs
    toList (Fail _)    = return []

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance MonadConv Seq Seq where
    convFrom = id
    convTo   = return

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}

instance MonadCond Seq Seq where
    ifM (Fail s)  _ _ = Fail s
    ifM (List []) _ e = e
    ifM _         t _ = t

    orElseM (Fail s)  _ = Fail s
    orElseM (List []) e = e
    orElseM t         _ = t

    {-# INLINE ifM     #-}
    {-# INLINE orElseM #-}

instance Monoid (Seq a) where
    mempty  = List []
    (List xs) `mappend` (List ys) = List $ xs ++ ys
    (Fail xs) `mappend` (Fail ys) = Fail $ xs ++ ys
    (Fail xs) `mappend` _         = Fail xs
    _         `mappend` (Fail ys) = Fail ys

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance Foldable Seq where
    foldr op z (List xs) = foldr op z xs
    foldr _  z _         = z

-- ----------------------------------------

substListM :: Monad m => Seq a -> (a -> m (Seq b)) -> m (Seq b)
substListM (List xs) k = do rs <- sequence . map k $ xs
                            return $ foldr mappend mempty rs
substListM (Fail s)  _ = return (Fail s)

-- ----------------------------------------


