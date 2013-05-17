{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.List.List
{- just for testing
    ( List(..)
    , LA
    )
-- -}
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadSequence

import           Data.Monoid

-- ----------------------------------------

data List a
    = List { unList :: [a] }
    | Fail { unFail :: [String] }
      deriving (Eq, Show)

-- ----------------------------------------

instance Sequence List where
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

instance Seq List where

instance ErrSeq [String] List where
    failS (Fail xs) = Left xs
    failS s         = Right s

-- ----------------------------------------

instance Functor List where
    fmap f (List xs) = List $ map f xs
    fmap _ (Fail s)  = Fail s

instance Applicative List where
    pure  = return
    (<*>) = ap

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monad List where
    return x = List [x]
    (List xs) >>= f = foldr mappend mempty . map f $ xs
    (Fail s)  >>= _ = Fail s
    fail   = Fail . (:[])

    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    {-# INLINE fail #-}

instance MonadPlus List where
    mzero = List []
    (List xs) `mplus` (List ys) = List $ xs ++ ys
    (Fail xs) `mplus` (Fail ys) = Fail $ xs ++ ys
    (Fail s ) `mplus` _         = Fail s
    _         `mplus` (Fail s ) = Fail s

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadError [String] List where
    throwError = Fail

    catchError (Fail s) h = h s
    catchError t        _ = t

    {-# INLINE throwError #-}

instance Monoid (List a) where
    mempty  = List []
    (List xs) `mappend` (List ys) = List $ xs ++ ys
    (Fail xs) `mappend` (Fail ys) = Fail $ xs ++ ys
    (Fail xs) `mappend` _         = Fail xs
    _         `mappend` (Fail ys) = Fail ys

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance MonadSequence List where
    fromList = List
    toList (List xs)   = return xs
    toList (Fail _)    = return []

    {-# INLINE fromList #-}
    {-# INLINE toList   #-}

instance MonadConv List List where
    convFrom = id
    convTo   = return

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}

instance MonadCond List List where
    ifM (Fail s)  _ _ = Fail s
    ifM (List []) _ e = e
    ifM _         t _ = t

    orElseM (Fail s)  _ = Fail s
    orElseM (List []) e = e
    orElseM t         _ = t

    {-# INLINE ifM     #-}
    {-# INLINE orElseM #-}

-- ----------------------------------------

substListM :: Monad m => List a -> (a -> m (List b)) -> m (List b)
substListM (List xs) k = do rs <- sequence . map k $ xs
                            return $ foldr mappend mempty rs
substListM (Fail s)  _ = return (Fail s)

-- ----------------------------------------


