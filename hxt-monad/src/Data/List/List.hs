{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.List.List
{- just for testing
    ( List(..)
    , LA

    , dropTree
    , foldTree
    , headTree
    , initTree
    , isFail
    , lastTree
    , reverseTree
    , sequenceTree
    , sizeTree
    , substTree
    , substTreeM
    , tailTree
    , takeTree
    , zipTree
    )
-- -}
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.MonadList
import           Data.Monoid

-- ----------------------------------------

type LA a b = a -> List b

data List a
    = List { unList :: [a] }
    | Fail String
      deriving (Eq, Show)

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
    (List xs) >>= f = List $ concat . map (unList . f) $ xs
    (Fail s)  >>= _ = Fail s
    fail   = Fail

    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    {-# INLINE fail #-}

instance MonadPlus List where
    mzero = List []
    (List xs) `mplus` (List ys) = List $ xs ++ ys
    (Fail s ) `mplus` _         = Fail s
    _         `mplus` (Fail s ) = Fail s

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadError String List where
    throwError = Fail

    catchError (Fail s) h = h s
    catchError t        _ = t

    {-# INLINE throwError #-}

instance Monoid (List a) where
    mempty  = List []
    (List xs) `mappend` (List ys) = List $ xs ++ ys
    (Fail s)  `mappend` _         = Fail s
    _         `mappend` (Fail s)  = Fail s

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance MonadList List where
    fromList = List
    toList (List xs)   = return xs
    toList (Fail _)    = return []

    {-# INLINE fromList #-}
    {-# INLINE toList #-}

instance MonadConv List [] where
    convFrom   = fromList
    convTo     = toList

    {-# INLINE convFrom #-}
    {-# INLINE convTo #-}

instance MonadConv List List where
    convFrom = id
    convTo   = return

    {-# INLINE convFrom #-}
    {-# INLINE convTo #-}

instance MonadCond List where
    ifM (Fail s)  _ _ = Fail s
    ifM (List []) _ e = e
    ifM _         t _ = t

    orElseM (Fail s)  _ = Fail s
    orElseM (List []) e = e
    orElseM t         _ = t

{- there's no need for exporting construtors

   Tip   = return  or Tip   = pure
   bin   = mappend or bin   = mplus      or bin = (<>)	-- bin smart constr for Bin
   Empty = mempty  or Empty = mzero
   Fail  = fail    or Fail  = throwError

-- -}

-- ----------------------------------------

substListM :: (Functor m, Monad m) => List a -> (a -> m (List b)) -> m (List b)
substListM (List xs) k = fmap (List . concat . map unList) . sequence . map k $ xs
substListM (Fail s)  _ = return (Fail s)

-- ----------------------------------------


