{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.List.Tree
-- {- just for testing
    ( Tree(..)
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

type LA a b = a -> Tree b

data Tree a
    = Tip a
    | Bin (Tree a) (Tree a)
    | Empty
    | Fail String
      deriving (Eq, Show)

-- ----------------------------------------

instance Functor Tree where
    fmap f (Tip x)   = Tip (f x)
    fmap f (Bin l r) = Bin (fmap f l) (fmap f r)
    fmap _  Empty    = Empty
    fmap _ (Fail s)  = Fail s

instance Applicative Tree where
    pure = Tip				-- pure = return
    f <*> x = f >>= \ f' -> fmap f' x	-- (<*>) = ap

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monad Tree where
    return = Tip
    (>>=)  = substTree
    fail   = Fail

    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    {-# INLINE fail #-}

instance MonadPlus Tree where
    mzero = Empty
    mplus = bin

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadError String Tree where
    throwError = Fail

    catchError (Fail s) h = h s
    catchError t        _ = t

    {-# INLINE throwError #-}

instance Monoid (Tree a) where
    mempty  = Empty
    mappend = bin

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance MonadList Tree where
    fromList = fromListTree
    toList   = return . toListTree

    {-# INLINE fromList #-}
    {-# INLINE toList #-}

instance MonadConv Tree [] where
    convFrom   = fromListTree
    convTo     = return . toListTree

    {-# INLINE convFrom #-}
    {-# INLINE convTo #-}

instance MonadConv Tree Tree where
    convFrom = id
    convTo   = return

    {-# INLINE convFrom #-}
    {-# INLINE convTo #-}

instance MonadCond Tree where
    ifM (Fail s) _ _ = Fail s
    ifM Empty    _ e = e
    ifM _ t _        = t

    orElseM (Fail s) _ = Fail s
    orElseM Empty    e = e
    orElseM t        _ = t

{- there's no need for exporting construtors

   Tip   = return  or Tip   = pure
   bin   = mappend or bin   = mplus      or bin = (<>)	-- bin smart constr for Bin
   Empty = mempty  or Empty = mzero
   Fail  = fail    or Fail  = throwError

-- -}

-- ----------------------------------------

substTree :: Tree a -> (a -> Tree b) -> Tree b
substTree (Tip x)   k = k x
substTree (Bin l r) k = bin (substTree l k) (substTree r k)
substTree  Empty    _ = Empty
substTree (Fail s)  _ = Fail s

-- | Monadic version of substTree

substTreeM :: Monad m => Tree a -> (a -> m (Tree b)) -> m (Tree b)
substTreeM (Tip x)   k = k x
substTreeM (Bin l r) k = do l1 <- substTreeM l k
                            r1 <- substTreeM r k
                            return (bin l1 r1)
substTreeM  Empty    _ = return Empty
substTreeM (Fail s)  _ = return (Fail s)

-- | smart constructor for Bin

bin :: Tree a -> Tree a -> Tree a
bin     Empty   t2          = t2
bin t1          Empty       = t1
bin t1@(Fail _) _           = t1
bin _           t2@(Fail _) = t2
bin t1          t2          = Bin t1 t2

foldTree :: (String -> b) -> b -> (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree ff e tf bf t = fold' t
    where
      fold' (Tip x)   = tf x
      fold' (Bin l r) = bf (fold' l) (fold' r)
      fold'  Empty    = e
      fold' (Fail s)  = ff s

zipTree :: (a -> b -> c) -> (Tree a -> Tree b -> Tree c)
zipTree op (Tip x)     (Tip y)     = Tip $ x `op` y
zipTree op (Bin l1 r1) (Bin l2 r2) = bin (zipTree op l1 l2) (zipTree op r1 r2)
zipTree _  (Fail s1)   _           = Fail s1
zipTree _  _           (Fail s2)   = Fail s2
zipTree _  _           _           = Empty

sequenceTree :: Monad m => Tree (m a) -> m (Tree a)
sequenceTree (Tip x)   = x >>= return . Tip
sequenceTree (Bin l r) = do l1 <- sequenceTree l
                            r1 <- sequenceTree r
                            return (Bin l1 r1)
sequenceTree  Empty    = return Empty
sequenceTree (Fail s)  = return (Fail s)

-- ----------------------------------------

fromListTree :: [a] -> Tree a
fromListTree
    = merge . map return
      where
        merge []  = mempty
        merge [t] = t
        merge ts  = merge . combine $ ts

        combine (x1:x2:xs) = x1 <> x2 : combine xs
        combine xs         = xs

toListTree :: Tree a -> [a]
toListTree t = t2l t []
    where
      t2l (Tip x)   xs = x : xs
      t2l (Bin l r) xs = t2l l . t2l r $ xs
      t2l _         xs = xs

-- ----------------------------------------

isFail :: Tree t -> Bool
isFail (Fail _) = True
isFail _        = False

sizeTree :: Tree a -> Int
sizeTree = foldTree (const 0) 0 (const 1) (+)

headTree :: Tree a -> a
headTree t = case l of
            Tip x -> x
            _     ->  error "Tree.headTree: empty tree"
    where
      l = fst . uncons' $ t

lastTree :: Tree a -> a
lastTree t = case l of
            Tip x -> x
            _     -> error "Tree.lastTree: empty tree"
    where
      l = fst . uncons' . reverseTree $ t

tailTree :: Tree a -> Tree a
tailTree t = case r of
            Empty -> throwError "Tree.tailTree: empty tree"
            _     -> r
    where
      r = snd . uncons' $ t

initTree :: Tree a -> Tree a
initTree t = case r of
            Empty -> throwError "Tree.initTree: empty tree"
            _     -> reverseTree r
    where
      r = snd . uncons' . reverseTree $ t

reverseTree :: Tree a -> Tree a
reverseTree (Bin l r) = Bin (reverseTree r) (reverseTree l)
reverseTree t         = t

takeTree :: Int -> Tree a -> Tree a
takeTree n = fst . splitTree n

dropTree :: Int -> Tree a -> Tree a
dropTree n = snd . splitTree n

splitTree :: Int -> Tree a -> (Tree a, Tree a)
splitTree n t = (l, r)
    where
      (_, l, r) = split' n t

{-# INLINE takeTree #-}
{-# INLINE dropTree #-}
{-# INLINE splitTree #-}

-- ----------------------------------------

-- auxiliary functions

-- | uncons is a special form for @split' 1@

uncons' :: Tree a -> (Tree a, Tree a)
uncons' t@(Tip _) = (t, Empty)
uncons' (Bin l r) = (t, l' <> r)
                     where
                       (t, l') = uncons' l
uncons' t         = (t, t)


split' :: Int -> Tree a -> (Int, Tree a, Tree a)
split' n t
    | n <= 0  = (0, mempty, t)

split' _ t@(Tip _) = (1, t, mempty)

split' n (Bin l r)
    | n1 <  n = (n1 + n2, l <> r1, r2     )
    | n1 == n = (n1,      l1,      l2 <> r)
    where
      (n1, l1, l2) = split'  n       l
      (n2, r1, r2) = split' (n - n1) r

split' _ t = (0, mempty, t)

-- ----------------------------------------
