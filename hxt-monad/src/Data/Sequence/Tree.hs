{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- ----------------------------------------
{- |

   A Sequence implementation with Trees

-}
-- ----------------------------------------

module Data.Sequence.Tree
    ( Tree

    , fromListTree
    , fromListTree'
    , toListTree
    , foldTree
    , sequenceTree
    , substTree
    , substTreeM
    , zipTree

    , drop	-- list like functions, import qualified
    , head
    , init
    , last
    , length
    , reverse
    , splitAt
    , tail
    , take
    )
where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.MonadSequence

import           Data.Monoid

import           Prelude                     hiding (drop, head, init, last,
                                              length, reverse, splitAt, tail,
                                              take)

-- ----------------------------------------

data Tree a
    = Tip a
    | Bin (Tree a) (Tree a)
    | Empty
      deriving (Eq, Show)

-- ----------------------------------------

instance NFData a => NFData (Tree a) where
    rnf (Tip x)   = rnf x
    rnf (Bin l r) = rnf l `seq` rnf r
    rnf Empty     = ()

instance Functor Tree where
    fmap f (Tip x)   = Tip (f x)
    fmap f (Bin l r) = Bin (fmap f l) (fmap f r)
    fmap _  Empty    = Empty

instance Applicative Tree where
    pure = Tip				-- pure = return
    f <*> x = f >>= \ f' -> fmap f' x	-- (<*>) = ap

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monad Tree where
    return = Tip
    (>>=)  = substTree
    fail   = const Empty

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}

instance MonadPlus Tree where
    mzero = Empty
    mplus = bin

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance Monoid (Tree a) where
    mempty  = Empty
    mappend = bin

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance MonadSequence Tree where
    fromList = toS
    toList   = return . fromS

    {-# INLINE fromList #-}
    {-# INLINE toList #-}

instance MonadConv Tree Tree where
    convFrom = id
    convTo   = return

    {-# INLINE convFrom #-}
    {-# INLINE convTo   #-}

instance MonadCond Tree Tree where
    ifM Empty    _ e = e
    ifM _ t _        = t

    orElseM Empty    e = e
    orElseM t        _ = t

instance Sequence Tree where
    emptyS          = Empty
    consS           = bin . Tip
    unconsS t       = case uncons' t of
                        (Tip x, t') -> Just (x, t')
                        _           -> Nothing

    nullS (Tip _)   = False
    nullS (Bin _ _) = False
    nullS _         = True

    fromS           = toListTree
    toS             = fromListTree
    substS          = substTreeM

    {-# INLINE emptyS  #-}
    {-# INLINE consS   #-}
    {-# INLINE unconsS #-}
    {-# INLINE nullS   #-}
    {-# INLINE toS     #-}
    {-# INLINE fromS   #-}
    {-# INLINE substS   #-}

instance ErrSeq (Tree String) Tree where
    failS t        = Right t

    {-# INLINE failS   #-}

-- ----------------------------------------

substTree :: Tree a -> (a -> Tree b) -> Tree b
substTree (Tip x)   k = k x
substTree (Bin l r) k = bin (substTree l k) (substTree r k)
substTree  Empty    _ = Empty

-- | Monadic version of substTree

substTreeM :: Monad m => Tree a -> (a -> m (Tree b)) -> m (Tree b)
substTreeM (Tip x)   k = k x
substTreeM (Bin l r) k = do l1 <- substTreeM l k
                            r1 <- substTreeM r k
                            return (bin l1 r1)
substTreeM  Empty    _ = return Empty

-- | smart constructor for Bin

bin :: Tree a -> Tree a -> Tree a
bin     Empty   t2          = t2
bin t1          Empty       = t1
bin t1          t2          = Bin t1 t2

foldTree :: b -> (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree e tf bf t = fold' t
    where
      fold' (Tip x)   = tf x
      fold' (Bin l r) = bf (fold' l) (fold' r)
      fold'  Empty    = e

zipTree :: (a -> b -> c) -> (Tree a -> Tree b -> Tree c)
zipTree op (Tip x)     (Tip y)     = Tip $ x `op` y
zipTree op (Bin l1 r1) (Bin l2 r2) = bin (zipTree op l1 l2) (zipTree op r1 r2)
zipTree _  _           _           = Empty

sequenceTree :: Monad m => Tree (m a) -> m (Tree a)
sequenceTree (Tip x)   = x >>= return . Tip
sequenceTree (Bin l r) = do l1 <- sequenceTree l
                            r1 <- sequenceTree r
                            return (Bin l1 r1)
sequenceTree  Empty    = return Empty

-- ----------------------------------------

-- | from list for finite lists
--
-- result is a balanced tree

fromListTree :: [a] -> Tree a
fromListTree
    = merge . map return
      where
        merge []  = mempty
        merge [t] = t
        merge ts  = merge . combine $ ts

        combine (x1:x2:xs) = Bin x1 x2 : combine xs    -- x1 <> x2 not necessary
        combine xs         = xs

-- | from list for infinite lists
--
-- result is a skew tree with growing right subtrees

fromListTree' :: [a] -> Tree a
fromListTree'
    = merge . map return
      where
        merge []        = mempty
        merge [t]       = t
        merge (t : ts)  = Bin t (merge . combine $ ts) -- don't use smart constructor: bin or <>

        combine (x1:x2:xs) = Bin x1 x2 : combine xs    -- x1 <> x2 not necessary
        combine xs         = xs


toListTree :: Tree a -> [a]
toListTree t = t2l t []
    where
      t2l (Tip x)   xs = x : xs
      t2l (Bin l r) xs = t2l l . t2l r $ xs
      t2l _         xs = xs

-- ----------------------------------------

length :: Tree a -> Int
length = foldTree 0 (const 1) (+)

head :: Tree a -> a
head t = case l of
            Tip x -> x
            _     ->  error "Tree.head: empty tree"
    where
      l = fst . uncons' $ t

last :: Tree a -> a
last t = case l of
            Tip x -> x
            _     -> error "Tree.last: empty tree"
    where
      l = fst . uncons' . reverse $ t

tail :: Tree a -> Tree a
tail t = case r of
            Empty -> fail "Tree.tail: empty tree"
            _     -> r
    where
      r = snd . uncons' $ t

init :: Tree a -> Tree a
init t = case r of
            Empty -> fail "Tree.init: empty tree"
            _     -> reverse r
    where
      r = snd . uncons' . reverse $ t

reverse :: Tree a -> Tree a
reverse (Bin l r) = Bin (reverse r) (reverse l)
reverse t         = t

take :: Int -> Tree a -> Tree a
take n = fst . splitAt n

drop :: Int -> Tree a -> Tree a
drop n = snd . splitAt n

splitAt :: Int -> Tree a -> (Tree a, Tree a)
splitAt n t = (l, r)
    where
      (_, l, r) = split' n t

{-# INLINE take #-}
{-# INLINE drop #-}
{-# INLINE splitAt #-}

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
