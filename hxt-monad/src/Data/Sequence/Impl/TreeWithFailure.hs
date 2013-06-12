{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- ----------------------------------------
{- |

   A Sequence implementation with Trees and failure

-}
-- ----------------------------------------

module Data.Sequence.Impl.TreeWithFailure
    ( Seq

    , foldTree
    , foldTreeR
    , fromListTree
    , fromListTree'
    , sequenceTree
    , toListTree
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
import           Control.Monad.Error

import           Data.Foldable               (Foldable)
import qualified Data.Foldable
import           Data.Monoid
import           Data.Sequence.ErrorSequence
import           Data.Sequence.Sequence

import           Prelude                     hiding (drop, head, init, last,
                                              length, reverse, splitAt, tail,
                                              take)

-- ----------------------------------------

data Seq a
    = Tip a
    | Bin (Seq a) (Seq a)
    | Empty
    | Fail (Seq String)
      deriving (Eq, Show)

-- ----------------------------------------

instance NFData a => NFData (Seq a) where
    rnf (Tip x)   = rnf x
    rnf (Bin l r) = rnf l `seq` rnf r
    rnf Empty     = ()
    rnf (Fail e)  = rnf e

instance Functor Seq where
    fmap f (Tip x)   = Tip (f x)
    fmap f (Bin l r) = Bin (fmap f l) (fmap f r)
    fmap _  Empty    = Empty
    fmap _ (Fail s)  = Fail s

instance Applicative Seq where
    pure = Tip				-- pure = return
    f <*> x = f >>= \ f' -> fmap f' x	-- (<*>) = ap

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monad Seq where
    return = Tip
    (>>=)  = substTree
    fail   = throwError . Tip

    {-# INLINE return #-}
    {-# INLINE (>>=)  #-}
    {-# INLINE fail   #-}

instance MonadPlus Seq where
    mzero = Empty
    mplus = bin

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadError (Seq String) Seq where
    throwError = Fail

    catchError (Fail s) h = h s
    catchError t        _ = t

    {-# INLINE throwError #-}

instance Monoid (Seq a) where
    mempty  = Empty
    mappend = bin

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance Sequence Seq where
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

instance Foldable Seq where
    foldr = foldTreeR
    {-# INLINE foldr   #-}

instance ErrorSequence (Seq String) Seq where
    failS (Fail s) = Left  s
    failS t        = Right t

    {-# INLINE failS   #-}

-- ----------------------------------------

substTree :: Seq a -> (a -> Seq b) -> Seq b
substTree (Tip x)   k = k x
substTree (Bin l r) k = bin (substTree l k) (substTree r k)
substTree  Empty    _ = Empty
substTree (Fail s)  _ = Fail s

-- | Monadic version of substTree

substTreeM :: Monad m => Seq a -> (a -> m (Seq b)) -> m (Seq b)
substTreeM (Tip x)   k = k x
substTreeM (Bin l r) k = do l1 <- substTreeM l k
                            r1 <- substTreeM r k
                            return (bin l1 r1)
substTreeM  Empty    _ = return Empty
substTreeM (Fail s)  _ = return (Fail s)

-- | smart constructor for Bin

bin :: Seq a -> Seq a -> Seq a
bin     Empty   t2          = t2
bin t1          Empty       = t1
bin    (Fail x)    (Fail y) = Fail $ bin x y
bin t1@(Fail _) _           = t1
bin _           t2@(Fail _) = t2
bin t1          t2          = Bin t1 t2

foldTreeR :: (a -> b -> b) -> b -> Seq a -> b
foldTreeR op z (Tip x)   = x `op` z
foldTreeR op z (Bin l r) = foldTreeR op (foldTreeR op z r) l
foldTreeR _  z _         = z

foldTree :: (Seq String -> b) -> b -> (a -> b) -> (b -> b -> b) -> (Seq a -> b)
foldTree ff e tf bf t = fold' t
    where
      fold' (Tip x)   = tf x
      fold' (Bin l r) = bf (fold' l) (fold' r)
      fold'  Empty    = e
      fold' (Fail s)  = ff s

zipTree :: (a -> b -> c) -> (Seq a -> Seq b -> Seq c)
zipTree op (Tip x)     (Tip y)     = Tip $ x `op` y
zipTree op (Bin l1 r1) (Bin l2 r2) = bin (zipTree op l1 l2) (zipTree op r1 r2)
zipTree _  (Fail s1)   (Fail s2)   = Fail $ bin s1 s2
zipTree _  (Fail s1)   _           = Fail s1
zipTree _  _           (Fail s2)   = Fail s2
zipTree _  _           _           = Empty

sequenceTree :: Monad m => Seq (m a) -> m (Seq a)
sequenceTree (Tip x)   = x >>= return . Tip
sequenceTree (Bin l r) = do l1 <- sequenceTree l
                            r1 <- sequenceTree r
                            return (Bin l1 r1)
sequenceTree  Empty    = return Empty
sequenceTree (Fail s)  = return (Fail s)

-- ----------------------------------------

-- | from list for finite lists
--
-- result is a balanced tree

fromListTree :: [a] -> Seq a
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

fromListTree' :: [a] -> Seq a
fromListTree'
    = merge . map return
      where
        merge []        = mempty
        merge [t]       = t
        merge (t : ts)  = Bin t (merge . combine $ ts) -- don't use smart constructor: bin or <>

        combine (x1:x2:xs) = Bin x1 x2 : combine xs    -- x1 <> x2 not necessary
        combine xs         = xs


toListTree :: Seq a -> [a]
toListTree t = t2l t []
    where
      t2l (Tip x)   xs = x : xs
      t2l (Bin l r) xs = t2l l . t2l r $ xs
      t2l _         xs = xs

-- ----------------------------------------

length :: Seq a -> Int
length = foldTree (const 0) 0 (const 1) (+)

head :: Seq a -> a
head t = case l of
            Tip x -> x
            _     ->  error "Seq.head: empty tree"
    where
      l = fst . uncons' $ t

last :: Seq a -> a
last t = case l of
            Tip x -> x
            _     -> error "Seq.last: empty tree"
    where
      l = fst . uncons' . reverse $ t

tail :: Seq a -> Seq a
tail t = case r of
            Empty -> fail "Seq.tail: empty tree"
            _     -> r
    where
      r = snd . uncons' $ t

init :: Seq a -> Seq a
init t = case r of
            Empty -> fail "Seq.init: empty tree"
            _     -> reverse r
    where
      r = snd . uncons' . reverse $ t

reverse :: Seq a -> Seq a
reverse (Bin l r) = Bin (reverse r) (reverse l)
reverse t         = t

take :: Int -> Seq a -> Seq a
take n = fst . splitAt n

drop :: Int -> Seq a -> Seq a
drop n = snd . splitAt n

splitAt :: Int -> Seq a -> (Seq a, Seq a)
splitAt n t = (l, r)
    where
      (_, l, r) = split' n t

{-# INLINE take #-}
{-# INLINE drop #-}
{-# INLINE splitAt #-}

-- ----------------------------------------

-- auxiliary functions

-- | uncons is a special form for @split' 1@

uncons' :: Seq a -> (Seq a, Seq a)
uncons' t@(Tip _) = (t, Empty)
uncons' (Bin l r) = (t, l' <> r)
                     where
                       (t, l') = uncons' l
uncons' t         = (t, t)

{- not yet used

-- | The dual to uncons', result is a pair
-- with 1. component : all elements but the last
-- and 2. component: the last or Empty in case of an empty tree

unsnoc' :: Seq a -> (Seq a, Seq a)
unsnoc' t@(Tip _) = (Empty, t)
unsnoc' (Bin l r) = (l <> r', t)
                     where
                       (r', t) = unsnoc' r
unsnoc' t         = (t, t)
-- -}

-- | split a tree into two, such that the left tree contains n
-- elements or less, and the right tree contains the remaining elements.
-- The first component contains the real number of elements in the first tree

split' :: Int -> Seq a -> (Int, Seq a, Seq a)
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
