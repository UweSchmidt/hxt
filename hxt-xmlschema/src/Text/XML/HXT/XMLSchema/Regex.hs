-- ----------------------------------------

module Text.XML.HXT.XMLSchema.Regex
where

import           Control.Applicative
import           Control.Monad

import           Data.List           (intercalate)

-- ----------------------------------------
--
-- | a nice symmetric data structure for representing none empty lists

data Tree2 a = Leaf a
             | Fork (Tree2 a) (Tree2 a)
               deriving (Show)

instance Functor Tree2 where
    fmap f t = map2 t
               where
                 map2 (Leaf x) = Leaf $ f x
                 map2 (Fork l r) = Fork (map2 l) (map2 r)

instance Applicative Tree2 where
    pure  = return
    (<*>) = ap

instance Monad Tree2 where
    return = Leaf
    (Leaf x) >>= f = f x
    (Fork l r) >>= f = Fork (l >>= f) (r >>= f)

foldTree2 :: (b -> b -> b) -> (a -> b)-> Tree2 a -> b
foldTree2 _  f (Leaf x) = f x
foldTree2 op f (Fork l r) = (foldTree2 op f l) `op` (foldTree2 op f r)

toListTree2 :: Tree2 a -> [a]
toListTree2 t
    = go t []
      where
        go (Leaf x)   = (x :)
        go (Fork l r) = go l . go r

headTree :: Tree2 a -> a
headTree (Leaf x) = x
headTree (Fork l _r) = headTree l

-- ----------------------------------------
--
-- | a data structure for none deterministic computations
-- with error reporting

data ResultSet a = Errs (Tree2 (String, String))
                 | Vals (Tree2 a)
                   deriving (Show)

instance Functor ResultSet where
    fmap f x = x >>= return . f

instance Applicative ResultSet where
    pure  = return
    (<*>) = ap

instance Monad ResultSet where
    return x = Vals (Leaf x)
    (Errs es) >>= _ = Errs es
    (Vals vs) >>= f = joinResultSets . fmap f $ vs

instance Alternative ResultSet where
    (<|>) = mplus
    empty = mzero

instance MonadPlus ResultSet where
    mzero = Errs . Leaf $ ("","")

    (Errs e1) `mplus` (Errs e2) = Errs (Fork e1 e2)
    (Errs _ ) `mplus` x2        = x2
    (Vals v1) `mplus` (Vals v2) = Vals (Fork v1 v2)
    x1        `mplus` _         = x1

-- | joining result sets
--
-- if there is a value all errors are discarded, and the values are collected
-- else all error are collected

joinResultSets :: Tree2 (ResultSet a) -> ResultSet a
joinResultSets (Leaf rs) = rs
joinResultSets (Fork l r)
    = (joinResultSets l) `mplus` (joinResultSets r)

-- ----------------------------------------

newtype Result s a = RS {unRS :: s -> ResultSet (a, s)}

instance Functor (Result s) where
    fmap f x = x >>= return . f

instance Applicative (Result s) where
    pure  = return
    (<*>) = ap

instance Monad (Result s) where
    return x = RS $ \ st ->
               return (x, st)
    x >>= f  = RS $ \ st ->
               do (res, st1) <- unRS x st
                  unRS (f res) st1

instance Alternative (Result s) where
    (<|>) = mplus
    empty = mzero

instance MonadPlus (Result s) where
    mzero = RS $ \ _st -> mzero
    x `mplus` y = RS $ \ st ->
                  unRS x st `mplus` unRS y st

throwErr :: String -> String -> RegexResult s a
throwErr x1 x2
    = RS $ \ _st -> Errs . Leaf $ (x1, x2)

-- ----------------------------------------

data Regex s a
    = Unit
    | Sym  (a -> Bool) (a -> s -> s) String    -- optional external repr. of predicate
    | Star  (Regex s a)
    | Alt   (Regex s a) (Regex s a)
    | Seq   (Regex s a) (Regex s a)
    | Rep   Int         (Regex s a)          -- 1 or more repetitions
    | Rng   Int Int     (Regex s a)          -- n..m repetitions
    | Perm  (Regex s a) (Regex s a)

instance Show (Regex s a) where
    show Unit           = "()"
    show (Sym _p _f "") = "<pred>"
    show (Sym _p _f r ) = r
    show (Star e)       = "(" ++ show e ++ ")*"
    show e@(Alt _ _)    = "(" ++ (intercalate "|" . map show . flattenAlt $ e) ++ ")"
    show (Seq e1 e2)    = show e1 ++ show e2
    show (Rep 1 e)      = "(" ++ show e ++ ")+"
    show (Rep i e)      = "(" ++ show e ++ "){" ++ show i ++ ",}"
    show (Rng 0 1 e)    = "(" ++ show e ++ ")?"
    show (Rng i j e)    = "(" ++ show e ++ "){" ++ show i ++ "," ++ show j ++ "}"
    show e@(Perm _ _)   = "(" ++ (intercalate "||" . map show . flattenPerm $ e) ++ ")"

flattenAlt :: Regex s a -> [Regex s a]
flattenAlt (Alt e1 e2) = flattenAlt e1 ++ flattenAlt e2
flattenAlt e           = [e]

flattenPerm :: Regex s a -> [Regex s a]
flattenPerm (Perm e1 e2) = flattenPerm e1 ++ flattenPerm e2
flattenPerm e            = [e]

-- ------------------------------------------------------------
--
-- smart constructors

mkUnit          :: (Regex s a)
mkUnit          = Unit

mkPrim'                 :: (a -> Bool) -> String -> (Regex s a)
mkPrim' p       = Sym p (const id)

mkPrim          :: (a -> Bool) -> (a -> s -> s) -> String -> (Regex s a)
mkPrim          = Sym

mkStar :: (Regex s a) -> (Regex s a)

mkStar e@Unit           = e                     -- ()* == ()
mkStar e@(Star _e1)     = e                     -- (r*)* == r*
mkStar (Rep 1 e1)       = mkStar e1             -- (r+)* == r*
mkStar e@(Alt _ _)      = Star (rmStar e)       -- (a*|b)* == (a|b)*
mkStar e                = Star e

rmStar :: (Regex s a) -> (Regex s a)

rmStar (Alt e1 e2)      = mkAlt (rmStar e1) (rmStar e2)
rmStar (Star e1)        = rmStar e1
rmStar (Rep 1 e1)       = rmStar e1
rmStar e1               = e1

mkAlt :: (Regex s a) -> (Regex s a) -> (Regex s a)
mkAlt e1 e2             = Alt e1 e2

mkAlts1 :: [(Regex s a)] -> (Regex s a)

mkAlts1 []               = error "mkAlts: illegal argument []"
mkAlts1 es               = foldr1 mkAlt es


mkSeq :: (Regex s a) -> (Regex s a) -> (Regex s a)

mkSeq Unit e2           = e2
mkSeq e1 Unit           = e1
mkSeq (Seq e1 e2) e3    = mkSeq e1 (mkSeq e2 e3)
mkSeq e1 e2             = Seq e1 e2


mkSeqs :: [(Regex s a)] -> (Regex s a)
mkSeqs                  = foldr mkSeq mkUnit

mkRep :: Int -> Regex s a -> Regex s a
mkRep 0 e               = mkStar $ rmUnit e
mkRep _ e@Unit          = e
mkRep i e
    | nullable e        = mkStar $ rmUnit e
    | otherwise         = Rep i e

mkRng   :: Int -> Int -> Regex s a -> Regex s a
mkRng 0  0  _e                  = mkUnit
mkRng 1  1  e                   = e
mkRng lb ub _e
    | lb > ub                   = error $ unwords ["mkRng: illegal range", show lb, "..", show ub]
mkRng _l _u e@Unit              = e
mkRng _l _u e@(Star _)          = e
mkRng lb _u (Rep i e)           = mkRep (lb * i) e
mkRng lb ub (Rng lb1 ub1 e)     = mkRng (lb * lb1) (ub * ub1) e
mkRng lb ub e
    | lb > 0 && nullable e      = mkRng 0 ub e
    | otherwise                 = Rng lb ub e

rmUnit :: Regex s a -> Regex s a
rmUnit (Alt e1 e2)              = mkAlt' (rmUnit e1) (rmUnit e2)
                                  where
                                    mkAlt' Unit e2' = e2'
                                    mkAlt' e1' Unit = e1'
                                    mkAlt' e1'  e2' = Alt e1' e2'
rmUnit e                        = e

mkPerm :: Regex s a -> Regex s a -> Regex s a
mkPerm Unit        e2            = e2
mkPerm e1          Unit          = e1
mkPerm e1          e2            = Perm e1 e2

mkPerms :: [Regex s a] -> Regex s a
mkPerms                          = foldr mkPerm mkUnit

-- ----------------------------------------

nullable :: (Regex s a) -> Bool
nullable Unit           = True
nullable (Sym _p _ _)   = False         -- assumption: p holds for at least one tree
nullable (Star _)       = True
nullable (Alt e1 e2)    = nullable e1 ||
                          nullable e2
nullable (Seq e1 e2)    = nullable e1 &&
                          nullable e2
nullable (Rep _i e)     = nullable e
nullable (Rng i _ e)    = i == 0 ||
                          nullable e
nullable (Perm e1 e2)   = nullable e1 &&
                          nullable e2

-- ----------------------------------------

type RegexResult s a = Result s (Regex s a)

-- | derivation by a single symbol

delta :: ShowSym a => a -> Regex s a -> RegexResult s a
delta c Unit
    = unexpected c "<eof>"

delta c (Sym p f e)
    | p c       = RS $ \ st -> Vals (Leaf (mkUnit, f c st))
    | otherwise = unexpected c e

delta c (Seq e1 e2)
    | nullable e1 = res1 `mplus` delta c e2
    | otherwise   = res1
    where
      res1 = delta c e1 `conc` e2

delta c (Alt e1 e2)
    = delta c e1 `mplus` delta c e2

delta c e@(Star e1)
    = delta c e1 `conc` e

delta c (Rep i e)
    = delta c e `conc` mkRep (i - 1) e

delta c (Rng i j e)
    = delta c e `conc` mkRng ((i-1) `max` 0) (j-1) e

delta c (Perm e1 e2)
    = (delta c e1 `perm` e2)
      `mplus`
      (delta c e2 `perm` e1)

-- | append a regex to every value in the result set: a kind of ifted mkSeq

conc :: RegexResult s a -> Regex s a -> RegexResult s a
conc rs re
    = (`mkSeq` re) <$> rs

-- | append a permutation to every value in the result set, a kind of lifted mkPerm

perm :: RegexResult s a -> Regex s a -> RegexResult s a
perm rs re
    = (`mkPerm` re) <$> rs


-- | delta lifted to words

delta' :: ShowSym a => [a] -> Regex s a -> RegexResult s a
delta' [] re
    = return re
delta' (x : xs) re
    = delta x re >>= delta' xs

-- ----------------------------------------

runDelta' :: ShowSym a => [a] -> Regex s a -> s -> ResultSet (Regex s a, s)
runDelta' xs re s
    = unRS (delta' xs re) s

matchRegex :: ShowSym a => [a] -> Regex s a -> s -> Either String s
matchRegex xs re s
    = evalRes $ runDelta' xs re s
      where
        -- prepare the error messages
        evalRes (Errs es) = Left $ unwords ["expected:", expected, "but input was:", got]
                            where
                              expected = intercalate "|" . map fst . toListTree2 $ es
                              got      = snd . headTree $ es

        -- look for the first nullable r.e. (the first parse) and take the associated state
        evalRes (Vals vs) = either noMatch Right . foldTree2 mplus' isNullable $ vs
                            where
                              isNullable (re', s')
                                  | nullable re' = Right s'
                                  | otherwise    = Left re'

                              noMatch e
                                  = Left $ unwords ["input missing to match regular expression:", show e]

                              mplus' x1@(Right _) _            = x1
                              mplus' _            x2@(Right _) = x2
                              mplus' x1           _            = x1

-- ----------------------------------------
--
-- error handling

unexpected :: ShowSym a => a -> String -> RegexResult s a
unexpected x e
    = throwErr (emsg e) (showSym x)
      where
        emsg ""           = "{?}"
        emsg s            = s

-- ----------------------------------------

class ShowSym a where
    showSym :: a -> String

-- ------------------------------------------------------------
{- a few tests

type IntSet = ResultSet Int

t1, t2 :: IntSet
t1 = Vals $ Fork (Leaf 1) (Leaf 3)
t2 = Errs $ Fork (Leaf ("xxx","??")) (Leaf ("yyy","cc"))

f1 :: Int -> ResultSet Int
f1 x
    | True      = fmap (+ x) t1
    | otherwise = Errs (Leaf ("zzz","zzz"))

-- ------------------------------------------------------------

type RE = Regex Int Char

instance ShowSym Char where
    showSym = (:[])

r1, r2, r3, r4, r5, r6 :: RE
r1 = mkPrim' (== 'a') (const (+1)) "a"
r2 = mkPrim' (== 'b') (const id) "b"
r6 = mkPrim' (== 'c') (const id) "c"
r3 = mkStar (mkAlt r1 r2)
r4 = mkRep 5 r1
r5 = mkRng 3 6 r1

countAs :: String -> ResultSet (RE, Int)
countAs xs = runDelta' xs r3 0

-- -}
-- ------------------------------------------------------------
