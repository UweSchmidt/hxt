{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}


-- ------------------------------------------------------------

{- |
   Copyright  : Copyright (C) 2014 - Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt <uwe@fh-wedel.de>
   Stability  : stable
   Portability: portable

   W3C XML Schema Regular Expression Matcher

   Grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>

-}

-- ------------------------------------------------------------

module Text.Regex.XMLSchema.Generic.Regex
    ( GenRegex

    , mkZero
    , mkZero'
    , mkUnit
    , mkSym
    , mkSym1
    , mkSymRng
    , mkWord
    , mkDot
    , mkStar
    , mkAll
    , mkAlt
    , mkElse
    , mkSeq
    , mkSeqs
    , mkRep
    , mkRng
    , mkOpt
    , mkDiff
    , mkIsect
    , mkExor
    , mkInterleave
    , mkCompl
    , mkBr
    , mkBr'

    , isZero
    , errRegex

    , nullable
    , nullable'

    , delta1
    , delta

    , firstChars

    , matchWithRegex
    , matchWithRegex'
    , splitWithRegex
    , splitWithRegex'
    , splitWithRegexCS
    , splitWithRegexCS'
    )
where

import Data.List        (intercalate)
import Data.Monoid      ((<>))
import Data.Set.CharSet
import Data.String      (IsString(..))

import Text.Regex.XMLSchema.Generic.StringLike

{-
import Debug.Trace      (traceShow)

trc :: Show a => String -> a -> a
trc msg x = traceShow (msg, x) x

-- -}
-- ------------------------------------------------------------

data GenRegex s
  = Zero s
  | Unit
  | Sym  CharSet
  | Dot
  | Star (GenRegex s)
  | Alt  (GenRegex s)        (GenRegex s)
  | Else (GenRegex s)        (GenRegex s)
  | Seq  (GenRegex s)        (GenRegex s)
  | Rep  Int                 (GenRegex s)           -- 1 or more repetitions
  | Rng  Int Int             (GenRegex s)           -- n..m repetitions
  | Diff (GenRegex s)        (GenRegex s)           -- r1 - r2
  | Isec (GenRegex s)        (GenRegex s)           -- r1 n r2
  | Exor (GenRegex s)        (GenRegex s)           -- r1 xor r2
  | Intl (GenRegex s)        (GenRegex s)           -- r1 interleavedWith r2
  | Br   (Label    s)        (GenRegex s)           -- (...) not yet parsed
  | Obr  (Label    s) s !Int (GenRegex s)           -- currently parsed (...)
  | Cbr [(Label s, s)]       (GenRegex s)           -- already completely parsed (...)
  deriving (Eq, Ord {-, Show -})

type Label s
  = Maybe s                           -- we need one special label for the whole expression
                                      -- see splitWithRegex
type SubexResults s
  = [(Label s, s)]

type Nullable s
  = (Bool, SubexResults s)

-- ------------------------------------------------------------

{- just for documentation

class Inv a where
    inv         :: a -> Bool

instance Inv (GenRegex s) where
    inv (Zero _)        = True
    inv Unit            = True
    inv (Sym p)         = not (nulCS p) && not (fullCS p)
    inv Dot             = True
    inv (Star e)        = inv e
    inv (Alt e1 e2)     = inv e1 &&
                          inv e2
    inv (Seq e1 e2)     = inv e1 &&
                          inv e2
    inv (Rep i e)       = i > 0 && inv e
    inv (Rng i j e)     = (i < j || (i == j && i > 1)) &&
                          inv e
    inv (Diff e1 e2)    = inv e1 &&
                          inv e2
    inv (Isec e1 e2)    = inv e1 &&
                          inv e2
    inv (Exor e1 e2)    = inv e1 &&
                          inv e2
-}

-- ------------------------------------------------------------
--
-- smart constructors

-- | construct the r.e. for the empty set.
-- An (error-) message may be attached

mkZero                                  :: s -> GenRegex s
mkZero                                  = Zero
{-# INLINE mkZero #-}

mkZero'                                 :: (StringLike s) =>
                                           String -> GenRegex s
mkZero'                                 = Zero . fromString
{-# INLINE mkZero' #-}

-- | construct the r.e. for the set containing the empty word

mkUnit                                  :: GenRegex s
mkUnit                                  = Unit
{-# INLINE mkUnit #-}

-- | construct the r.e. for a set of chars

mkSym                                   :: (StringLike s) =>
                                           CharSet -> GenRegex s
mkSym s
    | nullCS s                          = mkZero' "empty char range"
    | fullCS s                          = mkDot
    | otherwise                         = Sym s
{-# INLINE mkSym #-}

-- | construct an r.e. for a single char set
mkSym1                                  :: (StringLike s) =>
                                           Char -> GenRegex s
mkSym1                                  = mkSym . singleCS
{-# INLINE mkSym1 #-}

-- | construct an r.e. for an intervall of chars
mkSymRng                                :: (StringLike s) =>
                                           Char -> Char -> GenRegex s
mkSymRng c1 c2                          = mkSym $ rangeCS c1 c2
{-# INLINE mkSymRng #-}

-- | mkSym generaized for strings
mkWord                                  :: (StringLike s) =>
                                           [Char] -> GenRegex s
mkWord                                  = mkSeqs . map mkSym1

-- | construct an r.e. for the set of all Unicode chars
mkDot                                   :: GenRegex s
mkDot                                   = Dot
{-# INLINE mkDot #-}

-- | construct an r.e. for the set of all Unicode words

mkAll                                   :: (StringLike s) =>
                                           GenRegex s
mkAll                                   = mkStar mkDot
{-# INLINE mkAll #-}


-- | construct r.e. for r*
mkStar                                  :: (StringLike s) =>
                                           GenRegex s -> GenRegex s
mkStar (Zero _)                         = mkUnit                -- {}* == ()
mkStar e@Unit                           = e                     -- ()* == ()
mkStar e@(Star _e1)                     = e                     -- (r*)* == r*
mkStar (Rep 1 e1)                       = mkStar e1             -- (r+)* == r*
mkStar (Rep i e1)
    | i == 1
      ||
      nullable e1                       = mkStar e1             -- (r{i,})* == r*    when i == 1 or nullable r
mkStar e@(Rng _ _ e1)
    | nullable e                        = mkStar e1             -- (r{i,j})* == r*   when i == 0 or nullable r
mkStar e@(Alt _ _)                      = Star (rmStar e)       -- (a*|b)* == (a|b)*

                                                                {- this is wrong, not generally applicable
mkStar (Br l r s)                       = mkBr0 l (mkStar r) s  -- ({l}r)* == ({l}r*) because we want the longest match as result for the subexpression
                                                                -}
mkStar e                                = Star e

rmStar                                  :: (StringLike s) =>
                                           GenRegex s -> GenRegex s
rmStar (Alt e1 e2)                      = mkAlt (rmStar e1) (rmStar e2)
rmStar (Star e1)                        = rmStar e1
rmStar (Rep 1 e1)                       = rmStar e1
rmStar e1                               = e1

-- | construct the r.e for r1|r2

mkAlt                                   :: (StringLike s) =>
                                           GenRegex s -> GenRegex s -> GenRegex s
mkAlt e1            (Zero _)            = e1                            -- e1 u {} = e1
mkAlt (Zero _)      e2                  = e2                            -- {} u e2 = e2
mkAlt (Sym p1)      (Sym p2)            = mkSym $ p1 `unionCS` p2       -- melting of predicates
mkAlt e1            e2@(Sym _)          = mkAlt e2 e1                   -- symmetry: predicates always first
mkAlt e1@(Sym _)    (Alt e2@(Sym _) e3) = mkAlt (mkAlt e1 e2) e3        -- prepare melting of predicates
mkAlt (Sym _)       e2@Dot              = e2                            -- c|.     = .    for a c's
mkAlt e1@(Star Dot) _e2                 = e1                            -- A* u e1 = A*
mkAlt _e1           e2@(Star Dot)       = e2                            -- e1 u A* = A*
mkAlt (Alt e1 e2)   e3                  = mkAlt e1 (mkAlt e2 e3)        -- associativity
mkAlt e1 e2
    | e1 == e2                          = e1
    | otherwise                         = Alt e1 e2

-- | construct the r.e. for r1{|}r2 (r1 orElse r2).
--
-- This represents the same r.e. as r1|r2, but when
-- collecting the results of subexpressions in (...) and r1 succeeds, the
-- subexpressions of r2 are discarded, so r1 matches are prioritized
--
-- example
--
-- > splitSubex "({1}x)|({2}.)"   "x" = ([("1","x"),("2","x")], "")
-- >
-- > splitSubex "({1}x){|}({2}.)" "x" = ([("1","x")], "")

mkElse                                  :: (StringLike s) =>
                                           GenRegex s -> GenRegex s -> GenRegex s
mkElse e1            (Zero _)           = e1                            -- e1 u {} = e1
mkElse (Zero _)      e2                 = e2                            -- {} u e2 = e2
mkElse (Sym p1)      (Sym p2)           = mkSym $ p1 `unionCS` p2       -- melting of predicates
                                                                        -- no symmetry allowed
mkElse e1@(Sym _)  (Else e2@(Sym _) e3) = mkElse (mkElse e1 e2) e3      -- prepare melting of predicates
mkElse (Sym _)      e2@Dot              = e2                            -- c|.     = .    for a c's
mkElse e1@(Star Dot) _e2                = e1                            -- A* u e1 = A*
mkElse _e1           e2@(Star Dot)      = e2                            -- e1 u A* = A*
mkElse (Else e1 e2)   e3                = mkElse e1 (mkElse e2 e3)      -- associativity
mkElse e1 e2
    | e1 == e2                          = e1
    | otherwise                         = Else e1 e2

-- | Construct the sequence r.e. r1.r2

mkSeq                                   :: GenRegex s -> GenRegex s -> GenRegex s
mkSeq e1@(Zero _) _e2                   = e1
mkSeq _e1         e2@(Zero _)           = e2
mkSeq Unit        e2                    = e2
mkSeq (Cbr ss1 e1) e2                   = mkCbr ss1 (mkSeq e1 e2)               -- move finished submatches upwards
mkSeq e1          Unit                  = e1
mkSeq (Seq e1 e2) e3                    = mkSeq e1 (mkSeq e2 e3)
mkSeq e1 e2                             = Seq e1 e2

-- | mkSeq extened to lists
mkSeqs                                  :: [GenRegex s] -> GenRegex s
mkSeqs                                  = foldr mkSeq mkUnit

-- | Construct repetition r{i,}
mkRep                                   :: (StringLike s) =>
                                           Int -> GenRegex s -> GenRegex s
mkRep 0 e                               = mkStar e
mkRep _ e@(Zero _)                      = e
mkRep _ e
    | nullable e                        = mkStar e
mkRep i (Rep j e)                       = mkRep (i * j) e
mkRep i e                               = Rep i e

-- | Construct range r{i,j}
mkRng                                   :: (StringLike s) =>
                                           Int -> Int -> GenRegex s -> GenRegex s
mkRng 0  0  _e                          = mkUnit
mkRng 1  1  e                           = e
mkRng lb ub _e
    | lb > ub                           = mkZero' $
                                          "illegal range " ++
                                          show lb ++ ".." ++ show ub
mkRng _l _u e@(Zero _)                  = e
mkRng _l _u e@Unit                      = e
mkRng lb ub e                           = Rng lb ub e

-- | Construct option r?
mkOpt                                   :: (StringLike s) =>
                                           GenRegex s -> GenRegex s
mkOpt                                   = mkRng 0 1
{-# INLINE mkOpt #-}

-- | Construct difference r.e.: r1 {\\} r2
--
-- example
--
-- > match "[a-z]+{\\}bush" "obama"     = True
-- > match "[a-z]+{\\}bush" "clinton"   = True
-- > match "[a-z]+{\\}bush" "bush"      = False     -- not important any more

mkDiff                                  :: (StringLike s) =>
                                           GenRegex s -> GenRegex s -> GenRegex s
mkDiff e1@(Zero _) _e2                  = e1                                    -- {} - r2 = {}
mkDiff e1          (Zero _)             = e1                                    -- r1 - {} = r1
mkDiff _e1         (Star Dot)           = mkZero' "empty set in difference expr" -- r1 - .* = {}
mkDiff Dot         (Sym p)              = mkSym $ compCS p                      -- . - s  = ~s
mkDiff (Sym _)     Dot                  = mkZero' "empty set in difference expr" -- x - .  = {}
mkDiff (Sym p1)    (Sym p2)             = mkSym $ p1 `diffCS` p2                -- set diff
mkDiff e1          e2
    | e1 == e2                          = mkZero' "empty set in difference expr" -- r1 - r1 = {}
    | otherwise                         = Diff e1 e2

-- | Construct the Complement of an r.e.: whole set of words - r

mkCompl                                 :: (StringLike s) =>
                                           GenRegex s -> GenRegex s
mkCompl (Zero _)                        = mkAll
mkCompl (Star Dot)                      = mkZero' "empty set in compl expr"
mkCompl e                               = mkDiff (mkStar mkDot) e

-- | Construct r.e. for intersection: r1 {&} r2
--
-- example
--
-- > match ".*a.*{&}.*b.*" "-a-b-"  = True
-- > match ".*a.*{&}.*b.*" "-b-a-"  = True
-- > match ".*a.*{&}.*b.*" "-a-a-"  = False
-- > match ".*a.*{&}.*b.*" "---b-"  = False

mkIsect                                 :: (StringLike s) =>
                                           GenRegex s -> GenRegex s -> GenRegex s
mkIsect e1@(Zero _) _e2                 = e1                                    -- {} n r2 = {}
mkIsect _e1         e2@(Zero _)         = e2                                    -- r1 n {} = {}
mkIsect e1@(Unit)   e2                                                  -- () n r2 = () if nullable r2
    | nullable e2                       = e1                                    -- () n r2 = {} if not nullable r2
    | otherwise                         = mkZero' "intersection empty"
mkIsect e1          e2@(Unit)           = mkIsect e2 e1                         -- symmetric version of las 2 laws

mkIsect (Sym p1)    (Sym p2)            = mkSym $ p1 `intersectCS` p2           -- intersect sets
mkIsect e1@(Sym _)  Dot                 = e1                                    -- x n . = x
mkIsect Dot         e2@(Sym _)          = e2                                    -- . n x = x

mkIsect (Star Dot)  e2                  = e2                                    -- .* n r2 = r2
mkIsect e1          (Star Dot)          = e1                                    -- r1 n .* = r1
mkIsect e1          e2
    | e1 == e2                          = e1                                    -- r1 n r1 = r1
    | otherwise                         = Isec e1 e2

-- | Construct r.e. for exclusive or: r1 {^} r2
--
-- example
--
-- > match "[a-c]+{^}[c-d]+" "abc"  = True
-- > match "[a-c]+{^}[c-d]+" "acdc" = False
-- > match "[a-c]+{^}[c-d]+" "ccc"  = False
-- > match "[a-c]+{^}[c-d]+" "cdc"  = True

mkExor                                  :: (StringLike s) =>
                                           GenRegex s -> GenRegex s -> GenRegex s
mkExor (Zero _)     e2                  = e2
mkExor e1           (Zero _)            = e1
mkExor (Star Dot)   _e2                 = mkZero' "empty set in exor expr"
mkExor _e1          (Star Dot)          = mkZero' "empty set in exor expr"
mkExor (Sym p1)     (Sym p2)            = mkSym $ p1 `exorCS` p2
mkExor (Sym p1)     Dot                 = mkSym $ compCS p1
mkExor Dot          (Sym p2)            = mkSym $ compCS p2
mkExor e1           e2
    | e1 == e2                          = mkZero' "empty set in exor expr"       -- r1 xor r1 = {}
    | otherwise                         = Exor e1 e2

mkInterleave                            :: GenRegex s -> GenRegex s -> GenRegex s
mkInterleave e1@(Zero _) _              = e1
mkInterleave _           e2@(Zero _)    = e2
mkInterleave (Unit)      e2             = e2
mkInterleave e1          (Unit)         = e1
mkInterleave e1          e2             = Intl e1 e2

-- | Construct a labeled subexpression: ({label}r)

mkBr                                    :: s -> GenRegex s -> GenRegex s
mkBr l e                                = Br (Just l) e

mkBr'                                   :: StringLike s =>
                                           String -> GenRegex s -> GenRegex s
mkBr' l e                               = Br (Just $ fromString l) e

mkBrN                                   :: GenRegex s -> GenRegex s
mkBrN e                                 = Br Nothing e

mkObr                                   :: StringLike s =>
                                           Label s -> s -> Int -> GenRegex s -> GenRegex s
mkObr _ _ _ e@(Zero _)                  = e
mkObr l s n Unit                        = mkCbr [(l, takeS n s)] mkUnit
mkObr l s n e                           = Obr l s n e

mkCbr                                   :: SubexResults s -> GenRegex s -> GenRegex s
mkCbr  _  e@(Zero _)                    = e                             -- dead end, throw away subexpr matches
mkCbr ss (Cbr ss1 e)                    = mkCbr (ss <> ss1) e           -- join inner and this subexpr match
mkCbr ss  e                             = Cbr ss e

-- ------------------------------------------------------------

instance (StringLike s) => Show (GenRegex s) where
    show (Zero e)               = "{" ++ toString e ++ "}"
    show Unit                   = "()"
    show (Sym p)
        | p == compCS (stringCS "\n\r")
                                = "."
        | null (tail cs) &&
          rng1 (head cs)
                                = escRng . head $ cs
        | otherwise             = "[" ++ concat cs' ++ "]"
                                  where
                                  rng1 (x,y)    = x == y
                                  cs            = p -- charRngs . chars $ p
                                  cs'           = map escRng p
                                  escRng (x, y)
                                      | x == y  = esc x
                                      | succ x == y
                                                = esc x        ++ esc y
                                      | otherwise
                                                = esc x ++ "-" ++ esc y
                                  esc x
                                      | x `elem` "\\-[]{}()*+?.^"
                                                = '\\':x:""
                                      | x >= ' ' && x <= '~'
                                                = x:""
                                      | otherwise
                                                = "&#" ++ show (fromEnum x) ++ ";"
    show Dot                    = "\\a"
    show (Star Dot)             = "\\A"
    show (Star e)               = "(" ++ show e ++ "*)"
    show (Alt e1 e2)            = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
    show (Else e1 e2)           = "(" ++ show e1 ++ "{|}" ++ show e2 ++ ")"
    show (Seq e1 e2)            = "(" ++ show e1 ++ show e2 ++ ")"
    show (Rep 1 e)              = "(" ++ show e ++ "+)"
    show (Rep i e)              = "(" ++ show e ++ "{" ++ show i ++ ",})"
    show (Rng 0 1 e)            = "(" ++ show e ++ "?)"
    show (Rng i j e)            = "(" ++ show e ++ "{" ++ show i ++ "," ++ show j ++ "})"
    show (Diff e1 e2)           = "(" ++ show e1 ++ "{\\}" ++ show e2 ++ ")"
    show (Isec e1 e2)           = "(" ++ show e1 ++ "{&}" ++ show e2 ++ ")"
    show (Exor e1 e2)           = "(" ++ show e1 ++ "{^}" ++ show e2 ++ ")"
    show (Intl e1 e2)           = "(" ++ show e1 ++ "{:}" ++ show e2 ++ ")"
    show (Br  l     e)          = "({" ++ showL l ++ "}" ++ show e ++ ")"
    show (Obr l s n e)          = "({" ++ showL l ++ "=" ++ toString (takeS n s) ++ "}" ++ show e ++ ")"
    show (Cbr ss e)             = "([" ++ intercalate "," (map (\ (l, s) -> showL l ++ "=" ++ (show $ toString s)) ss) ++ "]"
                                  ++ show e ++
                                  ")"

showL                           :: Show s => Label s -> String
showL                           = rmq . maybe "" show
                                  where
                                  rmq ('\"':xs) = init xs
                                  rmq xs          = xs

-- ------------------------------------------------------------

isZero                          :: GenRegex s -> Bool
isZero (Zero _)                 = True
isZero _                        = False
{-# INLINE isZero #-}

errRegex                        :: (StringLike s) =>
                                   GenRegex s -> s
errRegex (Zero e)               = e
errRegex _                      = emptyS

-- ------------------------------------------------------------

nullable                        :: (StringLike s) =>
                                   GenRegex s -> Bool
nullable                        = fst . nullable'
{-# INLINE nullable #-}

nullable'                       :: (StringLike s) =>
                                   GenRegex s -> Nullable s

nullable' (Zero _)              = (False, [])
nullable' Unit                  = (True,  [])
nullable' Dot                   = (False, [])
nullable' (Sym _x)              = (False, [])

nullable' (Star _e)             = (True,  [])
nullable' (Rep _i e)            = nullable' e
nullable' (Rng i _ e)           = (i == 0, []) `unionN` nullable' e
nullable' (Seq e1 e2)           = nullable' e1 `isectN` nullable' e2

nullable' (Alt   e1 e2)         = nullable' e1 `unionN`  nullable' e2
nullable' (Else  e1 e2)         = nullable' e1 `orElseN` nullable' e2
nullable' (Isec  e1 e2)         = nullable' e1 `isectN`  nullable' e2
nullable' (Diff  e1 e2)         = nullable' e1 `diffN`   nullable' e2
nullable' (Exor  e1 e2)         = nullable' e1 `exorN`   nullable' e2
nullable' (Intl  e1 e2)         = nullable' e1 `isectN`  nullable' e2

nullable' (Br  l e)             = (True, [(l, emptyS   )]) `isectN` nullable' e
nullable' (Obr l s n e)         = (True, [(l, takeS n s)]) `isectN` nullable' e
nullable' (Cbr ss e)            = (True, ss)               `isectN` nullable' e

isectN                          :: Nullable s -> Nullable s -> Nullable s
isectN (True, ws1) (True, ws2)  = (True, ws1 ++ ws2)
isectN _           _            = (False, [])

unionN                          :: Nullable s -> Nullable s -> Nullable s
unionN (False, _) (False, _)    = (False, [])
unionN (_, ws1)   (_, ws2)      = (True, ws1 ++ ws2)

orElseN                         :: Nullable s -> Nullable s -> Nullable s
orElseN e1@(True, _ws1) _       = e1
orElseN _            e2         = e2

diffN                           :: Nullable s -> Nullable s -> Nullable s
diffN n1          (False, _)    = n1
diffN _           _             = (False, [])

exorN                           :: Nullable s -> Nullable s -> Nullable s
exorN n1@(True, _)  (False, _)  = n1
exorN (False, _)  n2@(True, _)  = n2
exorN _           _             = (False, [])

-- ------------------------------------------------------------

-- | FIRST for regular expressions
--
-- this is only an approximation, the real set of char may be smaller,
-- when the expression contains intersection, set difference or exor operators

firstChars                      :: (StringLike s) =>
                                   GenRegex s -> CharSet

firstChars (Zero _)             = emptyCS
firstChars Unit                 = emptyCS
firstChars (Sym p)              = p
firstChars Dot                  = allCS

firstChars (Star e1)            = firstChars e1
firstChars (Alt e1 e2)          = firstChars e1 `unionCS` firstChars e2
firstChars (Else e1 e2)         = firstChars e1 `unionCS` firstChars e2
firstChars (Seq e1 e2)
    | nullable e1               = firstChars e1 `unionCS` firstChars e2
    | otherwise                 = firstChars e1
firstChars (Rep _i e)           = firstChars e
firstChars (Rng _i _j e)        = firstChars e
firstChars (Diff e1 _e2)        = firstChars e1                                 -- this is an approximation
firstChars (Isec e1 e2)         = firstChars e1 `intersectCS` firstChars e2     -- this is an approximation
firstChars (Exor e1 e2)         = firstChars e1 `unionCS`     firstChars e2     -- this is an approximation
firstChars (Intl e1 e2)         = firstChars e1 `unionCS`     firstChars e2
firstChars (Br _l e)            = firstChars e
firstChars (Obr _l _s _n e)     = firstChars e
firstChars (Cbr _ss e)          = firstChars e

-- ------------------------------------------------------------

delta1 :: (StringLike s) => Char -> s -> GenRegex s -> GenRegex s
delta1 c inp e0
  = d' e0
  where
    d' e@(Zero _)           = e
    d' Unit                 = mkZero' $
                              "unexpected char " ++ show c
    d' (Sym p)
      | c `elemCS` p        = mkUnit
      | otherwise           = mkZero' $
                              "unexpected char " ++ show c
    d' Dot                  = mkUnit
    d' e@(Star Dot)         = e
    d' e@(Star e1)          = mkSeq  (d' e1) e
    d' (Alt e1 e2)          = mkAlt  (d' e1) (d' e2)
    d' (Else e1 e2)         = mkElse (d' e1) (d' e2)

    d' (Seq e1@(Br  l     e1') e2)
      | nullable e1'        = mkAlt (mkSeq (d' e1) e2)  -- longest submatch first
                                    (mkCbr [(l, emptyS)] (d' e2))

    d' (Seq e1@(Obr l s n e1') e2)
      | nu                  = mkAlt (mkSeq (d' e1) e2)
                                    (mkCbr ((l, takeS n s) : ws) (d' e2))
                              where
                                (nu, ws) = nullable' e1'
    d' (Seq e1 e2)
      | nullable e1         = mkAlt (mkSeq (d' e1) e2)
                                    (d' e2)
      | otherwise           = mkSeq (d' e1) e2
    d' (Rep i e)            = mkSeq (d' e) (mkRep (i-1) e)
    d' (Rng i j e)          = mkSeq (d' e) (mkRng ((i-1) `max` 0) (j-1) e)
    d' (Diff e1 e2)         = mkDiff  (d' e1) (d' e2)
    d' (Isec e1 e2)         = mkIsect (d' e1) (d' e2)
    d' (Exor e1 e2)         = mkExor  (d' e1) (d' e2)
    d' (Intl e1 e2)         = mkAlt   (mkInterleave (d' e1)     e2 )
                                      (mkInterleave     e1  (d' e2))

    d' (Br  l     e)        = d' (mkObr l inp 0 e)        -- a subex parse starts
    d' (Obr l s n e)        = mkObr l s (n + 1) (d' e)    -- a subex parse cont.
    d' (Cbr ss e)           = mkCbr ss (d' e)             -- the results of a subex parse

-- ------------------------------------------------------------

delta :: (StringLike s) => s -> GenRegex s -> GenRegex s
delta inp@(uncons -> Just (c, inp')) e0
  = d' e0
  where
    d' e@(Zero _)   = e   -- don't process whole input, parse has failed
    d' e@(Star Dot) = e   -- don't process input, derivative does not change
    d' e            = delta inp' ( -- trc ("delta(" ++ show c ++ ")=") $
                                   delta1 c inp e)

delta _empty e
  = e


matchWithRegex :: (StringLike s) =>
                  GenRegex s -> s -> Bool
matchWithRegex e s
  = nullable $ delta s e

matchWithRegex' :: (StringLike s) =>
                   GenRegex s -> s -> Maybe (SubexResults s)
matchWithRegex' e s
  = (\ (r, l) -> if r then Just l else Nothing) . nullable' $ delta s e

-- ------------------------------------------------------------

-- | This function wraps the whole regex in a subexpression before starting
-- the parse. This is done for getting access to
-- the whole parsed string. Therfore we need one special label, this label
-- is the Nothing value, all explicit labels are Just labels.

splitWithRegex :: (StringLike s) =>
                  GenRegex s -> s -> Maybe (SubexResults s, s)
splitWithRegex re inp
  = do
    (re', rest) <- splitWithRegex' (mkBrN re) inp
    return ( snd . nullable' $ re', rest)

splitWithRegexCS :: (StringLike s) =>
                    GenRegex s -> CharSet -> s -> Maybe (SubexResults s, s)
splitWithRegexCS re cs inp
  = do
    (re', rest) <- splitWithRegexCS' (mkBrN re) cs inp
    return ( snd . nullable' $ re', rest)

-- ----------------------------------------
--
-- | The main scanner function

{- linear recursive function, can lead to stack overflow

splitWithRegex'                 :: Eq l => GenRegex s -> String -> Maybe (GenRegex s, String)
splitWithRegex' re ""
    | nullable re               = Just (re, "")
    | otherwise                 = Nothing

splitWithRegex' re inp@(c : inp')
    | isZero re                 = Nothing
    | otherwise                 = evalRes . splitWithRegex' (delta1 re c) $ inp'
    where
    evalRes Nothing
        | nullable re           = Just (re, inp)
        | otherwise             = Nothing
    evalRes res                 = res
-}

-- tail recursive version of above function

splitWithRegex' :: (StringLike s) =>
                   GenRegex s -> s -> Maybe (GenRegex s, s)
splitWithRegex' re inp
  = splitWithRegex''
    ( if nullable re
      then Just (re, inp)         -- first possible result: empty prefix
      else Nothing                -- empty prefix not a result
    ) re inp

splitWithRegex'' :: (StringLike s) =>
                    Maybe (GenRegex s, s) -> GenRegex s -> s -> Maybe (GenRegex s, s)

splitWithRegex'' lastRes re inp@(uncons -> Just (c, inp'))
  | isZero re = lastRes
  | otherwise = splitWithRegex'' nextRes re' $ inp'
  where
    re' = delta1 c inp re
    nextRes
      | nullable re' = Just (re', inp')
      | otherwise    = lastRes

splitWithRegex'' lastRes _re _empty
  = lastRes

-- ----------------------------------------
--
-- | speedup version for splitWithRegex'
--
-- This function checks whether the input starts with a char from FIRST re.
-- If this is not the case, the split fails. The FIRST set can be computed once
-- for a whole tokenizer and reused by every call of split

splitWithRegexCS' :: (StringLike s) =>
                     GenRegex s -> CharSet -> s -> Maybe (GenRegex s, s)
splitWithRegexCS' re cs inp@(uncons -> Just (c, _inp'))
  | c `elemCS` cs = splitWithRegex' re inp

splitWithRegexCS' re _cs inp
  | nullable re = Just (re, inp)
  | otherwise = Nothing

-- ------------------------------------------------------------
