-- ------------------------------------------------------------

{- |
   Module     : Text.Regex.XMLSchema.String.Regex
   Copyright  : Copyright (C) 2010 - Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt <uwe@fh-wedel.de>
   Stability  : stable
   Portability: portable

   W3C XML Schema Regular Expression Matcher

   Grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>

-}

-- ------------------------------------------------------------

module Text.Regex.XMLSchema.String.Regex
    ( Regex
    , GenRegex

    , mkZero
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

import Data.List        -- ( intercalate )

import Data.Set.CharSet

-- ------------------------------------------------------------

data GenRegex l = Zero String
                | Unit
                | Sym  CharSet
                | Dot
                | Star (GenRegex l)
                | Alt  (GenRegex l)   (GenRegex l)
                | Else (GenRegex l)   (GenRegex l)
                | Seq  (GenRegex l)   (GenRegex l)
                | Rep  Int            (GenRegex l)              -- 1 or more repetitions
                | Rng  Int Int        (GenRegex l)              -- n..m repetitions
                | Diff (GenRegex l)   (GenRegex l)              -- r1 - r2
                | Isec (GenRegex l)   (GenRegex l)              -- r1 n r2
                | Exor (GenRegex l)   (GenRegex l)              -- r1 xor r2
                | Intl (GenRegex l)   (GenRegex l)              -- r1 interleavedWith r2
                | Br   (Label    l)   (GenRegex l) String       -- currently parsed (...)
                | Cbr  (GenRegex l)   [(Label l, String)]       --already completely parsed (...)
                  deriving (Eq, Ord {-, Show -})

type Regex      = GenRegex String
type Label l    = Maybe l                                       -- we need one special label for the whole expression
                                                                -- see splitWithRegex
type Nullable l = (Bool, [(Label l, String)])

-- ------------------------------------------------------------

{- just for documentation

class Inv a where
    inv         :: a -> Bool

instance Inv (GenRegex l) where
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

mkZero                                  :: String -> GenRegex l
mkZero                                  = Zero
{-# INLINE mkZero #-}

-- | construct the r.e. for the set containing the empty word

mkUnit                                  :: GenRegex l
mkUnit                                  = Unit
{-# INLINE mkUnit #-}

-- | construct the r.e. for a set of chars

mkSym                                   :: CharSet -> GenRegex l
mkSym s
    | nullCS s                          = mkZero $ "empty char range"
    | fullCS s                          = mkDot
    | otherwise                         = Sym s
{-# INLINE mkSym #-}

-- | construct an r.e. for a single char set
mkSym1                                  :: Char -> GenRegex l
mkSym1                                  = mkSym . singleCS
{-# INLINE mkSym1 #-}

-- | construct an r.e. for an intervall of chars
mkSymRng                                :: Char -> Char -> GenRegex l
mkSymRng c1 c2                          = mkSym $ rangeCS c1 c2
{-# INLINE mkSymRng #-}

-- | mkSym generaized for strings
mkWord                                  :: [Char] -> GenRegex l
mkWord                                  = mkSeqs . map mkSym1

-- | construct an r.e. for the set of all Unicode chars
mkDot                                   :: GenRegex l
mkDot                                   = Dot
{-# INLINE mkDot #-}

-- | construct an r.e. for the set of all Unicode words

mkAll                                   :: Eq l => GenRegex l
mkAll                                   = mkStar mkDot
{-# INLINE mkAll #-}


-- | construct r.e. for r*
mkStar                                  :: Eq l => GenRegex l -> GenRegex l
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

rmStar                                  :: Eq l => GenRegex l -> GenRegex l
rmStar (Alt e1 e2)                      = mkAlt (rmStar e1) (rmStar e2)
rmStar (Star e1)                        = rmStar e1
rmStar (Rep 1 e1)                       = rmStar e1
rmStar e1                               = e1

-- | construct the r.e for r1|r2

mkAlt                                   :: Eq l => GenRegex l -> GenRegex l -> GenRegex l
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

mkElse                                  :: Eq l => GenRegex l -> GenRegex l -> GenRegex l
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

mkSeq                                   :: GenRegex l -> GenRegex l -> GenRegex l
mkSeq e1@(Zero _) _e2                   = e1
mkSeq _e1         e2@(Zero _)           = e2
mkSeq Unit        e2                    = e2
mkSeq (Cbr e1 ss1) e2                   = mkCbr (mkSeq e1 e2) ss1               -- move finished submatches upwards
mkSeq e1          Unit                  = e1
mkSeq (Seq e1 e2) e3                    = mkSeq e1 (mkSeq e2 e3)
mkSeq e1 e2                             = Seq e1 e2

-- | mkSeq extened to lists
mkSeqs                                  :: [GenRegex l] -> GenRegex l
mkSeqs                                  = foldr mkSeq mkUnit

-- | Construct repetition r{i,}
mkRep                                   :: Eq l => Int -> GenRegex l -> GenRegex l
mkRep 0 e                               = mkStar e
mkRep _ e@(Zero _)                      = e
mkRep _ e
    | nullable e                        = mkStar e
mkRep i (Rep j e)                       = mkRep (i * j) e
mkRep i e                               = Rep i e

-- | Construct range r{i,j}
mkRng                                   :: Int -> Int -> GenRegex l -> GenRegex l
mkRng 0  0  _e                          = mkUnit
mkRng 1  1  e                           = e
mkRng lb ub _e
    | lb > ub                           = Zero $
                                          "illegal range " ++
                                          show lb ++ ".." ++ show ub
mkRng _l _u e@(Zero _)                  = e
mkRng _l _u e@Unit                      = e
mkRng lb ub e                           = Rng lb ub e

-- | Construct option r?
mkOpt                                   :: GenRegex l -> GenRegex l
mkOpt                                   = mkRng 0 1
{-# INLINE mkOpt #-}

-- | Construct difference r.e.: r1 {\\} r2
--
-- example
--
-- > match "[a-z]+{\\}bush" "obama"     = True
-- > match "[a-z]+{\\}bush" "clinton"   = True
-- > match "[a-z]+{\\}bush" "bush"      = False     -- not important any more

mkDiff                                  :: Eq l => GenRegex l -> GenRegex l -> GenRegex l
mkDiff e1@(Zero _) _e2                  = e1                                    -- {} - r2 = {}
mkDiff e1          (Zero _)             = e1                                    -- r1 - {} = r1
mkDiff _e1         (Star Dot)           = mkZero "empty set in difference expr" -- r1 - .* = {}
mkDiff Dot         (Sym p)              = mkSym $ compCS p                      -- . - s  = ~s
mkDiff (Sym _)     Dot                  = mkZero "empty set in difference expr" -- x - .  = {}
mkDiff (Sym p1)    (Sym p2)             = mkSym $ p1 `diffCS` p2                -- set diff
mkDiff e1          e2
    | e1 == e2                          = mkZero "empty set in difference expr" -- r1 - r1 = {}
    | otherwise                         = Diff e1 e2

-- | Construct the Complement of an r.e.: whole set of words - r

mkCompl                                 :: Eq l => GenRegex l -> GenRegex l
mkCompl (Zero _)                        = mkAll
mkCompl (Star Dot)                      = mkZero "empty set in compl expr"
mkCompl e                               = mkDiff (mkStar mkDot) e

-- | Construct r.e. for intersection: r1 {&} r2
--
-- example
--
-- > match ".*a.*{&}.*b.*" "-a-b-"  = True
-- > match ".*a.*{&}.*b.*" "-b-a-"  = True
-- > match ".*a.*{&}.*b.*" "-a-a-"  = False
-- > match ".*a.*{&}.*b.*" "---b-"  = False

mkIsect                                 :: Eq l => GenRegex l -> GenRegex l -> GenRegex l
mkIsect e1@(Zero _) _e2                 = e1                                    -- {} n r2 = {}
mkIsect _e1         e2@(Zero _)         = e2                                    -- r1 n {} = {}
mkIsect e1@(Unit)   e2                                                  -- () n r2 = () if nullable r2
    | nullable e2                       = e1                                    -- () n r2 = {} if not nullable r2
    | otherwise                         = mkZero "intersection empty"
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

mkExor                                  :: Eq l => GenRegex l -> GenRegex l -> GenRegex l
mkExor (Zero _)     e2                  = e2
mkExor e1           (Zero _)            = e1
mkExor (Star Dot)   _e2                 = mkZero "empty set in exor expr"
mkExor _e1          (Star Dot)          = mkZero "empty set in exor expr"
mkExor (Sym p1)     (Sym p2)            = mkSym $ p1 `exorCS` p2
mkExor (Sym p1)     Dot                 = mkSym $ compCS p1
mkExor Dot          (Sym p2)            = mkSym $ compCS p2
mkExor e1           e2
    | e1 == e2                          = mkZero "empty set in exor expr"       -- r1 xor r1 = {}
    | otherwise                         = Exor e1 e2

mkInterleave                            :: GenRegex l -> GenRegex l -> GenRegex l
mkInterleave e1@(Zero _) _              = e1
mkInterleave _           e2@(Zero _)    = e2
mkInterleave (Unit)      e2             = e2
mkInterleave e1          (Unit)         = e1
mkInterleave e1          e2             = Intl e1 e2

mkBr0                                   :: Label l -> GenRegex l -> String -> GenRegex l
mkBr0 _ e@(Zero _) _                    = e
mkBr0 l Unit       s                    = mkCbr mkUnit [(l,reverse s)]
mkBr0 l e          s                    = Br l e s

-- | Construct a labeled subexpression: ({label}r)

mkBr                                    :: l -> GenRegex l -> GenRegex l
mkBr  l e                               = mkBr0 (Just l) e ""

mkBr'                                   :: GenRegex l -> GenRegex l
mkBr' e                                 = mkBr0 Nothing e ""

mkCbr                                   :: GenRegex l -> [(Label l, String)] -> GenRegex l
mkCbr  e@(Zero _) _                     = e                             -- dead end, throw away subexpr matches
mkCbr (Cbr e ss1) ss                    = mkCbr e (ss ++ ss1)           -- join inner and this subexpr match
mkCbr  e          ss                    = Cbr e ss

-- ------------------------------------------------------------

instance Show l => Show (GenRegex l) where
    show (Zero e)               = "{" ++ e ++ "}"
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
    show (Br l e  s)            = "({" ++ showL l ++ (if null s
                                                      then ""
                                                      else "=" ++ reverse s
                                                     ) ++ "}" ++ show e ++ ")"
    show (Cbr  e ss)            = "([" ++ intercalate "," (map (\(l,s) -> showL l ++ "=" ++ s) ss) ++ "]"
                                  ++ show e ++
                                  ")"

showL                           :: Show l => Label l -> String
showL                           = rmq . maybe "" show
                                  where
                                  rmq ('\"':xs) = init xs
                                  rmq xs          = xs

-- ------------------------------------------------------------

isZero                          :: GenRegex l -> Bool
isZero (Zero _)                 = True
isZero _                        = False
{-# INLINE isZero #-}

errRegex                        :: GenRegex l -> String
errRegex (Zero e)               = e
errRegex _                      = ""

-- ------------------------------------------------------------

nullable                        :: GenRegex l -> Bool
nullable                        = fst . nullable'
{-# INLINE nullable #-}

nullable'                       :: GenRegex l -> Nullable l

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

nullable' (Br  l e s)           = (True, [(l, reverse s)]) `isectN` nullable' e
nullable' (Cbr e  ss)           = (True, ss)               `isectN` nullable' e

isectN                          :: Nullable l -> Nullable l -> Nullable l
isectN (True, ws1) (True, ws2)  = (True, ws1 ++ ws2)
isectN _           _            = (False, [])

unionN                          :: Nullable l -> Nullable l -> Nullable l
unionN (False, _) (False, _)    = (False, [])
unionN (_, ws1)   (_, ws2)      = (True, ws1 ++ ws2)

orElseN                         :: Nullable l -> Nullable l -> Nullable l
orElseN e1@(True, _ws1) _       = e1
orElseN _            e2         = e2

diffN                           :: Nullable l -> Nullable l -> Nullable l
diffN n1          (False, _)    = n1
diffN _           _             = (False, [])

exorN                           :: Nullable l -> Nullable l -> Nullable l
exorN n1@(True, _)  (False, _)  = n1
exorN (False, _)  n2@(True, _)  = n2
exorN _           _             = (False, [])

-- ------------------------------------------------------------

-- | FIRST for regular expressions
--
-- this is only an approximation, the real set of char may be smaller,
-- when the expression contains intersection, set difference or exor operators

firstChars                      :: GenRegex l -> CharSet

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
firstChars (Br _l e _s)         = firstChars e
firstChars (Cbr e _ss)          = firstChars e

-- ------------------------------------------------------------

delta1                          :: Eq l => GenRegex l -> Char -> GenRegex l
delta1 e@(Zero _)   _           = e
delta1 Unit         c           = mkZero $
                                  "unexpected char " ++ show c
delta1 (Sym p)      c
    | c `elemCS` p              = mkUnit
    | otherwise                 = mkZero $
                                  "unexpected char " ++ show c

delta1 Dot          _           = mkUnit

delta1 e@(Star Dot) _           = e
delta1 e@(Star e1)  c           = mkSeq   (delta1 e1 c) e

delta1 (Alt e1 e2)  c           = mkAlt   (delta1 e1 c) (delta1 e2 c)

delta1 (Else e1 e2) c           = mkElse  (delta1 e1 c) (delta1 e2 c)

delta1 (Seq e1@(Br l e1' s) e2) c
    | n                         = mkAlt   (mkSeq (delta1 e1 c) e2)
                                          (mkCbr (delta1 e2 c) ((l, reverse s) : ws))
                                  where
                                  (n, ws) = nullable' e1'
delta1 (Seq e1 e2)  c
    | nullable e1               = mkAlt   (mkSeq (delta1 e1 c) e2)
                                          (delta1 e2 c)
    | otherwise                 = mkSeq   (delta1 e1 c) e2

delta1 (Rep i e)    c           = mkSeq   (delta1 e  c) (mkRep (i-1) e)

delta1 (Rng i j e)  c           = mkSeq   (delta1 e  c) (mkRng ((i-1) `max` 0) (j-1) e)

delta1 (Diff e1 e2) c           = mkDiff  (delta1 e1 c) (delta1 e2 c)

delta1 (Isec e1 e2) c           = mkIsect (delta1 e1 c) (delta1 e2 c)

delta1 (Exor e1 e2) c           = mkExor  (delta1 e1 c) (delta1 e2 c)

delta1 (Intl e1 e2) c           = mkAlt   (mkInterleave (delta1 e1 c)         e2   )
                                          (mkInterleave         e1    (delta1 e2 c))

delta1 (Br l e s)   c           = mkBr0 l (delta1 e  c) (c:s)

delta1 (Cbr e ss)   c           = mkCbr   (delta1 e  c) ss

-- ------------------------------------------------------------

delta                           :: Eq l => GenRegex l -> String -> GenRegex l
delta                           = foldl' delta1

matchWithRegex                  :: Eq l => GenRegex l -> String -> Bool
matchWithRegex e                = nullable . delta e

matchWithRegex'                 :: Eq l => GenRegex l -> String -> Maybe [(Label l,String)]
matchWithRegex' e               = (\ (r, l) -> if r then Just l else Nothing) . nullable' . delta e

-- ------------------------------------------------------------

-- | This function wraps the whole regex in a subexpression before starting
-- the parse. This is done for getting acces to
-- the whole parsed string. Therfore we need one special label, this label
-- is the Nothing value, all explicit labels are Just labels.

splitWithRegex                  :: Eq l => GenRegex l -> String -> Maybe ([(Label l,String)], String)
splitWithRegex re inp           = do
                                  (re', rest) <- splitWithRegex' (mkBr' re) inp
                                  return ( snd . nullable' $ re', rest)

splitWithRegexCS                :: Eq l => GenRegex l -> CharSet -> String -> Maybe ([(Label l,String)], String)
splitWithRegexCS re cs inp      = do
                                  (re', rest) <- splitWithRegexCS' (mkBr' re) cs inp
                                  return ( snd . nullable' $ re', rest)

-- ----------------------------------------
--
-- | The main scanner function

{- linear recursive function, can lead to stack overflow

splitWithRegex'                 :: Eq l => GenRegex l -> String -> Maybe (GenRegex l, String)
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

splitWithRegex'                 :: Eq l => GenRegex l -> String -> Maybe (GenRegex l, String)
splitWithRegex' re              = splitWithRegex''
                                  ( if nullable re
                                    then Just (re, "")          -- first possible result: empty prefix
                                    else Nothing                -- empty prefix not a result
                                  ) re

splitWithRegex''                :: Eq l => Maybe (GenRegex l, String) -> GenRegex l -> String -> Maybe (GenRegex l, String)
splitWithRegex'' lastRes _re "" = lastRes

splitWithRegex'' lastRes re (c : inp')
    | isZero re                 = lastRes
    | otherwise                 = splitWithRegex'' nextRes re' $ inp'
    where
    re'                         = delta1 re c
    nextRes
        | nullable re'          = Just (re', inp')
        | otherwise             = lastRes

-- ----------------------------------------
--
-- | speedup version for splitWithRegex'
--
-- This function checks whether the input starts with a char from FIRST re.
-- If this is not the case, the split fails. The FIRST set can be computed once
-- for a whole tokenizer and reused by every call of split

splitWithRegexCS'               :: Eq l => GenRegex l -> CharSet -> String -> Maybe (GenRegex l, String)

splitWithRegexCS' re cs inp@(c : _)
    | c `elemCS` cs             = splitWithRegex' re inp

splitWithRegexCS' re _cs inp
    | nullable re               = Just (re, inp)
    | otherwise                 = Nothing

-- ------------------------------------------------------------
