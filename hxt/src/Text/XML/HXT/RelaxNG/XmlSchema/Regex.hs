-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG.XmlSchema.Regex
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   W3C XML Schema Regular Expression Matcher

   Grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>

-}

-- ------------------------------------------------------------

module Text.XML.HXT.RelaxNG.XmlSchema.Regex
    ( Regex
    , chars
    , charRngs
    , mkZero
    , mkUnit
    , mkSym
    , mkSym1
    , mkSymRng
    , mkDot
    , mkStar
    , mkAlt
    , mkSeq
    , mkRep
    , mkRng
    , mkOpt
    , mkDif
    , mkCompl
    , isZero
    , nullable
    , delta
    , matchWithRE
    , (<&&>)
    , (<||>)
    )
where

import Data.List        ( foldl' )

-- ------------------------------------------------------------

data Regex      = Zero String
                | Unit
                | Sym (Char -> Bool)
                | Dot
                | Star Regex
                | Alt Regex Regex
                | Seq Regex Regex
                | Rep Int Regex         -- 1 or more repetitions
                | Rng Int Int Regex     -- n..m repetitions
                | Dif Regex Regex       -- r1 - r2

-- ------------------------------------------------------------

{- just for documentation

class Inv a where
    inv         :: a -> Bool

instance Inv Regex where
    inv (Zero _)        = True
    inv Unit            = True
    inv (Sym p)         = not . null . chars $ p
    inv Dot             = True
    inv (Star e)        = inv e
    inv (Alt e1 e2)     = inv e1 &&
                          inv e2
    inv (Seq e1 e2)     = inv e1 &&
                          inv e2
    inv (Rep i e)       = i > 0 && inv e
    inv (Rng i j e)     = (i < j || (i == j && i > 1)) &&
                          inv e
    inv (Dif e1 e2)     = inv e1 &&
                          inv e2
-}

-- ------------------------------------------------------------
-- | enumerate all chars specified by a predicate
--
-- this function is expensive, it should only be used for testing

chars                           :: (Char -> Bool) -> [Char]
chars p                         = filter p $ [minBound .. maxBound]

charRngs                        :: [Char] -> [(Char, Char)]
charRngs []                     = []
charRngs (x:xs)                 = charRng x xs
                                where
                                charRng y []            = (x,y) : []
                                charRng y xs'@(x1:xs1)
                                    | x1 == succ y      = charRng x1 xs1
                                    | otherwise         = (x,y) : charRngs xs'

-- ------------------------------------------------------------
--
-- smart constructors

mkZero                          :: String -> Regex
mkZero                          = Zero
{-# INLINE mkZero #-}

mkUnit                          :: Regex
mkUnit                          = Unit
{-# INLINE mkUnit #-}

mkSym                           :: (Char -> Bool) -> Regex
mkSym                           = Sym
{-# INLINE mkSym #-}

mkSym1                          :: Char -> Regex
mkSym1  c                       = mkSym (==c)

mkSymRng                        :: Char -> Char -> Regex
mkSymRng c1 c2
    | c1 == minBound &&
      c2 == maxBound            = mkDot
    | c1 <= c2                  = mkSym  $ (>= c1) <&&> (<= c2)
    | otherwise                 = mkZero $ "empty char range"

mkDot                           :: Regex
mkDot                           = Dot
{-# INLINE mkDot #-}

mkStar                          :: Regex -> Regex
mkStar (Zero _)                 = mkUnit                -- {}* == ()
mkStar e@Unit                   = e                     -- ()* == ()
mkStar e@(Star _e1)             = e                     -- (r*)* == r*
mkStar (Rep 1 e1)               = mkStar e1             -- (r+)* == r*
mkStar e@(Alt _ _)              = Star (rmStar e)       -- (a*|b)* == (a|b)*
mkStar e                        = Star e

rmStar                          :: Regex -> Regex
rmStar (Alt e1 e2)              = mkAlt (rmStar e1) (rmStar e2)
rmStar (Star e1)                = rmStar e1
rmStar (Rep 1 e1)               = rmStar e1
rmStar e1                       = e1

mkAlt                                   :: Regex -> Regex -> Regex
mkAlt e1            (Zero _)            = e1                            -- e1 u {} = e1
mkAlt (Zero _)      e2                  = e2                            -- {} u e2 = e2
mkAlt e1@(Star Dot) _e2                 = e1                            -- A* u e1 = A*
mkAlt _e1           e2@(Star Dot)       = e2                            -- e1 u A* = A*
mkAlt (Sym p1)      (Sym p2)            = mkSym $ p1 <||> p2            -- melting of predicates
mkAlt e1            e2@(Sym _)          = mkAlt e2 e1                   -- symmetry: predicates always first
mkAlt e1@(Sym _)    (Alt e2@(Sym _) e3) = mkAlt (mkAlt e1 e2) e3        -- prepare melting of predicates
mkAlt (Alt e1 e2)   e3                  = mkAlt e1 (mkAlt e2 e3)        -- associativity
mkAlt e1 e2                             = Alt e1 e2

mkSeq                           :: Regex -> Regex -> Regex
mkSeq e1@(Zero _) _e2           = e1
mkSeq _e1         e2@(Zero _)   = e2
mkSeq Unit        e2            = e2
mkSeq e1          Unit          = e1
mkSeq (Seq e1 e2) e3            = mkSeq e1 (mkSeq e2 e3)
mkSeq e1 e2                     = Seq e1 e2

mkRep           :: Int -> Regex -> Regex
mkRep 0 e                       = mkStar e
mkRep _ e@(Zero _)              = e
mkRep _ e@Unit                  = e
mkRep i e                       = Rep i e

mkRng   :: Int -> Int -> Regex -> Regex
mkRng 0  0  _e                  = mkUnit
mkRng 1  1  e                   = e
mkRng lb ub _e
    | lb > ub                   = Zero $
                                  "illegal range " ++
                                  show lb ++ ".." ++ show ub
mkRng _l _u e@(Zero _)          = e
mkRng _l _u e@Unit              = e
mkRng lb ub e                   = Rng lb ub e

mkOpt                           :: Regex -> Regex
mkOpt                           = mkRng 0 1

mkDif   :: Regex -> Regex -> Regex
mkDif e1@(Zero _) _e2           = e1
mkDif e1          (Zero _)      = e1
mkDif _e1         (Star Dot)    = mkZero "empty set in difference expr"
mkDif Dot         (Sym p)       = mkSym (not . p)
mkDif (Sym _)     Dot           = mkZero "empty set of chars in difference expr"
mkDif (Sym p1)    (Sym p2)
    | null . chars $ (\ x -> p1 x && not (p2 x))
                                = mkZero "empty set of chars in difference expr"
mkDif e1          e2            = Dif e1 e2

mkCompl                         :: Regex -> Regex
mkCompl                         = mkDif mkDot

-- ------------------------------------------------------------

instance Show Regex where
    show (Zero s)       = "{err:" ++ s ++ "}"
    show Unit           = "()"
    show (Sym p)
        | null (tail cs) &&
          rng1 (head cs)
                        = escRng . head $ cs
        | otherwise     = "[" ++ concat cs' ++ "]"
                          where
                          rng1 (x,y)    = x == y
                          cs            = charRngs . chars $ p
                          cs'           = map escRng cs
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
    show Dot            = "."
    show (Star e)       = "(" ++ show e ++ ")*"
    show (Alt e1 e2)    = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
    show (Seq e1 e2)    = show e1 ++ show e2
    show (Rep 1 e)      = "(" ++ show e ++ ")+"
    show (Rep i e)      = "(" ++ show e ++ "){" ++ show i ++ ",}"
    show (Rng 0 1 e)    = "(" ++ show e ++ ")?"
    show (Rng i j e)    = "(" ++ show e ++ "){" ++ show i ++ "," ++ show j ++ "}"
    show (Dif e1 e2)    = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"

-- ------------------------------------------------------------

isZero                  :: Regex -> Bool
isZero (Zero _)         = True
isZero _                = False
{-# INLINE isZero #-}

nullable                :: Regex -> Bool
nullable (Zero _)       = False
nullable Unit           = True
nullable (Sym _p)       = False         -- assumption: p holds for at least one char
nullable Dot            = False
nullable (Star _)       = True
nullable (Alt e1 e2)    = nullable e1 ||
                          nullable e2
nullable (Seq e1 e2)    = nullable e1 &&
                          nullable e2
nullable (Rep _i e)     = nullable e
nullable (Rng i _ e)    = i == 0 ||
                          nullable e
nullable (Dif e1 e2)    = nullable e1 &&
                          not (nullable e2)

-- ------------------------------------------------------------

delta   :: Regex -> Char -> Regex
delta e@(Zero _)  _     = e
delta Unit        c     = mkZero $
                          "unexpected char " ++ show c
delta (Sym p)     c
    | p c               = mkUnit
    | otherwise         = mkZero $
                          "unexpected char " ++ show c ++ ", expected: " ++ oneof ++ chars'
                          where
                          (cs, ds) = splitAt 40 (chars p)
                          oneof
                              | null (tail cs) = ""
                              | otherwise      = "one of "
                          chars'
                              | null (tail cs) = "'" ++ cs ++ "'"
                              | null ds   = "[" ++ cs ++ "]"
                              | otherwise = "[" ++ cs ++ "...]"

delta Dot         _     = mkUnit
delta e@(Star e1) c     = mkSeq (delta e1 c) e
delta (Alt e1 e2) c     = mkAlt (delta e1 c) (delta e2 c)
delta (Seq e1 e2) c
    | nullable e1       = mkAlt (mkSeq (delta e1 c) e2) (delta e2 c)
    | otherwise         = mkSeq (delta e1 c) e2
delta (Rep i e)   c     = mkSeq (delta e c) (mkRep (i-1) e)
delta (Rng i j e) c     = mkSeq (delta e c) (mkRng ((i-1) `max` 0) (j-1) e)
delta (Dif e1 e2) c     = mkDif (delta e1 c) (delta e2 c)

-- ------------------------------------------------------------

delta'          :: Regex -> String -> Regex
delta'          = foldl' delta

matchWithRE             :: Regex -> String -> Maybe String
matchWithRE e
    = res . delta' e
    where
    res (Zero err)      = Just err
    res re
        | nullable re   = Nothing       -- o.k.
        | otherwise     = Just $ "input does not match " ++ show e

-- ------------------------------------------------------------

(<&&>)          :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool)
f <&&> g        = \ x -> f x && g x             -- liftA2 (&&)

{-# INLINE (<&&>) #-}


(<||>)          :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool)
f <||> g        = \ x -> f x || g x             -- liftA2 (||)

{-# INLINE (<||>) #-}

-- ------------------------------------------------------------
