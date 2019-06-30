-- ------------------------------------------------------------

{- |
   Module     : Data.Set.CharSet
   Copyright  : Copyright (C) 2010 - Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Char sets implemeted as non-overlapping ordered lists of intervalls

-}

-- ------------------------------------------------------------

module Data.Set.CharSet
    ( CharSet
    , emptyCS
    , allCS
    , singleCS
    , stringCS
    , rangeCS
    , nullCS
    , fullCS
    , unionCS
    , diffCS
    , intersectCS
    , exorCS
    , compCS
    , elemCS
    , toListCS
    )
where

-- ------------------------------------------------------------

type CharSet            = [(Char, Char)]

emptyCS                 :: CharSet
emptyCS                 = []
{-# INLINE emptyCS #-}

allCS                   :: CharSet
allCS                   = [(minBound, maxBound)]
{-# INLINE allCS #-}

singleCS                :: Char -> CharSet
singleCS c              = [(c,c)]
{-# INLINE singleCS #-}

stringCS                :: String -> CharSet
stringCS                = foldr (unionCS . singleCS) emptyCS
{-# INLINE stringCS #-}

rangeCS                 :: Char -> Char -> CharSet
rangeCS l u
    | l <= u            = [(l,u)]
    | otherwise         = emptyCS
{-# INLINE rangeCS #-}

nullCS                  :: CharSet -> Bool
nullCS                  = null
{-# INLINE nullCS #-}

fullCS                  :: CharSet -> Bool
fullCS [(lb, ub)]
                        = lb == minBound
                          &&
                          ub == maxBound
fullCS _                = False

elemCS                  :: Char -> CharSet -> Bool
elemCS i                = foldr (\ (lb, ub) b -> i >= lb && (i <= ub || b)) False
{-# INLINE elemCS #-}

toListCS                :: CharSet -> [Char]
toListCS                = concatMap (\ (lb, ub) -> [lb..ub])

unionCS                 :: CharSet -> CharSet -> CharSet

unionCS [] s2           = s2
unionCS s1 []           = s1

unionCS s1@((l1,u1):s1') s2@((l2,u2):s2')
    | l1 <  l2          = union l1 u1            $ unionCS s1' s2
    | l1 == l2          = union l1 (u1 `max` u2) $ unionCS s1' s2'
    | otherwise         = union l2 u2            $ unionCS s1  s2'
    -- l1 >  l2
    where
    union l u []        = [(l,u)]
    union l u s@((l', u') : s')
        | u < pred l'   = (l,u) : s
        | otherwise     = union l (u `max` u') s'

diffCS                  :: CharSet -> CharSet -> CharSet

diffCS [] _             = []
diffCS s  []            = s

diffCS s1@((l1,u1):s1') s2@((l2,u2):s2')
    | u1 < l2           = (l1,u1) : diffCS s1' s2       -- whole intervall remains in set
    | u2 < l1           = diffCS s1  s2'                -- no elements to remove
                                                        -- intervalls overlap
    | otherwise         = p ++ diffCS (s ++ s1') s2
    where
    p | l1 < l2         = [(l1, pred l2)]               -- = rangeCS l1 (pred l2), but prevent underflow
      | otherwise       = []
    s | u2 < u1         = [(succ u2, u1)]               -- = rangeCS (succ u2) u1, but prevent overflow
      | otherwise       = []

compCS                  :: CharSet -> CharSet
compCS                  = (allCS `diffCS`)

intersectCS             :: CharSet -> CharSet -> CharSet

intersectCS []  _s2     = []
intersectCS _s1  []     = []

intersectCS s1@((l1,u1):s1') s2@((l2,u2):s2')
    | u1 < l2           = intersectCS s1' s2
    | u2 < l1           = intersectCS s1  s2'
                                                        -- intervalls overlap
    | otherwise         = i : isect
    where
    i                   = (l1 `max` l2, u1 `min` u2)
    isect
        | u1 < u2       = intersectCS s1' s2
        | otherwise     = intersectCS s1  s2'

exorCS                  :: CharSet -> CharSet -> CharSet

exorCS [] s2            = s2
exorCS s1 []            = s1

exorCS s1@(i1@(l1,u1):s1') s2@(i2@(l2,u2):s2')
    | u1 < l2           = i1 : exorCS s1' s2
    | u2 < l1           = i2 : exorCS s1  s2'
                                                        -- intervalls overlap
    | otherwise         = i ++ exor'
    where
    i   | l1 < l2       = [(l1, pred l2)]
        | l2 < l1       = [(l2, pred l1)]
        | otherwise     = []
    exor'
        | u1 < u2       = exorCS                  s1' ((succ u1, u2) : s2')
        | u2 < u1       = exorCS ((succ u2, u1) : s1')                 s2'
        | otherwise     = exorCS                  s1'                  s2'

-- ------------------------------------------------------------
