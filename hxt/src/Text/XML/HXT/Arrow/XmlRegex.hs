-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlRegex
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Regular Expression Matcher working on lists of XmlTrees

   It's intended to import this module with an explicit
   import declaration for not spoiling the namespace
   with these somewhat special arrows

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlRegex
    ( XmlRegex
    , mkZero
    , mkUnit
    , mkPrim
    , mkPrimA
    , mkDot
    , mkStar
    , mkAlt
    , mkAlts
    , mkSeq
    , mkSeqs
    , mkRep
    , mkRng
    , mkOpt
    , mkPerm
    , mkPerms
    , nullable
    , delta
    , matchXmlRegex
    , splitXmlRegex
    , scanXmlRegex
    , matchRegexA
    , splitRegexA
    , scanRegexA
    )
where

import Control.Arrow.ListArrows

import Data.Maybe

import Text.XML.HXT.DOM.Interface

-- ------------------------------------------------------------
-- the exported regex arrows

-- | check whether a sequence of XmlTrees match an Xml regular expression
--
-- The arrow for 'matchXmlRegex'.
--
-- The expession is build up from simple arrows acting as predicate ('mkPrimA') for
-- an XmlTree and of the usual cobinators for sequence ('mkSeq'), repetition
-- ('mkStar', mkRep', 'mkRng') and choice ('mkAlt', 'mkOpt')

matchRegexA             :: XmlRegex -> LA XmlTree XmlTree -> LA XmlTree XmlTrees
matchRegexA re ts       = ts >>. (\ s -> maybe [s] (const []) . matchXmlRegex re $ s)

-- | split the sequence of trees computed by the filter a into
--
-- The arrow for 'splitXmlRegex'.
--
-- a first part matching the regex and a rest,
-- if a prefix of the input sequence does not match the regex, the arrow fails
-- else the pair containing the result lists is returned

splitRegexA             :: XmlRegex -> LA XmlTree XmlTree -> LA XmlTree (XmlTrees, XmlTrees)
splitRegexA re ts       = ts >>. (maybeToList . splitXmlRegex re)

-- | scan the input sequence with a regex and give the result as a list of lists of trees back
-- the regex must at least match one input tree, so the empty sequence should not match the regex
--
-- The arrow for 'scanXmlRegex'.

scanRegexA              :: XmlRegex -> LA XmlTree XmlTree -> LA XmlTree XmlTrees
scanRegexA re ts        = ts >>. (fromMaybe [] . scanXmlRegex re)

-- ------------------------------------------------------------

data XmlRegex   = Zero String
                | Unit
                | Sym (XmlTree -> Bool)
                | Dot
                | Star XmlRegex
                | Alt XmlRegex XmlRegex
                | Seq XmlRegex XmlRegex
                | Rep Int XmlRegex              -- 1 or more repetitions
                | Rng Int Int XmlRegex          -- n..m repetitions
                | Perm XmlRegex XmlRegex

-- ------------------------------------------------------------

{- just for documentation

class Inv a where
    inv         :: a -> Bool

instance Inv XmlRegex where
    inv (Zero _)        = True
    inv Unit            = True
    inv (Sym p)         = p holds for some XmlTrees
    inv Dot             = True
    inv (Star e)        = inv e
    inv (Alt e1 e2)     = inv e1 &&
                          inv e2
    inv (Seq e1 e2)     = inv e1 &&
                          inv e2
    inv (Rep i e)       = i > 0 && inv e
    inv (Rng i j e)     = (i < j || (i == j && i > 1)) &&
                          inv e
    inv (Perm e1 e2)    = inv e1 &&
                          inv e2
-}
-- ------------------------------------------------------------
--
-- smart constructors

mkZero          :: String -> XmlRegex
mkZero          = Zero

mkUnit          :: XmlRegex
mkUnit          = Unit

mkPrim          :: (XmlTree -> Bool) -> XmlRegex
mkPrim          = Sym

mkPrimA         :: LA XmlTree XmlTree -> XmlRegex
mkPrimA a       = mkPrim (not . null . runLA a)

mkDot           :: XmlRegex
mkDot           = Dot

mkStar                  :: XmlRegex -> XmlRegex
mkStar (Zero _)         = mkUnit                -- {}* == ()
mkStar e@Unit           = e                     -- ()* == ()
mkStar e@(Star _e1)     = e                     -- (r*)* == r*
mkStar (Rep 1 e1)       = mkStar e1             -- (r+)* == r*
mkStar e@(Alt _ _)      = Star (rmStar e)       -- (a*|b)* == (a|b)*
mkStar e                = Star e

rmStar  :: XmlRegex -> XmlRegex
rmStar (Alt e1 e2)      = mkAlt (rmStar e1) (rmStar e2)
rmStar (Star e1)        = rmStar e1
rmStar (Rep 1 e1)       = rmStar e1
rmStar e1               = e1

mkAlt                                   :: XmlRegex -> XmlRegex -> XmlRegex
mkAlt e1            (Zero _)            = e1                            -- e1 u {} = e1
mkAlt (Zero _)      e2                  = e2                            -- {} u e2 = e2
mkAlt e1@(Star Dot) _e2                 = e1                            -- A* u e1 = A*
mkAlt _e1           e2@(Star Dot)       = e2                            -- e1 u A* = A*
mkAlt (Sym p1)      (Sym p2)            = mkPrim $ \ x -> p1 x || p2 x  -- melting of predicates
mkAlt e1            e2@(Sym _)          = mkAlt e2 e1                   -- symmetry: predicates always first
mkAlt e1@(Sym _)    (Alt e2@(Sym _) e3) = mkAlt (mkAlt e1 e2) e3        -- prepare melting of predicates
mkAlt (Alt e1 e2)   e3                  = mkAlt e1 (mkAlt e2 e3)        -- associativity
mkAlt e1 e2                             = Alt e1 e2

mkAlts                          :: [XmlRegex] -> XmlRegex
mkAlts                          = foldr mkAlt (mkZero "")

mkSeq                           :: XmlRegex -> XmlRegex -> XmlRegex
mkSeq e1@(Zero _) _e2           = e1
mkSeq _e1         e2@(Zero _)   = e2
mkSeq Unit        e2            = e2
mkSeq e1          Unit          = e1
mkSeq (Seq e1 e2) e3            = mkSeq e1 (mkSeq e2 e3)
mkSeq e1 e2                     = Seq e1 e2

mkSeqs                          :: [XmlRegex] -> XmlRegex
mkSeqs                          = foldr mkSeq mkUnit

mkRep           :: Int -> XmlRegex -> XmlRegex
mkRep 0 e                       = mkStar e
mkRep _ e@(Zero _)              = e
mkRep _ e@Unit                  = e
mkRep i e                       = Rep i e

mkRng   :: Int -> Int -> XmlRegex -> XmlRegex
mkRng 0  0  _e                  = mkUnit
mkRng 1  1  e                   = e
mkRng lb ub _e
    | lb > ub                   = Zero $
                                  "illegal range " ++
                                  show lb ++ ".." ++ show ub
mkRng _l _u e@(Zero _)          = e
mkRng _l _u e@Unit              = e
mkRng lb ub e                   = Rng lb ub e

mkOpt   :: XmlRegex -> XmlRegex
mkOpt   = mkRng 0 1

mkPerm                           :: XmlRegex -> XmlRegex -> XmlRegex
mkPerm e1@(Zero _) _             = e1
mkPerm _           e2@(Zero _)   = e2
mkPerm Unit        e2            = e2
mkPerm e1          Unit          = e1
mkPerm e1          e2            = Perm e1 e2

mkPerms                          :: [XmlRegex] -> XmlRegex
mkPerms                          = foldr mkPerm mkUnit

-- ------------------------------------------------------------

instance Show XmlRegex where
    show (Zero s)       = "{err:" ++ s ++ "}"
    show Unit           = "()"
    show (Sym _p)       = "{single tree pred}"
    show Dot            = "."
    show (Star e)       = "(" ++ show e ++ ")*"
    show (Alt e1 e2)    = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
    show (Seq e1 e2)    = show e1 ++ show e2
    show (Rep 1 e)      = "(" ++ show e ++ ")+"
    show (Rep i e)      = "(" ++ show e ++ "){" ++ show i ++ ",}"
    show (Rng 0 1 e)    = "(" ++ show e ++ ")?"
    show (Rng i j e)    = "(" ++ show e ++ "){" ++ show i ++ "," ++ show j ++ "}"
    show (Perm e1 e2)   = "(" ++ show e1 ++ show e2 ++ "|" ++ show e2 ++ show e1 ++ ")"

-- ------------------------------------------------------------

nullable        :: XmlRegex -> Bool
nullable (Zero _)       = False
nullable Unit           = True
nullable (Sym _p)       = False         -- assumption: p holds for at least one tree
nullable Dot            = False
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

-- ------------------------------------------------------------

delta   :: XmlRegex -> XmlTree -> XmlRegex
delta e@(Zero _)   _    = e
delta Unit         c    = mkZero $
                          "unexpected tree " ++ show c
delta (Sym p)      c
    | p c               = mkUnit
    | otherwise         = mkZero $
                          "unexpected tree " ++ show c
delta Dot          _    = mkUnit
delta e@(Star e1)  c    = mkSeq (delta e1 c) e
delta (Alt e1 e2)  c    = mkAlt (delta e1 c) (delta e2 c)
delta (Seq e1 e2)  c
    | nullable e1       = mkAlt (mkSeq (delta e1 c) e2) (delta e2 c)
    | otherwise         = mkSeq (delta e1 c) e2
delta (Rep i e)    c    = mkSeq (delta e c) (mkRep (i-1) e)
delta (Rng i j e)  c    = mkSeq (delta e c) (mkRng ((i-1) `max` 0) (j-1) e)
delta (Perm e1 e2) c    = case e1' of
                            (Zero _) -> mkPerm e1 (delta e2 c)
                            _        -> mkPerm e1' e2
                          where
                          e1' = delta e1 c

-- ------------------------------------------------------------

delta'          :: XmlRegex -> XmlTrees -> XmlRegex
delta'          = foldl delta

-- | match a sequence of XML trees with a regular expression over trees
--
-- If the input matches, the result is Nothing, else Just an error message is returned

matchXmlRegex           :: XmlRegex -> XmlTrees -> Maybe String
matchXmlRegex e
    = res . delta' e
    where
    res (Zero er)       = Just er
    res re
        | nullable re   = Nothing       -- o.k.
        | otherwise     = Just $ "input does not match " ++ show e

-- ------------------------------------------------------------

-- | split a sequence of XML trees into a pair of a a matching prefix and a rest
--
-- If there is no matching prefix, Nothing is returned

splitXmlRegex           :: XmlRegex -> XmlTrees -> Maybe (XmlTrees, XmlTrees)
splitXmlRegex re        = splitXmlRegex' re []

splitXmlRegex'          :: XmlRegex -> XmlTrees -> XmlTrees -> Maybe (XmlTrees, XmlTrees)
splitXmlRegex' re res []
    | nullable re       = Just (reverse res, [])
    | otherwise         = Nothing

splitXmlRegex' (Zero _) _ _
                        = Nothing

splitXmlRegex' re res xs@(x:xs')
    | isJust res'       = res'
    | nullable re       = Just (reverse res, xs)
    | otherwise         = Nothing
    where
    re'  = delta re x
    res' = splitXmlRegex' re' (x:res) xs'

-- ------------------------------------------------------------

-- | scan a sequence of XML trees and split it into parts matching the given regex
--
-- If the parts cannot be split because of a missing match, or because of the
-- empty sequence as match, Nothing is returned

scanXmlRegex                            :: XmlRegex -> XmlTrees -> Maybe [XmlTrees]
scanXmlRegex re ts                      = scanXmlRegex' re (splitXmlRegex re ts)

scanXmlRegex'                           :: XmlRegex -> Maybe (XmlTrees, XmlTrees) -> Maybe [XmlTrees]
scanXmlRegex' _  Nothing                = Nothing
scanXmlRegex' _  (Just (rs, []))        = Just [rs]
scanXmlRegex' _  (Just ([], _))         = Nothing       -- re is nullable (the empty word matches), nothing split off
                                                        -- would give infinite list of empty lists
scanXmlRegex' re (Just (rs, rest))
    | isNothing res                     = Nothing
    | otherwise                         = Just (rs : fromJust res)
    where
    res = scanXmlRegex' re (splitXmlRegex re rest)

-- ------------------------------------------------------------
