-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DTDValidation.RE
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   A module for regular expression matching based on derivatives of regular expressions.

   The code was taken from Joe English (<http://www.flightlab.com/~joe/sgml/validate.html>).
   Tested and extended by Martin Schmidt.

   Further references for the algorithm:

   Janusz A. Brzozowski.

        Derivatives of Regular Expressions. Journal of the ACM, Volume 11, Issue 4, 1964.

   Mark Hopkins.

        Regular Expression Package. Posted to comp.compilers, 1994.
        Available per FTP at <ftp://iecc.com/pub/file/regex.tar.gz>.
-}

-- ------------------------------------------------------------

module Text.XML.HXT.DTDValidation.RE
    ( RE(..)

    , re_unit
    , re_zero
    , re_sym
    , re_rep
    , re_plus
    , re_opt
    , re_seq
    , re_alt
    , re_dot

    , checkRE
    , matches
    , nullable
    , printRE
  )
where

import           Data.List (foldl')

-- |
-- Data type for regular expressions.

data RE a =
        RE_ZERO String          --' L(0)   = {} (empty set)
        | RE_UNIT               --' L(1)   = { [] } (empty sequence)
        | RE_SYM a              --' L(x)   = { [x] }
        | RE_DOT                --' accept any single symbol
        | RE_REP (RE a)         --' L(e*)  = { [] } `union` L(e+)
        | RE_PLUS (RE a)        --' L(e+)  = { x ++ y | x <- L(e), y <- L(e*) }
        | RE_OPT (RE a)         --' L(e?)  = L(e) `union` { [] }
        | RE_SEQ (RE a) (RE a)  --' L(e,f) = { x ++ y | x <- L(e), y <- L(f) }
        | RE_ALT (RE a) (RE a)  --' L(e|f) = L(e) `union` L(f)
        deriving (Show, Eq, Ord)



-- ------------------------------------------------------------
-- Constructor functions to simplify regular expressions when constructing them.

-- |
-- Constructs a regular expression for an empty set.
--
--    * 1.parameter errMsg :  error message
--
--    - returns : regular expression for an empty set

re_zero                 :: String -> RE a
re_zero m               = RE_ZERO m


-- |
-- Constructs a regular expression for an empty sequence.
--
--    - returns : regular expression for an empty sequence

re_unit                 :: RE a
re_unit                 = RE_UNIT


-- |
-- Constructs a regular expression for accepting a symbol
--
--    * 1.parameter sym :  the symbol to be accepted
--
--    - returns : regular expression for accepting a symbol

re_sym                  :: a -> RE a
re_sym x                = RE_SYM x


-- |
-- Constructs a regular expression for accepting any singel symbol
--
--    - returns : regular expression for accepting any singel symbol

re_dot                  :: RE a
re_dot                  = RE_DOT


-- |
-- Constructs an optional repetition (*) of a regular expression
--
--    * 1.parameter re_a :  regular expression to be repeted
--
--    - returns : new regular expression

re_rep                  :: RE a -> RE a
re_rep RE_UNIT          = RE_UNIT
re_rep (RE_ZERO _)      = RE_UNIT
re_rep e@(RE_REP _)     = RE_REP (rem_rep e)            -- remove nested reps
re_rep e@(RE_ALT _ _)   = RE_REP (rem_rep e)            -- remove nested reps in alternatives
re_rep e                = RE_REP e

-- |
-- remove redundant nested *'s in RE
-- theoretically this is unneccessary,
-- but without this simplification the runtime can increase exponentally
-- when computing deltas, e.g. for a** or (a|b*)* which is the same as (a|b)*

rem_rep                     :: RE a -> RE a
rem_rep (RE_ALT RE_UNIT e2) = e2
rem_rep (RE_ALT e1 e2)      = RE_ALT (rem_rep e1) (rem_rep e2)
rem_rep (RE_REP e1)         = rem_rep e1
rem_rep e1                  = e1


-- |
-- Constructs a repetition (+) of a regular expression
--
--    * 1.parameter re_a :  regular expression to be repeted
--
--    - returns : new regular expression

re_plus                 :: RE a -> RE a
re_plus RE_UNIT         = RE_UNIT
re_plus (RE_ZERO m)     = RE_ZERO m
re_plus e
    | nullable e        = re_rep e            -- nullable e => e+ == e*
    | otherwise         = re_seq e (re_rep e)


-- |
-- Constructs an option (?) of a regular expression
--
--    * 1.parameter re_a :  regular expression to be optional
--
--    - returns : new regular expression

re_opt                  :: (Ord a) => RE a -> RE a
re_opt RE_UNIT          = RE_UNIT
re_opt (RE_ZERO _)      = RE_UNIT
re_opt e                = re_alt RE_UNIT e

-- |
-- Constructs a sequence (,) of two regular expressions
--
--    * 1.parameter re_a :  first regular expression in sequence
--
--    - 2.parameter re_b :  second regular expression in sequence
--
--    - returns : new regular expression

re_seq                                 :: RE a -> RE a -> RE a
re_seq e1@(RE_ZERO _)   _              = e1                         -- simplification
re_seq RE_UNIT          e2             = e2                         -- simplification
re_seq _                e2@(RE_ZERO _) = e2                         -- simplification
re_seq e1               RE_UNIT        = e1                         -- simplification
re_seq (RE_SEQ e11 e12) e2             = re_seq e11 (re_seq e12 e2) -- right assoc.
re_seq e1               e2             = RE_SEQ e1 e2


-- |
-- Constructs an alternative (|) of two regular expressions
--
--    * 1.parameter re_a :  first regular expression of alternative
--
--    - 2.parameter re_b :  second regular expression of alternative
--
--    - returns : new regular expression

re_alt                                      :: (Ord a) => RE a -> RE a -> RE a
re_alt (RE_ZERO _)      e2                  = e2
re_alt e1               (RE_ZERO _)         = e1
re_alt (RE_ALT e11 e12) e2                  = re_alt e11 (re_alt e12 e2)  -- is right assoc
re_alt e1               e2@(RE_ALT e21 e22)
    | e1 == e21                             = e2             -- duplicates removed, the effective rule
    | e1 >  e21                             = re_alt e21 (re_alt e1 e22)  -- sort alternatives
    | otherwise                             = RE_ALT e1 e2
re_alt e1               e2
    | e1 == e2                              = e2             -- simplification, the effective rule
    | e1 >  e2                              = re_alt e2 e1   -- sort alts for unique repr.
    | otherwise                             = RE_ALT e1 e2



-- ------------------------------------------------------------


-- |
-- Checks if a regular expression matches the empty sequence.
--
-- nullable e == [] `in` L(e)
--
-- This check indicates if a regular expression fits to a sentence or not.
--
--    * 1.parameter re :  regular expression to be checked
--
--    - returns : true if regular expression matches the empty sequence,
--                otherwise false

nullable                ::  RE a -> Bool
nullable (RE_ZERO _)    = False
nullable RE_UNIT        = True
nullable (RE_SYM _)     = False
nullable (RE_REP _)     = True
nullable (RE_PLUS e)    = nullable e
nullable (RE_OPT _)     = True
nullable (RE_SEQ e f)   = nullable e && nullable f
nullable (RE_ALT e f)   = nullable e || nullable f
nullable RE_DOT         = False


-- |
-- Derives a regular expression with respect to one symbol.
--
-- L(delta e x) = x \ L(e)
--
--    * 1.parameter re :  regular expression to be derived
--
--    - 2.parameter sym :  the symbol on which the regular expression is applied
--
--    - returns : the derived regular expression

delta :: (Ord a, Show a) => RE a -> a -> RE a
delta re x = case re of
        RE_ZERO _               -> re                                   -- re_zero m
        RE_UNIT                 -> re_zero ("Symbol " ++ show x ++ " unexpected.")
        RE_SYM sym
                | x == sym      -> re_unit
                | otherwise     -> re_zero ("Symbol " ++ show sym ++ " expected, but symbol " ++ show x ++ " found.")
        RE_REP  e               -> re_seq (delta e x) re                -- (re_rep e)
        RE_PLUS e               -> re_seq (delta e x) (re_rep e)
        RE_OPT  e               -> delta e x
        RE_SEQ  e f
                | nullable e    -> re_alt (re_seq (delta e x) f) (delta f x)
                | otherwise     -> re_seq (delta e x) f
        RE_ALT  e f             -> re_alt (delta e x) (delta f x)
        RE_DOT                  -> re_unit


-- |
-- Derives a regular expression with respect to a sentence.
--
--    * 1.parameter re :  regular expression
--
--    - 2.parameter s :  sentence to which the regular expression is applied
--
--    - returns : the derived regular expression

matches :: (Ord a, Show a) => RE a -> [a] -> RE a
matches e = foldl' delta e


-- |
-- Checks if an input matched a regular expression. The function should be
-- called after matches.
--
-- Was the sentence used in @matches@ in the language of the regular expression?
-- -> matches e s == s `in` L(e)?
--
--    * 1.parameter re :  the derived regular expression
--
--    - returns : empty String if input matched the regular expression, otherwise
--               an error message is returned

checkRE :: (Eq a, Show a) => RE a -> String
checkRE (RE_UNIT)       = ""
checkRE (RE_ZERO m)     = m
checkRE re
        | nullable re   = ""
        | otherwise     = "Input must match " ++ printRE re



-- ------------------------------------------------------------



-- |
-- Constructs a string representation of a regular expression.
--
--    * 1.parameter re :  a regular expression
--
--    - returns : the string representation of the regular expression

printRE :: (Eq a, Show a) => RE a -> String
printRE re'
    = "( " ++ printRE1 re' ++ " )"
      where
      -- printRE1 :: (Eq a, Show a) => RE a -> String
      printRE1 re = case re of
          RE_ZERO m                             -> "ERROR: " ++ m
          RE_UNIT                               -> ""
          RE_SYM sym                            -> show sym
          RE_DOT                                -> "."
          RE_REP e
              | isSingle e                      -> printRE1 e ++ "*"
              | otherwise                       -> "(" ++ printRE1 e ++ ")*"
          RE_PLUS e
              | isSingle e                      -> printRE1 e ++ "+"
              | otherwise                       -> "(" ++ printRE1 e ++ ")+"
          RE_OPT e
              | isSingle e                      -> printRE1 e ++ "?"
              | otherwise                       -> "(" ++ printRE1 e ++ ")?"
          RE_SEQ e1 (RE_REP e2)
              | e1 == e2                        -> printRE1 (RE_PLUS e1)
          RE_SEQ e1 (RE_SEQ (RE_REP e2) e3)
              | e1 == e2                        -> printRE1 (RE_SEQ (RE_PLUS e1) e3)
          RE_SEQ e f
              | isAlt e  && not (isAlt f)       -> "(" ++ printRE1 e ++ ") , " ++ printRE1 f
              | not (isAlt e) && isAlt f        -> printRE1 e ++ " , (" ++ printRE1 f ++ ")"
              | isAlt e  && isAlt f             -> "(" ++ printRE1 e ++ ") , (" ++ printRE1 f ++ ")"
              | otherwise                       -> printRE1 e ++ " , " ++ printRE1 f
          RE_ALT RE_UNIT f                      -> printRE1 (RE_OPT f)
          RE_ALT e f
              | isSeq e  && not (isSeq f)       -> "(" ++ printRE1 e ++ ") | " ++ printRE1 f
              | not (isSeq e) && isSeq f        -> printRE1 e ++ " | (" ++ printRE1 f ++ ")"
              | isSeq e  && isSeq f             -> "(" ++ printRE1 e ++ ") | (" ++ printRE1 f ++ ")"
              | otherwise                       -> printRE1 e ++ " | " ++ printRE1 f


      isSingle :: RE a -> Bool
      isSingle (RE_ZERO _)    = True
      isSingle RE_UNIT        = True
      isSingle (RE_SYM _)     = True
      isSingle _              = False


      isSeq :: (Eq a) => RE a -> Bool
      isSeq (RE_SEQ e1 (RE_REP e2))
          | e1 == e2          = False  -- is transformed back into RE_PLUS
      isSeq (RE_SEQ _ _)      = True
      isSeq _                 = False


      isAlt :: RE a -> Bool
      isAlt (RE_ALT RE_UNIT _)= False  -- is transformed back into a RE_OPT
      isAlt (RE_ALT _ _)      = True
      isAlt _                 = False
