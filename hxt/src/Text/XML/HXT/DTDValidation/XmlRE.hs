-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DTDValidation.XmlRE
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   A module for regular expression matching, adapted for XML DTDs.

   This module is based on the module RE.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DTDValidation.XmlRE
    ( RE

    , checkRE
    , matches
    , printRE

    , re_unit
    , re_zero
    , re_sym
    , re_rep
    , re_plus
    , re_opt
    , re_seq
    , re_alt
    , re_dot
    )
where

-- import           Debug.Trace                         (trace)

import           Data.List                           (foldl')

import           Text.XML.HXT.DTDValidation.RE       hiding (matches)

import           Text.XML.HXT.Arrow.Edit             (removeComment,
                                                      removeWhiteSpace)
import qualified Text.XML.HXT.DOM.XmlNode            as XN
import           Text.XML.HXT.DTDValidation.TypeDefs

-- |
-- Derives a regular expression with respect to a list of elements.
--
--    * 1.parameter re :  regular expression
--
--    - 2.parameter list :  list of elements to which the regular expression is applied
--
--    - returns : the derived regular expression

matches :: RE String -> XmlTrees -> RE String
matches re list
    = foldl' delta re (removeUnimportantStuff $$ list)
      where
      removeUnimportantStuff :: XmlArrow
      removeUnimportantStuff = processBottomUp (removeWhiteSpace >>> removeComment)

      -- trace of growth of REs
      -- delta' re el = delta (trace (("RE : " ++) . (++ "\n" ) . show $ re) re) el


-- |
-- Derives a regular expression with respect to one element.
--
-- L(delta e x) = x \ L(e)
--
--    * 1.parameter re :  regular expression to be derived
--
--    - 2.parameter el :  the element on which the regular expression is applied
--
--    - returns : the derived regular expression

delta :: RE String -> XmlTree -> RE String
delta re el
    | not (allowed el) = re
    | otherwise        = case re of
        RE_ZERO m                -> re_zero m
        RE_UNIT                  -> re_zero (elemName el ++" unexpected.")
        RE_SYM sym
            | sym == k_pcdata    -> if ((XN.isText el) || (XN.isCdata el))
                                    then re_unit
                                    else re_zero ("Character data expected, but "++ elemName el ++" found.")
            | expectedNode el sym -> re_unit
            | otherwise           -> re_zero ("Element "++ show sym ++" expected, but "++ elemName el ++" found.")
        RE_REP e                  -> re_seq (delta e el) (re_rep e)
        RE_PLUS e                 -> re_seq (delta e el) (re_rep e)
        RE_OPT e                  -> delta e el
        RE_SEQ e f
            | nullable e          -> re_alt (re_seq (delta e el) f) (delta f el)
            | otherwise           -> re_seq (delta e el) f
        RE_ALT e f                -> re_alt (delta e el) (delta f el)
        RE_DOT                    -> re_unit

    where
    expectedNode        :: XmlTree -> String -> Bool
    expectedNode n sym
        | XN.isElem n   = nameOfElem n == sym
        | otherwise     = False

    elemName            :: XmlTree -> String
    elemName n
        | XN.isElem n   = "element "++ show (nameOfElem n)
        | otherwise     = "character data"

    allowed     :: XmlTree -> Bool
    allowed n   = XN.isElem n || XN.isText n || XN.isCdata n

-- ------------------------------------------------------------
