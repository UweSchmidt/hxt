-- |
-- A module for regular expression matching, adapted for XML DTDs.
--
-- This module is based on the module RE.
--
-- Author .\\artin Schmidt

module Text.XML.HXT.Validator.XmlRE
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

-- import Debug.Trace(trace)

import Text.XML.HXT.Validator.RE hiding (matches)

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.DOM.EditFilters
    ( removeComment
    , removeWhiteSpace
    )

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
    = foldl delta re (removeUnimportantStuff $$ list)
      where
      removeUnimportantStuff :: XmlFilter
      removeUnimportantStuff = processBottomUp (removeWhiteSpace `o` removeComment)
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
            | sym == k_pcdata    -> let node = getNode el in
                                    if ((isXTextNode node) || (isXCdataNode node))
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
    expectedNode :: XmlTree -> String -> Bool
    expectedNode (NTree (XTag n _) _) sym = (qualifiedName n) == sym
    expectedNode _ _                      = False

    elemName :: XmlTree -> String
    elemName (NTree (XTag n _) _)       = "element "++ show (qualifiedName n)
    elemName _                          = "character data"

    allowed :: XmlTree -> Bool
    allowed (NTree (XTag _ _) _)        = True
    allowed (NTree (XText _) _)         = True
    allowed (NTree (XCdata _) _)        = True
    allowed _                           = False
