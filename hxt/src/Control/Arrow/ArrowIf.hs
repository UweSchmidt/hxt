-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowIf
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Conditionals for List Arrows

   This module defines conditional combinators for list arrows.

   The empty list as result represents False, none empty lists True.
-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowIf
    ( module Control.Arrow.ArrowIf
    )
where

import Control.Arrow
import Control.Arrow.ArrowList

import Data.List
    ( partition )

-- ------------------------------------------------------------

-- | The interface for arrows as conditionals.
--
-- Requires list arrows because False is represented as empty list, True as none empty lists.
--
-- Only 'ifA' and 'orElse' don't have default implementations

class ArrowList a => ArrowIf a where

    -- | if lifted to arrows

    ifA                 :: a b c -> a b d -> a b d -> a b d

    -- | shortcut: @ ifP p = ifA (isA p) @

    ifP                 :: (b -> Bool) -> a b d -> a b d -> a b d
    ifP p               = ifA (isA p)

    -- | negation: @ neg f = ifA f none this @

    neg                 :: a b c -> a b b
    neg f               = ifA f none this

    -- | @ f \`when\` g @ : when the predicate g holds, f is applied, else the identity filter this

    when                :: a b b -> a b c -> a b b
    f `when` g          = ifA g f this

    -- | shortcut: @ f \`whenP\` p = f \`when\` (isA p) @

    whenP               :: a b b -> (b -> Bool) -> a b b
    f `whenP` g         = ifP g f this

    -- | @ f \`whenNot\` g @ : when the predicate g does not hold, f is applied, else the identity filter this

    whenNot             :: a b b -> a b c -> a b b
    f `whenNot` g       = ifA g this f

    -- | like 'whenP'

    whenNotP            :: a b b -> (b -> Bool) -> a b b
    f `whenNotP` g      = ifP g this f

    -- | @ g \`guards\` f @ : when the predicate g holds, f is applied, else none

    guards              :: a b c -> a b d -> a b d
    f `guards` g        = ifA f g none

    -- | like 'whenP'

    guardsP             :: (b -> Bool) -> a b d -> a b d
    f `guardsP` g       = ifP f g none

    -- | shortcut for @ f `guards` this @

    filterA             :: a b c -> a b b
    filterA f           = ifA f this none

    -- | @ f \`containing\` g @ : keep only those results from f for which g holds
    --
    -- definition: @ f \`containing\` g = f >>> g \`guards\` this @

    containing          :: a b c -> a c d -> a b c
    f `containing` g    = f >>> g `guards` this

    -- | @ f \`notContaining\` g @ : keep only those results from f for which g does not hold
    --
    -- definition: @ f \`notContaining\` g = f >>> ifA g none this @

    notContaining       :: a b c -> a c d -> a b c
    f `notContaining` g = f >>> ifA g none this

    -- | @ f \`orElse\` g @ : directional choice: if f succeeds, the result of f is the result, else g is applied
    orElse              :: a b c -> a b c -> a b c

    -- | generalisation of 'orElse' for multi way branches like in case expressions.
    --
    -- An auxiliary data type 'IfThen' with an infix constructor ':->' is used for writing multi way branches
    --
    -- example: @ choiceA [ p1 :-> e1, p2 :-> e2, this :-> default ] @
    choiceA             :: [IfThen (a b c) (a b d)] -> a b d
    choiceA             = foldr ifA' none
                          where
                          ifA' (g :-> f) = ifA g f


    -- | tag a value with Left or Right, if arrow has success, input is tagged with Left, else with Right
    tagA                :: a b c -> a b (Either b b)
    tagA p              = ifA p (arr Left) (arr Right)


    -- | split a list value with an arrow and returns a pair of lists.
    -- This is the arrow version of 'span'. The arrow is deterministic.
    --
    -- example: @ runLA (spanA (isA (\/= \'-\'))) \"abc-def\" @ gives @ [(\"abc\",\"-def\")] @ as result

    spanA               :: a b b -> a [b] ([b],[b])
    spanA p             = ifA ( arrL (take 1) >>> p )
                          ( arr head &&& (arr tail >>> spanA p)
                            >>>
                            arr (\ ~(x, ~(xs,ys)) -> (x : xs, ys))
                          )
                          ( arr (\ l -> ([],l)) )

    -- | partition a list of values into a pair of lists
    --
    -- This is the arrow Version of 'Data.List.partition'

    partitionA          :: a b b -> a [b] ([b],[b])
    partitionA  p       = listA ( arrL id >>> tagA p )
                          >>^
                          ( (\ ~(l1, l2) -> (unTag l1, unTag l2) ) . partition (isLeft) )
                          where
                          isLeft (Left _) = True
                          isLeft _        = False
                          unTag = map (either id id)

-- ------------------------------------------------------------

-- | an auxiliary data type for 'choiceA'

data IfThen a b = a :-> b

-- ------------------------------------------------------------
