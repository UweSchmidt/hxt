-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowList
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ArrowList.hs,v 1.11 2006/06/01 12:59:04 hxml Exp $

The List Arrow Class

This module defines the interface for list arrows.

A list arrow is a function, that gives a list of results
for a given argument. A single element result represents a normal function.
An empty list oven indicates, the function is undefined for the given argument.
The empty list may also represent False, none empty lists True.
A list with more than one element gives all results for a nondeterministic function.

-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowList
    ( ArrowList(..)
    )
where

import Control.Arrow

infixl 8 >>., >.

infixl 2 $<, $<<, $<<<, $<<<<
infixl 2 $<$

-- ------------------------------------------------------------

-- | The interface for list arrows
--
-- Only 'mkA', 'arr2A', 'isA' '(>>.)' don't have default implementations

class (Arrow a, ArrowPlus a, ArrowZero a, ArrowApply a) => ArrowList a where

    -- | construction of a 2 argument arrow from a binary function
    -- |
    -- | example: @ a1 &&& a2 >>> arr2 f @

    arr2                :: (b1 -> b2 -> c) -> a (b1, b2) c
    arr2                = arr . uncurry

    -- | construction of a 3 argument arrow from a 3-ary function
    -- |
    -- | example: @ a1 &&& a2 &&& a3 >>> arr3 f @

    arr3                :: (b1 -> b2 -> b3 -> c) -> a (b1, (b2, b3)) c
    arr3 f              = arr (\ ~(x1, ~(x2, x3)) -> f x1 x2 x3)

    -- | construction of a 4 argument arrow from a 4-ary function
    -- |
    -- | example: @ a1 &&& a2 &&& a3 &&& a4 >>> arr4 f @

    arr4                :: (b1 -> b2 -> b3 -> b4 -> c) -> a (b1, (b2, (b3, b4))) c
    arr4 f              = arr (\ ~(x1, ~(x2, ~(x3, x4))) -> f x1 x2 x3 x4)

    -- | construction of a 2 argument arrow from a singe argument arrow

    arr2A               :: (b -> a c d) -> a (b, c) d

    -- | constructor for a list arrow from a function with a list as result

    arrL                :: (b -> [c]) -> a b c

    -- | constructor for a list arrow with 2 arguments

    arr2L               :: (b -> c -> [d]) -> a (b, c) d
    arr2L               = arrL . uncurry

    -- | constructor for a const arrow: @ constA = arr . const @

    constA              :: c -> a b c
    constA              = arr . const

    -- | constructor for a const arrow: @ constL = arrL . const @

    constL              :: [c] -> a b c
    constL              = arrL . const

    -- | builds an arrow from a predicate.
    -- If the predicate holds, the single list containing the input is returned, else the empty list

    isA                 :: (b -> Bool) -> a b b

    -- | combinator for converting the result of a list arrow into another list
    --
    -- example: @ foo >>. reverse @ reverses the the result of foo
    --
    -- example: @ foo >>. take 1 @ constructs a deterministic version of foo by deleting all further results

    (>>.)               :: a b c -> ([c] -> [d]) -> a b d

    -- | combinator for converting the result of an arrow into a single element result

    (>.)                :: a b c -> ([c] ->  d ) -> a b d
    af >. f             = af >>. ((:[]) . f)

    -- | combinator for converting an arrow into a determinstic version with all results collected in a single element list
    --
    -- @ listA af = af >>. (:[]) @
    --
    -- this is useful when the list of results computed by an arrow must be manipulated (e.g. sorted)
    --
    -- example for sorting the results of a filter
    --
    -- > collectAndSort         :: a b c -> a b c
    -- >
    -- > collectAndSort collect = listA collect >>> arrL sort

    listA               :: a b c -> a b [c]
    listA af            = af >>.  (:[])

    -- | the inverse of 'listA'
    --
    -- @ listA af >>> unlistA = af @
    --
    -- unlistA is defined as @ arrL id @

    unlistA             :: a [b] b
    unlistA             = arrL id

    -- | the identity arrow, alias for returnA

    this                :: a b b
    this                = returnA

    -- | the zero arrow, alias for zeroArrow

    none                :: a b c
    none                = zeroArrow

    -- | converts an arrow, that may fail, into an arrow that always succeeds
    --
    -- example: @ withDefault none \"abc\" @ is equivalent to @ constA \"abc\" @

    withDefault         :: a b c -> c -> a b c
    withDefault a d     = a >>. \ x -> if null x then [d] else x

    -- | makes a list arrow deterministic, the number of results is at most 1
    --
    -- definition
    --
    -- > single f = f >>. take 1
    --
    -- examples with strings:
    --
    -- > runLA ( single none ) "x" == []
    -- > runLA ( single this ) "x" == ["x"]
    -- > runLA ( single
    -- >         (constA "y"
    -- >          <+> this ) ) "x" == ["y"]

    single              :: a b c -> a b c
    single f            = f >>. take 1

    -- | compute an arrow from the input and apply the arrow to this input
    --
    -- definition: @ (f &&& this) >>> app @
    --
    -- in a point free style, there is no way to use an argument in 2 places,
    -- this is a combinator for simulating this. first the argument is used to compute an arrow,
    -- then this new arrow is applied to the input
    --
    -- applyA coresponds to: @ apply f x = let g = f x in g x @
    --
    -- see also: '$<', '$<<', '$<<<', '$<<<<', '$<$'

    applyA              :: a b (a b c) -> a b c
    applyA f            = (f &&& this) >>> app

    -- | compute the parameter for an arrow with extra parameters from the input
    -- and apply the arrow for all parameter values to the input
    --
    -- a kind of \"function call\" for arrows, useful for joining arrows
    --
    -- > infixl 2 ($<)
    --
    -- definition:
    --
    -- > g $< f = applyA (f >>> arr g)
    --
    -- if @f@ fails, the whole arrow fails, e.g. @ g \$\< none == none @
    --
    -- if @f@ computes n values and @g@ is deterministic, the whole arrow computes n values
    --
    -- examples with simple list arrows with strings
    --
    -- > prefixString   :: String -> a String String
    -- > prefixString s =  arr (s++)
    -- >
    -- > runLA ( prefixString $< none           ) "x" == []
    -- > runLA ( prefixString $< constA "y"     ) "x" == ["yx"]
    -- > runLA ( prefixString $< this           ) "x" == ["xx"]
    -- > runLA ( prefixString $< constA "y"
    -- >                         <+> constA "z" ) "x" == ["yx","zx"]
    -- > runLA ( prefixString $< constA "y"
    -- >                         <+> this
    -- >                         <+> constA "z" ) "x" == ["yx","xx","zx"]
    --
    -- see also: 'applyA', '$<<', '$<<<', '$<<<<', '$<$'

    ($<)                :: (c -> a b d) -> a b c -> a b d
    g $< f              = applyA (f >>> arr g)

    -- | binary version of '$<'
    --
    -- example with simple list arrows with strings
    --
    -- > infixString    :: String -> String -> a String String
    -- > infixString s1 s2
    -- >                = arr (\ s -> s1 ++ s ++ s2)
    -- >
    -- > runLA ( infixString $<< constA "y" &&& constA "z" ) "x" = ["yxz"]
    -- > runLA ( infixString $<< this &&& this             ) "x" = ["xxx"]
    -- > runLA ( infixString $<< constA "y"
    -- >                         &&& (constA "z" <+> this) ) "x" = ["yxz", "yxx"]

    ($<<)               :: (c1 -> c2 -> a b d) -> a b (c1, c2) -> a b d
    f $<< g             = applyA (g >>> arr2 f)

    -- | version of '$<' for arrows with 3 extra parameters
    --
    -- typical usage
    --
    -- > f $<<< g1 &&& g2 &&& g3

    ($<<<)              :: (c1 -> c2 -> c3 -> a b d) -> a b (c1, (c2, c3)) -> a b d
    f $<<< g            = applyA (g >>> arr3 f)

    -- | version of '$<' for arrows with 4 extra parameters
    --
    -- typical usage
    --
    -- > f $<<<< g1 &&& g2 &&& g3 &&& g4

    ($<<<<)             :: (c1 -> c2 -> c3 -> c4 -> a b d) -> a b (c1, (c2, (c3, c4))) -> a b d
    f $<<<< g           = applyA (g >>> arr4 f)

    -- | compute the parameter for an arrow @f@ with an extra parameter by an arrow @g@
    -- and apply all the results from @g@ sequentially to the input
    --
    -- > infixl 2 ($<$)
    --
    -- typical usage:
    --
    -- > g :: a b c
    -- > g = ...
    -- >
    -- > f :: c -> a b b
    -- > f x = ... x ...
    -- >
    -- > f $<$ g
    --
    -- @f@ computes the extra parameters for @g@ from the input of type @b@ and @g@ is applied with this
    -- parameter to the input. This allows programming in a point wise style in @g@, which becomes
    -- neccessary, when a value is needed more than once.
    --
    -- this combinator is useful, when transforming a single value (document) step by step,
    -- with @g@ for collecting the data for all steps, and @f@ for transforming the input step by step
    --
    -- if @g@ is deterministic (computes exactly one result),
    -- @ g $\<$ f == g $\< f @ holds
    --
    -- if @g@ fails, @ f $<$ g == this @
    --
    -- if @g@ computes more than one result, @f@ is applied sequentially to the input for every result from @g@
    --
    -- examples with simple list arrows with strings
    --
    -- > prefixString   :: String -> a String String
    -- > prefixString s =  arr (s++)
    -- >
    -- > runLA ( prefixString $<$ none                      ) "x" == ["x"]
    -- > runLA ( prefixString $<$ constA "y"                ) "x" == ["yx"]
    -- > runLA ( prefixString $<$ constA "y" <+> constA "z" ) "x" == ["zyx"]
    -- > runLA ( prefixString $<$ constA "y" <+> this
    -- >                          <+> constA "z"            ) "x" == ["zxyx"]
    --
    -- example with two extra parameter
    --
    -- > g1 :: a b c1
    -- > g2 :: a b c2
    -- >
    -- > f          :: (c1, c2) -> a b b
    -- > f (x1, x2) =  ... x1 ... x2 ...
    -- >
    -- > f $<$ g1 &&& g2
    --
    -- see also: 'applyA', '$<'

    ($<$)               :: (c -> (a b b)) -> a b c -> a b b
    g $<$ f             = applyA (listA (f >>> arr g) >>> arr seqA)

    -- | merge the result pairs of an arrow with type @a a1 (b1, b2)@
    -- by combining the tuple components with the @op@ arrow
    --
    -- examples with simple list arrows working on strings and XmlTrees
    --
    -- >     a1 :: a String (XmlTree, XmlTree)
    -- >     a1 = selem "foo" [this >>> mkText]
    -- >          &&&
    -- >          selem "bar" [arr (++"0") >>> mkText]
    -- >
    -- >     runLA (a1 >>> mergeA (<+>) >>> xshow this) "42" == ["<foo>42</foo>","<bar>420</bar>"]
    -- >     runLA (a1 >>> mergeA (+=)  >>> xshow this) "42" == ["<foo>42<bar>420</bar></foo>"]
    --
    -- see also: 'applyA', '$<' and '+=' in class 'Text.XML.HXT.Arrow.ArrowXml'

    mergeA              :: (a (a1, b1) a1 -> a (a1, b1) b1 -> a (a1, b1) c) ->
                           a (a1, b1) c
    mergeA op           = (\ x -> arr fst `op` constA (snd x)) $< this

    -- | useful only for arrows with side effects: perform applies an arrow to the input
    -- ignores the result and returns the input
    --
    -- example: @ ... >>> perform someTraceArrow >>> ... @

    perform             :: a b c -> a b b
    perform f           = listA f &&& this >>> arr snd

    -- | generalization of arrow combinator '<+>'
    --
    -- definition: @ catA = foldl (\<+\>) none @

    catA                :: [a b c] -> a b c
    catA                = foldl (<+>) none

    -- | generalization of arrow combinator '>>>'
    --
    -- definition: @ seqA = foldl (>>>) this @

    seqA                :: [a b b] -> a b b
    seqA                = foldl (>>>) this

-- ------------------------------------------------------------
