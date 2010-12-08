-- ------------------------------------------------------------

{- |
   Module     : Data.Tree.NavigatableTree.XPathAxis
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Navigatable trees need to have operations to move up, down, left and right.
   With these elementary operations, the XPath axises can be defined.
-}

-- ------------------------------------------------------------

module Data.Tree.NavigatableTree.XPathAxis
where

import Data.Maybe               ( maybeToList )
import Data.Tree.NavigatableTree.Class

import Control.Arrow            ( (>>>) )
import Control.Monad            ( (>=>) )

-- ------------------------------------------------------------
--
-- mothers little helpers

-- | collect all trees by moving into one direction, starting tree is included

maybeStar               :: (a -> Maybe a) -> (a -> [a])
maybeStar f x            = x : maybe [] (maybeStar f) (f x)

-- | collect all trees by moving into one direction, starting tree is not included

maybePlus               :: (a -> Maybe a) -> (a -> [a])
maybePlus f x           =      maybe [] (maybeStar f) (f x)

{-# INLINE maybePlus #-}

-- ------------------------------------------------------------
-- XPath axis

-- | XPath axis: parent

parentAxis              :: NavigatableTree t => t a -> [t a]
parentAxis              = maybeToList . mvUp
{-# INLINE parentAxis #-}

-- | XPath axis: ancestor

ancestorAxis            :: NavigatableTree t => t a -> [t a]
ancestorAxis            = maybePlus mvUp
{-# INLINE ancestorAxis #-}

-- | XPath axis: ancestor or self

ancestorOrSelfAxis      :: NavigatableTree t => t a -> [t a]
ancestorOrSelfAxis      = maybeStar mvUp
{-# INLINE ancestorOrSelfAxis #-}

-- | XPath axis: child

childAxis               :: NavigatableTree t => t a -> [t a]
childAxis               = (mvDown >>> maybeToList) >=> maybeStar mvRight
{-# INLINE childAxis #-}

-- | XPath axis: descendant

descendantAxis          :: NavigatableTree t => t a -> [t a]
descendantAxis          = descendantOrSelfAxis >>> tail
{-# INLINE descendantAxis #-}

-- | XPath axis: descendant or self

descendantOrSelfAxis    :: NavigatableTree t => t a -> [t a]
descendantOrSelfAxis    = visit []
    where
    visit  k t          = t : maybe k (visit' k) (mvDown t)
    visit' k t          = visit (maybe k (visit' k) (mvRight t)) t

-- | not an official XPath axis but useful: reverse descendant or self, used in preceding axis

revDescendantOrSelfAxis :: NavigatableTree t => t a -> [t a]
revDescendantOrSelfAxis t
                        = t : concatMap revDescendantOrSelfAxis (reverse $ childAxis t)

-- | XPath axis: following sibling

followingSiblingAxis    :: NavigatableTree t => t a -> [t a]
followingSiblingAxis    = maybePlus mvRight
{-# INLINE followingSiblingAxis #-}

-- | XPath axis: preceeding sibling

precedingSiblingAxis    :: NavigatableTree t => t a -> [t a]
precedingSiblingAxis    = maybePlus mvLeft
{-# INLINE precedingSiblingAxis #-}

-- | XPath axis: self

selfAxis                :: NavigatableTree t => t a -> [t a]
selfAxis                = (:[])
{-# INLINE selfAxis #-}

-- | XPath axis: following

followingAxis           :: NavigatableTree t => t a -> [t a]
followingAxis           = ancestorOrSelfAxis >=> followingSiblingAxis >=> descendantOrSelfAxis

-- | XPath axis: preceding

precedingAxis           :: NavigatableTree t => t a -> [t a]
precedingAxis           = ancestorOrSelfAxis >=> precedingSiblingAxis >=> revDescendantOrSelfAxis

-- | move to the root

mvToRoot                :: NavigatableTree t => t a -> t a
mvToRoot                = ancestorOrSelfAxis >>> last
{-# INLINE mvToRoot #-}

isAtRoot                :: NavigatableTree t => t a -> Bool
isAtRoot                = null . ancestorAxis
{-# INLINE isAtRoot #-}

-- ------------------------------------------------------------
