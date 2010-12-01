-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowNavigatableTree
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   List arrows for navigatable trees

   Trees that implement the "Data.Tree.NavigatableTree.Class" interface, can be processed
   with these arrows.
-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowNavigatableTree
where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf

import           Data.Maybe

import           Data.Tree.NavigatableTree.Class        ( NavigatableTree
                                                        , TreeToNavigatableTree
                                                        )
import qualified Data.Tree.NavigatableTree.Class        as T
import qualified Data.Tree.NavigatableTree.XPathAxis    as T

-- ------------------------------------------------------------

-- | The interface for navigatable tree arrows
--
-- all functions have default implementations

class (ArrowList a) => ArrowNavigatableTree a where

    -- move one step towards the root
    moveUp		:: NavigatableTree t => a (t b) (t b)
    moveUp		= arrL $ maybeToList . T.mvUp

    -- descend one step to the leftmost child
    moveDown		:: NavigatableTree t => a (t b) (t b)
    moveDown		= arrL $ maybeToList . T.mvDown

    -- move to the left neighbour
    moveLeft		:: NavigatableTree t => a (t b) (t b)
    moveLeft		= arrL $ maybeToList . T.mvLeft

    -- move to the right neighbour
    moveRight		:: NavigatableTree t => a (t b) (t b)
    moveRight		= arrL $ maybeToList . T.mvRight

-- derived functions

parentAxis              :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
parentAxis              = arrL T.parentAxis

-- | XPath axis: ancestor

ancestorAxis            :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
ancestorAxis            = arrL T.ancestorAxis

-- | XPath axis: ancestor or self

ancestorOrSelfAxis      :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
ancestorOrSelfAxis      = arrL T.ancestorOrSelfAxis

-- | XPath axis: child

childAxis               :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
childAxis               = arrL T.childAxis

-- | XPath axis: descendant

descendantAxis          :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
descendantAxis          = arrL T.descendantAxis

-- | XPath axis: descendant or self

descendantOrSelfAxis    :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
descendantOrSelfAxis    = arrL T.descendantOrSelfAxis

-- | not an XPath axis but useful: descendant or following

descendantOrFollowingAxis    :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
descendantOrFollowingAxis    = descendantAxis <+> followingAxis

-- | not an official XPath axis but useful: reverse descendant or self, used in preceding axis

revDescendantOrSelfAxis :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
revDescendantOrSelfAxis = arrL T.revDescendantOrSelfAxis

-- | XPath axis: following sibling

followingSiblingAxis    :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
followingSiblingAxis    = arrL T.followingSiblingAxis

-- | XPath axis: preceeding sibling

precedingSiblingAxis    :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
precedingSiblingAxis    = arrL T.precedingSiblingAxis

-- | XPath axis: self

selfAxis                :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
selfAxis                = arrL T.selfAxis

-- | XPath axis: following

followingAxis           :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
followingAxis           = arrL T.followingAxis

-- | XPath axis: preceding

precedingAxis           :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
precedingAxis           = arrL T.precedingAxis

-- ------------------------------------------------------------

-- | move to the root

moveToRoot              :: (Arrow a, NavigatableTree t) => a (t b) (t b)
moveToRoot              = arr T.mvToRoot

isAtRoot                :: (ArrowList a, NavigatableTree t) => a (t b) (t b)
isAtRoot                = isA (null . T.ancestorAxis)

-- ------------------------------------------------------------

-- | Conversion from a tree into a navigatable tree

fromTree                :: ( ArrowList a
                           , TreeToNavigatableTree t nt
                           ) =>
                           a (t b) (nt b)
fromTree                = arr T.fromTree


-- | Conversion from a navigatable tree into an ordinary tree

toTree                  :: ( ArrowList a
                           , TreeToNavigatableTree t nt
                           ) =>
                           a (nt b) (t b)
toTree                  = arr T.toTree

-- | apply an operation using navigation to an ordinary tree

withNavTree             :: ( ArrowList a
                           , TreeToNavigatableTree t  nt
                           ) =>
                           a (nt b) (nt c) -> a (t b) (t c)
withNavTree f           = fromTree >>> f >>> toTree

-- ------------------------------------------------------------

moveUntil               :: (ArrowIf a) =>
                           a b b1 -> a b1 c -> a b b1
moveUntil axis p        = single ( axis >>> (p `guards` this) )

-- ------------------------------------------------------------
