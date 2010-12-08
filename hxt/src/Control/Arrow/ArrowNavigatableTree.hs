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

import           Control.Arrow
import           Control.Arrow.ArrowList
import           Control.Arrow.ArrowIf

import           Data.Maybe

import           Data.Tree.NavigatableTree.Class        ( NavigatableTree
                                                        , NavigatableTreeToTree
                                                        , NavigatableTreeModify
                                                        )
import qualified Data.Tree.NavigatableTree.Class        as T
import qualified Data.Tree.NavigatableTree.XPathAxis    as T

-- ------------------------------------------------------------

-- | The interface for navigatable tree arrows
--
-- all functions have default implementations

class (ArrowList a) => ArrowNavigatableTree a where

    -- move one step towards the root
    moveUp              :: NavigatableTree t => a (t b) (t b)
    moveUp              = arrL $ maybeToList . T.mvUp

    -- descend one step to the leftmost child
    moveDown            :: NavigatableTree t => a (t b) (t b)
    moveDown            = arrL $ maybeToList . T.mvDown

    -- move to the left neighbour
    moveLeft            :: NavigatableTree t => a (t b) (t b)
    moveLeft            = arrL $ maybeToList . T.mvLeft

    -- move to the right neighbour
    moveRight           :: NavigatableTree t => a (t b) (t b)
    moveRight           = arrL $ maybeToList . T.mvRight

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

addNav                  :: ( ArrowList a
                           , NavigatableTreeToTree nt t
                           ) =>
                           a (t b) (nt b)
addNav                  = arr T.fromTree


-- | Conversion from a navigatable tree into an ordinary tree

remNav                  :: ( ArrowList a
                           , NavigatableTreeToTree nt t
                           ) =>
                           a (nt b) (t b)
remNav                  = arr T.toTree

-- | apply an operation using navigation to an ordinary tree
--
-- This root and all children may be visited in arbitrary order

withNav                 :: ( ArrowList a
                           , NavigatableTreeToTree nt t
                           ) =>
                           a (nt b) (nt c) -> a (t b) (t c)
withNav f               = addNav >>> f >>> remNav


-- | apply a simple operation without use of navigation to a navigatable tree
--
-- This enables to apply arbitrary tree operations to navigatable trees

withoutNav              :: ( ArrowList a
                           , NavigatableTreeToTree nt t
                           , NavigatableTreeModify nt t
                           ) =>
                           a (t b) (t b) -> a (nt b) (nt b)
withoutNav f            = ( (remNav >>> f)                      -- apply the simple arrow to the tree
                            &&&
                            this                                -- remember the navigation context
                          )
                          >>> arr (uncurry T.substThisTree)             -- resore the context
                             
-- ------------------------------------------------------------

-- | Filter an axis with an ordinary tree predicate
--
-- Example: In a tree of Ints find all nodes in the subtrees (in preorder) that have label 42
--
-- > descendantAxis >>> filterAxis (hasNode (== 42))
--
-- Example: In an XML Tree find the following nodes of a node with attribute id and value 42
--
-- > descendantAxis >>> filterAxis (hasAttrValue "id" (=="42")) >>> followingAxis

filterAxis              :: ( ArrowIf a
                           , NavigatableTreeToTree nt t
                           ) =>
                           a (t b) c -> a (nt b) (nt b)

filterAxis p            = (remNav >>> p) `guards` this
{-# INLINE filterAxis #-}


-- | Move to the next tree on a given axis. Deterministic arrow
--
-- Example: Move to the next node in a preorder visit: next child or else next following
--
-- > moveOn descendantOrFollowingAxis

moveOn                  :: ( ArrowList a
                           , NavigatableTree t
                           ) =>
                           a (t b) (t b) -> a (t b) (t b)
moveOn axis             = single $ axis
{-# INLINE moveOn #-}

-- ------------------------------------------------------------

-- | Change the current subtree of a navigatable tree.
--
-- The arrow for computing the changes should be deterministic. If it fails
-- nothing is changed.

changeThisTree          :: ( ArrowList a
                           , ArrowIf a
                           , NavigatableTreeToTree nt t
                           , NavigatableTreeModify nt t
                           ) =>
                           a (t b) (t b) -> a (nt b) (nt b)
changeThisTree cf       = withoutNav $ single cf `orElse` this

-- | Substitute the current subtree of a navigatable tree by a given tree

substThisTree           :: ( ArrowList a
                           , ArrowIf a
                           , NavigatableTreeToTree nt t
                           , NavigatableTreeModify nt t
                           ) =>
                           t b -> a (nt b) (nt b)
substThisTree t         = changeThisTree (constA t)

-- ------------------------------------------------------------

-- | apply an ordinary arrow to the current subtree of a navigatabe tree and add the result trees in front of the current tree.
--
-- If this arrow is applied to the root, it will fail, because we want a tree as result, not a forest.

addToTheLeft            :: ( ArrowList a
                           , NavigatableTreeToTree nt t
                           , NavigatableTreeModify nt t
                           ) =>
                           a (t b) (t b) -> a (nt b) (nt b)
addToTheLeft            = addToOneSide $
                          foldl (\ acc t -> acc >>= T.addTreeLeft t)
{-# INLINE addToTheLeft #-}

-- | apply an ordinary arrow to the current subtree of a navigatabe tree and add the result trees behind the current tree.
--
-- If this arrow is applied to the root, it will fail, because we want a tree as result, not a forest.

addToTheRight           :: ( ArrowList a
                           , NavigatableTreeToTree nt t
                           , NavigatableTreeModify nt t
                           ) =>
                           a (t b) (t b) -> a (nt b) (nt b)
addToTheRight           = addToOneSide $
                          foldr (\ t acc -> acc >>= T.addTreeRight t)
{-# INLINE addToTheRight #-}


-- | addToOneSide does the real work for 'addToTheLeft' and 'addToTheRight'

addToOneSide            :: ( ArrowList a
                           , NavigatableTreeToTree nt t
                           , NavigatableTreeModify nt t
                           ) =>
                           ( Maybe (nt b) -> [t b] -> Maybe (nt b) ) ->
                           a (t  b) (t  b) ->
                           a (nt b) (nt b)
addToOneSide side f     = ( ( remNav >>> listA f )
                            &&&
                            this
                          )
                          >>>
                          arrL ( uncurry (\ ts nt -> side (Just nt) ts)
                                 >>>
                                 maybeToList
                               )

-- ------------------------------------------------------------

-- | drop the direct left sibling tree of the given navigatable tree
--
-- If this arrow is applied to the root or a leftmost tree, it will fail, because there is nothing to remove

dropFromTheLeft         :: ( ArrowList a
                           -- , NavigatableTreeToTree nt t
                           , NavigatableTreeModify nt t
                           ) =>
                           a (nt b) (nt b)
dropFromTheLeft            = arrL $ T.dropTreeLeft >>> maybeToList
{-# INLINE dropFromTheLeft #-}

-- | drop the direct left sibling tree of the given navigatable tree
--
-- If this arrow is applied to the root or a rightmost tree, it will fail, because there is nothing to remove

dropFromTheRight        :: ( ArrowList a
                           , NavigatableTreeModify nt t
                           ) =>
                           a (nt b) (nt b)
dropFromTheRight            = arrL $ T.dropTreeRight >>> maybeToList
{-# INLINE dropFromTheRight #-}

-- ------------------------------------------------------------

