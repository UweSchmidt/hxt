-- ------------------------------------------------------------

{- |
   Module     : Data.Tree.NavigatableTree.Class
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Interface definition for navigatable trees.
   Navigatable trees need to have operations to move up, down, left and right.
   With these elementary operations, most of the XPath axises can be defined.
-}

-- ------------------------------------------------------------

module Data.Tree.NavigatableTree.Class
where

-- ------------------------------------------------------------

-- | The interface for navigatable trees

class NavigatableTree t where
    -- move one step towards the root
    mvUp		:: t a -> Maybe (t a)

    -- descend one step to the leftmost child
    mvDown		:: t a -> Maybe (t a)

    -- move to the left neighbour
    mvLeft		:: t a -> Maybe (t a)

    -- move to the right neighbour
    mvRight		:: t a -> Maybe (t a)

-- ------------------------------------------------------------

-- | Conversion between trees and navigatable trees,
--
-- There is only a single navigatable tree implementation for a given tree allowed
-- (see the functional dependencies)

class TreeToNavigatableTree t nt | t -> nt, nt -> t where
    -- construct a navigatable tree
    fromTree            :: t a -> nt a

    -- remove navigation
    toTree              :: nt a -> t a

    -- change the tree but remain the navigation
    substTree            :: t a -> nt a -> nt a

-- ------------------------------------------------------------
