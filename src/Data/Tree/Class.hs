-- ------------------------------------------------------------

{- |
   Module     : Data.Tree.Class
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Interface definition for trees
-}

-- ------------------------------------------------------------

module Data.Tree.Class
    ( module Data.Tree.Class )
where

-- | The interface for trees

class Tree t where
    -- | tree construction: a new tree is constructed by a node attribute and a list of children
    mkTree		::              a -> [t a] -> t a

    -- | leaf construction: leafs don't have any children
    --
    -- definition: @ mkLeaf n = mkTree n [] @

    mkLeaf		::                       a -> t a
    mkLeaf n		= mkTree n []

    -- | leaf test: list of children empty?

    isLeaf		::		       t a -> Bool
    isLeaf		= null . getChildren

    -- | innner node test: @ not . isLeaf @

    isInner		::		       t a -> Bool
    isInner		= not . isLeaf

    -- | select node attribute

    getNode		::                     t a -> a

    -- | select children

    getChildren		::                     t a -> [t a]

    -- | edit node attribute

    changeNode		::         (a -> a) -> t a -> t a

    -- | edit children

    changeChildren	:: ([t a] -> [t a]) -> t a -> t a

    -- | substitute node: @ setNode n = changeNode (const n) @

    setNode		::                a -> t a -> t a
    setNode n		= changeNode (const n)

    -- | substitute children: @ setChildren cl = changeChildren (const cl) @

    setChildren	::            [t a] -> t a -> t a
    setChildren cl	= changeChildren (const cl)

    -- | fold for trees

    foldTree		::  (a -> [b] -> b) -> t a -> b

    -- | all nodes of a tree

    nodesTree		::		       t a -> [a]

    -- | depth of a tree

    depthTree		::		       t a -> Int

    -- | number of nodes in a tree

    cardTree		::                     t a -> Int

    -- | format tree for readable trace output
    --
    -- a /graphical/ representation of the tree in text format

    formatTree		::    (a -> String) -> t a -> String

-- ------------------------------------------------------------
