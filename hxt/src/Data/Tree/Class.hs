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
    mkTree              ::              a -> [t a] -> t a

    -- | leaf construction: leafs don't have any children
    --
    -- definition: @ mkLeaf n = mkTree n [] @

    mkLeaf              ::                       a -> t a
    mkLeaf n            = mkTree n []
    {-# INLINE mkLeaf #-}

    -- | leaf test: list of children empty?

    isLeaf              ::                     t a -> Bool
    isLeaf              = null . getChildren
    {-# INLINE isLeaf #-}

    -- | innner node test: @ not . isLeaf @

    isInner             ::                     t a -> Bool
    isInner             = not . isLeaf
    {-# INLINE isInner #-}

    -- | select node attribute

    getNode             ::                     t a -> a

    -- | select children

    getChildren         ::                     t a -> [t a]

    -- | edit node attribute

    changeNode          ::         (a -> a) -> t a -> t a

    -- | edit children

    changeChildren      :: ([t a] -> [t a]) -> t a -> t a

    -- | substitute node: @ setNode n = changeNode (const n) @

    setNode             ::                a -> t a -> t a
    setNode n           = changeNode (const n)
    {-# INLINE setNode #-}

    -- | substitute children: @ setChildren cl = changeChildren (const cl) @

    setChildren         ::            [t a] -> t a -> t a
    setChildren cl      = changeChildren (const cl)
    {-# INLINE setChildren #-}

    -- | fold for trees

    foldTree            ::  (a -> [b] -> b) -> t a -> b

    -- | all nodes of a tree

    nodesTree           ::                     t a -> [a]
    nodesTree           = foldTree (\ n rs -> n : concat rs)
    {-# INLINE nodesTree #-}

    -- | depth of a tree

    depthTree           ::                     t a -> Int
    depthTree           = foldTree (\ _ rs -> 1 + maximum (0 : rs))

    -- | number of nodes in a tree

    cardTree            ::                     t a -> Int
    cardTree            = foldTree (\ _ rs -> 1 + sum rs)

    -- | format tree for readable trace output
    --
    -- a /graphical/ representation of the tree in text format

    formatTree          ::    (a -> String) -> t a -> String
    formatTree nf n     = formatNTree' nf (showString "---") (showString "   ") n ""

-- ------------------------------------------------------------
-- |
-- convert a tree into a pseudo graphical string representation

formatNTree'    :: Tree t => (a -> String) -> (String -> String) -> (String -> String) -> t a -> String -> String

formatNTree' node2String pf1 pf2 tree
    = formatNode
      . formatChildren pf2 l
    where
    n           = getNode     tree
    l           = getChildren tree
    formatNode  = pf1 . foldr (.) id (map trNL (node2String n)) . showNL
    trNL '\n'   = showNL . pf2
    trNL c      = showChar c
    showNL      = showChar '\n'
    formatChildren _ []
        = id
    formatChildren pf (t:ts)
        | null ts
            = pfl'
              . formatTr pf2' t
        | otherwise
            = pfl'
              . formatTr pf1' t
              . formatChildren pf ts
        where
        pf0'    = pf . showString indent1
        pf1'    = pf . showString indent2
        pf2'    = pf . showString indent3
        pfl'    = pf . showString indent4
        formatTr        = formatNTree' node2String pf0'
        indent1 = "+---"
        indent2 = "|   "
        indent3 = "    "
        indent4 = "|\n"

-- eof ------------------------------------------------------------
