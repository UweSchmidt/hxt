{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : Data.Tree.NTree.TypeDefs
   Copyright  : Copyright (C) 2005-2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Interface definition for trees

   n-ary tree structure (rose trees)

-}

-- ------------------------------------------------------------

module Data.Tree.NTree.TypeDefs
    ( module Data.Tree.Class
    , module Data.Tree.NTree.TypeDefs
    )
where

import Control.DeepSeq

import Data.Tree.Class
import Data.Typeable

-- | n-ary ordered tree (rose trees)
--
-- a tree consists of a node and a possible empty list of children.
-- If the list of children is empty, the node is a leaf, else it's
-- an inner node.
--
-- NTree implements Eq, Ord, Show and Read

data NTree  a	= NTree a (NTrees a)
    deriving
    (Eq, Ord, Show, Read, Typeable)

-- | shortcut for a sequence of n-ary trees

type NTrees   a	= [NTree a]

-- ------------------------------------------------------------

instance (NFData a) => NFData (NTree a) where
    rnf (NTree n cl)			= rnf n `seq` rnf cl

-- | NTree implements class Functor

instance Functor NTree where
    fmap f ~(NTree n cl)		= NTree (f n) (map (fmap f) cl)


-- | Implementation of "Data.Tree.Class" interface for rose trees

instance Tree NTree where
    mkTree n cl				= NTree n cl

    getNode           ~(NTree n _ )	= n
    getChildren       ~(NTree _ cl)	= cl

    changeNode     cf ~(NTree n cl)	= NTree (cf n) cl
    changeChildren cf ~(NTree n cl)	= NTree n (cf cl)

    foldTree        f ~(NTree n cs)	= f n (map (foldTree f) cs)

    nodesTree		= foldTree (\ n rs -> n : concat rs)
    depthTree		= foldTree (\ _ rs -> 1 + maximum (0 : rs))
    cardTree		= foldTree (\ _ rs -> 1 + sum rs)
    formatTree nf n	= formatNTreeF nf (showString "---") (showString "   ") n ""

-- ------------------------------------------------------------
-- |
-- convert a tree into a pseudo graphical string reprsentation

formatNTreeF	:: (node -> String) -> (String -> String) -> (String -> String) -> NTree node -> String -> String

formatNTreeF node2String pf1 pf2 (NTree n l)
    = formatNode
      . formatChildren pf2 l
    where
    formatNode	= pf1 . foldr (.) id (map trNL (node2String n)) . showNL
    trNL '\n'	= showNL . pf2
    trNL c	= showChar c
    showNL	= showChar '\n'
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
	pf0'	= pf . showString indent1
	pf1'	= pf . showString indent2
	pf2'	= pf . showString indent3
	pfl'	= pf . showString indent4
	formatTr	= formatNTreeF node2String pf0'
	indent1 = "+---"
	indent2 = "|   "
	indent3 = "    "
	indent4 = "|\n"

-- eof ------------------------------------------------------------
