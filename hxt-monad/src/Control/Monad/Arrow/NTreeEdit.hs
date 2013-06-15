-- ------------------------------------------------------------

{- |
   arrows for efficient editing of rose trees
-}

-- ------------------------------------------------------------

module Control.Monad.Arrow.NTreeEdit
where

import           Control.Monad.Arrow.ArrowIf
import           Control.Monad.Arrow.ArrowList

import           Data.Maybe
import           Data.Sequence.ArrowTypes      (LA, runLA)

import           Data.Tree.NTree.Edit
import           Data.Tree.NTree.TypeDefs

-- ------------------------------------------------------------

-- | Edit parts of a rose tree
--
-- The subtrees to be modified are selected by the first part of the IfThen pairs
-- The modification by the second part

editNTreeA              :: [IfThen (LA (NTree b) c) (LA (NTree b) (NTree b))] ->
                           LA (NTree b) (NTree b)
editNTreeA cs           = arrL $ editNTreeBottomUp ef
    where
    ef                  = listToMaybe . (runLA . foldr (\ (g :-> h) -> ifA g (listA h)) none $ cs)

fmapNTreeA              :: (b -> Maybe b) -> LA (NTree b) (NTree b)
fmapNTreeA f            = return . mapNTree' f

-- eof ------------------------------------------------------------
