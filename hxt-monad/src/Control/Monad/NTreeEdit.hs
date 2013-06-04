-- ------------------------------------------------------------

{- |
   arrows for efficient editing of rose trees
-}

-- ------------------------------------------------------------

module Control.Monad.NTreeEdit
where

import           Control.Monad.Arrow
import           Control.Monad.ArrowIf
import           Control.Monad.ArrowList

import           Data.Maybe
import           Data.Sequence.Types      (LA, runLA)
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
fmapNTreeA f            = arr $ mapNTree' f

-- eof ------------------------------------------------------------
