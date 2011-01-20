-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.NTreeEdit
   Copyright  : Copyright (C) 2011 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   arrows for efficient editing of rose trees

-}

-- ------------------------------------------------------------

module Control.Arrow.NTreeEdit
where

import Control.Arrow
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowList
import Control.Arrow.ListArrow

import Data.Maybe
import Data.Tree.NTree.TypeDefs
import Data.Tree.NTree.Edit

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
