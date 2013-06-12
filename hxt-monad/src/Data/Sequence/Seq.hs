-- ----------------------------------------

module Data.Sequence.Seq
    ( Seq )
where

-- select the sequence implementation from the
-- 4 choices: ordinary lists, list with failure,
-- binary trees with labels at the leafs, and
-- binary trees with failure
--
-- comment out the preferred implementation

import Data.Sequence.Impl.List            ()
import Data.Sequence.Impl.ListWithFailure ()
import Data.Sequence.Impl.Tree            ()
import Data.Sequence.Impl.TreeWithFailure -- ()

-- ----------------------------------------


