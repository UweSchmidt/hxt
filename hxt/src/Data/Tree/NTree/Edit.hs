-- ------------------------------------------------------------

{- |
   Module     : 
   Copyright  : Copyright (C) 2011 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Space and time efficient editing of rose trees
-}

-- ------------------------------------------------------------

module Data.Tree.NTree.Edit
where

import Data.Maybe

import Data.Tree.NTree.TypeDefs

-- import           Debug.Trace


-- | editNTreeBottomUp is a space optimized tree edit function
--
-- The nodes in a tree are visited bottom up. An edit function is applied to
-- all nodes. A Nothing result of the editing function indicates no changes.
-- This is used to share the input tree within the resulting tree.
--
-- The following law holds:
--
-- > editNTreeBottomUp (const Nothing) t == [t]
--
-- In this case the resulting tree does not only represent the same value
-- but it is the same machine value (relative to some evaluations of closures
-- during the tree walk
--
-- With a simple fold like editing function the whole tree would be reconstructed
-- in memory

editNTreeBottomUp               :: (NTree a -> Maybe [NTree a]) -> NTree a -> [NTree a]
editNTreeBottomUp f t0          = maybe [t0] id . editNTreeBU $ t0
    where

 -- editNTreeBU                 :: NTree a -> Maybe [NTree a]
    editNTreeBU t@(NTree n cs)
        | isNothing r'
          &&
          isJust cl'            = Just [t']                     -- children have been change but not the node itself
        | otherwise             = r'                            -- nothing has been changes or node has been changed 
        where
        cl'                     = editNTreesBU cs               -- the edited children
        t'                      = case cl' of                   -- the node to be processed with f
                                  Nothing       -> t            -- possibly with the new children (bottom up)
                                  Just cs'      -> NTree n cs'
        r'                      = f t'                          -- the edited result

 -- editNTreesBU                :: [NTree a] -> Maybe [NTree a]
    editNTreesBU []             = Nothing
    editNTreesBU (t : ts)       = mergeRes
                                  (editNTreeBU  t )
                                  (editNTreesBU ts)
        where
        mergeRes r'             = case r' of
                                  Nothing       -> maybe Nothing (Just . (t :))
                                  Just ts'      -> Just . (ts' ++) . fromMaybe ts


-- | A space optimized map for NTrees
--
-- Subtrees, that are not changed are reused in the resulting tree
-- See also: editNTreeBottomUp

mapNTree'                       :: (a -> Maybe a) -> NTree a -> NTree a
mapNTree' f t0                  = maybe t0 id . map' $ t0
    where

    -- map'                     :: NTree a -> Maybe (NTree a)
    map' (NTree n cs)           = mergeRes (f n) (maps' cs)
        where
        mergeRes Nothing Nothing        = Nothing
        mergeRes Nothing (Just cs')     = Just (NTree n             cs')
        mergeRes (Just n') cl           = Just (NTree n' (fromMaybe cs cl))

    -- maps'                    :: [NTree a] -> Maybe [NTree a]
    maps' []                    = Nothing
    maps' (t : ts)              = mergeRes
                                  (map'  t )
                                  (maps' ts)
        where
        mergeRes r'             = case r' of
                                  Nothing       -> maybe Nothing (Just . (t :))
                                  Just t'       -> Just . (t' :) . fromMaybe ts

-- eof ------------------------------------------------------------