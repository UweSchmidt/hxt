-- |
-- Navigable tree structure which allow a program to traverse
-- for XPath expressions
-- copied and modified from HXML (<http://www.flightlab.com/~joe/hxml/>)
--

module Text.XML.HXT.XPath.NavTree
    ( module Text.XML.HXT.XPath.NavTree
    , module Data.Tree.NTree.TypeDefs
    )
where

import Data.Maybe
import Data.Tree.NTree.TypeDefs

import Text.XML.HXT.DOM.Interface       ( XNode
                                        , xmlnsNamespace
                                        , namespaceUri
                                        )
import Text.XML.HXT.DOM.XmlNode         ( isRoot
                                        , isElem
                                        , getName
                                        , getAttrl
                                        )

-- -----------------------------------------------------------------------------

-- NavTree
--
-- | navigable tree with nodes of type node
--
-- a navigable tree consists of a n-ary tree for the current fragment tree,
-- a navigable tree for all ancestors, and two n-ary trees for
-- the previous- and following siblings

data NavTree a  = NT { self                     :: (NTree a)
                     , selfIndex                :: Int
                     , ancestors                :: [NavTree a]
                     , previousSiblings         :: [NTree a]
                     , followingSiblings        :: [NTree a]
                     }
                  deriving (Show)               -- deriving not reasonable for Eq and Ord

-- -----------------------------------------------------------------------------


-- |
-- converts a n-ary tree in a navigable tree

ntree                           :: NTree a -> NavTree a
ntree nd                        = NT nd (-1) [] [] []

-- |
-- converts a navigable tree in a n-ary tree

subtreeNT                       :: NavTree a -> NTree a
subtreeNT (NT nd _ _ _ _)       = nd


-- |
-- function for selecting the value of the current fragment tree

dataNT                          :: NavTree a -> a
dataNT (NT (NTree a _) _ _ _ _) = a

-- |
-- function for selecting all children of a tree

childrenNT                      :: NavTree a -> [NTree a]
childrenNT (NT (NTree _ cs) _ _ _ _)
                                = cs

-- |
-- position of tree in parent

indexNT                         :: NavTree a -> Int
indexNT (NT _ ix _ _ _)         = ix

-- |
-- path (index list) of a navigatable tree

pathNT                          :: NavTree a -> [Int]
pathNT                          = tail . reverse . map selfIndex . ancestorOrSelfAxis

-- -----------------------------------------------------------------------------

-- functions for traversing up, down, left and right in a navigable tree

upNT
 , downNT
 , leftNT
 , rightNT                              :: NavTree a -> Maybe (NavTree a)

upNT      (NT _ _ (p:_) _ _)            = Just p
upNT      (NT _ _ [] _ _)               = Nothing
downNT  t@(NT (NTree _ (c:cs)) _ u _ _) = Just (NT c 0 (t:u) [] cs)
downNT    (NT (NTree _ []    ) _ _ _ _) = Nothing
leftNT    (NT s ix u (l:ls) r)          = Just (NT l (ix - 1) u ls (s:r))
leftNT    (NT _ _  _ []     _)          = Nothing
rightNT   (NT s ix u l (r:rs))          = Just (NT r (ix + 1) u (s:l) rs)
rightNT   (NT _ _  _ _ []    )          = Nothing


-- preorderNT t = t : concatMap preorderNT (children t)
-- where children = maybe [] (maybeStar rightNT) . downNT

preorderNT              :: NavTree a -> [NavTree a]
preorderNT              = visit []
    where
    visit  k t          = t : maybe k (visit' k) (downNT t)
    visit' k t          = visit (maybe k (visit' k) (rightNT t)) t

revPreorderNT           :: NavTree a -> [NavTree a]
revPreorderNT t         = t : concatMap revPreorderNT (reverse (children t))
    where
    children            = maybe [] (maybeStar rightNT) . downNT

getChildrenNT           :: NavTree a -> [NavTree a]
getChildrenNT node      = maybe [] follow (downNT node)
    where
    follow n            = n : maybe [] follow (rightNT n)

-- -----------------------------------------------------------------------------
-- Miscellaneous useful combinators

-- |
-- Kleisli composition:

o'                      :: (b -> [c]) -> (a -> [b]) -> (a -> [c])
f `o'` g                = \x -> g x >>= f

-- Some useful anamorphisms:

maybeStar, maybePlus    :: (a -> Maybe a) -> a -> [a]
maybeStar f a           = a : maybe [] (maybeStar f) (f a)
maybePlus f a           =     maybe [] (maybeStar f) (f a)

-- -----------------------------------------------------------------------------
-- functions for representing XPath axes. All axes except the namespace-axis are supported


parentAxis              :: NavTree a -> [NavTree a]
parentAxis              = maybeToList . upNT

ancestorAxis            :: NavTree a -> [NavTree a]
ancestorAxis            = ancestors                     -- or: maybePlus upNT

ancestorOrSelfAxis      :: NavTree a -> [NavTree a]
ancestorOrSelfAxis t    = t : ancestors t               -- or: maybeStar upNT

childAxis               :: NavTree a -> [NavTree a]
childAxis               = maybe [] (maybeStar rightNT) . downNT

descendantAxis          :: NavTree a -> [NavTree a]
descendantAxis          = tail . preorderNT             -- concatMap preorderNT . childAxis

descendantOrSelfAxis    :: NavTree a -> [NavTree a]
descendantOrSelfAxis    = preorderNT

followingSiblingAxis    :: NavTree a -> [NavTree a]
followingSiblingAxis    = maybePlus rightNT

precedingSiblingAxis    :: NavTree a -> [NavTree a]
precedingSiblingAxis    = maybePlus leftNT

selfAxis                :: NavTree a -> [NavTree a]
selfAxis                = (:[])

followingAxis           :: NavTree a -> [NavTree a]
followingAxis           = preorderNT     `o'` followingSiblingAxis `o'` ancestorOrSelfAxis

precedingAxis           :: NavTree a -> [NavTree a]
precedingAxis           = revPreorderNT  `o'` precedingSiblingAxis `o'` ancestorOrSelfAxis


attributeAxis           :: NavTree XNode -> [NavTree XNode]
attributeAxis t@(NT xt _ a _ _)
    | isElem xt
      &&
      not (isRoot xt)   = foldr (\ (ix, attr) -> ((NT attr ix (t:a) [] []):)) [] al
    | otherwise         = []
    where
    aix xs              = zip [(0 - length xs) .. (-1)] xs
    al                  = filter ((/= xmlnsNamespace) . maybe "" namespaceUri . getName . snd)
                          . aix
                          . fromMaybe []
                          . getAttrl $ xt

-- attributes are indexed in the path with negative indices
-- this corresponds to document order and makes the index paths
-- for attributes and children disjoint.
-- The attribute index is never referenced when navigating in trees

-- ------------------------------------------------------------
