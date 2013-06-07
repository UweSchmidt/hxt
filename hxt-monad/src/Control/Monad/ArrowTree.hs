{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- ------------------------------------------------------------

{- |
   Monadic sequence arrows for tree processing.

   Trees that implement the "Data.Tree.Class" interface, can be processed
   with these arrows.
-}

-- ------------------------------------------------------------

module Control.Monad.ArrowTree
    ( module Control.Monad.ArrowTree
    , Tree
    )
where

import           Control.Monad.Arrow
import           Control.Monad.ArrowIf
import           Control.Monad.ArrowList
import           Control.Monad.MonadSequence

import           Data.Tree.Class             (Tree)
import qualified Data.Tree.Class             as T


infixl 5 />, //>, </

-- ------------------------------------------------------------

-- | monadic version of constructing a leaf

newLeaf :: (Tree t, Monad m) => b -> m (t b)
newLeaf = return . T.mkLeaf
{-# INLINE newLeaf #-}

-- | monadic version of constructing an inner node

newTree :: (Tree t, Monad m) => b -> [t b] -> m (t b)
newTree n = return . T.mkTree n
{-# INLINE newTree #-}

-- | arrow version of constructing a leaf

mkLeaf :: (Tree t, Monad m) => b -> c -> m (t b)
mkLeaf = constA . T.mkLeaf
{-# INLINE mkLeaf #-}

-- | arrow version of constructing an inner node

mkTree :: (Tree t, Monad m) => b -> [t b] -> c -> m (t b)
mkTree n = constA . T.mkTree n
{-# INLINE mkTree #-}

    -- | select the children of the root of a tree

getChildren         :: (Tree t, MonadList s m) => (t b) -> m (t b)
getChildren         = arrL T.getChildren
{-# INLINE getChildren #-}

-- | select the node info of the root of a tree

getNode             :: (Tree t, MonadList s m) => t b -> m b
getNode             = arr T.getNode
{-# INLINE getNode #-}

-- | select the attribute of the root of a tree

hasNode :: (Tree t, MonadList s m) => (b -> Bool) -> t b -> m (t b)
hasNode p = (getNode >>> isA p) `guards` this
{-# INLINE hasNode #-}


-- | substitute the children of the root of a tree

setChildren :: (Tree t, Monad m) => [t b] -> t b -> m (t b)
setChildren cs = arr (T.setChildren cs)
{-# INLINE setChildren #-}

-- | substitute the attribute of the root of a tree

setNode :: (Tree t, Monad m) => b -> t b -> m (t b)
setNode n = arr (T.setNode n)
{-# INLINE setNode #-}

-- | edit the children of the root of a tree

changeChildren :: (Tree t, Monad m) => ([t b] -> [t b]) -> t b -> m (t b)
changeChildren csf  = arr (T.changeChildren csf)
{-# INLINE changeChildren #-}

-- | edit the attribute of the root of a tree

changeNode :: (Tree t, Monad m) => (b -> b) -> t b -> m (t b)
changeNode nf = arr (T.changeNode nf)
{-# INLINE changeNode #-}

-- | apply an arrow element wise to all children of the root of a tree
-- collect these results and substitute the children with this result
--
-- example: @ processChildren isText @ deletes all subtrees, for which isText does not hold
--
-- example: @ processChildren (none \`when\` isCmt) @ removes all children, for which isCmt holds

processChildren :: (Tree t, MonadList s m) => (t b -> m (t b)) -> (t b -> m (t b))
processChildren f
    = arr T.getNode
      &&&
      listA (arrL T.getChildren >>> f)      -- compute new children
      >>>
      arr2 T.mkTree

-- | similar to processChildren, but the new children are computed by processing
-- the whole input tree
--
-- example: @ replaceChildren (deep isText) @ selects all subtrees for which isText holds
-- and substitutes the children component of the root node with this list

replaceChildren :: (Tree t, MonadList s m) => (t b -> m (t b)) -> (t b -> m (t b))
replaceChildren f
    = arr T.getNode
      &&&
      listA f                               -- compute new children
      >>>
      arr2 T.mkTree

-- |
-- pronounced \"slash\", meaning g inside f
--
-- defined as @ f \/> g = f >>> getChildren >>> g @
--
-- example: @ hasName \"html\" \/> hasName \"body\" \/> hasName \"h1\" @
--
-- This expression selects
-- all \"h1\" elements in the \"body\" element of an \"html\" element, an expression, that
-- corresponds 1-1 to the XPath selection path \"html\/body\/h1\"

(/>) :: (Tree t, MonadList s m) => (b -> m (t c)) -> (t c -> m d) -> (b -> m d)
f /> g = f >>> getChildren >>> g
{-# INLINE (/>) #-}

-- |
-- pronounced \"double slash\", meaning g arbitrarily deep inside f
--
-- defined as @ f \/\/> g = f >>> getChildren >>> deep g @
--
-- example: @ hasName \"html\" \/\/> hasName \"table\" @
--
-- This expression selects
-- all top level \"table\" elements within an \"html\" element, an expression.
-- Attention: This does not correspond
-- to the XPath selection path \"html\/\/table\". The latter on matches all table elements
-- even nested ones, but @\/\/>@ gives in many cases the appropriate functionality.

(//>) :: (Tree t, MonadList s m) => (b -> m (t c)) -> ((t c) -> m d) -> (b -> m d)
f //> g = f >>> getChildren >>> deep g
{-# INLINE (//>) #-}

-- |
-- pronounced \"outside\" meaning f containing g
--
-- defined as @ f \<\/ g = f \`containing\` (getChildren >>> g) @

(</) :: (Tree t, MonadList s m) =>
        (t b -> m (t b)) -> (t b -> m (t b)) -> (t b -> m (t b))

f </ g =
    f `containing` (getChildren >>> g)

{-# INLINE (</) #-}

-- | recursively searches a whole tree for subtrees, for which a predicate holds.
-- The search is performed top down. When a tree is found, this becomes an element of the result
-- list. The tree found is not further examined for any subtress, for which the predicate also could hold.
-- See 'multi' for this kind of search.
--
-- example: @ deep isHtmlTable @ selects all top level table elements in a document
-- (with an appropriate definition for isHtmlTable) but no tables occuring within a table cell.

deep :: (Tree t, MonadList s m) => (t b -> m c) -> (t b -> m c)
deep f
    = f                                     -- success when applying f
      `orElse`
      (getChildren >>> deep f)              -- seach children


-- | recursively searches a whole tree for subrees, for which a predicate holds.
-- The search is performed bottom up.
--
-- example: @ deepest isHtmlTable @ selects all innermost table elements in a document
-- but no table elements containing tables. See 'deep' and 'multi' for other search strategies.

deepest :: (Tree t, MonadList s m) => (t b -> m c) -> (t b -> m c)
deepest f
    = (getChildren >>> deepest f)           -- seach children
      `orElse`
      f                                     -- no success: apply f to root


-- | recursively searches a whole tree for subtrees, for which a predicate holds.
-- The search is performed top down. All nodes of the tree are searched, even within the
-- subtrees of trees for which the predicate holds.
--
-- example: @ multi isHtmlTable @ selects all table elements, even nested ones.

multi :: (Tree t, MonadList s m) => (t b -> m c) -> (t b -> m c)
multi f
    = f                                     -- combine result for root
      <+>
      (getChildren >>> multi f)             -- with result for all descendants

-- | recursively transforms a whole tree by applying an arrow to all subtrees,
-- this is done bottom up depth first, leaves first, root as last tree
--
-- example: @ processBottomUp (getChildren \`when\` isHtmlFont) @ removes all font tags
-- in a HTML document, even nested ones
-- (with an appropriate definition of isHtmlFont)

processBottomUp :: (Tree t, MonadList s m) => (t b -> m (t b)) -> (t b -> m (t b))
processBottomUp f
    = processChildren (processBottomUp f)   -- process all descendants first
      >>>
      f                                     -- then process root


-- | similar to 'processBottomUp', but recursively transforms a whole tree by
-- applying an arrow to all subtrees
-- with a top down depth first traversal strategie.
-- In many cases 'processBottomUp' and 'processTopDown'
-- give same results.

processTopDown :: (Tree t, MonadList s m) => (t b -> m (t b)) -> (t b -> m (t b))
processTopDown f
    = f                                     -- first process root
      >>>
      processChildren (processTopDown f)    -- then process all descendants of new root


-- | recursively transforms a whole tree by applying an arrow to all subtrees,
-- but transformation stops when a predicte does not hold for a subtree,
-- leaves are transformed first

processBottomUpWhenNot :: (Tree t, MonadList s m) =>
                          (t b -> m (t b)) -> (t b -> m (t b)) -> (t b -> m (t b))
processBottomUpWhenNot f p
    = ( processChildren (processBottomUpWhenNot f p)
        >>>
        f
      ) `whenNot` p

-- | recursively transforms a whole tree by applying an arrow to all subtrees,
-- but transformation stops when a tree is successfully transformed.
-- the transformation is done top down
--
-- example: @ processTopDownUntil (isHtmlTable \`guards\` tranformTable) @
-- transforms all top level table elements into something else, but inner tables remain unchanged

processTopDownUntil :: (Tree t, MonadList s m) =>
                       (t b -> m (t b)) -> (t b -> m (t b))
processTopDownUntil f
    = f
      `orElse`
      processChildren (processTopDownUntil f)


-- | computes a list of trees by applying an arrow to the input
-- and inserts this list in front of index i in the list of children
--
-- example: @ insertChildrenAt 0 (deep isCmt) @ selects all subtrees for which isCmt holds
-- and copies theses in front of the existing children

insertChildrenAt :: (Tree t, MonadList s m) =>
                    Int -> (t b -> m (t b)) -> (t b -> m (t b))
insertChildrenAt i f
    = listA f &&& this >>> arr2 insertAt
      where
        insertAt newcs
            = T.changeChildren (\ cs -> let (cs1, cs2) = splitAt i cs in
                                        cs1 ++ newcs ++ cs2
                               )

-- | similar to 'insertChildrenAt', but the insertion position is searched with a predicate

insertChildrenAfter :: (Tree t, MonadList s m) =>
                       (t b -> m (t b)) -> (t b -> m (t b)) -> (t b -> m (t b))
insertChildrenAfter p f
    = replaceChildren
      ( ( ( listA getChildren
            >>>
            spanA p
          )
          &&&
          listA f
        )
        >>> arr2L (\ (xs1, xs2) xs -> xs1 ++ xs ++ xs2)
      )


-- | an arrow for inserting a whole subtree with some holes in it (a template)
-- into a document. The holes can be filled with contents from the input.
--
-- Example
--
-- > insertTreeTemplateTest :: MonadList s m => b -> m XmlTree
-- > insertTreeTemplateTest
-- >     = doc
-- >       >>>
-- >       insertTreeTemplate template pattern
-- >     where
-- >     doc                                                                -- the input data
-- >        = constA "<x><y>The Title</y><z>The content</z></x>"
-- >          >>> xread
-- >     template                                                           -- the output template with 2 holes: xxx and yyy
-- >        = constA "<html><head><title>xxx</title></head><body><h1>yyy</h1></body></html>"
-- >          >>> xread
-- >     pattern
-- >        = [ hasText (== "xxx")                                          -- fill the xxx hole with the input contents from element "x/y"
-- >            :-> ( getChildren >>> hasName "y" >>> deep isText )
-- >
-- >          , hasText (== "yyy")                                          -- fill the yyy hole with the input contents from element "x/z"
-- >            :-> ( getChildren >>> hasName "z" >>> getChildren )
-- >          ]
--
-- computes the XML tree for the following document
--
-- > "<html><head><title>The Title</title></head><body><h1>The content</h1></body></html>"

insertTreeTemplate :: (Tree t, MonadList s m) =>
                      (t b -> m (t b)) ->                             -- the the template
                      [IfThen (t b -> m c) (t b -> m (t b))] ->       -- the list of nodes in the template to be substituted
                      (t b -> m (t b))
insertTreeTemplate template choices
    = insertTree $< this
    where
      insertTree t
          = template                              -- swap input and template
            >>>
            processTemplate
          where
            processTemplate
                = choiceA choices'                -- check whether node is a "hole" within the template
                  `orElse`
                  processChildren processTemplate -- else descent into template tree
            choices'
                = map feedTree choices            -- modify choices, such that the input is feed into the action arrows
            feedTree (cond :-> action)
                = cond :-> (constA t >>> action)  -- the real input becomes the input at the holes

