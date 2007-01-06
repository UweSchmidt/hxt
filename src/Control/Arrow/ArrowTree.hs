-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowTree
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ArrowTree.hs,v 1.12 2006/11/30 16:05:24 hxml Exp $

List arrows for tree processing.

Trees that implement the "Data.Tree.Class" interface, can be processed
with these arrows.

-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowTree
    ( ArrowTree(..)
    , Tree
    )
where

import Data.Tree.Class (Tree)
import qualified Data.Tree.Class as T hiding (Tree)

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf

infixl 5 />, </

-- ------------------------------------------------------------

-- | The interface for tree arrows
--
-- all functions have default implementations

class (ArrowPlus a, ArrowIf a) => ArrowTree a where

    -- | select the children of the root of a tree

    getChildren		:: Tree t => a (t b) (t b)
    getChildren		= arrL T.getChildren

    -- | select the attribute of the root of a tree

    getNode		:: Tree t => a (t b) b
    getNode		= arr T.getNode

    -- | substitute the children of the root of a tree

    setChildren		:: Tree t =>            [t b] -> a (t b) (t b)
    setChildren cs	= arr (T.setChildren cs)

    -- | substitute the attribute of the root of a tree

    setNode		:: Tree t =>                b -> a (t b) (t b)
    setNode n		= arr (T.setNode n)

    -- | edit the children of the root of a tree

    changeChildren	:: Tree t => ([t b] -> [t b]) -> a (t b) (t b)
    changeChildren csf	= arr (T.changeChildren csf)

    -- | edit the attribute of the root of a tree

    changeNode		:: Tree t =>        (b  -> b) -> a (t b) (t b)
    changeNode nf	= arr (T.changeNode nf)

								-- compound arrows

    -- | apply an arrow element wise to all children of the root of a tree
    -- collect these results and substitute the children with this result
    --
    -- example: @ processChildren isText @ deletes all subtrees, for which isText does not hold
    --
    -- example: @ processChildren (none \`when\` isCmt) @ removes all children, for which isCmt holds

    processChildren	:: Tree t => a (t b) (t b) -> a (t b) (t b)
    processChildren f	= listA (getChildren >>> f)		-- new children, deterministic filter: single element result
			  &&&
			  this					-- pair with root
			  >>>
			  arr2 T.setChildren			-- apply binary filter

    -- | similar to processChildren, but the new children are computed by processing
    -- the whole input tree
    --
    -- example: @ replaceChildren (deep isText) @ selects all subtrees for which isText holds
    -- and substitutes the children component of the root node with this list

    replaceChildren	:: Tree t => a (t b) (t b) -> a (t b) (t b)
    replaceChildren f	= listA f				-- compute new children
			  &&&
			  this
			  >>>
			  arr2 T.setChildren

    -- |
    -- pronounced \"slash\", meaning g inside f
    --
    -- defined as @ f \/> g = f >>> getChildren >>> g @

    (/>)		:: Tree t => a (t b) (t b) -> a (t b) (t b) -> a (t b) (t b)
    f /> g		= f >>> getChildren >>> g


    -- |
    -- pronounced \"outside\" meaning f containing g
    --
    -- defined as @ f <\/ g = f \`containing\` (getChildren >>> g) @

    (</)		:: Tree t => a (t b) (t b) -> a (t b) (t b) -> a (t b) (t b)
    f </ g		= f `containing` (getChildren >>> g)


    -- | recursively searches a whole tree for subtrees, for which a predicate holds.
    -- The search is performed top down and stops when a tree is found.
    --
    -- example: @ deep isHtmlTable @ selects all top level table elements in a document
    -- (with an appropriate definition for isHtmlTable)

    deep		:: Tree t => a (t b) (t b) -> a (t b) (t b)
    deep f		= f					-- success when applying f
			  `orElse`
			  (getChildren >>> deep f)		-- seach children


    -- | recursively searches a whole tree for subrees, for which a predicate holds.
    -- The search is performed bottom up and stops when a tree is found.
    --
    -- example: @ deepest isHtmlTable @ selects all innermost table elements in a document

    deepest		:: Tree t => a (t b) (t b) -> a (t b) (t b)
    deepest f		= (getChildren >>> deepest f)		-- seach children
			  `orElse`
			  f					-- no success: apply f to root


    -- | recursively searches a whole tree for subtrees, for which a predicate holds.
    -- The search is performed top down, but does not stop when a tree is found.
    --
    -- example: @ multy isHtmlTable @ selects all table elements, even nested ones.

    multi		:: Tree t => a (t b) c -> a (t b) c
    multi f		= f					-- combine result for root
			  <+>
			  (getChildren >>> multi f)		-- with result for all descendants

    -- | recursively transforms a whole tree by applying an arrow to all subtrees,
    -- this is done bottom up depth first, leaves first, root as last tree
    --
    -- example: @ processBottomUp (getChildren \`when\` isHtmlFont) @ removes all font tags in a HTML document, even nested ones
    -- (with an appropriate definition of isHtmlFont)

    processBottomUp	:: Tree t => a (t b) (t b) -> a (t b) (t b)
    processBottomUp f	= processChildren (processBottomUp f)	-- process all descendants first
			  >>>
			  f					-- then process root

    -- | similar to 'processBottomUp', but recursively transforms a whole tree by applying an arrow to all subtrees
    -- with a top down depth first traversal strategie. In many cases 'processBottomUp' and 'processTopDown'
    -- give same results.

    processTopDown	:: Tree t => a (t b) (t b) -> a (t b) (t b)
    processTopDown f	= f					-- first process root
			  >>>
			  processChildren (processTopDown f)	-- then process all descendants of new root


    -- | recursively transforms a whole tree by applying an arrow to all subtrees,
    -- but transformation stops when a predicte does not hold for a subtree,
    -- leaves are transformed first

    processBottomUpWhenNot
			:: Tree t => a (t b) (t b) -> a (t b) (t b) -> a (t b) (t b)
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

    processTopDownUntil	:: Tree t => a (t b) (t b) -> a (t b) (t b)
    processTopDownUntil f
			= f
			  `orElse`
			  processChildren (processTopDownUntil f)

    -- | computes a list of trees by applying an arrow to the input
    -- and inserts this list in front of index i in the list of children
    --
    -- example: @ insertChildrenAt 0 (deep isCmt) @ selects all subtrees for which isCmt holds
    -- and copies theses in front of the existing children

    insertChildrenAt	:: Tree t =>           Int -> a (t b) (t b) -> a (t b) (t b)
    insertChildrenAt i f
			= listA f &&& this >>> arr2 insertAt
			  where
			  insertAt newcs
			      = T.changeChildren (\ cs -> let
						          (cs1, cs2) = splitAt i cs
						          in
						          cs1 ++ newcs ++ cs2
						 )

    -- | similar to 'insertChildrenAt', but the insertion position is searched with a predicate

    insertChildrenAfter	:: Tree t => a (t b) (t b) -> a (t b) (t b) -> a (t b) (t b)
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
    -- > insertTreeTemplateTest	:: ArrowXml a => a b XmlTree
    -- > insertTreeTemplateTest
    -- >     = doc
    -- >       >>>
    -- >       insertTemplate template pattern
    -- >     where
    -- >     doc								-- the input data
    -- > 	= constA "<x><y>The Title</y><z>The content</z></x>"
    -- > 	  >>> xread
    -- >     template								-- the output template with 2 holes: xxx and yyy
    -- > 	= constA "<html><head><title>xxx</title></head><body><h1>yyy</h1></body></html>"
    -- > 	  >>> xread
    -- >     pattern
    -- > 	= [ hasText (== "xxx")						-- fill the xxx hole with the input contents from element "x/y"
    -- > 	    :-> ( getChildren >>> hasName "y" >>> deep isText )
    -- > 
    -- > 	  , hasText (== "yyy")						-- fill the yyy hole with the input contents from element "x/z"
    -- > 	    :-> ( getChildren >>> hasName "z" >>> getChildren )
    -- > 	  ]
    --
    -- computes the XML tree for the following document
    --
    -- > "<html><head><title>The Title</title></head><body><h1>The content</h1></body></html>"

    insertTreeTemplate	:: Tree t =>
			   a (t b) (t b) ->					-- the the template
			   [IfThen (a (t b) c) (a (t b) (t b))] ->		-- the list of nodes in the template to be substituted
			   a (t b) (t b)
    insertTreeTemplate template choices
	= insertTree $< this
	  where
	  insertTree t
	      = template					-- swap input and template
		>>>
		processTemplate
	      where
	      processTemplate
		  = choiceA choices'				-- check whether node is a "hole" within the template
		    `orElse`
		    processChildren processTemplate		-- else descent into template tree
	      choices'
		  = map feedTree choices			-- modify choices, such that the input is feed into the action arrows
	      feedTree (cond :-> action)
		  = cond :-> (constA t >>> action)		-- the real input becomes the input at the holes
