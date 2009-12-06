-- ------------------------------------------------------------

{- |
   Module     : Data.Tree.NTree.Filter
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Filter for n-ary tree structure with filter combinators
   copied and modified from HaXml (<http://www.cs.york.ac.uk/fp/HaXml/>)

   Similar but more flexible functions for tree processing are defined in the
   arrow classes "Control.Arrow.ArrowList", "Control.Arrow.ArrowIf", "Control.Arrow.ArrowTree" and "Control.Arrow.ArrowState".
   For new applications, especially for XML processing, it's recommended to use the arrow interface
   "Text.XML.HXT.Arrow" instead of this filter approach as part of the api "Text.XML.HXT.Parser"

-}

-- ------------------------------------------------------------

module Data.Tree.NTree.Filter
    ( module Data.Tree.NTree.TypeDefs
    , TFilter
    , TSFilter
    , satisfies

    -- * Filter
    , none		-- simple filter
    , this

    , isOf
    , isOfNode		-- predicate filter

    , mkNTree		-- contructor filter

    , replaceNode	-- simple editing
    , replaceChildren

    , modifyNode
    , modifyNode0
    , modifyChildren
    , substChildren
    , processChildren	-- filter combinators

    , o
    , (.>)
    , seqF
    , (..>)
    , (+++)
    , cat

    , orElse		-- choices
    , iff		-- if then else filter
    , choice
    , IfThen(..)
    , when
    , whenNot
    , guards
    , neg		-- negation
    , containing	-- previous: with, but with is keyword in haskell extensions
    , notContaining	-- previous: without

    , (/>)		-- selectors
    , (</)
    , deep		-- recursive search
    , deepest
    , multi
    , processBottomUp	-- recursive editing
    , processBottomUpIfNot
    , processTopDown
    , processTopDownUntil
    , insertChildrenAt
    , insertChildrenAfter

      -- * Monadic Filter
    , thisM		-- monadic filter
    , noneM
    , (.>>)		-- monadic composition
    , seqM		-- generalisation of .>>
    , (+++>>)		-- monadic parallel combination, monadic +++
    , catM		-- monadic form of cat
    , ifM		-- monadic if then else
    , choiceM		-- monadic choice
    , whenM		-- special monadic ifs
    , whenNotM
    , guardsM
    , containingM
    , processChildrenM	-- monadic versions of recursive editing
    , processTopDownM
    , processBottomUpM
    , liftMf		-- lift a normal filter to a monadic filter
    , ($$)		-- apply a filter to a list
    , ($$<)		-- apply a monad filter to a list
    , performAction	-- run a command within a filter
    )

where


import Data.Tree.NTree.TypeDefs

infixl 6 `containing`, `notContaining`, `containingM`
infixr 5 `o`, +++, +++>>
infixl 5 />, </, `orElse`, .>, ..>, .>>
infixr 4 `when`, `whenNot`, `guards`, `whenM`, `whenNotM`, `guardsM`
infixr 3 :->
infixr 0 $$, $$<

-- |
-- tree filter type: a function mapping a tree onto a list of trees
--
-- filter can be used in various ways, as predicates, selectors, transformers, ...

type TFilter  node	= NTree  node -> NTrees node

-- |
-- a filter for sequences

type TSFilter node	= NTrees node -> NTrees node

-- ------------------------------------------------------------

-- |
-- satisfies converts a result of a predicate filter into a boolean
--
-- is a shortcut for not . null
--
-- typical use in guards or ifs: @if (satisfies f) t then ... else ... @
--
--    * 1.parameter f :  the predicate filter
--
--    - 2.parameter t :  the tree to be tested
--
--    - returns : @b = not (null (f t))@

satisfies	:: (a -> [b]) -> a -> Bool
satisfies f	= not . null . f

-- ------------------------------------------------------------
--
-- simple filter

-- |
-- the null filter, returns the empty list

none		:: a -> [b]
none _		= []

-- |
-- the unit filter, returns the single element list containing the argument

this		:: a -> [a]
this n		= [n]

-- ------------------------------------------------------------
--
-- selecting filter

-- |
-- conversion from predicate function to filter
--
--    * 1.parameter p :  the predicate for testing the tree
--
--    - returns : 'this' or 'none' depending on the predicate

isOf		:: (a -> Bool) -> (a -> [a])
isOf p t
    = if p t then [t] else []

-- |
-- select filter, selects trees with node values with a specific property
--
--    * 1.parameter p :  the predicate for testing the node value
--
--    - returns : @[]@ or @[t]@ depending on @p t@
--
-- a special case of 'isOf' filter

isOfNode	:: (node -> Bool) -> TFilter node
isOfNode p	= isOf (p . getNode)

-- ------------------------------------------------------------
--
-- simple editing filters

-- |
-- filter for substituting an arbitray tree by a constant
--
--    * 1.parameter t :  the result tree, the input tree is ignored
--
--    - returns : the filter

mkNTree		:: NTree node -> TFilter node
mkNTree t	= const [t]

-- |
-- filter for replacing the node
--
--    * 1.parameter n :  the new node
--
--    - returns : the editing filter

replaceNode	:: node -> TFilter node
replaceNode n (NTree _ cs)
    = [NTree n cs]

-- |
-- filter for replacing the children
--
--    * 1.parameter cs :  cs the list of children
--
--    - returns : the filter

replaceChildren	:: NTrees node -> TFilter node
replaceChildren cs (NTree n _)
    = [NTree n cs]

modifyNode	:: (node -> Maybe node) -> TFilter node
modifyNode tf (NTree n cs)
    = maybe [] (\ n' -> [NTree n' cs]) (tf n)

-- |
-- filter for editing the node
--
--    * 1.parameter nf :  the XNode editing funtion
--
--    - returns : the filter

modifyNode0	:: (node -> node) -> TFilter node
modifyNode0 tf	= modifyNode (Just . tf)

-- |
-- filter for editing the children
--
-- all children are processed with a filter mapping lists to lists,
-- this enables not only elementwise editing by lifting a normal
-- filter to a list filter with @(f $$)@ (see '($$)') but also manipulation
-- of the order of the elements, e.g. "reverse" is an appropriate childen
-- editing function.
--
--    * 1.parameter csf :  the children editing function
--
--    - returns : the filter
--
-- see also : 'processChildren'

modifyChildren	:: TSFilter node -> TFilter node
modifyChildren f -- (NTree n cs)
    -- = [NTree n (f cs)]
    = substChildren (f . getChildren)

-- |
-- filter for substituting the children of a tree by
-- a new list of childen computed by applying a filter to the input tree.
-- 'modifyChildren' can be expressed by 'substChildren':
-- @modifyChildren f t@ is equal to @substChildren (f . getChildren)@

substChildren	:: TFilter node -> TFilter node
substChildren f t@(NTree n _cs)
    = [NTree n (f t)]

-- ------------------------------------------------------------
--
-- combinators (copied from HaXml)
--

-- |
-- sequential composition of filters, usually written in infix notation f2 `o` f1.
--
-- for predicate filter the logical AND
--
--    * 1.parameter f2 :  the 2. filter
--
--    - 2.parameter f1 :  the 1. filter
--
--    - returns : the fiter applying first f1 to n and then f2 to the result (like function composition)

o		:: (a -> [b]) -> (c -> [a]) -> (c -> [b])
f `o` g		= concatMap f . g

-- |
-- pronounced \"followed by\", defined as: @f .> g = g \`o\` f@.
--
-- allows filter composition in a more readable way from left to right
--
--    * 1.parameter f1 :  the 1. filter
--
--    - 2.parameter f2 :  the 2. filter
--
--    - returns : the composition of f1 and f2
--
-- see also : 'o', '(..>)'

(.>)		:: (a -> [b]) -> (b -> [c]) -> (a -> [c])
f .> g		= g `o` f

-- |
-- apply a list of filters sequentially with '(.>)', for predicate filters the generalized AND
--
-- see also : '(.>)'

seqF		:: [a -> [a]] -> (a -> [a])
seqF		= foldl (.>) this

-- |
-- binary parallel composition, the logical OR for predicate filter
--
--    * 1.parameter f1 :  the 1. filter
--
--    - 2.parameter f2 :  the 2. filter
--
--    - returns : the filter for applying f1 and f2 both to an argument tree and concatenating the results

(+++)		:: (a -> [b]) -> (a -> [b]) -> (a -> [b])
f +++ g		= \ t -> f t ++ g t

-- |
-- special sequential composition.
--
-- filter f is applied to an argument t.
-- then filter g is applied to all elements of the result list,
-- but the argument t is also passed as extra parameter to g.
--
-- This allows for step by step transformations of a tree
-- with access to the original tree in every
-- transformation step.
--
-- see also : '(.>)', 'o'

(..>)		:: (a -> [b]) -> (a -> b -> [d]) -> (a -> [d])
f ..> g		= \ t -> (g t `o` f) t

-- |
-- apply a list of filters, a \"union\" for lists, for predicate filters the generalized OR
--
--    * 1.parameter fs :  the list of filters
--
--    - returns : the composing filter

cat		:: [a -> [b]] -> (a -> [b])
cat fs  t
    = concat [ f t | f <- fs ]

-- |
-- Filter for editing the children of a tree element wise
--
--    * 1.parameter cf :  the filter applied to the children
--
--    - returns : the editing filter
--
-- see also : 'modifyChildren'

processChildren	:: TFilter node -> TFilter node
processChildren ft (NTree n cs)
    = [NTree n (ft $$ cs)]

-- |
-- infix operator for applying a filter to a list of trees
--
--    * 1.parameter f :  the filter
--
--    - 2.parameter ts :  the list of trees
--
--    - returns : the concatenated list of results

($$)		:: (a -> [b]) -> [a] -> [b]
f $$ l
    = concatMap f l

-- ------------------------------------------------------------
--
-- choices
--

-- |
-- directional choice, usually written in infix notation as f `orElse` g
--
--    * 1.parameter f :  the 1. filter
--
--    - 2.parameter g :  the 2. filter
--
--    - 3.parameter t :  the tree
--
--    - returns : the filter, that applies f to t, if the result is not the empty list, the result is found, else g t is the result

orElse		:: (a -> [b]) -> (a -> [b]) -> (a -> [b])
f `orElse` g
    = \ t-> let
	    res = f t
	    in
	    if null res
	    then g t
	    else res

-- |
-- if then else lifted to filters
--
--    * 1.parameter p :  the predicate filter
--
--    - 2.parameter t :  the \"then\" filter
--
--    - 3.parameter e :  the \"else\" filter
--
--    - returns : the resulting conditional filter

iff		:: (a -> [c]) -> (a -> [b]) -> (a -> [b]) -> (a -> [b])
iff p f g	= \ c -> if (satisfies p) c then f c else g c

-- |
-- when the predicate p holds, f is applied, else the identity filter this
--
--    * 1.parameter f :  the conditinally applied filter
--
--    - 2.parameter p :  the predicate
--
--    - returns : the conditional filter
--
-- see also : 'iff', 'whenNot', 'guards', 'whenM'

when		:: (a -> [a]) -> (a -> [a]) -> (a -> [a])
f `when` g	= iff g f this

-- |
-- the complementary filter of when
--
-- shortcut for f `when` neg g
--
-- see also : 'iff', 'when', 'whenNotM', 'neg'

whenNot		:: (a -> [a]) -> (a -> [a]) -> (a -> [a])
f `whenNot` g	= iff g this f

-- |
-- when the predicate p holds, f is applied, else the null filter none
--
--    * 1.parameter p :  the predicate filter
--
--    - 2.parameter f :  the conditionally applied filter
--
--    - returns : the conditional filter
--
-- see also : 'iff', 'when', 'guardsM'

guards		:: (a -> [b]) -> (a -> [b]) -> (a -> [b])
g `guards` f	= iff g f none


-- |
-- negation lifted to filters
--
--    * 1.parameter f :  the predicate filter
--
--    - returns : the filter, that succeeds, when f failed

neg		:: (a -> [c]) -> a -> [a]
neg f		= iff f none this

-- |
-- auxiliary datatype for cases within choice filter

data IfThen a b = a :-> b

-- |
-- multiway branch. The list of cases @f :-> g@ is processed sequentially,
-- in the first case for that f holds g is applied, if no case matches, 'none' is
-- applied.
-- This filter can be used like a case expression: @choice [ p1 :-> f1, p2 :-> f2, ... , this :-> defaultFilter]@
--
-- see also : 'iff', 'choiceM'

choice	:: [IfThen (a -> [c]) (a -> [b])] -> (a -> [b])
choice []
    = none

choice ((g :-> f) : cs)
    = iff g f (choice cs)

-- |
-- pruning: keep only those results from f for which g holds, usually written in infix notation as f `containing` g
--
--    * 1.parameter f :  the processing filter
--
--    - 2.parameter g :  the predicate filter
--
--    - 3.parameter t :  the tree
--
--    - returns : all trees r from f t, for which g r holds (is not the empty list)
--
-- see also : 'notContaining'

containing		:: (a -> [b]) -> (b -> [c]) -> a -> [b]
f `containing` g
    = filter (satisfies g) . f

-- |
-- pruning: keep only those results from f for which g does not hold
--
-- see also : 'containing'

notContaining		:: (a -> [b]) -> (b -> [c]) -> a -> [b]
f `notContaining` g
    = filter (null . g) . f

-- |
-- pruning: monadic version of containing, usually written in infix notation as f `containingM` g
--
--    * 1.parameter f :  the monadic processing filter
--
--    - 2.parameter g :  the predicate filter
--
--    - 3.parameter t :  the tree
--
--    - returns : all trees r from f t, for which g r holds (is not the empty list)
--
-- see also : 'notContaining'

containingM		:: Monad m => (a -> m [b]) -> (b -> [c]) -> a -> m [b]
f `containingM` g
    = \ t -> do
             res <- f t
             return $ filter (satisfies g) res

-- |
-- pronounced \"slash\", meaning g inside f

(/>)		:: TFilter node -> TFilter node -> TFilter node
f /> g
    = g `o` getChildren `o` f

-- |
-- pronounced \"outside\" meaning f containing g

(</)		:: TFilter node -> TFilter node -> TFilter node
f </ g
    = f `containing` (g `o` getChildren)

-- ------------------------------------------------------------
--
-- recursive search filters

-- |
-- top down search.
--
-- search terminates, when filter f succeeds
-- can e.g. be used for finding all outermost tag node of a specific kind

deep		:: TFilter node -> TFilter node
deep f
    = f `orElse` (deep f `o` getChildren)

-- |
-- bottom up search.
--
-- first the children are processed,
-- if this does not succeed, the node itself is processed
-- can e.g. be used for finding all innermost tag nodes of a specific kind

deepest		:: TFilter node -> TFilter node
deepest f
    = (getChildren .> deepest f) `orElse` f

-- |
-- process all nodes of the whole tree.
--
-- can e.g. be used for finding all nodes of a specific kind

multi		:: TFilter node -> TFilter node
multi f
    = f +++ (getChildren .> multi f)

-- ------------------------------------------------------------
--
-- recursive transformation filters
--

-- |
-- bottom up transformation
--
--    * 1.parameter f :  the /simple/ transforming filter
--
--    - returns : the filter that applies f to all subtrees and the tree itself in a deepth first left to right manner
--
-- see also : 'processTopDown', 'processBottomUpIfNot'

processBottomUp	:: TFilter node -> TFilter node
processBottomUp f
    = processChildren (processBottomUp f) .> f

-- |
-- top down transformation
--
--    * 1.parameter f :  the /simple/ transforming filter
--
--    - returns : the filter that applies f first to the tree and then recursively to all subtrees of the result
--
-- see also : 'processBottomUp'

processTopDown	:: TFilter node -> TFilter node
processTopDown f
    = f .> processChildren (processTopDown f)

-- |
-- guarded bottom up transformation, stops at subtrees for which a predicate p holds
--
--    * 1.parameter f :  the transforming filter
--
--    - 2.parameter p :  the predicate filter for the guard
--
--    - returns : the filter for processing all (sub-)trees
--
-- see also : 'processBottomUp'

processBottomUpIfNot	:: TFilter node -> TFilter node -> TFilter node
processBottomUpIfNot f p
    = (processChildren (processBottomUpIfNot f p) .> f) `whenNot` p

-- |
-- top down transformation until a node to be transformed is found
--
--    * 1.parameter f :  the /simple/ transforming filter
--
--    - returns : the filter that applies f first to the tree and, if the filter does not succeed,
--		  recursively to all children of the input tree.
--
-- Example:
--
-- @processTopDownUntil none@
--
-- is the identity filter (maybe a bit more inefficient).
--
-- Example:
--
-- @processTopDownUntil (add1Attr \"border\" \"2\" \`containing\` isTag \"table\")@
--
-- is a filter for adding an attribute border=\"2\" in all top level table tags.
--		  The content of table tags will remain unchanged.
--
-- see also : 'processTopDown', 'processBottomUp'

processTopDownUntil	:: TFilter node -> TFilter node
processTopDownUntil f t
    | null res
	= processChildren (processTopDownUntil f) t
    | otherwise
	= res
    where
    res = f t

-- ------------------------------------------------------------
--

-- |
-- insertion of trees into the list of children at a given position
--
-- useful for inserting something into the list of children at a given position
-- the list of children is split with the @splitAt@ function
-- the nodes are inserted between these two sublists.
--
-- examples: @insertChildrenAt 0 ins t@ inserts all elements computed with @ins t@ in front
-- of the childen of @t@, @insertChildrenAt 1 ins t@ behind the first child
--
-- see also: 'insertChildrenAfter'


insertChildrenAt	:: Int -> TFilter node -> TFilter node
insertChildrenAt i ins t
    = replaceChildren (cl1 ++ ins t ++ cl2) t
    where
    (cl1, cl2) = splitAt i $ getChildren t

-- |
-- insertion of trees into the list of children after specific elements
--
-- useful for inserting something into the list of children of a node
-- the list of children is split with the @span@ function and the filter p as predicate
-- the nodes are inserted between these two sublists
--
-- examples: @insertChildrenAfter none ins t@ inserts all elements computed with @ins t@ in front
-- of the childen of @t@, @insertChildrenAfter this ins t@ appends the elements to the children
--
-- see also: 'insertChildrenAt'

insertChildrenAfter	:: TFilter node -> TFilter node -> TFilter node
insertChildrenAfter p ins t
    = replaceChildren (cl1 ++ ins t ++ cl2) t
    where
    (cl1, cl2) = span (not . null . p) $ getChildren t

-- ------------------------------------------------------------
--
-- monadic filter

-- |
-- the monadic version of the identity filter this.
--
-- see also : 'this'

thisM		:: Monad m => (a -> m [a])
thisM		= liftMf this

-- |
-- the monadic version of the null filter none.
--
-- see also : 'none'

noneM		:: Monad m => (a -> m [b])
noneM		= liftMf none

-- |
-- sequential composition of monadic filters, monadic version of \".>\".
--
--    * 1.parameter f1 :  the 1. monadic filter
--
--    - 2.parameter f2 :  the 2. monadic filter
--
--    - returns : the monadic fiter applying first f1 to n and then f2 to the result (like function composition)
--
-- see also : '(.>)'

(.>>)	:: Monad m => (a -> m[b]) -> (b -> m [c]) -> (a -> m[c])
cmd1 .>> cmd2
    = \ t ->
      do
      r1 <- cmd1 t
      r2 <- mapM cmd2 r1
      return (concat r2)

-- |
-- generalized sequential composition of monadic filters

seqM	:: Monad m => [a -> m[a]] -> (a -> m[a])
seqM
    = foldr (.>>) thisM

-- |
-- binary parallel composition, the logical OR for predicate filter
--
--    * 1.parameter f1 :  the 1. filter
--
--    - 2.parameter f2 :  the 2. filter
--
--    - returns : the filter for applying f1 and f2 both to an argument tree and concatenating the results
-- see also: 'cat', '+++', 'catM'

(+++>>)		:: Monad m => (a -> m [b]) -> (a -> m [b]) -> (a -> m [b])
f +++>> g	= \ t -> do
			 r1 <- f t
			 r2 <- g t
			 return (r1 ++ r2)

-- |
-- apply a list of monadic filters
--
--    * 1.parameter fs :  the list of filters
--
--    - returns : the composing filter
-- see also: 'cat', '+++', '+++>>'

catM	:: Monad m => [a -> m [b]] -> (a -> m [b])
catM fs  t
    = do
      res <- sequence (map ($ t) fs)
      return $ concat res

-- |
-- monadic if-then-else.
--
--    * 1.parameter p :  the predicate
--
--    - 2.parameter thenP :  the then part: the monadic filter, that is applied if p holds for the input tree
--
--    - 3.parameter elseP :  the else part
--
--    - returns : the monadic filter for the conditional

ifM		:: Monad m => (a -> [b]) -> (a -> m [c]) -> (a -> m [c]) -> (a -> m [c])
ifM p thenPart elsePart
    = \ t ->
      if (satisfies p) t
      then thenPart t
      else elsePart t

-- |
-- monadic version of multiway branch. The list of cases @f :-> g@ is processed sequentially,
-- in the first case for that f holds g is applied, if no case matches, 'noneM' is
-- applied.
-- This filter can be used like a case expression: @choiceM [ p1 :-> f1, p2 :-> f2, ... , thisM :-> defaultFilter]@
--
-- see also : 'choice', 'ifM'

choiceM		:: Monad m => [IfThen (a -> [c]) (a -> m [b])] -> (a -> m [b])
choiceM []
    = noneM

choiceM ((g :-> f) : cs)
    = ifM g f (choiceM cs)


-- |
-- when the predicate p holds, the monadic filter f is applied, else the identity filter.
--
--    * 1.parameter f :  the conditinally applied monadic filter
--
--    - 2.parameter p :  the simple predicate
--
--    - returns : the conditional filter
--
-- see also : 'ifM', 'when', 'guardsM', 'whenNotM'

whenM		:: Monad m => (a -> m [a]) -> (a -> [b]) -> (a -> m [a])
f `whenM` g	= ifM g f thisM

-- |
-- the complementary filter of whenM.
--
-- see also : 'ifM', 'whenM', 'whenNot'

whenNotM	:: Monad m => (a -> m [a]) -> (a -> [b]) -> (a -> m [a])
f `whenNotM` g	= ifM g thisM f

-- |
-- when the predicate p holds, the monadic filter f is applied, else the null filter.
--
--    * 1.parameter p :  the simple predicate filter
--
--    - 2.parameter f :  the conditionally applied monadic filter
--
--    - returns : the conditional filter
--
-- see also : 'ifM', 'guards', 'whenM'

guardsM		:: Monad m => (a -> [b]) -> (a -> m [c]) -> (a -> m [c])
g `guardsM` f	= ifM g f noneM

-- |
-- Filter for editing the children of a tree with a monadic filter
--
--    * 1.parameter cf :  the monadic filter applied to the children
--
--    - returns : the monadic editing filter
--
-- see also : 'processChildren'

processChildrenM	:: Monad m => (NTree node -> m [NTree node]) -> (NTree node -> m [NTree node])
processChildrenM ft (NTree n cs)
    = do
      res <- ft $$< cs
      return [NTree n res]

-- |
-- monadic variant of 'processTopDown'

processTopDownM		:: Monad m => (NTree node -> m [NTree node]) -> (NTree node -> m [NTree node])
processTopDownM f
    = f .>> processChildrenM (processTopDownM f)

-- |
-- monadic variant of 'processBottomUp'

processBottomUpM	:: Monad m => (NTree node -> m [NTree node]) -> (NTree node -> m [NTree node])
processBottomUpM f
    = processChildrenM (processBottomUpM f) .>> f

-- |
-- infix operator for applying a monadic filter to a list of trees,
-- typically used in do-notation for processing of intermediate results.
--
--    * 1.parameter f :  the monadic filter
--
--    - 2.parameter ts :  the list of trees
--
--    - returns : the concatenated list of results
--
-- see also : '($$)'

($$<)	:: Monad m => (a -> m [b]) -> [a] -> m [b]
cmd $$< l
    = do
      r <- mapM cmd l
      return (concat r)

-- |
-- lift a filter to a monadic filter
--
--    * 1.parameter f :  the /simple/ filter
--
--    - returns : the lifted monadic version

liftMf	:: Monad m => (a -> [b]) -> a -> m [b]
liftMf f t
    = return (f t)

-- |
-- run an arbitray command on a tree t and return the tree,
-- used for inserting arbitray commands in a filter pipeline
--
--    * 1.parameter cmd :  the command
--
--    - 2.parameter t :  the argument tree
--
--    - returns : the unchanged tree as a single element list

performAction	:: Monad m => (a -> m b) -> a -> m [a]
performAction cmd t
    = do
      cmd t
      return [t]

-- eof ------------------------------------------------------------
