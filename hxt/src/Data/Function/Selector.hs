{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies  -XFlexibleInstances #-}

module Data.Function.Selector
where

import Prelude 		hiding (id,(.))

import Control.Arrow
import Control.Category

infixr 3 .&&&.

-- ------------------------------------------------------------

-- | A Selector is a pair of an access function and a modifying function
-- for reading and updating parts of a composite type

data Selector s a       = S { getS :: s -> a
			    , setS :: a -> s -> s
			    }

chgS			:: Selector s a -> (a -> a) -> (s -> s)
chgS sel f s		= setS sel x s
			  where
			  x = f . getS sel $ s

chgM			:: (Monad m) => Selector s a -> (a -> m a) -> (s -> m s)
chgM sel f s		= do
			  y <- f x
			  return $ setS sel y s
			  where
			  x = getS sel $ s

-- | Alias for constructor S

mkSelector		:: (s -> a) -> (a -> s -> s) -> Selector s a
mkSelector		= S

-- (.), (>>>), (<<<)

instance Category Selector where
    id				= S { getS = id
				    , setS = const
				    }
    (S g2 s2) . (S g1 s1)	= S { getS = g2 . g1
				    , setS = \ x s ->
				            let x1  = g1 s    in
				            let x1' = s2 x x1 in
				            s1 x1' s
				    }

idS                     	:: Selector s s
idS                     	= id

(.&&&.)		                :: Selector s a -> Selector s b -> Selector s (a, b)
(.&&&.) (S g1 s1) (S g2 s2)	= S { getS = g1 &&& g2
                                    , setS = \ (x, y) -> s2 y . s1 x
                                    }

-- ------------------------------------------------------------
-- TODO: delete theses
{-
subS                    :: Selector b c -> Selector a b -> Selector a c
subS = (.)

pairS                   :: Selector s a -> Selector s b -> Selector s (a, b)
pairS = (.&&&.)

chgS                    :: Selector s a -> (a -> a) -> (s -> s)
chgS = update

putS                    :: Selector s a -> a -> (s -> s)
putS = setS

-}
-- ------------------------------------------------------------
{-
type Selector s a       = (s -> a, a -> s -> s)

subS                    :: Selector b c -> Selector a b -> Selector a c
subS (g2, s2) (g1, s1)  = ( g2 . g1
                          , s1s2
                          )
                          where
                          s1s2 x s = s'
                              where
                              x1  = g1 s
                              x1' = s2 x x1
                              s'  = s1 x1' s

pairS                   :: Selector s a -> Selector s b -> Selector s (a, b)
pairS (g1, s1) (g2, s2) = ( g1 &&& g2
                          , \ (x, y) -> s2 y . s1 x
                          )

chgS                    :: Selector s a -> (a -> a) -> (s -> s)
chgS (g, s) f x         = s (f (g x)) x

getS                    :: Selector s a -> s -> a
getS                    = fst                           -- getS (g, _s) x = g x

putS                    :: Selector s a -> a -> (s -> s)
putS s v                = chgS s (const v)

idS                     :: Selector s s
idS                     = (id, const)
-}
-- ------------------------------------------------------------

-- | Selectors for pairs and 3-tuples: comp1, comp2, comp3,
-- this can be extended to n-tuples

class Comp1 s a | s -> a where  comp1	:: Selector s a
class Comp2 s a | s -> a where  comp2	:: Selector s a
class Comp3 s a | s -> a where  comp3	:: Selector s a


instance Comp1 (a, b) a where		comp1	= S { getS = fst
						    , setS = \ x1 (_, x2) -> (x1, x2)
						    }

instance Comp2 (a, b) b where		comp2	= S { getS = snd
						    , setS = \ x2 (x1, _) -> (x1, x2)
						    }

instance Comp1 (a, b, c) a where	comp1	= S { getS = \ (x1, _, _) -> x1
						    , setS = \ x1 (_, x2, x3) -> (x1, x2, x3)
						    }

instance Comp2 (a, b, c) b where	comp2	= S { getS = \ (_, x2, _) -> x2
						    , setS = \ x2 (x1, _, x3) -> (x1, x2, x3)
						    }

instance Comp3 (a, b, c) c where	comp3	= S { getS = \ (_, _, x3) -> x3
						    , setS = \ x3 (x1, x2, _) -> (x1, x2, x3)
						    }

-- ------------------------------------------------------------

