-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.StateListArrow
   Copyright  : Copyright (C) 2005-8 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Implementation of list arrows with a state

-}

-- ------------------------------------------------------------

module Control.Arrow.StateListArrow
    ( SLA(..)
    , fromSLA
    )
where

import           Prelude hiding (id, (.))

import           Control.Category

import           Control.Arrow
import           Control.Arrow.ArrowIf
import           Control.Arrow.ArrowList
import           Control.Arrow.ArrowNF
import           Control.Arrow.ArrowState
import           Control.Arrow.ArrowTree

import           Control.Parallel.Strategies

-- ------------------------------------------------------------

-- | list arrow combined with a state

newtype SLA s a b = SLA { runSLA :: s -> a -> (s, [b]) }

instance Category (SLA s) where
    id                  = SLA $ \ s x -> (s, [x])

    SLA g . SLA f	= SLA $ \ s x -> let
					 ~(s1, ys) = f s x
					 sequence' s' []
					     = (s', [])
					 sequence' s' (x':xs')
					     = let
					       ~(s1', ys') = g s' x'
					       ~(s2', zs') = sequence' s1' xs'
					       in
					       (s2', ys' ++ zs')
					 in
					 sequence' s1 ys
	                                 
instance Arrow (SLA s) where
    arr f		= SLA $ \ s x -> (s, [f x])

    first (SLA f)	= SLA $ \ s ~(x1, x2) -> let
						 ~(s', ys1) = f s x1
						 in
						 (s', [ (y1, x2) | y1 <- ys1 ])

    -- just for efficiency
    second (SLA g)	= SLA $ \ s ~(x1, x2) -> let
						 ~(s', ys2) = g s x2
						 in
						 (s', [ (x1, y2) | y2 <- ys2 ])

    -- just for efficiency
    SLA f *** SLA g	= SLA $ \ s ~(x1, x2) -> let
						 ~(s1, ys1) = f s  x1
						 ~(s2, ys2) = g s1 x2
						 in
						 (s2, [ (y1, y2) | y1 <- ys1, y2 <- ys2 ])

    -- just for efficiency
    SLA f &&& SLA g	= SLA $ \ s x -> let
					 ~(s1, ys1) = f s  x
					 ~(s2, ys2) = g s1 x
					 in
					 (s2, [ (y1, y2) | y1 <- ys1, y2 <- ys2 ])


instance ArrowZero (SLA s) where
    zeroArrow		= SLA $ \ s -> const (s, [])


instance ArrowPlus (SLA s) where
    SLA f <+> SLA g	= SLA $ \ s x -> let
					 ~(s1, rs1) = f s  x
					 ~(s2, rs2) = g s1 x
					 in
					 (s2, rs1 ++ rs2)

instance ArrowChoice (SLA s) where
    left (SLA f)	= SLA $ \ s -> let
			               lf x = (s1, map Left y)
					      where
					      ~(s1, y) = f s x
                                       rf x = (s, [Right x])
				       in
				       either lf rf

    right (SLA f)	= SLA $ \ s -> let
				       lf x = (s, [Left x])
			               rf x = (s1, map Right y)
					      where
					      ~(s1, y) = f s x
				       in
				       either lf rf


instance ArrowApply (SLA s) where
    app			= SLA $ \ s (SLA f, x) -> f s x


instance ArrowList (SLA s) where
    arrL f		= SLA $ \ s x -> (s, (f x))
    arr2A f		= SLA $ \ s ~(x, y) -> runSLA (f x) s y
    constA c		= SLA $ \ s   -> const (s, [c])
    isA p		= SLA $ \ s x -> (s, if p x then [x] else [])
    SLA f >>. g		= SLA $ \ s x -> let
				         ~(s1, ys) = f s x
					 in
					 (s1, g ys)
    -- just for efficency
    perform (SLA f)	= SLA $ \ s x -> let
					 ~(s1, _ys) = f s x
					 in
					 (s1, [x])

instance ArrowIf (SLA s) where
    ifA (SLA p) ta ea	= SLA $ \ s x -> let
				         ~(s1, res) = p s x
					 in
					 runSLA ( if null res
						  then ea
						  else ta
						) s1 x

    (SLA f) `orElse` g
			= SLA $ \ s x ->  let
				          r@(s1, res) = f s x
					  in
					  if null res
					  then runSLA g s1 x
					  else r


instance ArrowState s (SLA s) where
    changeState cf	= SLA $ \ s x -> (cf s x, [x])
    accessState	af	= SLA $ \ s x -> (s, [af s x])

instance ArrowTree (SLA s)

instance (NFData s) => ArrowNF (SLA s) where
    rnfA (SLA f)	= SLA $ \ s x -> let res = f s x
                                         in
                                         res `demanding` rnf res

-- ------------------------------------------------------------

-- | conversion of state list arrows into arbitray other
-- list arrows.
--
-- allows running a state list arrow within another arrow:
--
-- example:
--
-- > ... >>> fromSLA 0 (... setState ... getState ... ) >>> ...
--
-- runs a state arrow with initial state 0 (e..g. an Int) within
-- another arrow sequence

fromSLA		:: ArrowList a => s -> SLA s b c -> a b c
fromSLA s f	=  arrL (snd . (runSLA f s))


-- ------------------------------------------------------------
