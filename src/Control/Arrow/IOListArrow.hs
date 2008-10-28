-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.IOListArrow
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Implementation of pure list arrows with IO

-}

-- ------------------------------------------------------------

module Control.Arrow.IOListArrow
    ( IOLA(..)
    )
where
import Prelude hiding (id, (.))

import Control.Category

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO

-- ------------------------------------------------------------

-- | list arrow combined with IO monad

newtype IOLA a b = IOLA { runIOLA :: a -> IO [b] }

instance Category IOLA where
    id                  = arr id

    IOLA g . IOLA f	= IOLA $ \ x -> do
					ys <- f x
					zs <- sequence . map g $ ys
					return (concat zs)

instance Arrow IOLA where
    arr f		= IOLA $ \ x -> return [f x]

    first (IOLA f)	= IOLA $ \ ~(x1, x2) -> do
					        ys1 <- f x1
					        return [ (y1, x2) | y1 <- ys1 ]

    -- just for efficiency
    second (IOLA g)	= IOLA $ \ ~(x1, x2) -> do
					        ys2 <- g x2
					        return [ (x1, y2) | y2 <- ys2 ]

    -- just for efficiency
    IOLA f *** IOLA g	= IOLA $ \ ~(x1, x2) -> do
						ys1 <- f x1
					        ys2 <- g x2
					        return [ (y1, y2) | y1 <- ys1, y2 <- ys2 ]

    -- just for efficiency
    IOLA f &&& IOLA g	= IOLA $ \ x -> do
					ys1 <- f x
					ys2 <- g x
					return [ (y1, y2) | y1 <- ys1, y2 <- ys2 ]


instance ArrowZero IOLA where
    zeroArrow		= IOLA $ const (return [])


instance ArrowPlus IOLA where
    IOLA f <+> IOLA g	= IOLA $ \ x -> do
					rs1 <- f x
					rs2 <- g x
					return (rs1 ++ rs2)


instance ArrowChoice IOLA where
    left (IOLA f)	= IOLA $ either
			           (\ x -> f x >>= (\ y -> return (map Left y)))
                                   (return . (:[]) . Right)
    right (IOLA f)	= IOLA $ either
                                   (return . (:[]) . Left)
			           (\ x -> f x >>= (\ y -> return (map Right y)))

instance ArrowApply IOLA where
    app			= IOLA $ \ (IOLA f, x) -> f x

instance ArrowList IOLA where
    arrL f		= IOLA $ \ x -> return (f x)
    arr2A f		= IOLA $ \ ~(x, y) -> runIOLA (f x) y
    constA c		= IOLA $ const (return [c])
    isA p		= IOLA $ \x -> return (if p x then [x] else [])
    IOLA f >>. g	= IOLA $ \x -> do
				       ys <- f x
				       return (g ys)


instance ArrowIf IOLA where
    ifA (IOLA p) ta ea	= IOLA $ \x -> do
				       res <- p x
				       runIOLA (if null res then ea else ta) x
    (IOLA f) `orElse` g
			= IOLA $ \x -> do
				       res <- f x
				       if null res then runIOLA g x else return res

instance ArrowIO IOLA where
    arrIO cmd		= IOLA $ \x -> do
			       	       res <- cmd x
				       return [res]

instance ArrowIOIf IOLA where
    isIOA p		= IOLA $ \x -> do
				       res <- p x
				       return (if res then [x] else [])

instance ArrowTree IOLA

-- ------------------------------------------------------------
