-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowExc
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: not portable

   The exception arrow class

-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowExc
    ( ArrowExc(..)
    )
where

import           Control.Arrow
import           Control.Arrow.ArrowIO

import           Control.Exception      ( SomeException
                                        )

class (Arrow a, ArrowChoice a, ArrowZero a, ArrowIO a) => ArrowExc a where
    tryA        :: a b c -> a b (Either SomeException c)

    catchA      :: a b c -> a SomeException c -> a b c
    catchA f h  = tryA f
                  >>>
                  ( h ||| returnA )


-- ------------------------------------------------------------
