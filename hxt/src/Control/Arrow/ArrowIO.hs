-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowIO
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

  Lifting of IO actions to arrows

-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowIO
    ( ArrowIO(..)
    , ArrowIOIf(..)
    )
where

import Control.Arrow

-- | the interface for converting an IO action into an arrow

class Arrow a => ArrowIO a where

    -- | construct an arrow from an IO action
    arrIO               :: (b -> IO c) -> a b c

    -- | construct an arrow from an IO action without any parameter
    arrIO0              :: IO c -> a b c
    arrIO0 f            = arrIO (const f)

    -- | construction of a 2 argument arrow from a binary IO action
    -- |
    -- | example: @ a1 &&& a2 >>> arr2 f @

    arrIO2              :: (b1 -> b2 -> IO c) -> a (b1, b2) c
    arrIO2 f            = arrIO (\ ~(x1, x2) -> f x1 x2)

    -- | construction of a 3 argument arrow from a 3-ary IO action
    -- |
    -- | example: @ a1 &&& a2 &&& a3 >>> arr3 f @

    arrIO3              :: (b1 -> b2 -> b3 -> IO c) -> a (b1, (b2, b3)) c
    arrIO3 f            = arrIO (\ ~(x1, ~(x2, x3)) -> f x1 x2 x3)

    -- | construction of a 4 argument arrow from a 4-ary IO action
    -- |
    -- | example: @ a1 &&& a2 &&& a3 &&& a4 >>> arr4 f @

    arrIO4              :: (b1 -> b2 -> b3 -> b4 -> IO c) -> a (b1, (b2, (b3, b4))) c
    arrIO4 f            = arrIO (\ ~(x1, ~(x2, ~(x3, x4))) -> f x1 x2 x3 x4)


-- | the interface for converting an IO predicate into a list arrow

class (Arrow a, ArrowIO a) => ArrowIOIf a where

    -- | builds an arrow from an IO predicate
    --
    -- if the predicate holds, the single list containing the input is returned, else the empty list,
    -- similar to 'Control.Arrow.ArrowList.isA'

    isIOA               :: (b -> IO Bool) -> a b b

-- ------------------------------------------------------------
