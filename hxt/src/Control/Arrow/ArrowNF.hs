-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowNF
   Copyright  : Copyright (C) 2005-8 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: non-portable

   Arrows for evaluation of normal form results

-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowNF
where

import Control.Arrow
import Control.DeepSeq

-- |
-- complete evaluation of an arrow result using 'Control.DeepSeq'
--
-- this is sometimes useful for preventing space leaks, especially after reading
-- and validation of a document, all DTD stuff is not longer in use and can be
-- recycled by the GC.

strictA	:: (Arrow a, NFData b) => a b b
strictA	= arr $ \ x -> deepseq x x

class (Arrow a) => ArrowNF a where
    rnfA	:: (NFData c) => a b c -> a b c
    rnfA f	= f >>> strictA

-- ------------------------------------------------------------
