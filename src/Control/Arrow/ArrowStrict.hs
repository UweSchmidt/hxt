-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.ArrowStrict
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Arrows for complete evaluation of result

-}

-- ------------------------------------------------------------

module Control.Arrow.ArrowStrict
where

import Control.Arrow
import Control.Strategies.DeepSeq

-- |
-- complete evaluation of an arrow result using 'Control.Strategies.DeepSeq.deepSeq'
-- and 'Control.Strategies.DeepSeq.strict'
--
-- this is sometimes useful for preventing space leaks, especially after reading
-- and validation of a document, all DTD stuff is not longer in use and can be
-- recycled by the GC.

strictA	:: (Arrow a, DeepSeq b) => a b b
strictA	= arr strict
