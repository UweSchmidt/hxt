-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG
   Copyright  : Copyright (C) 2010 Uwe Schmidt, Torben Kuseler
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   This helper module exports elements from the basic Relax NG libraries:
   Validator, CreatePattern, PatternToString and DataTypes.
   It is the main entry point to the Relax NG schema validator of the Haskell
   XML Toolbox.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.RelaxNG
  ( module Text.XML.HXT.RelaxNG.PatternToString
  , module Text.XML.HXT.RelaxNG.Validator
  , module Text.XML.HXT.RelaxNG.DataTypes
  , module Text.XML.HXT.RelaxNG.CreatePattern
  , module Text.XML.HXT.RelaxNG.SystemConfig
  )
where

import Text.XML.HXT.RelaxNG.PatternToString
import Text.XML.HXT.RelaxNG.Validator
import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.CreatePattern
import Text.XML.HXT.RelaxNG.SystemConfig

-- ------------------------------------------------------------

