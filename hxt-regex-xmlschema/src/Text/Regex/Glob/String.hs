-- ------------------------------------------------------------

{- |
   Module     : Text.Regex.Glob.String
   Copyright  : Copyright (C) 2011- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt <uwe@fh-wedel.de>
   Stability  : stable
   Portability: portable

   csh glob style pattern matcher
-}

-- ------------------------------------------------------------

module Text.Regex.Glob.String
  {-# DEPRECATED "use the more general 'Text.Regex.Glob.Generic' instead" #-}
  ( Regex
  , match
  , matchNoCase
  , parseRegex
  , parseRegexNoCase
  )
where

import           Text.Regex.Glob.Generic             (Regex)
import qualified Text.Regex.Glob.Generic             as G

-- ------------------------------------------------------------

match            :: String -> String -> Bool
match            = G.match

matchNoCase      :: String -> String -> Bool
matchNoCase      = G.matchNoCase

parseRegex       :: String -> Regex
parseRegex       = G.parseRegex

parseRegexNoCase :: String -> Regex
parseRegexNoCase = G.parseRegexNoCase

-- ------------------------------------------------------------
