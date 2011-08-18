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
    ( Regex
    , match
    , matchNoCase
    , parseRegex
    , parseRegexNoCase
    )
where

import Text.Regex.XMLSchema.String.Regex
import Text.Regex.Glob.String.RegexParser

-- ------------------------------------------------------------

match           :: String -> String -> Bool
match           = matchWithRegex . parseRegex

matchNoCase     :: String -> String -> Bool
matchNoCase     = matchWithRegex . parseRegexNoCase

-- ------------------------------------------------------------
