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

module Text.Regex.Glob.Generic
    ( GenRegex
    , Regex
    , RegexText
    , RegexTextLazy
    , RegexByteString
    , RegexByteStringLazy
    , match
    , matchNoCase
    , parseRegex
    , parseRegexNoCase
    )
where

import Text.Regex.Glob.Generic.RegexParser     (parseRegex, parseRegexNoCase)
import Text.Regex.XMLSchema.Generic.Regex      (matchWithRegex)
import Text.Regex.XMLSchema.Generic.StringLike
import Text.Regex.XMLSchema.Generic            (GenRegex,
                                                Regex,
                                                RegexText,
                                                RegexTextLazy,
                                                RegexByteString,
                                                RegexByteStringLazy
                                               )
     

-- ------------------------------------------------------------

match           :: StringLike s => s -> s -> Bool
match           = matchWithRegex . parseRegex

matchNoCase     :: StringLike s => s -> s -> Bool
matchNoCase     = matchWithRegex . parseRegexNoCase

-- ------------------------------------------------------------
