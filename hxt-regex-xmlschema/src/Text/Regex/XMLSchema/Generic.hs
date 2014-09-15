-- ------------------------------------------------------------

{- |
   Module     : Text.Regex.XMLSchema.Generic
   Copyright  : Copyright (C) 2014- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt <uwe@fh-wedel.de>
   Stability  : stable
   Portability: portable

   Convenient functions for W3C XML Schema Regular Expression Matcher.
   For internals see 'Text.Regex.XMLSchema.Generic.Regex' and
   'Text.Regex.XMLSchema.Generic.Matching'

   Grammar can be found under <http://www.w3.org/TR/xmlschema11-2/#regexs>
-}

-- ------------------------------------------------------------

module Text.Regex.XMLSchema.Generic
    ( GenRegex
    , Regex
    , RegexText
    , RegexTextLazy
    , RegexByteString
    , RegexByteStringLazy
      
    , grep
    , grepExt
    , grepRE
    , grepREwithLineNum

    , match
    , matchExt
    , matchSubex

    , sed
    , sedExt

    , split
    , splitExt
    , splitSubex

    , tokenize
    , tokenizeExt
    , tokenize'
    , tokenizeExt'
    , tokenizeSubex

    , matchRE
    , matchSubexRE
    , sedRE
    , splitRE
    , splitSubexRE
    , tokenizeRE
    , tokenizeRE'
    , tokenizeSubexRE

    , mkZero
    , mkZero'
    , mkUnit
    , mkSym1
    , mkSymRng
    , mkWord
    , mkDot
    , mkStar
    , mkAll
    , mkAlt
    , mkElse
    , mkSeq
    , mkSeqs
    , mkRep
    , mkRng
    , mkOpt
    , mkDiff
    , mkIsect
    , mkExor
    , mkCompl
    , mkBr
    , mkBr'
    , isZero
    , errRegex

    , parseRegex
    , parseRegexExt
    , parseContextRegex
    )
where

import Text.Regex.XMLSchema.Generic.Matching
import Text.Regex.XMLSchema.Generic.Regex
import Text.Regex.XMLSchema.Generic.RegexParser

import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL

type Regex               = GenRegex String
type RegexText           = GenRegex T.Text
type RegexTextLazy       = GenRegex TL.Text
type RegexByteString     = GenRegex B.ByteString
type RegexByteStringLazy = GenRegex BL.ByteString

-- ------------------------------------------------------------
