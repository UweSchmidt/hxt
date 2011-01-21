-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.XmlCharParser
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   UTF-8 character parser and simple XML token parsers
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.XmlCharParser
    ( xmlChar                   -- xml char parsers
    , xmlNameChar
    , xmlNameStartChar
    , xmlNCNameChar
    , xmlNCNameStartChar
    , xmlLetter
    , xmlSpaceChar
    )
where

import Data.Char.Properties.XMLCharProps( isXmlChar
                                        , isXmlNameChar
                                        , isXmlNameStartChar
                                        , isXmlNCNameChar
                                        , isXmlNCNameStartChar
                                        , isXmlLetter
                                        , isXmlSpaceChar
                                        )

import Data.String.Unicode

import Text.ParserCombinators.Parsec

-- ------------------------------------------------------------
--
-- Char (2.2)
--

-- |
-- parse a single Unicode character

xmlChar                 :: GenParser Char state Unicode
xmlChar                 = satisfy isXmlChar <?> "legal XML character"
{-# INLINE xmlChar #-}

-- |
-- parse a XML name character

xmlNameChar             :: GenParser Char state Unicode
xmlNameChar             = satisfy isXmlNameChar <?> "legal XML name character"
{-# INLINE xmlNameChar #-}

-- |
-- parse a XML name start character

xmlNameStartChar        :: GenParser Char state Unicode
xmlNameStartChar        = satisfy isXmlNameStartChar <?> "legal XML name start character"
{-# INLINE xmlNameStartChar #-}

-- |
-- parse a XML NCName character

xmlNCNameChar           :: GenParser Char state Unicode
xmlNCNameChar           = satisfy isXmlNCNameChar <?> "legal XML NCName character"
{-# INLINE xmlNCNameChar #-}

-- |
-- parse a XML NCName start character

xmlNCNameStartChar      :: GenParser Char state Unicode
xmlNCNameStartChar      = satisfy isXmlNCNameStartChar <?> "legal XML NCName start character"
{-# INLINE xmlNCNameStartChar #-}

-- |
-- parse a XML letter character

xmlLetter               :: GenParser Char state Unicode
xmlLetter               = satisfy isXmlLetter <?> "legal XML letter"
{-# INLINE xmlLetter #-}

-- |
-- White Space (2.3)
--
-- end of line handling (2.11)
-- \#x0D and \#x0D\#x0A are mapped to \#x0A
-- is done in XmlInput before parsing
-- otherwise \#x0D in internal parsing, e.g. for entities would normalize,
-- would be transformed

xmlSpaceChar            :: GenParser Char state Char
xmlSpaceChar            = satisfy isXmlSpaceChar <?> "white space"
{-# INLINE xmlSpaceChar #-}

-- ------------------------------------------------------------

