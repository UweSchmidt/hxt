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
    ( XParser
    , SimpleXParser
    , XPState(..)
    , withNormNewline
    , withoutNormNewline

    , xmlChar                   -- xml char parsers
    , xmlNameChar
    , xmlNameStartChar
    , xmlNCNameChar
    , xmlNCNameStartChar
    , xmlLetter
    , xmlSpaceChar
    , xmlCRLFChar
    )
where

import           Data.Char.Properties.XMLCharProps (isXmlCharCR, isXmlLetter,
                                                    isXmlNCNameChar,
                                                    isXmlNCNameStartChar,
                                                    isXmlNameChar,
                                                    isXmlNameStartChar,
                                                    isXmlSpaceCharCR)

import           Data.String.Unicode

import           Text.ParserCombinators.Parsec

-- ------------------------------------------------------------

type XParser s a        = GenParser Char (XPState s) a
type SimpleXParser a    = XParser () a

data XPState s          = XPState
    { xps_normalizeNewline :: ! Bool
    , xps_userState        :: s
    }

withNormNewline         :: a -> XPState a
withNormNewline x       = XPState True x

withoutNormNewline      :: a -> XPState a
withoutNormNewline x    = XPState False x

-- ------------------------------------------------------------
--
-- Char (2.2)
--

-- |
-- parse a single Unicode character

xmlChar                 :: XParser s Unicode
xmlChar                 = ( satisfy isXmlCharCR
                            <|>
                            xmlCRLFChar
                          )
                          <?> "legal XML character"
{-# INLINE xmlChar #-}

-- |
-- parse a XML name character

xmlNameChar             :: XParser s Unicode
xmlNameChar             = satisfy isXmlNameChar <?> "legal XML name character"
{-# INLINE xmlNameChar #-}

-- |
-- parse a XML name start character

xmlNameStartChar        :: XParser s Unicode
xmlNameStartChar        = satisfy isXmlNameStartChar <?> "legal XML name start character"
{-# INLINE xmlNameStartChar #-}

-- |
-- parse a XML NCName character

xmlNCNameChar           :: XParser s Unicode
xmlNCNameChar           = satisfy isXmlNCNameChar <?> "legal XML NCName character"
{-# INLINE xmlNCNameChar #-}

-- |
-- parse a XML NCName start character

xmlNCNameStartChar      :: XParser s Unicode
xmlNCNameStartChar      = satisfy isXmlNCNameStartChar <?> "legal XML NCName start character"
{-# INLINE xmlNCNameStartChar #-}

-- |
-- parse a XML letter character

xmlLetter               :: XParser s Unicode
xmlLetter               = satisfy isXmlLetter <?> "legal XML letter"
{-# INLINE xmlLetter #-}

-- |
-- White Space (2.3)
--
-- end of line handling (2.11) will be done before or with 'xmlCRLFChar' parser

xmlSpaceChar            :: XParser s Char
xmlSpaceChar            = ( satisfy isXmlSpaceCharCR
                            <|>
                            xmlCRLFChar
                          )
                          <?> "white space"
{-# INLINE xmlSpaceChar #-}

-- |
-- White Space Normalization
--
-- end of line handling (2.11)
-- \#x0D and \#x0D\#x0A are mapped to \#x0A

xmlCRLFChar            :: XParser s Char
xmlCRLFChar            = ( do
                           _ <- char '\r'
                           s <- getState
                           if xps_normalizeNewline s
                              then option '\n' (char '\n')
                              else return '\r'
                         )
                         <?> "newline"

-- ------------------------------------------------------------

