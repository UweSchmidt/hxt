-- ------------------------------------------------------------

{- |
   Module     : Data.String.EncodingNames
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Constants for character encodings

-}

-- ------------------------------------------------------------

module Data.String.EncodingNames
where

-- ------------------------------------------------------------
--
-- encoding names

isoLatin1
  , iso8859_1, iso8859_2, iso8859_3, iso8859_4, iso8859_5
  , iso8859_6, iso8859_7, iso8859_8, iso8859_9, iso8859_10
  , iso8859_11, iso8859_13, iso8859_14, iso8859_15, iso8859_16
  , usAscii, ucs2, utf8, utf16, utf16be, utf16le, unicodeString :: String

isoLatin1       = iso8859_1
iso8859_1       = "ISO-8859-1"
iso8859_2       = "ISO-8859-2"
iso8859_3       = "ISO-8859-3"
iso8859_4       = "ISO-8859-4"
iso8859_5       = "ISO-8859-5"
iso8859_6       = "ISO-8859-6"
iso8859_7       = "ISO-8859-7"
iso8859_8       = "ISO-8859-8"
iso8859_9       = "ISO-8859-9"
iso8859_10      = "ISO-8859-10"
iso8859_11      = "ISO-8859-11"
iso8859_13      = "ISO-8859-13"
iso8859_14      = "ISO-8859-14"
iso8859_15      = "ISO-8859-15"
iso8859_16      = "ISO-8859-16"
usAscii         = "US-ASCII"
ucs2            = "ISO-10646-UCS-2"
utf8            = "UTF-8"
utf16           = "UTF-16"
utf16be         = "UTF-16BE"
utf16le         = "UTF-16LE"
unicodeString   = "UNICODE"

-- ------------------------------------------------------------
