-- ------------------------------------------------------------

{- 
   Module     : Text.XML.HXT.Parser.ProtocolHandlerUtil
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Protocol handler utility functions

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.ProtocolHandlerUtil
    ( parseContentType
    )

where

import Text.XML.HXT.DOM.XmlKeywords

import Text.XML.HXT.DOM.Util	( stringToUpper
				, stringTrim
				)

import qualified Text.ParserCombinators.Parsec as P

-- ------------------------------------------------------------

-- |
-- Try to extract charset spec from Content-Type header
-- e.g. \"text\/html; charset=ISO-8859-1\"
--
-- Sometimes the server deliver the charset spec in quotes
-- these are removed

parseContentType	:: P.Parser [(String, String)]
parseContentType
    = P.try ( do
	      mimeType <- ( do
			    mt <- P.many (P.noneOf ";")
			    rtMT mt
			  )
	      charset  <- ( do
			    P.char ';'
			    P.many  (P.oneOf " \t'")
			    P.string "charset="
			    P.option '"' (P.oneOf "\"'")
			    cs <- P.many1 (P.noneOf "\"'")
			    return [ (transferEncoding, stringToUpper cs) ]
			  )
	      return (mimeType ++ charset)
	    )
      P.<|>
      ( do
	mt <- P.many (P.noneOf ";")
	rtMT mt
      )
    where
    rtMT mt = return [ (transferMimeType, stringTrim mt) ]

-- ------------------------------------------------------------

