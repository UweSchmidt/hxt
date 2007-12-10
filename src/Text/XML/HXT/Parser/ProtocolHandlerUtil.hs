-- ------------------------------------------------------------
--
-- protocol handler utility functions
--
-- Version : $Id: ProtocolHandlerUtil.hs,v 1.4 2005/04/14 12:52:53 hxml Exp $

module Text.XML.HXT.Parser.ProtocolHandlerUtil
    ( parseContentType
    )

where

import Text.XML.HXT.DOM.XmlKeywords

import Text.XML.HXT.DOM.Util
    ( stringToUpper
    )

import qualified Text.ParserCombinators.Parsec as P

-- ------------------------------------------------------------
--
-- try to extract charset spec from Content-Type header
-- e.g. "text/html; charset=ISO-8859-1"

-- sometimes the server deliver the charset spec in quotes
-- these are removed

parseContentType	:: P.Parser [(String, String)]
parseContentType
    = P.try ( do
	      mimeType <- ( do
			    mt <- P.many (P.noneOf ";")
			    return [ (transferMimeType, mt) ]
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
	mimeType <- P.many P.anyChar
	return [ (transferMimeType, mimeType) ]
      )

-- ------------------------------------------------------------

