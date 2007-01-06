-- ------------------------------------------------------------
--
-- GET for http access with curl
--
-- Version : $Id: GetHTTPCurl.hs,v 1.3 2005/04/14 12:52:52 hxml Exp $

module Text.XML.HXT.IO.GetHTTPCurl
    ( getCont
    )

where

import Text.XML.HXT.DOM.XmlKeywords

import Text.XML.HXT.Parser.ProtocolHandlerUtil
			( parseContentType
			)

import Text.ParserCombinators.Parsec
			( Parser
			, parse
			, anyChar
			, char
			, digit
			, getInput
			, many1
			, manyTill
			, spaces
			, string
			, (<|>)
			)
import qualified Text.ParserCombinators.Parsec as Parsec (try)	-- try

import System.PipeOpen	( popen
			)

import Data.Char	( toLower
			)

import IO

-- ------------------------------------------------------------
--
-- the http protocol handler implemented by calling external program curl

getCont		:: String -> String -> String -> IO (Either String ([(String, String)], String))
getCont curlOptions uri proxy
    = do
      (res, errs, rc) <- popen cmd allArgs
      if rc /= 0
	 then return $ Left ( "http error when requesting URI "
			      ++ show uri
			      ++ ": (rc=" ++ show rc ++ ") "
			      ++ errs
			    )
	 else let
	      (st, al, contents) = parseResponse res
	      in
	      if st >= 200 && st < 300
		 then return $ Right (al, contents)
		 else return $ Left ( "http error when accessing URI "
				      ++ show uri
				      ++ ": "
				      ++ show st
				    )
    where
    cmd		= "curl"

    allArgs	= args
		  ++ proxyArgs proxy
		  ++ words curlOptions

    args 	= [ "--silent"
		  , "--show-error"
		  , "--dump-header", "-"
		  , uri
		  ]
    proxyArgs ""
		= []
    proxyArgs prx
		= [ "--proxy", prx ]

parseResponse	:: String -> (Int, [(String, String)], String)
parseResponse inp
    = ( either
          ( const (999, [(transferMessage, "illegal HTTP response")], inp))
          id
	.
	parse parseHttpResponse "HTTP Header"
      ) inp

-- ------------------------------------------------------------
--
-- parsers for HTTP response

parseHttpResponse		:: Parser (Int, [(String, String)], String)
parseHttpResponse
    = do
      (rc, rh) <- parseResp
      rhs      <- parseHeaders
      content  <- getInput
      return (rc, rh ++ rhs, content)
    where
    crlf		:: Parser ()
    crlf
	= do
	  ( Parsec.try (string "\r\n") <|> string "\n" )
	  return ()

    parseResp		:: Parser (Int, [(String, String)])
    parseResp
	= do
	  vers <- ( do
		    http <- string "HTTP/"
		    mav <- many1 digit
		    char '.'
		    miv <- many1 digit
		    return (http ++ mav ++ "." ++ miv)
		  )
	  spaces
	  ds <- many1 digit
	  spaces
	  reason <- manyTill anyChar crlf
	  return ( read ds,
		   [(transferMessage, reason), (transferVersion, vers)]
		 )

    parseHeaders	:: Parser [(String, String)]
    parseHeaders
	= ( do
	    crlf
	    return []
	  )
	  <|>
	  ( do
	    header1 <- parse1Header
	    rest    <- parseHeaders
	    return (header1 ++ rest)
	  )
	  <|>
	  ( do
	    return [(httpPrefix ++ "IllegalHeaders", "")]
	  )

    parse1Header	:: Parser [(String, String)]
    parse1Header
	= do
	  header <- manyTill anyChar (char ':')
	  spaces
	  value  <- manyTill anyChar crlf
	  let ct = parseCT header value
	  return $ ct ++ [(httpPrefix ++ header, value)]
	where
	parseCT	:: String -> String -> [(String, String)]
	parseCT h v
	    | map toLower h == "content-type"
		= either (const []) id . parse parseContentType h $ v
	    | otherwise
		= []

-- ------------------------------------------------------------
