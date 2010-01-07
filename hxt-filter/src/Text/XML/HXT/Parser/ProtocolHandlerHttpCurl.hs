-- ------------------------------------------------------------
--
-- protocol handler functions for native http access

module Text.XML.HXT.Parser.ProtocolHandlerHttpCurl
    ( getHttpContentsWithCurl
    )

where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.DOM.XmlState

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

import qualified Text.ParserCombinators.Parsec
    ( try
    )

import Network.URI
    ( URI
    )

import System.PipeOpen
    ( popen
    )

import Data.Char
    ( toLower
    )

-- ------------------------------------------------------------
--
-- the http protocol handler implemented by calling external program curl

getHttpContentsWithCurl	:: URI -> XmlStateFilter a
getHttpContentsWithCurl uri n
    = do
      trace 2 ( "getHttpContentWithCurl: reading from URL " ++ show uri )

      proxy       <- getSysParam a_proxy
      curlOptions <- getSysParam a_options_curl

      let allArgs = args ++ proxyArgs proxy ++ words curlOptions

      trace 4 ( "getHttpContentWithCurl: running " ++ show (unwords (cmd : allArgs)) )

      (res, errs, rc) <- io $ popen cmd allArgs

      trace 4 ( "getHttpContentWithCurl: PID:    " ++ show rc   )
      trace 4 ( "getHttpContentWithCurl: stdin:  " ++ show res  )
      trace 4 ( "getHttpContentWithCurl: stderr: " ++ show errs )

      if rc /= 0
	 then addFatal ( "http error when requesting URI (rc=" ++ show rc ++ ") "
			 ++ show uri
			 ++ ": "
			 ++ errs
		       ) n
	 else let
	      (st, al, contents) = parseResponse res
	      in
	      liftMf ( addAttrl (const al)
		       .>
		       replaceChildren (xtext contents)
		     )
	      .>> ( if st >= 200 && st < 300
		    then thisM
		    else
		    addFatal ( "http error when accessing URI "
			       ++ show uri
			       ++ ": "
			       ++ show st
			       ++ " "
			       ++ (valueOf transferMessage $ newRoot al)
			     )
		  )
	      $ n
    where
    cmd  = "curl"
    args = [ "--silent"
	   , "--show-error"
	   , "--dump-header", "-"
	   , show uri
	   ]
    proxyArgs ""
	= []
    proxyArgs prx
	= [ "--proxy", prx ]

parseResponse	:: String -> (Int, XmlTrees, String)
parseResponse inp
    = case (parse parseHttpResponse "HTTP Header" inp) of
      Right res -> res
      Left  _   -> (999, xattr transferMessage "illegal HTTP response", inp)

-- ------------------------------------------------------------
--
-- parsers for HTTP response

parseHttpResponse		:: Parser (Int, XmlTrees, String)
parseHttpResponse
    = do
      allResponses <- many1
		      ( do
			(rc, rh) <- parseResp
			rhs      <- parseHeaders
			return (rc, rh, rhs)
		      )
      let (rc, rh, rhs) = last allResponses
      content  <- getInput
      return (rc, rh ++ rhs, content)
    where

    crlf		:: Parser ()
    crlf
	= do
	  _ <- ( Text.ParserCombinators.Parsec.try (string "\r\n") <|> string "\n" )
	  return ()

    parseResp		:: Parser (Int, XmlTrees)
    parseResp
	= do
	  vers <- ( do
		    http <- string "HTTP/"
		    mav <- many1 digit
		    _ <- char '.'
		    miv <- many1 digit
		    return (http ++ mav ++ "." ++ miv)
		  )
	  spaces
	  ds <- many1 digit
	  spaces
	  reason <- manyTill anyChar crlf
	  return ( read ds,
		   xattr transferMessage reason ++ xattr transferVersion vers
		 )

    parseHeaders	:: Parser XmlTrees
    parseHeaders
	= ( do
	    crlf
	    return []
	  )
	  <|>
	  ( do
	    header1 <- parse1Header
	    rest    <- parseHeaders
	    return $ header1 ++ rest
	  )
	  <|>
	  ( do
	    return $ xattr (httpPrefix ++ "IllegalHeaders") ""
	  )

    parse1Header	:: Parser XmlTrees
    parse1Header
	= do
	  header <- manyTill anyChar (char ':')
	  spaces
	  value  <- manyTill anyChar crlf
	  let ct = parseCT header value
	  return $ ct ++ xattr (httpPrefix ++ header) value
	where
	parseCT	:: String -> String -> XmlTrees
	parseCT h v
	    | map toLower h == "content-type"
		= ( case (parse parseContentType h v) of
		    Right res -> concatMap (uncurry xattr) res
		    Left  _   -> []
		  )
	    | otherwise
		= []

-- ------------------------------------------------------------
