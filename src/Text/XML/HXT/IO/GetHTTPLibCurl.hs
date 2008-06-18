-- ------------------------------------------------------------
--
-- GET for http access with curl
--
-- Version : $Id: GetHTTPCurl.hs,v 1.3 2005/04/14 12:52:52 hxml Exp $

module Text.XML.HXT.IO.GetHTTPLibCurl
    ( getCont
    )

where
import Control.Concurrent.MVar
import Control.Monad
    ( when )

import Data.Char
    ( isDigit
    )
import Data.List
    ( isPrefixOf
    )

import Network.Curl

import System.IO
import System.IO.Unsafe
    ( unsafePerformIO
    )

import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.XmlOptions
    ( isTrueValue
    )
import Text.XML.HXT.Parser.ProtocolHandlerUtil
    ( parseContentType
    )
{-
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

import System.PipeOpen
    ( popen
    )
-}

-- ------------------------------------------------------------
--
-- the global flag for initializing curl in the 1. call

isInitCurl	:: MVar Bool
isInitCurl	= unsafePerformIO $ newMVar False

initCurl	:: IO ()
initCurl
    = do
      i <- takeMVar isInitCurl
      when (not i) ( do
		     curl_global_init 3
		     return ()
		   )
      putMVar isInitCurl True

-- ------------------------------------------------------------
--
-- the http protocol handler implemented by calling external program curl

getCont		:: [(String, String)] -> String -> IO (Either String ([(String, String)], String))
getCont options uri -- curlOptions uri proxy
    = do
      initCurl
      resp <- curlGetResponse uri curlOptions
      return $ evalResponse resp
    where
    curlOptions = concatMap (uncurry copt) options ++ defaultOptions
    defaultOptions
	= [ CurlFailOnError False
	  , CurlHeader True
	  , CurlNoProgress True
	  ]
    evalResponse r
	| rc /= CurlOK
	    = Left ( "http error when requesting URI "
		     ++ show uri
		     ++ ": (curl return code=" ++ show rc ++ ") "
		   )
	| rs < 200 && rs >= 300
	    = Left ( "http error when accessing URI "
		     ++ show uri
		     ++ ": "
		     ++ show rsl
		   )
	| otherwise
	    = Right ( headers, respBody r
		    )
	where
	headers
	    = map (\ (k, v) -> (httpPrefix ++ k, v)) rsh
	      ++ statusLine (words rsl)

	statusLine (vers : code : msg)
	    = [ (transferVersion, vers)
	      , (transferMessage, unwords msg)
	      , (transferStatus, code)
	      ]
        statusLine _
	    = []

	rc  = respCurlCode    r
	rs  = respStatus      r
	rsl = respStatusLine  r
	rsh = respHeaders     r

-- ------------------------------------------------------------

copt	:: String -> String -> [CurlOption]
copt k v
    | "curl" `isPrefixOf` k
	= opt2copt (drop 4 k) v

    | k == a_proxy
	= ( if isIntArg v
	    then [CurlProxyPort $ read pp]
	    else []
	  )
          ++ [CurlProxy ph]

    | k == a_options_curl
	= curlOptionString v

    | otherwise
	= []
    where
    (ph,pp') = span (/=':') v
    pp       = drop 1 pp'

opt2copt	:: String -> String -> [CurlOption]
opt2copt k v
    | k `elem` ["-A", "--user-agent"]	= [CurlUserAgent v]
    | k `elem` ["-b", "--cookie"]	= [CurlCookie v]
    | k == "--connect-timeout"
      &&
      isIntArg v			= [CurlConnectTimeout (read v)]
    | k == "--crlf"			= [CurlCRLF $ isTrue v]
    | k `elem` ["-d", "--data"]		= [CurlPostFields [v]]
    | k `elem` ["-e", "--referer"]	= [CurlReferer v]
    | k `elem` ["-H", "--header"]	= [CurlHttpHeaders [v]]
    | k == "--ignore-content-length"	= [CurlIgnoreContentLength $ isTrue v]
    | k `elem` ["-I", "--head"]		= [CurlNoBody $ isTrue v]
    | otherwise				= []
    where

isTrue		:: String -> Bool
isTrue s  	= null s || isTrueValue s

isIntArg	:: String -> Bool
isIntArg s 	= not (null s) && all isDigit s

curlOptionString	:: String -> [CurlOption]
curlOptionString
    = concatMap (uncurry copt) . opts . words
    where
    opts l
	| null l			= []
	| not ("-" `isPrefixOf` k)	= opts l1		-- k not an option: ignore
	| null l1			= opts (k:"":l1)	-- last option
	| "-" `isPrefixOf` v		= (k, "") : opts (v:l)	-- k option without arg
	| otherwise			= (k, v) : opts l2	-- k with value
	where
	(k:l1) = l
	(v:l2) = l1

{-
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

parseHttpResponse		:: Parser (Int, [(String, String)], String)
parseHttpResponse
    = do
      allResponses <- many1 parse1Response
      let (rc, rh, rhs) = last allResponses
      content <- getInput
      return (rc, rh ++ rhs, content)
    where

    parse1Response
	= do
	  (rc, rh) <- parseResp
	  rhs      <- parseHeaders
	  return (rc, rh, rhs)

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
-}
-- ------------------------------------------------------------
