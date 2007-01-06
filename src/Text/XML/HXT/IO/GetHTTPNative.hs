-- ------------------------------------------------------------
--
-- GET for native http access
--
-- Version : $Id: GetHTTPNative.hs,v 1.2 2005/04/14 12:52:52 hxml Exp $

module Text.XML.HXT.IO.GetHTTPNative
    ( module Text.XML.HXT.IO.GetHTTPNative
    )

where

import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.Util			( stringTrim )
import Text.XML.HXT.Parser.ProtocolHandlerUtil	( parseContentType )

import Text.ParserCombinators.Parsec		( parse )

import Data.Maybe	( fromJust
			)
import System.IO	( hPutStrLn
			, stderr
			)
import System.IO.Error	( ioeGetErrorString
			, try
			)
import Network.Browser	( Proxy(..)
			, browse
			, defaultGETRequest
			, request
			, setOutHandler
			, setErrHandler
			, setProxy
			)
import Network.HTTP	( Header(..)
			, HeaderName(..)
			, Response(..)
			, httpVersion
			)
import Network.Socket	( withSocketsDo
			)
import Network.URI	( URI
			, parseURIReference
			)

-- ------------------------------------------------------------
--
-- the native http protocol handler

-- ------------------------------------------------------------
--
-- the http protocol handler, haskell implementation

getCont		:: String -> String -> IO (Either String ([(String, String)], String))
getCont uri proxy
    = do
      res <- try (getHttp False uri1 proxy)
      either processError processResponse res
    where
    uri1 = fromJust (parseURIReference uri)

    processError e
	= return $ Left ( "http error when requesting URI "
			  ++ show uri
			  ++ ": "
			  ++ ioeGetErrorString e
			  ++ " (perhaps server does not understand HTTP/1.1) "
			)

    processResponse response
	| st >= 200 && st < 300
	    = return $ Right (al, cs)

	| otherwise
	    = return $ Left ( "http error when accessing URI "
			      ++ show (show uri)
			      ++ ": "
			      ++ show st
			      ++ " "
			      ++ rspReason response
			    )
	where
	al = convertResponseHeaders response
	cs = rspBody response
	st = convertResponseStatus (rspCode response)

    getHttp		:: Bool -> URI -> String -> IO Response
    getHttp trc' uri' proxy'
	= withSocketsDo $
	  browse ( do
		   setOutHandler (trcFct)
		   setErrHandler (trcFct)

		   setProxy' proxy'
		   (_ruri, rsp) <- request rq
		   return rsp
		 )
	where
	trcFct s
	    | trc'
		= hPutStrLn stderr ("-- (5) http: " ++ s)
	    | otherwise
		= return ()

	rq = defaultGETRequest uri'
	
	setProxy' ""	= return ()
	setProxy' p	= setProxy (Proxy p Nothing)

    convertResponseStatus	:: (Int, Int, Int) -> Int
    convertResponseStatus (a, b, c)
	= 100 * a + 10 * b + c

    convertResponseHeaders	:: Response -> [(String, String)]
    convertResponseHeaders r'
	= cvResponseCode (rspCode r')
	  ++
	  cvResponseReason (rspReason r')
          ++
	  cvResponseHeaders (rspHeaders r')
	where
	cvResponseCode	:: (Int, Int, Int) -> [(String, String)]
	cvResponseCode st'
	    = [ (transferStatus,	show (convertResponseStatus st'))
	      , (transferVersion,	httpVersion)
	      ]

	cvResponseReason	:: String -> [(String, String)]
	cvResponseReason r''
	    = [ (transferMessage, (stringTrim r'')) ]

	cvResponseHeaders	:: [Header] -> [(String, String)]
	cvResponseHeaders
	    = concatMap cvResponseHeader

	cvResponseHeader	:: Header -> [(String, String)]
	cvResponseHeader (Header name value)
	    | name == HdrContentType
		= ( case (parse parseContentType (show HdrContentType) value) of
		    Right res -> res
		    Left  _   -> []
		  )
                  ++
		  addHttpAttr
	    | otherwise
		= addHttpAttr
	    where
	    addHttpAttr = [ (httpPrefix ++ (show name), value) ]


-- ------------------------------------------------------------
