-- ------------------------------------------------------------
--
-- GET for native http access
--
-- Version : $Id: GetHTTPNative.hs,v 1.2 2005/04/14 12:52:52 hxml Exp $

module Text.XML.HXT.IO.GetHTTPNative
    ( module Text.XML.HXT.IO.GetHTTPNative
    )

where

import Control.Arrow

import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.TypeDefs                ( Attributes )
import Text.XML.HXT.DOM.Util                    ( stringTrim )

import Text.XML.HXT.Arrow.XmlOptions            ( a_if_modified_since
						, a_if_unmodified_since
						)

import Text.XML.HXT.Parser.ProtocolHandlerUtil  ( parseContentType )

import Text.ParserCombinators.Parsec            ( parse )

import Data.Char                                ( isDigit
						)
import Data.Maybe                               ( fromJust
						)
import System.IO                                ( hPutStrLn
						, stderr
						)
import System.IO.Error                          ( ioeGetErrorString
						, try
						)
import Network.Browser                          ( Proxy(..)
						, BrowserAction
						, browse
						, defaultGETRequest
						, request
						, setOutHandler
						, setErrHandler
						, setProxy
						, setAllowRedirects
						, setMaxRedirects
						)
import Network.HTTP                             ( Header(..)
						, HeaderName(..)
						, Request(..)
						, Response(..)
						, httpVersion
						, replaceHeader
						)
import Network.Socket                           ( withSocketsDo
						)
import Network.URI                              ( URI
						, parseURIReference
						)

-- ------------------------------------------------------------
--
-- the native http protocol handler

-- ------------------------------------------------------------
--
-- the http protocol handler, haskell implementation

getCont         :: Bool -> String -> String -> Bool -> Attributes ->
		   IO (Either ([(String, String)], String)
                              ([(String, String)], String)
                      )
getCont strictInput proxy uri redirect options
    = do
      res <- try (getHttp False uri1 proxy redirect options)
      either processError processResponse res
    where
    uri1 = fromJust (parseURIReference uri)

    processError e
        = return $
          Left ( [ (transferStatus, "999")
                 , (transferMessage, "HTTP library error")
                 ]
               , "http error when requesting URI "
                 ++ show uri
                 ++ ": "
                 ++ ioeGetErrorString e
                 ++ " (perhaps server does not understand HTTP/1.1) "
               )

    processResponse response
        | rc >= 200 && rc < 300
            = if strictInput
              then length cs `seq` return res
              else                 return res

        | otherwise
            = return $
              Left ( rs
                   , "http error when accessing URI "
                     ++ show uri
                     ++ ": "
                     ++ show rc
                     ++ " "
                     ++ rr
                   )
        where
        rc  = convertResponseStatus $ rspCode response
        rr  = rspReason response
        res = Right (rs, cs)
        rs  = rst ++ rsh
        rst = [ (transferStatus, show rc)
              , (transferMessage,     rr)
              ]
        rsh = convertResponseHeaders response
        cs  = rspBody response

    getHttp             :: Bool -> URI -> String -> Bool -> Attributes -> IO (Response String)
    getHttp trc' uri' proxy' redirect' options'
        = withSocketsDo $
          browse ( do
                   sequence_ configHttp
                   (_ruri, rsp) <- request $ theRequest
                   return rsp
                 )
        where
        theRequest :: Request String
        theRequest
	    = configHeaders $ defaultGETRequest uri' 

        configHeaders :: Request String -> Request String
	configHeaders
	    = foldr (>>>) id . map (uncurry replaceHeader) . concatMap (uncurry setHOption) $ options

	configHttp
	    = setOutHandler (trcFct)
              : setErrHandler (trcFct)
              : ( if null proxy'
		  then return ()
		  else setProxy (Proxy proxy' Nothing)
		)
	      : setAllowRedirects redirect'
	      : concatMap (uncurry setOption) options'

        trcFct s
            | trc'
                = hPutStrLn stderr ("-- (5) http: " ++ s)
            | otherwise
                = return ()

    convertResponseStatus       :: (Int, Int, Int) -> Int
    convertResponseStatus (a, b, c)
        = 100 * a + 10 * b + c

    convertResponseHeaders      :: Response String -> [(String, String)]
    convertResponseHeaders r'
        = cvResponseCode (rspCode r')
          ++
          cvResponseReason (rspReason r')
          ++
          cvResponseHeaders (rspHeaders r')
        where
        cvResponseCode  :: (Int, Int, Int) -> [(String, String)]
        cvResponseCode st'
            = [ (transferStatus,        show (convertResponseStatus st'))
              , (transferVersion,       httpVersion)
              ]

        cvResponseReason        :: String -> [(String, String)]
        cvResponseReason r''
            = [ (transferMessage, (stringTrim r'')) ]

        cvResponseHeaders       :: [Header] -> [(String, String)]
        cvResponseHeaders
            = concatMap cvResponseHeader

        cvResponseHeader        :: Header -> [(String, String)]
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


setOption	:: String -> String -> [BrowserAction t ()]
setOption k v
    | k == "max-redirs"
      &&
      isIntArg v                        = [setMaxRedirects (Just $ read v)]
    | k == "max-redirs"
      &&
      null v                            = [setMaxRedirects Nothing]

    | otherwise	                        = []

setHOption	:: String -> String -> [(HeaderName, String)]
setHOption k v
    | k `elem` ["-A", "user-agent"]     = [(HdrUserAgent,         v)]
    | k `elem` ["-e", "referer"]        = [(HdrReferer,           v)]
    | k == a_if_modified_since          = [(HdrIfModifiedSince,   v)]
    | k == a_if_unmodified_since        = [(HdrIfUnmodifiedSince, v)]
    | otherwise                         = []


isIntArg        :: String -> Bool
isIntArg s      = not (null s) && all isDigit s

-- ------------------------------------------------------------
