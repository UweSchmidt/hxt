-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.IO.GetHTTPNative
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   HXT interface for native HTTP access via package HTTP
-}

-- ------------------------------------------------------------

module Text.XML.HXT.IO.GetHTTPNative
    ( module Text.XML.HXT.IO.GetHTTPNative
    )

where

import Control.Arrow
import Control.Exception                        ( try )

import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.TypeDefs                ( Attributes )
import Text.XML.HXT.DOM.Util                    ( stringTrim )

import Text.XML.HXT.Arrow.XmlOptions            ( a_if_modified_since
						, a_if_unmodified_since
						)

import Text.XML.HXT.Parser.ProtocolHandlerUtil  ( parseContentType )

import Text.ParserCombinators.Parsec            ( parse )

import qualified Data.ByteString.Lazy           as B

import Data.Char                                ( isDigit
						)
import Data.Int                                 ( Int64 )
import Data.List                                ( isPrefixOf
                                                )
import Data.Maybe
import System.IO                                ( hPutStrLn
						, stderr
						)
import System.IO.Error                          ( ioeGetErrorString
						)

import Network.Browser                          ( Proxy(..)
						, BrowserAction
						, browse
						, defaultGETRequest_
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

-- import qualified Debug.Trace as T
-- ------------------------------------------------------------
--
-- the native http protocol handler

-- ------------------------------------------------------------
--
-- the http protocol handler, haskell implementation

getCont         :: Bool -> String -> String -> Bool -> Attributes ->
		   IO (Either ([(String, String)],       String)
                              ([(String, String)], B.ByteString)
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
        | ( (rc >= 200 && rc < 300)
            ||
            rc == 304		-- not modified is o.k., this rc only occurs together with if-modified-since
          )            
          &&
          fileSizeOK
            = do
              if strictInput
                then B.length cs `seq` return res
                else                   return res
        
        | not fileSizeOK
            = return $
              ers "999 max-filesize exceeded"
        
        | otherwise
            = return $
              ers (show rc ++ " " ++ rr)
        where
        fileSizeOK = case getCurlMaxFileSize options of
                     Nothing -> True
                     Just mx -> B.length cs <= mx
        rc  = convertResponseStatus $ rspCode response
        rr  = rspReason response
        res   = Right (rs, cs)
        ers e = Left (rs, "http error when accessing URI " ++ show uri ++ ": " ++ e)
        rs  = rst ++ rsh
        rst = [ (transferStatus, show rc)
              , (transferMessage,     rr)
              ]
        rsh = convertResponseHeaders response
        cs  = rspBody response

    getHttp             :: Bool -> URI -> String -> Bool -> Attributes -> IO (Response B.ByteString)
    getHttp trc' uri' proxy' redirect' options'
        = withSocketsDo $
          browse ( do
                   sequence_ configHttp
                   (_ruri, rsp) <- request $ theRequest
                   return rsp
                 )
        where
        theRequest :: Request B.ByteString
        theRequest
	    = configHeaders $ defaultGETRequest_ uri' 

        configHeaders :: Request B.ByteString -> Request B.ByteString
	configHeaders
	    = foldr (>>>) id . map (uncurry replaceHeader) . concatMap (uncurry setHOption) $ options'

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

    convertResponseHeaders      :: Response B.ByteString -> [(String, String)]
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
setOption k0 v
    | k == "max-redirs"
      &&
      isIntArg v                        = [setMaxRedirects (Just $ read v)]
    | k == "max-redirs"
      &&
      null v                            = [setMaxRedirects Nothing]

    | otherwise	                        = []
  where
    k = dropCurlPrefix k0
    
curlPrefix	:: String
curlPrefix      = "curl--"

dropCurlPrefix :: String -> String
dropCurlPrefix k
  | curlPrefix `isPrefixOf` k  = drop (length curlPrefix) k
  | otherwise                  = k

setHOption	:: String -> String -> [(HeaderName, String)]
setHOption k0 v
    | k `elem` [ "-A"
               , "user-agent"
               , "curl--user-agent"
               ]                        = [(HdrUserAgent,         v)]
    | k `elem` [ "-e"
               , "referer"]             = [(HdrReferer,           v)]
    | k == a_if_modified_since          = [(HdrIfModifiedSince,   v)]
    | k == a_if_unmodified_since        = [(HdrIfUnmodifiedSince, v)]
    | otherwise                         = []
  where
    k = dropCurlPrefix k0

isIntArg        :: String -> Bool
isIntArg s      = not (null s) && all isDigit s

getCurlMaxFileSize :: Attributes -> Maybe Int64
getCurlMaxFileSize options
  = (\ s -> if isIntArg s 
            then Just (read s) 
            else Nothing
    )
    . fromMaybe ""
    . lookup (curlPrefix ++ "max-filesize") 
    $ options
    
-- ------------------------------------------------------------
