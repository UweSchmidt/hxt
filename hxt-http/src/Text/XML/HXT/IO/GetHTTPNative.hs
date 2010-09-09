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
import Text.XML.HXT.DOM.Util                    ( stringTrim )
import Text.XML.HXT.Parser.ProtocolHandlerUtil  ( parseContentType )

import Text.ParserCombinators.Parsec            ( parse )

import Data.Maybe       ( fromJust
                        )
import System.IO        ( hPutStrLn
                        , stderr
                        )
import System.IO.Error  ( ioeGetErrorString
                        , try
                        )
import Network.Browser  ( Proxy(..)
                        , browse
                        , defaultGETRequest
                        , request
                        , setOutHandler
                        , setErrHandler
                        , setProxy
                        )
import Network.HTTP     ( Header(..)
                        , HeaderName(..)
                        , Response(..)
                        , httpVersion
                        )
import Network.Socket   ( withSocketsDo
                        )
import Network.URI      ( URI
                        , parseURIReference
                        )

-- ------------------------------------------------------------
--
-- the native http protocol handler

-- ------------------------------------------------------------
--
-- the http protocol handler, haskell implementation

getCont         :: Bool -> String -> String -> IO (Either ([(String, String)], String)
                                                          ([(String, String)], String)
                                                  )
getCont strictInput proxy uri
    = do
      res <- try (getHttp False uri1 proxy)
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

    getHttp             :: Bool -> URI -> String -> IO (Response String)
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

        setProxy' ""    = return ()
        setProxy' p     = setProxy (Proxy p Nothing)

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


-- ------------------------------------------------------------
