-- ------------------------------------------------------------
--
-- protocol handler functions for native http access
--

module Text.XML.HXT.Parser.ProtocolHandlerHttpNative
    ( getHttpContentsWithHttp
    )

where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.DOM.XmlState

import Text.XML.HXT.Parser.ProtocolHandlerUtil
    ( parseContentType
    )

import Text.XML.HXT.DOM.Util
    ( stringTrim
    )

import Text.ParserCombinators.Parsec
    ( parse
    )

import System.IO
import System.IO.Error                  ( ioeGetErrorString
                                        )

import Control.Exception                ( try
                                        )

import Network.URI
    ( URI
    )

import Network.Socket
    ( withSocketsDo
    )

import Network.HTTP             -- http modules
import Network.Browser

-- ------------------------------------------------------------
--
-- the native http protocol handler

-- ------------------------------------------------------------
--
-- the http protocol handler, haskell implementation

getHttpContentsWithHttp :: URI -> XmlStateFilter a
getHttpContentsWithHttp uri n
    = do
      traceLevel <- getTraceLevel
      trace 2 ("getHttpContent: reading from URL " ++ show uri)
      proxy <- getSysParam a_proxy
      res <- io $  try (getHttp traceLevel uri proxy)
      case res of
               Left e
                   -> readErr ( "http error when requesting URI "
                                ++ show uri
                                ++ ": "
                                ++ ioeGetErrorString (e :: IOError)
                                ++ " (perhaps server does not understand HTTP/1.1) "
                              )
               Right response
                   -> let
                      al = convertResponseHeaders response
                      cs = xtext (rspBody response)
                      st = convertResponseStatus (rspCode response)
                      in
                      if st >= 200 && st < 300
                         then
                         return $ (addAttrl (const al) .> replaceChildren cs) n
                         else
                         readErr ( "http error when accessing URI "
                                   ++ show (show uri)
                                   ++ ": "
                                   ++ show st
                                   ++ " "
                                   ++ rspReason response
                                 )
    where
    readErr msg = addFatal msg n

    getHttp             :: Int -> URI -> String -> IO (Response String)
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
            | trc' >= 5
                = hPutStrLn stderr ("-- (" ++ show trc' ++ ") http: " ++ s)
            | otherwise
                = return ()

        rq = defaultGETRequest uri'

        setProxy' ""    = return ()
        setProxy' p     = setProxy (Proxy p Nothing)

    convertResponseStatus       :: (Int, Int, Int) -> Int
    convertResponseStatus (a, b, c)
        = 100 * a + 10 * b + c

    convertResponseHeaders      :: Response String -> XmlTrees
    convertResponseHeaders r'
        = cvResponseCode (rspCode r')
          ++
          cvResponseReason (rspReason r')
          ++
          cvResponseHeaders (rspHeaders r')
        where
        cvResponseCode  :: (Int, Int, Int) -> XmlTrees
        cvResponseCode st'
            = xattr transferStatus (show (convertResponseStatus st'))
              ++
              xattr transferVersion httpVersion

        cvResponseReason        :: String -> XmlTrees
        cvResponseReason r''
            = xattr transferMessage (stringTrim r'')

        cvResponseHeaders       :: [Header] -> XmlTrees
        cvResponseHeaders
            = concatMap cvResponseHeader

        cvResponseHeader        :: Header -> XmlTrees
        cvResponseHeader (Header name value)
            | name == HdrContentType
                = ( case (parse parseContentType (show HdrContentType) value) of
                    Right res -> concatMap (uncurry xattr) res
                    Left  _   -> []
                  )
                  ++
                  addHttpAttr
            | otherwise
                = addHttpAttr
            where
            addHttpAttr = xattr (httpPrefix ++ (show name)) value


-- ------------------------------------------------------------
