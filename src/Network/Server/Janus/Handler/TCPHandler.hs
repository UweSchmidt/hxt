-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Handler.TCPHandler
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: TCPHandler.hs, v1.2 2007/04/29 00:00:00 janus Exp $

   Janus TCP Binding

   A Handler to accept TCP connections.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Handler.TCPHandler
    (
    -- tcp handler
      tcpHandler
    )
where

import Control.Concurrent

-- import Data.ByteString as BStr (hGet, hPut, length) -- length
import Data.Char
import Data.Maybe

import Foreign.Marshal.Array
import Foreign.Ptr

import Network
import Network.URI
-- import Network.BSD ( getProtocolNumber )
-- import Network.Socket

-- import System.Posix.Signals as OS hiding (Handler)
import System.Posix hiding (Handler)
import System.IO

import Text.XML.HXT.Arrow

import Network.Server.Janus.Messaging
import Network.Server.Janus.Core as Shader
import Network.Server.Janus.Transaction as TA
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

import Network.Server.HWS.Util hiding (emptyLine, accept)

chunksize :: Int
chunksize = 32768

{- |
A HandlerCreator mapping the Handler configuration to newHandler,
which operates on explicit configuration parameters.
-}
tcpHandler :: HandlerCreator
tcpHandler = proc (conf, shader) -> do
    let handler = proc _ -> do
        port    <- getValDef _config_port "80"                     -<  conf
        -- newHandler (read port) shader                           -<< ()
        tcpHandler' (read port) shader                             -<< ()
    returnA                                                        -<  handler

-- Nachrichtenkanal je Handler
-- TERMINATE Message
-- TCPHandler: jeden Sockel registrieren, beim Close wieder l�chen, Listener-Thread fr TERMINATE
tcpHandler' :: Integer -> Shader -> Handler
tcpHandler' port shader =
    proc _ -> do
        lock    <- arrIO $ newMVar                                      -<  ()
        sock    <- arrIO0 $ (withSocketsDo $ 
                        installHandler sigPIPE Ignore Nothing
                        >>
                        listenOn (PortNumber (fromInteger port)))       -<  ()
        acceptRequest sock lock                                         -<< ()
        -- sClose sock
    where
        acceptRequest sock lock =
            proc _ -> do
                -- arrIO $ takeMVar -<  lock
                (hnd, hname, pnum) <- arrIO $ Network.accept                    -<  sock
                processA
                    (finallyA
                        (processHandle (hname, pnum) hnd shader lock `orElse` (arrIO0 $ putStrLn "Aborted."))
                        (finallyA 
                                (exceptZeroA_ $ hClose hnd)
                                (this) --arrIO0 $ putMVar lock ())
                                )
                        )                                                       -<< ()
                acceptRequest sock lock                                         -<  ()
        processHandle (hname, pnum) hnd shader' lock =
            proc _ -> do
                exceptZeroA_ (hSetBuffering hnd LineBuffering)                  -<< ()
                req     <- exceptZeroA $ getRequest                             -<  hnd
                processRequest (hname, pnum) hnd shader' lock                   -<  req
        processRequest (hname, pnum) hnd shader' lock =
            proc (req, body) -> do
                -- "global" <-@ mkPlainMsg $ "processing request..."            -<< ()
                -- addr        <- arrIO $ inet_ntoa                             -<  haddr
                (buffer :: Ptr Char)
                        <- arrIO $ mallocArray                                  -<  chunksize

                ta      <- createTA 1 Init                                      -<  ()
                ta2     <- setVal _transaction_handler "TCPHandler"
                           >>>
                           setVal _transaction_tcp_remoteHost hname
                           >>>
                           setVal _transaction_tcp_remotePort (show pnum)       -<< ta
                ts_start    <- getCurrentTS                                     -<  ()
                ta3         <- setTAStart ts_start                              -<< ta2

                let body' = escapeURIString (\ch -> ch /= '\r') body
                ta4         <- setVal _transaction_requestFragment
                                      (concat $ (concat req):[body'])           -<< ta3
                ta_str      <- xshow (constA ta4)                               -<< ta4

                "global"    <-@ mkSimpleLog "TCPHandler:newHandler" ("initial transaction is: " ++ ta_str) l_debug -<< ()
                ta5         <- TA.setTAState Processing                         -<  ta4
                ta6         <- shader'                                          -<  ta5
                response    <- getValDef _transaction_responseFragment ""       -<  ta6
                ta_str'     <- xshow (constA ta6)                               -<< ()

                "global"    <-@ mkSimpleLog "TCPHandler:newHandler" ("final transaction is: " ++ ta_str') l_debug -<< ()
                exceptZeroA $ hPutStr hnd                                       -<< response
                exceptZeroA_ $ hFlush hnd                                       -<< ()

                (single $ proc in_ta -> do
                    uid <- getVal _transaction_http_response_body_hdlop -<  in_ta
                    (HandleVal hIn) <- getSV ("/local/files/_" ++ uid)  -<< ()
                    delStateTree ("/local/files/_" ++ uid)              -<< ()
                    exceptZeroA_ $ handleCopy hIn hnd buffer chunksize  -<< ()
                    exceptZeroA_ $ hFlush hnd                           -<< ()
                    exceptZeroA_ $ hClose hIn                           -<< ()
                    returnA                                             -<  in_ta
                    ) `orElse` this                                             -<< ta6

                ts_end      <- getCurrentTS                                     -<  ()
                ta7         <- setTAEnd ts_end                                  -<< ta6
                runtime     <- getTARunTime                                     -<  ta7

                "global"    <-@ mkPlainMsg $ "OK. Took " ++
                                    (show runtime) ++ " ms\n"                   -<< ()
                returnA                                                         -<  ()
        processRequest _ _ _ _ = zeroArrow
        handleCopy hIn hOut buffer blocksize =
            do
                actual <- hGetBuf hIn buffer blocksize
                hPutBuf hOut buffer actual
                -- content <- BStr.hGet hIn blocksize
                -- BStr.hPut hOut content
                eof <- hIsEOF hIn
                if not eof
                   then handleCopy hIn hOut buffer blocksize 
                   else return ()

{- |
Reads from a given handle and returns a tuple, where the first element represents the headers of an HTTP Request and the second
element the body of the request.
Basing on the HWS-WP request read implementation, this function currently is specialized on HTTP. This should change in the future.
-}
getRequest :: Handle -> IO ([String], String)
getRequest h = do
  l <- hGetLine h
  if (emptyLine l)
     then getRequest h
     else do
        (req, bodysize) <- getRequest' l h
        body <- if isJust bodysize
                    then getBody h (fromJust bodysize)
                    else return ""
        return (req, body)

{- |
Delivers a list of headers and an according body size if recognized. The body itself is not read, only its size
is inferred based on a content-length header.
-}
getRequest' :: String -> Handle -> IO ([String], Maybe Int)
getRequest' l h = do
    if (emptyLine l)
        then do
            return ([l], Nothing)
        else do
            l' <- hGetLine h
            (ls, hint') <- getRequest' l' h
            let hint = parseContentLength l'
            return ((l:ls), if isJust hint then hint else hint')

{- |
Delivers a list of headers and an according body size if recognized. The body itself is not read, only its size
is inferred based on a content-length header.
-}
getBody :: Handle -> Int -> IO String
getBody handle size = do
    if size <= 0
        then return []
        else do
            c <- hGetChar handle
            d <- getBody handle (size - 1)
            return $ c:d

{- |
Recognizes an empty line (only containing a line break).
-}
emptyLine :: String -> Bool
emptyLine "\r" = True
emptyLine _    = False

{- |
Tries to recognize an HTTP content-length header in a given line. If found, the contained body size is delivered, otherwise
Nothing.
-}
parseContentLength :: String -> Maybe Int
parseContentLength header =
    let (header_type, val) = break (==':') header
    in case val of
          ':':val'  -> if (Prelude.map toLower header_type) == "content-length"
                            then Just (read $ stripWS val')
                            else Nothing
          _         -> Nothing

{- not yet used
newHandler :: Integer -> Shader -> Handler
newHandler port shader =
    proc _ -> do
        sock <- exceptZeroA_
                    (do
                        proto   <- getProtocolNumber "tcp"
                        sock    <- socket AF_INET Stream proto
                        return sock)                                                -<< ()
        finallyA
            (proc _ -> do
                sock' <- exceptZeroA_
                    (do
                        proto <- getProtocolNumber "tcp"
                        sock'' <- socket AF_INET Stream proto
                        setSocketOption sock ReuseAddr 1
                        bindSocket sock'' (SockAddrInet (fromInteger port) iNADDR_ANY)
                        listen sock'' 1
                        return sock''
                        )                                               -<< ()
                acceptRequest sock' shader                              -<< ()
                )
            (arrIO0 $ sClose sock)                                                  -<< ()
        where
            acceptRequest sock shader' =
                proc _ -> do
                    (newsock, sockinfo)     <- exceptZeroA_ (Network.Socket.accept sock)           -<< ()
                    handle  <- exceptZeroA_ (socketToHandle newsock ReadWriteMode)  -<< ()
                    processA
                        (finallyA
                            (processRequest sockinfo handle shader')
                            (arrIO $ \t -> do { hClose handle; return t })
                            )                                                       -<< ()
                    acceptRequest sock shader'                                      -<< ()
            processRequest sockinfo handle shader' =
                proc _ -> do
                    exceptZeroA_ (hSetBuffering handle LineBuffering)               -<< ()
                    req     <- arrIO $ getRequest                                   -<  handle
                    process sockinfo handle shader'                                 -<  req
            process (SockAddrInet port' haddr) handle shader' =
                proc (req, body) -> do
                    -- "global" <-@ mkPlainMsg $ "processing request..."            -<< ()
                    addr        <- arrIO $ inet_ntoa                                -<  haddr
                    ta      <- createTA 1 Init                                      -<  ()
                    ta2     <- setVal _transaction_handler "TCPHandler"
                               >>>
                               setVal _transaction_tcp_remoteIp addr
                               >>>
                               setVal _transaction_tcp_remotePort (show port')      -<< ta
                    ts_start    <- getCurrentTS                                     -<  ()
                    ta3         <- setTAStart ts_start                              -<< ta2
                    let body' = escapeURIString (\ch -> ch /= '\r') body
                    ta4         <- setVal _transaction_requestFragment
                                          (concat $ (concat req):[body'])           -<< ta3
                    ta_str      <- xshow (constA ta4)                               -<< ta4

                    "global"    <-@ mkSimpleLog "TCPHandler:newHandler" ("initial transaction is: " ++ ta_str) l_debug -<< ()
                    ta5         <- TA.setTAState Processing                         -<  ta4
                    ta6         <- shader'                                          -<  ta5
                    response    <- getValDef _transaction_responseFragment ""       -<  ta6
                    ta_str'     <- xshow (constA ta6)                               -<< ()

                    "global"    <-@ mkSimpleLog "TCPHandler:newHandler" ("final transaction is: " ++ ta_str') l_debug -<< ()
                    arrIO $ hPutStr handle                                          -<< response

                    (listA $ proc in_ta -> do
                        uid <- getVal _transaction_http_response_body_hdlop         -<  in_ta
                        (HdlOpVal op) <- getSV ("/local/files/_" ++ uid)            -<< ()
                        arrIO $ op                                                  -<< handle
                        )                                                           -<  ta6

                    ts_end      <- getCurrentTS                                     -<  ()
                    ta7         <- setTAEnd ts_end                                  -<< ta6
                    runtime     <- getTARunTime                                     -<  ta7

                    "global"    <-@ mkPlainMsg $ "OK. Took " ++
                                        (show runtime) ++ " ms\n"                   -<< ()
                    returnA                                                         -<  ()
            process _ _ _ = zeroArrow
-}
