-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module Network.Server.HWS.Response where

import Data.Maybe (isJust, fromJust)
import Time
import IO
-- import IOExts

import Monad
-- import MutableArray
import Text.Html

import Network.Server.HWS.Config
import Network.Server.HWS.Util

--import PrelHandle

-----------------------------------------------------------------------------
-- Responses

data ResponseBody
  = NoBody
  | FileBody Integer{-size-} FilePath
  | HereItIs String

data Response
  = Response {
      respCode     :: ResponseCode,
      respHeaders  :: [String],
      respCoding   :: [TransferCoding],	-- either empty or terminated with 
					-- ChunkedTransferEncoding 
					-- (RFC2616, sec 3.6)
      respBody     :: ResponseBody,	-- filename of body
      respSendBody :: Bool,		-- actually send the body?
					--  (False for HEAD requests)
      respContentType :: Maybe String   -- the content type. If Nothing, there
                                        -- must be a ContentType header
   }


instance Show Response where
   showsPrec _ (Response s hs _ _ _ _) 
	 = foldr (\s' r -> s' . r) id (shows s : (' ':) : shows s : map (showString . ('\n':)) hs)
	 . showChar '\n'

-- CU 24.04.2006
generateResponse :: Response -> String -> String
generateResponse (Response{ respCode=cod,
                         respHeaders=headers,
                         respBody=bod,
                         respContentType=cType }) date =
    statusLine cod
    ++ "\r\n" ++ 
    serverHeader 
    ++ "\r\n" ++ 
    date 
    ++ "\r\n" ++ 
    (if isJust cType then (contentTypeHeader (fromJust cType)) ++ "\r\n" else "")
    ++ 
    (case bod of
      HereItIs str     -> contentLengthHeader (toInteger $ length str) ++ "\r\n" 
      FileBody fsize _ -> contentLengthHeader fsize ++ "\r\n" 
      _ -> "")
    ++
    (concat . map (\str -> str ++ "\r\n") $ headers)
    ++ "\r\n" ++
    (case bod of
      HereItIs str     -> str
      FileBody _ _ -> ""
      _ -> "")
    
-- Send the headers in a response, and create some that should
-- always be there.
sendHeaders :: Handle -> Response -> IO ()
sendHeaders h (Response{ respCode=cod,
                         respHeaders=headers,
                         respBody=bod,
                         respContentType=cType }) = do
    hPutStrCrLf h (statusLine cod)
    hPutStrCrLf h serverHeader
    date <- dateHeader
    hPutStrCrLf h date
    when (isJust cType) $ hPutStrCrLf h (contentTypeHeader (fromJust cType))
    case bod of
      HereItIs str     -> hPutStrCrLf h (contentLengthHeader (toInteger $ length str))
      FileBody fsize _ -> hPutStrCrLf h (contentLengthHeader fsize)
      _ -> return ()
    mapM_ (hPutStrCrLf h) headers
    hPutStr h crlf

-- This really should use some sort of sub-request mechanism,
-- like in Apache
sendErrorPage :: Config -> Handle -> ResponseCode -> IO ()
sendErrorPage conf h rc = do
  let HereItIs epage = generateErrorPage rc conf
  let resp = Response
                { respCode = rc
                , respHeaders = []
                , respCoding = []
                , respBody = HereItIs epage
                , respSendBody = True
                , respContentType = Just "text/html"
                }
  sendHeaders h resp
  hPutStr h epage

bufsize :: Int
bufsize = 4 * 1024 :: Int

-- squirt data from 'rd' into 'wr' as fast as possible.  We use a 4k
-- single buffer.
squirt :: Monad t2 => t -> t1 -> t2 ()
squirt _ _ = do
  return ()
--  arr <- stToIO (newCharArray (0, bufsize-1))
--  let loop = do r <- hGetBufBA rd arr bufsize
--		if (r == 0) 
--		   then return ()
--		   else if (r < bufsize) 
--     			    then hPutBufBA wr arr r
--     			    else hPutBufBA wr arr bufsize >> loop
--  loop

statusLine :: ResponseCode -> String
statusLine cod = httpVersion ++ ' ': show cod

httpVersion :: String
httpVersion = "HTTP/1.1"

data TransferCoding
  = ChunkedTransferCoding
  | GzipTransferCoding
  | CompressTransferCoding
  | DeflateTransferCoding
  deriving Eq

transferCodingStr :: TransferCoding -> String
transferCodingStr ChunkedTransferCoding  = "chunked"
transferCodingStr GzipTransferCoding     = "gzip"
transferCodingStr CompressTransferCoding = "compress"
transferCodingStr DeflateTransferCoding  = "deflate"

validTransferCoding :: [TransferCoding] -> Bool
validTransferCoding codings
  | null codings 
    || last codings == ChunkedTransferCoding 
       && ChunkedTransferCoding `notElem` init codings = True
  | otherwise = False

-----------------------------------------------------------------------------
-- Response Headers

dateHeader :: IO String
dateHeader = do
   -- Dates in HTTP/1.1 have to be GMT, which is equivalent to UTC
  clock_time <- getClockTime
  let utc = toUTCTime clock_time
  let time_str = formatTimeSensibly utc
  return ("Date: " ++ time_str)

serverHeader :: String
serverHeader = "Server: " ++ serverSoftware ++ '/':serverVersion

contentLengthHeader :: Integer -> String
contentLengthHeader i = "Content-Length: " ++ show i

contentTypeHeader :: String -> String
contentTypeHeader t = "Content-Type: " ++ t

lastModifiedHeader :: ClockTime -> String
lastModifiedHeader t = "Last-Modified: " ++ formatTimeSensibly (toUTCTime t)

transferCodingHeader :: TransferCoding -> String
transferCodingHeader te = "Transfer-Coding: " ++ transferCodingStr te

-----------------------------------------------------------------------------
-- Response codes

data ResponseCode
	= RC_Cont				-- 100
	| RC_SwitchingProtocols			-- 101
	| RC_OK					-- 200
	| RC_Created				-- 201
	| RC_Accepted				-- 202
	| RC_NonAuthoritiveInformation		-- 203
	| RC_NoContent				-- 204
	| RC_ResetContent			-- 205
	| RC_PartialContent			-- 206
	| RC_MultipleChoices			-- 300
	| RC_MovedPermanently			-- 301
	| RC_Found				-- 302
	| RC_SeeOther				-- 303
	| RC_NotModified			-- 304
	| RC_UseProxy				-- 305
	| RC_TemporaryRedirect			-- 307
	| RC_BadRequest				-- 400
	| RC_Unauthorized			-- 401
	| RC_PaymentRequired			-- 402
	| RC_Forbidden				-- 403
	| RC_NotFound				-- 404
	| RC_MethodNotAllowed			-- 405
	| RC_NotAcceptable			-- 406
	| RC_ProxyAuthenticationRequired	-- 407
	| RC_RequestTimeOut			-- 408
	| RC_Conflict				-- 409
	| RC_Gone				-- 410
	| RC_LengthRequired			-- 411
	| RC_PreconditionFailed			-- 412
	| RC_RequestEntityTooLarge		-- 413
	| RC_RequestURITooLarge			-- 414
	| RC_UnsupportedMediaType		-- 415
	| RC_RequestedRangeNotSatisfiable	-- 416
	| RC_ExpectationFailed			-- 417
	| RC_InternalServerError		-- 500
	| RC_NotImplemented			-- 501
	| RC_BadGateway				-- 502
	| RC_ServiceUnavailable			-- 503
	| RC_GatewayTimeOut			-- 504
	| RC_VersionNotSupported		-- 505

instance Show ResponseCode where
  show RC_Cont				= "100 Continue"
  show RC_SwitchingProtocols		= "101 Switching Protocols"
  show RC_OK				= "200 OK"
  show RC_Created			= "201 Created"
  show RC_Accepted			= "202 Accepted"
  show RC_NonAuthoritiveInformation	= "203 Non-Authoritative Information"
  show RC_NoContent			= "204 No Content"
  show RC_ResetContent			= "205 Reset Content"
  show RC_PartialContent		= "206 Partial Content"
  show RC_MultipleChoices		= "300 Multiple Choices"
  show RC_MovedPermanently		= "301 Moved Permanently"
  show RC_Found				= "302 Found"
  show RC_SeeOther			= "303 See Other"
  show RC_NotModified			= "304 Not Modified"
  show RC_UseProxy			= "305 Use Proxy"
  show RC_TemporaryRedirect		= "307 Temporary Redirect"
  show RC_BadRequest			= "400 Bad Request"
  show RC_Unauthorized			= "401 Unauthorized"
  show RC_PaymentRequired		= "402 Payment Required"
  show RC_Forbidden			= "403 Forbidden"
  show RC_NotFound			= "404 Not Found"
  show RC_MethodNotAllowed		= "405 Method Not Allowed"
  show RC_NotAcceptable			= "406 Not acceptable"
  show RC_ProxyAuthenticationRequired	= "407 Proxy Authentication required"
  show RC_RequestTimeOut		= "408 Request Time-Out"
  show RC_Conflict			= "409 Conflict"
  show RC_Gone				= "410 Gone"
  show RC_LengthRequired		= "411 Length Required"
  show RC_PreconditionFailed		= "412 Precondition Failed"
  show RC_RequestEntityTooLarge		= "413 Request Entity Too Large"
  show RC_RequestURITooLarge		= "414 Request-URI Too Large"
  show RC_UnsupportedMediaType		= "415 Unsupported Media Type"
  show RC_RequestedRangeNotSatisfiable	= "416 Requested range not satisfiable"
  show RC_ExpectationFailed		= "417 Expectation Failed"
  show RC_InternalServerError		= "500 Internal Server Error"
  show RC_NotImplemented		= "501 Not Implemented"
  show RC_BadGateway			= "502 Bad Gateway"
  show RC_ServiceUnavailable		= "503 Service Unavailable"
  show RC_GatewayTimeOut		= "504 Gateway Time-out"
  show RC_VersionNotSupported		= "505 HTTP Version not supported"

-----------------------------------------------------------------------------
-- Error pages

-- We generate some html for the client to display on an error.

generateErrorPage :: ResponseCode -> Config -> ResponseBody
generateErrorPage cod conf
  = HereItIs (renderHtml (genErrorHtml cod conf))

genErrorHtml :: ResponseCode -> Config -> Html
genErrorHtml cod conf
  = header << thetitle << response
    +++ body <<
	 (h1 << response
	  +++ hr
	  +++ serverSoftware +++ '/' +++ serverVersion
	  -- ToDo: use real hostname if we don't have a serverName
	  +++ case serverName conf of
	        "" -> noHtml
		me -> " on " +++ me +++ br
	  +++ case serverAdmin conf of
	        "" -> noHtml
		her -> "Server Admin: " +++ 
		       hotlink ("mailto:"++her) [toHtml her]
	 )
  where
--    descr = responseDescription code
    response = show cod -- +++ ' ' +++ descr
