-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.HTTPShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: HTTPShader.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus HTTP Binding

   Shaders to provide HTTP support in Janus server configurations.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.HTTPShader
   (
   -- HTTP shaders
     requestShader
   , responseShader
   , fileShader
   , mimeShader
   , initMimeDB
   , cgiShader
   , httpStatusShader
   , htmlStatusShader
   )
where

import Data.ByteString as BStr (hGet, hPut) -- length
import Data.Array ()
import Data.Array.IO ()
import Data.Array.MArray ()
import Data.Map ( Map, empty, insert, toList )
import Data.Word ()

import Network.URI

import System.IO
import System.Eval.Haskell

import Text.Regex
import Text.XML.HXT.Arrow

import Network.Server.Janus.Core as Shader
import Network.Server.Janus.Messaging
import Network.Server.Janus.Transaction
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

import Network.Server.HWS.Request as HWSRequest
import Network.Server.HWS.Response as HWSResponse
import Network.Server.HWS.Util as HWSUtil hiding (emptyLine, accept)

-- getParams :: JanusArrow Context XmlTree (String, String)
-- getParam :: String -> JanusArrow Context XmlTree String
-- setStatus :: Int -> JanusArrow Context XmlTree XmlTree
-- setBody :: String -> JanusArrow Context XmlTree XmlTree
-- setMIME :: String -> JanusArrow Context XmlTree XmlTree
-- ...

{- |
Parses \/transaction\/request_fragment as HTTP request and transforms it into a \/transaction\/http\/request subtree.
Fails, if \/transaction\/request_fragment is not present or if the request or the contained URI cannot be parsed.
Creates a \/transaction\/http\/request tree with a body subnode (value is the HTTP request body) and with these attributes:
url, uri_scheme, uri_path, uri_query, uri_frag, method. Furthermore, an empty tree \/transaction\/http\/response\/body is
created.
-}
requestShader :: ShaderCreator
requestShader =
   mkStaticCreator $
   proc in_ta -> do
      http_request <- getVal _transaction_requestFragment
                      <+!>
                      ( "requestShader"
                      , TAValueNotFound
                      , "No request fragment found."
                      , [("value", show _transaction_requestFragment)]
                        )                                                                          -< in_ta

      let http_request_list         = splitRegex (mkRegex "\r") http_request
      let (http_header, http_body)  = findbody http_request_list

      let parsed_request            = parseRequest http_header http_body
      (uri, method, body) <-
         ( case parsed_request of
           Bad _    -> zeroArrow
           Ok req   -> constA ( HWSRequest.reqURI req
                              , HWSRequest.reqCmd req
                              , maybe ("") (id) (HWSRequest.reqBody req)
                              )
         )
         <+!> ( "requestShader"
               , FormatError
               , "Bad HTTP request."
               , [("request", http_request)])                                                      -<<  ()

      let http_header' = Prelude.map (trimColon . break (== ':')) (tail http_header)
                           where trimColon (l, r) = (l, (stringTrim . tail) r)

      let uri' = parseURIReference (show uri)

      uri'' <-
         (if isJust uri'
            then arr $ fromJust
            else zeroArrow
            )
         <+!> ("requestShader", FormatError, "Bad URI.", [("URI", show uri)])                      -<<  uri'

      ( setVal _transaction_http_request_body body
        >>>
        insEmptyTree _transaction_http_response_body
        >>>
        setVal _transaction_http_request_url             (show uri)
        >>>
        setVal _transaction_http_request_uriScheme       (uriScheme uri'')
        >>>
        setVal _transaction_http_request_uriPath         (uriPath uri'')
        >>>
        setVal _transaction_http_request_uriQuery        (uriQuery uri'')
        >>>
        setVal _transaction_http_request_uriFrag         (uriFragment uri'')
        >>>
        setVal _transaction_http_request_method          (requestCmdString method)
        >>>
        addHeader http_header'
        )                                                                                          -<< in_ta
   where
      findbody []                = ([], Nothing)
      findbody (x:xs)
         | Prelude.null x  = findbody' xs
         | otherwise       =
            let (headers, body) = findbody xs in
                                    (x:headers, body)
      findbody' []               = ([], Nothing)
      findbody' (x:_)            = ([], Just (unEscapeString x))
      addHeader ((name, val):xs) = setVal (_transaction_http_request_header_ name) val
                                   >>>
                                   addHeader xs
      addHeader []               = this

{- |
Creates the response fragment (\/transaction\/response_fragment) for deliverance to the client by the handler. The response is based on
the \/transaction\/http\/response subtree, where the following data is read:
\/transaction\/http\/response\/body - The body of the response, defaults to the empty string.
\/transaction\/http\/response\/\@mime - The content type of the response, defaults to \"text\/html\".
\/transaction\/http\/response\/\@status - The status code of the response, defaults to 200 OK.
\/transaction\/http\/response\/body\/\@filesize - The size of the response body. If no filesize is defined, the filesize is computed from the
body. If a filesize is present, the body is actually NOT copied to the response - in this case, it is expected that the body is generated
by the handler (due to performance issues).
-}
responseShader :: ShaderCreator
responseShader =
   mkStaticCreator $
   proc in_ta -> do
      body   <- getValDef _transaction_http_response_body    ""                       -<  in_ta
      mime   <- getValDef _transaction_http_response_mime   "text/html"               -<  in_ta
      status <- getValDef _transaction_http_response_status "200" >>> parseA          -<  in_ta
      header <- listA $ listValPairs (_transaction_http_response_header_ "*")         -<  in_ta
      let status' = respCodeToHWS status
      let header' = Prelude.map (\(name, val) -> name ++ ": " ++ val) header
      fsize  <- getValDef _transaction_http_response_body_filesize ""                 -<  in_ta
      let response = Response {
         respCode          = status',
         respHeaders       = header',
         respCoding        = [],
         respBody          = if fsize /= "" then FileBody (read fsize) undefined else HereItIs body,
         respSendBody      = True,
         respContentType   = Just mime
         }
      date <- arrIO0 $ dateHeader                                                      -<  ()

      "global" <-@ mkSimpleLog "HTTPShader.hs:responseShader" ("responseShader generated: " ++ (generateResponse response date)) l_debug -<< ()

      setVal _transaction_responseFragment (generateResponse response date)            -<< in_ta

{- |
Populates response payload of \/transaction\/http\/response with static file content based on \/transaction\/http\/request\/\@url_*.
The base URL, which is interpreted as root for the file request, can be defined by \/shader\/config\/\@base_url (defaults to \"\/\").
The directory from which files are loaded (the path following the base-URL is concatenated to the base directory to gain
the fully qualified filename) can be defined by \/shader\/config\/\@maps_to (defaults to \".\/\", the base directory of Janus).
The transaction node, to which file content is loaded, can be defined by \/shader\/config\/\@loads_to (defaults to \/transaction\/http\/response\/body,
the location for the HTTP response body). The Shader fails if no URI is present in the Transaction.
If no resource is specified by the requester (URL \"\/\"), the shader redirects the request to a default root file. This file can be defined by
\/shader\/config\/\@default_file (defaults to \"\/index.html\"). There is no explicit option to suppress this behaviour, however, if no redirection
is desired one can configure the default_file option to \"\/\". Technically redirection still occurs in this case, but doesn't actually change
the effective target URL. Attention: If redirection occurs, the Transaction's URI-Path gets updated, but the over-all URL stays unchanged. This
is to identify requests originally directed at a different URL. As a result the redirection is not completely transparent for adjacent shaders.
-}
fileShader :: ShaderCreator
fileShader =
   mkDynamicCreator $ arr $ \(conf, _) ->
   proc in_ta -> do
      defFile  <- getValDef _shader_config_deffile "/index.html"                           -<  conf
      base     <- getValDef _shader_config_baseUrl "/"                                     -<  conf
      mapsto   <- getValDef _shader_config_mapsTo "./"                                     -<  conf
      loadsto  <- getValDef _shader_config_loadsTo (show _transaction_http_response_body)  -<  conf
      target   <- getVal _transaction_http_request_uriPath
                  <+!>
                  ( "fileShader"
                  , TAValueNotFound
                  , "No URI found."
                  , [("value", show _transaction_http_request_uriPath)])                   -<  in_ta
      let target'    = if target == "/" then defFile else target
      let baseURL    = maybe nullURI id (parseURIReference ("http://host" ++ base))
      let targetURL  = maybe nullURI id (parseURIReference ("http://host" ++ target'))
      let filename   = mapsto ++ (show $ relativeFrom targetURL baseURL)
      in_ta'   <- setVal _transaction_http_request_uriPath target'                         -<< in_ta

      "global" <-@ mkSimpleLog "HTTPShader.hs:fileShader" ("fileShader requested file: " ++ filename) l_info -<< ()

      (proc x -> do
         uid   <- getUID (-1)                                                          -<  ()
         -- file  <- exceptZeroA BStr.readFile                                            -<  filename
         file  <- exceptZeroA $ openFile filename                                      -<  ReadMode
         fsize <- exceptZeroA $ hFileSize                                              -<< file
         -- exceptZeroA $ hClose                                                          -<  file
         -- file'  <- exceptZeroA $ openFile filename                                     -<< ReadWriteMode
         ("/local/files/_" ++ (show uid)) <$! (HandleVal file) -<< ()

         -- ("/local/files/_" ++ (show uid)) <$! HdlOpVal (\hOut -> do
         --                                                   (buffer :: IOUArray Int Word8) <- newArray (1,64) 0
         --                                                   putStrLn ("Opening... " ++ show uid)
         --                                                   hFile <- openFile filename ReadWriteMode
         --                                                   putStrLn ("Copying... " ++ show uid)
                                                            -- hPutStr hOut "Content"
         --                                                   handleCopy hFile hOut 128
                                                            -- content <- hGetContents hFile
                                                            -- putStrLn ("Reading... " ++ show uid)
                                                            -- content <- BStr.hGet hFile 128
                                                            -- putStrLn ("Writing... " ++ show uid)
                                                            -- BStr.hPut hOut content
                                                            -- hPutStr h content
                                                            -- handleCopy hFile hOut buffer 64
         --                                                   putStrLn ("Closing... " ++ show uid)
         --                                                   hClose hFile
         --                                                   putStrLn ("Finished... " ++ show uid)
         --                                                   )                          -<< ()
         -- x'    <- setVal (loadsto ++ "/@filesize") (show $ BStr.length file)           -<< x
         x'    <- setVal (jpAttr loadsto "filesize") (show $ fsize)                    -<< x
         x''   <- setVal (jpAttr loadsto "hdlop")    (show $ uid)                      -<< x'
         setVal (jp loadsto) "xxx"                                                     -<< x''
         )
         <+!> ( "fileShader"
	      , FileNotFound
	      , ("File '" ++ filename ++ "' not found.")
	      , [("file", filename)]
	      )                                                                                 -<< in_ta'
   where
      handleCopy hIn hOut blocksize =
         do
            content <- BStr.hGet hIn blocksize
            BStr.hPut hOut content
            eof <- hIsEOF hIn
            if not eof
               then handleCopy hIn hOut blocksize
               else return ()
--   where
--      handleCopy hIn hOut buffer blocksize =
--         do
--            readWords <- hGetArray hIn buffer blocksize
--            hPutArray hOut buffer readWords
--            if readWords == blocksize
--               then handleCopy hIn hOut buffer blocksize
--               else return ()
--   where
--      handleCopy hIn hOut = do
--                              eof <- hIsEOF hIn
--                              if not eof
--                                 then (hGetChar hIn >>= hPutChar hOut >> handleCopy hIn hOut)
--                                 else return ()

{- |
Determines the MIME type belonging to a given file extension. The filename is taken from \/transaction\/http\/request\/\@uri_path. If no URL
can be found, the Shader fails. The determined MIME type is stored at \/transaction\/http\/response\/\@mime.
The default type, which is delivered if no type can be inferred by means of the filename extension, can be defined by \/shader\/config\/\@default
(defaults to \"text\/plain\").
The MIME types are inferred from a database in the global scope (global:\/mime). For each extension, a subnode is stored in this database
(e.g. global:\/mime\/jpg). The value of each subnode is the associated MIME type, in this case \"image\/jpeg\". If a MIME type cannot be inferred
from the database, the configuration of the mimeShader is searched. E.g., for a file extension .tex, \/shader\/config\/types\/\@tex would be accessed.
If both database and configuration based type inference fail, the default type is selected.
-}
mimeShader :: ShaderCreator
mimeShader =
   mkDynamicCreator $ arr $ \(conf, _) ->
   proc in_ta -> do
      name           <- getVal _transaction_http_request_uriPath
                        <+!>
                        ( "mimeShader"
                        , TAValueNotFound
                        , "No URI found."
                        , [("value", show _transaction_http_request_url)]
                        )                                                               -<  in_ta
      defaultType    <- getValDef _shader_config_default "text/plain"                   -<  conf

      mimeType       <- ( getSVS $ "/global/mime/" ++ extension name)
                        `orElse`
                        getValDef (_shader_config_types_ (extension name)) defaultType  -<< conf

      "global"       <-@ mkSimpleLog "HTTPShader.hs:mimeShader" ("mimeShader selected: " ++ mimeType) l_info -<< ()

      setVal _transaction_http_response_mime mimeType                                   -<< in_ta
   where
      extension :: String -> String
      extension fn = go (reverse fn) ""
        where  go []      _   = ""
               go ('.':_) ext = ext
               go (x:s)   ext = go s (x:ext)

{- |
Loads the MIME extension database. The filename is specified by \/shader\/config\/\@typefile. If no file is defined, the Shader fails.
IMPLEMENTATION CURRENTLY NOT FAILSAFE, FILE FORMAT IS WEAK. SHOULD BE REPLACED BY THE ShaderLib dataLoadShader.
-}
initMimeDB :: ShaderCreator
initMimeDB =
   mkDynamicCreator $ arr $ \(conf, _) ->
   proc through -> do
      "/global/mime" <$! NullVal                                                                -<  ()
      filename <- getVal _shader_config_typefile
                  <+!>
                  ( "initMimeDB"
                  , TAValueNotFound
                  , "No data file specified."
                  , [("value", show _shader_config_typefile)]
                  )                                                                             -<  conf
      h        <- ( exceptZeroA $ openFile filename )
                  <+!>
                  ( "initMimeDB"
                  , FileNotFound
                  , ("File '" ++ filename ++ "' not found.")
                  , [("file", filename)]
                  )                                                                             -<< ReadMode
      arrIO $ hSetBuffering h                                                                   -<< LineBuffering
      processFile h filename                                                                    -<< ()
      returnA                                                                                   -<  through
   where
      processFile handle filename =
         proc _ -> do
            line <- arrIO $ hGetLine               -<  handle
            let (mime:exts) = words line
            addExts exts mime                      -<< ()
            eof <- arrIO $ hIsEOF                  -<  handle
            (if eof
               then this
               else processFile handle filename)   -<< ()
      addExts [] _          =
         this
      addExts (ext:xs) mime =
         (("/global/mime/" ++ ext) <-! mime)
         >>>
         constA ()
         >>>
         addExts xs mime

{- |
Parses the query part of the URL from \/transaction\/http\/request\/\@url and builds \/transaction\/http\/request\/cgi attributes.
Each parameter is stored as an attribute, e.g. \/transaction\/http\/request\/cgi\/\@session. The Shader fails if not URL can be found.
TODO: Alternatively aquires data from HTTP headers (\/transaction\/http\/request\/headers attributes) (HTTP POST method).
-}
cgiShader :: ShaderCreator
cgiShader =
   mkStaticCreator $
   proc in_ta -> do
      method <- getValDef _transaction_http_request_method "GET"                       -<  in_ta
      params <- cgiParams method                                                       -<< in_ta

      "global" <-@ mkSimpleLog "HTTPShader.hs:cgiShader" ("cgiShader detected: " ++ show params) l_info -<< ()

      ta2   <- insEmptyTree _transaction_http_request_cgi                              -<  in_ta
      ta3   <- (seqA . map (uncurry insertParam)) params                               -<< ta2
      returnA                                                                          -<  ta3
   where
   insertParam key val
       = setVal (_transaction_http_request_cgi_ ('@' : key)) val

   cgiParams "GET"
       = proc in_ta -> do
            url   <- getVal _transaction_http_request_url
                     <+!>
                     ( "cgiShader"
                     , TAValueNotFound
                     , "No URL found."
                     , [("value", show _transaction_http_request_url)]
                     )                                                                   -<  in_ta
            let searchpath    = uriQuery (maybe nullURI id (parseURIReference url))
            let searchpath'   = if (Prelude.null searchpath) then (searchpath) else (tail searchpath)
            returnA  -< toList $ parseSearchPath searchpath'

   cgiParams "POST"
       = proc in_ta -> do
            mimeType <- getValDef (_transaction_http_request_header_ "Content-Type") "" -< in_ta
            body     <- getValDef  _transaction_http_request_body                    "" -< in_ta
            returnA   -< ( if mimeType == "application/x-www-form-urlencoded"
                           then toList $ parseSearchPath body
                           else []
                         )

   cgiParams _
       = constA []

{- |
Generates the HTTP status code at \/transaction\/http\/response\/\@status based on the state of the transaction. The Failure state is mapped
onto status code 404, the Processing (no unhandled errors occured) state is mapped onto status code 200.
-}
httpStatusShader :: ShaderCreator
httpStatusShader =
   mkStaticCreator $
   proc in_ta -> do
      ta_state    <- getTAState                                                        -<  in_ta
      ta          <- (if ta_state == Processing
                        then setVal _transaction_http_response_status "200"
                        else setVal _transaction_http_response_status "404"
                     )                                                                 -<< in_ta
      setTAState Processing                                                            -<  ta

{- |
Generates a response body based on the status code (for all status codes >= 400). The bodies are read from files, where a file can be
associated to each status code. The status is read from \/transaction\/http\/response\/\@status. If the status is unspecified, a default
status can be defined by \/shader\/config\/\@default (defaults to 200). Each status file is defined by \/shader\/config\/\@page_X, where X
represents the according status code. If no file is defined for a given status code or the specified file cannot be read, the Shader fails.
-}
htmlStatusShader :: ShaderCreator
htmlStatusShader =
   mkDynamicCreator $ arr $ \(conf, _) ->
   proc in_ta -> do
      defaultStatus     <- getValDef _shader_config_default "200"                                   -<  conf
      (status :: Int)   <- getValDef _transaction_http_response_status defaultStatus >>> parseA     -<< in_ta

      "global" <-@ mkSimpleLog "HTTPShader.hs:htmlStatusShader" ("status code found: " ++ show status) l_info -<< ()

      (if status >= 400
         then proc ta -> do
            statusFile <- getVal (_shader_config_ ("@page_" ++ show status))
                          <+!>
                          ( "htmlStatusShader"
                          , TAValueNotFound
                          , ("No file specified for status code " ++ show status)
                          , [("status", show status)]
                          )                                                                        -<< conf
            file       <- exceptZeroA readBinary
                          <+!>
                          ( "htmlStatusShader"
                          , FileNotFound
                          , ("File '" ++ statusFile ++ "' not found.")
                          , [("file", statusFile)]
                          )                                                                        -<< statusFile
            setVal _transaction_http_response_body file                                            -<< ta
         else this
         )                                                                                         -<< in_ta




-- Utils
{- |
Reads a file in binary mode and returns it as a String.
-}
readBinary :: String -> IO String
readBinary filename =
   do
      handle   <- openBinaryFile filename ReadMode
      file     <- getIt handle
      hClose handle
      return file
   where
      getIt handle = do
         eof   <- hIsEOF handle
         if eof
            then return []
            else do
               c  <- hGetChar handle
               d  <- getIt handle
               return $ c:d

{- |
Delivers a Map of name-value-pairs, parsed from a URL query part. The query part has to delivered without the leading ?-character.
-}
parseSearchPath :: String -> Map String String
parseSearchPath searchpath =
      if key /= "" then insert key val res else res
      where
         (res, key, val) = parseSearchPath' searchpath

parseSearchPath' :: String -> (Map String String, String, String)
parseSearchPath' ('=':input) = parseSearchPath'' input
parseSearchPath' (ch:input)  = (res, ch:key, val)
      where
         (res, key, val)    = parseSearchPath' input
parseSearchPath' [] = (empty, [], [])

parseSearchPath'' :: String -> (Map String String, String, String)
parseSearchPath'' ('&':input) = (insert key val res, [], [])
      where
         (res, key, val)    = parseSearchPath' input
parseSearchPath'' (ch:input)  = (res, key, ch:val)
      where
         (res, key, val)    = parseSearchPath'' input
parseSearchPath'' [] = (empty, [], [])

{- |
Maps a HTTP status code represented as a number to the HWS (Haskell Web Server) response code type.
-}
respCodeToHWS :: Int -> ResponseCode
respCodeToHWS code =
   case code of
      100   -> RC_Cont
      101   -> RC_SwitchingProtocols
      200   -> RC_OK
      201   -> RC_Created
      202   -> RC_Accepted
      203   -> RC_NonAuthoritiveInformation
      204   -> RC_NoContent
      205   -> RC_ResetContent
      206   -> RC_PartialContent
      300   -> RC_MultipleChoices
      301   -> RC_MovedPermanently
      302   -> RC_Found
      303   -> RC_SeeOther
      304   -> RC_NotModified
      305   -> RC_UseProxy
      307   -> RC_TemporaryRedirect
      400   -> RC_BadRequest
      401   -> RC_Unauthorized
      402   -> RC_PaymentRequired
      403   -> RC_Forbidden
      404   -> RC_NotFound
      405   -> RC_MethodNotAllowed
      406   -> RC_NotAcceptable
      407   -> RC_ProxyAuthenticationRequired
      408   -> RC_RequestTimeOut
      409   -> RC_Conflict
      410   -> RC_Gone
      411   -> RC_LengthRequired
      412   -> RC_PreconditionFailed
      413   -> RC_RequestEntityTooLarge
      414   -> RC_RequestURITooLarge
      415   -> RC_UnsupportedMediaType
      416   -> RC_RequestedRangeNotSatisfiable
      417   -> RC_ExpectationFailed
      500   -> RC_InternalServerError
      501   -> RC_NotImplemented
      502   -> RC_BadGateway
      503   -> RC_ServiceUnavailable
      504   -> RC_GatewayTimeOut
      505   -> RC_VersionNotSupported
      _     -> RC_NotImplemented

