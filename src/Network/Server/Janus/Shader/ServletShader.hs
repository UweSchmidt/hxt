-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.ServletShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ServletShader.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus Java style Servlet wrapper
   
   A module to provide a Java Servlet like interface of programming, which is
   mapped onto Janus Shaders.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.ServletShader
    (
    -- data types
      HttpServletContext
    , HttpServletRequest
    , HttpServletResponse

    -- wrapper functions
    , hostServlet
    , reflectServlet
    )
where

import Network.URI
import Text.XML.HXT.Arrow
        
import Network.Server.Janus.Core
import Network.Server.Janus.HTMLBuilder
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

data HttpServletContext = HSCON {
    servletid       :: String,
    attrs           :: [(String, String)],
    state           :: [(String, String)]
}

data HttpServletRequest = HSREQ {
    session         :: Int,
    session_state   :: XmlTree,
    url             :: URI,
    params          :: [(String, String)],
    user            :: String,
    client_ip       :: String,
    client_port     :: String
}

data HttpServletResponse = HSRES {
    header          :: [(String, String)],
    state_chg       :: [(String, String)],
    new_s_state     :: XmlTree,
    status          :: Int,
    body            :: String
}

{- |
Returns a ShaderCreator based on a function, which takes a servlet context, a servlet request and a servlet response and infers an Arrow
delivering a servlet response.
-}
hostServlet :: (HttpServletContext -> HttpServletRequest -> HttpServletResponse -> JanusArrow Context a HttpServletResponse) -> ShaderCreator
hostServlet servlet =
    mkDynamicCreator $ arr $ \(conf, _) -> 
    proc in_ta -> do
        shaderid    <- getVal _shader_config_id                                     -<  conf
        attributes  <- listA $ listValPairs _shader_config                          -<  conf
        state'      <- listA $ listStatePairsStr ("/local/servlets/" ++ shaderid)   -<< ()
        sessionid   <- getVal _transaction_session_sessionid                        -<  in_ta
        s_state     <- getTree _transaction_session_state                           -<  in_ta
        authuser    <- getValDef _transaction_session_state_authuser ""             -<  in_ta
        url'        <- maybeA $ 
                        getVal _transaction_http_request_url
                        >>> 
                        (arr parseURIReference)                                     -<  in_ta   
        cgi         <- listA $ listValPairs (_transaction_http_request_cgi_ "@*")   -<  in_ta
        remote_ip   <- getVal _transaction_tcp_remoteIp                             -<  in_ta
        remote_port <- getVal _transaction_tcp_remotePort                           -<  in_ta

        let context = HSCON { 
                          servletid = shaderid
                        , attrs = attributes
                        , state = state'
                      }
        let request = HSREQ { 
                          session = (read sessionid)
                        , session_state = s_state
                        , url = url'
                        , params = cgi
                        , user = authuser
                        , client_ip = remote_ip
                        , client_port = remote_port 
                      }
        let response = HSRES { 
                           header = []
                        , state_chg = []
                        , new_s_state = s_state
                        , status = 404
                        , body = ""
                       }
        let responseArrow = servlet context request response
        
        response'   <- responseArrow                                                 -<< undefined
        
        to_state (state_chg response') shaderid                                      -<< ()
        ( delTree _transaction_session_state
          >>> 
          insTree _transaction_session (constA $ new_s_state response')
          >>>
          setVal _transaction_http_response_body (body response')       
          >>>
          setVal _transaction_http_response_status (show $ status response') )      -<< in_ta
    where
        to_state ((name, val):xs) sid   = 
                            (("/local/servlets/" ++ sid ++ "/" ++ name) <-! val)
                            >>>
                            (to_state xs sid)
        to_state [] _           = this

{- |
An example to demonstrate the Java style Servlet interface. A value named "test" with string value "test" is added to
the state by means of the response value. The returned body simply contains the URI requested, which is taken from the
servlet request.
-}
reflectServlet :: ShaderCreator
reflectServlet = 
    hostServlet 
        (\context request response ->
            proc _ -> do
                htmltree    <- createbody (url request) (state context)     -<  undefined
                htmlstr     <- html2Str                                     -<  htmltree
                let result = response { status = 200, body = htmlstr, state_chg = [("test","test")] }
                returnA                                                     -<  result
            )
    where
        createbody url' elements =   
            html
                +>> [  headers 
                        += title "Reflection Servlet"
                     , htmlbody 
                        += heading 1 ("Request URL is ... " ++ show url')
                        += heading 3 (show elements)
                    ]
