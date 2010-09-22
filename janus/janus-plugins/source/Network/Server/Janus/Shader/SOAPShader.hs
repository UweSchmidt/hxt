-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.SOAPShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: SOAPShader.hs, v1.0 2006/11/07 00:00:00 janus Exp $

   Janus SOAP Binding

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.SOAPShader
    (
    -- shaders
      soapRequestShader
    , soapResponseShader
    , shopPrice
    )
where

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.ParserInterface

import Network.Server.Janus.Core
import Network.Server.Janus.XmlHelper


{- |
Parses a SOAP message contained in /transaction/http/request/body and provides it under /transaction/soap/request.
The Shader fails if no HTTP Request body is present.
-}
soapRequestShader :: ShaderCreator
soapRequestShader =
    mkCreator $ \_ _ ->
    (proc in_ta -> do
        soapStr <- getVal "/transaction/http/request/body"                              -<  in_ta

        -- arrIO $ putStrLn                                                                -<  soapStr

        let soapStr' = trim soapStr

        soap    <- listA $
                    constA soapStr'
                    >>>
                    parseXmlContent
                    >>>
                    propagateNamespaces                                                 -<< ()

        arrIO $ putStrLn                                                                -<  show soap

        ta      <-
            (insEmptyTree "/transaction/soap/request"
                >>>
                insEmptyTree "/transaction/soap/response/body"
                )                                                                       -<  in_ta

        ta'     <-
            (proc in_ta' -> do
                action  <- getVal "/transaction/http/request/header/SOAPAction" -<  in_ta'
                setVal "/transaction/soap/request/@action" action               -<< in_ta'
                )
            `orElse`
            this                                                                        -<  ta

        ta''    <- setVal "/transaction/soap/request/@binding" "HTTP"                   -<  ta'

        ta'''   <-
            (proc in_ta'' -> do
                body    <- listA $ getTree "/SOAP-ENV:Envelope/Body/*"          -<  head soap
                addBody body                                                    -<< in_ta''
                )                                                                       -<< ta''

        arrIO $ putStrLn                                                                -<  "soap:" ++ show ta'''

        -- ta'     <- soap2TA body                                                         -<< ta
        -- "/transaction/soap/request/header"

        returnA                                                                         -<  ta'''
        )
    where
        soap2TA ((name, val):xs)    =
                (setVal ("/transaction/soap/request/body/" ++ name) val
                    >>>
                    setVal ("/transaction/soap/request/body/" ++ name ++ "/@ns") "http://www.superduper.de"
                    >>>
                    soap2TA xs
                    )
        soap2TA []                  = this
        addBody (tree:xs)           = addTree "/transaction/soap/request/body" (constA tree)
                                        >>>
                                        addBody xs
        addBody []                  = this

{- |
Generates a SOAP message in /transaction/http/response/body based on /transaction/soap/response
-}
soapResponseShader :: ShaderCreator
soapResponseShader =
    mkCreator $ \_ _ ->
    (proc in_ta -> do
        let qid_env     = mkNsName  "SOAP-ENV:Envelope"         "http://schemas.xmlsoap.org/soap/envelope/"
        let qid_enc     = mkSNsName "SOAP-ENV:encodingStyle"
        -- let qid_header  = mkSNsName "SOAP-ENV:Header"
        let qid_body    = mkSNsName "SOAP-ENV:Body"
        body <- listA $
            (proc ta -> do
                body    <- getTree "/transaction/soap/response/body"            -<  ta
                arrIO $ putStrLn                                                -<  show body
                entries <- listVals "/body/*"                                   -<  body
                arrIO $ putStrLn                                                -<  show entries
                -- ns   <- getVal $ "/body/" ++ entries ++ "/@ns"               -<< body
                -- arrIO $ putStrLn                                             -<  show ns
                let ns  = "http://www.superduper.de"
                val     <- getVal $ "/body/" ++ entries                         -<< body
                arrIO $ putStrLn                                                -<  show val
                returnA                                                         -<  (entries, ns, val)
                )                                                                       -<  in_ta

        arrIO $ putStrLn                                                                -<  show body

        -- SOAPAction header

        let soap = mkqelem qid_env
                        [qattr qid_enc (txt "http://schemas.xmlsoap.org/soap/encoding/")]
                        [mkqelem qid_body [] (createNodes body)]

        soapStr     <- xshow soap                                                       -<< ()

excel
arrIO $ putStrLn                                                                -<  soapStr

        setVal "/transaction/http/response/body" soapStr                                -<< in_ta
        )
    where
        createNodes ((name, ns, val):xs)    = (mkqelem (qid_entry name ns)
                                []
                                [eelem "value" += (txt val)]
                                ) : createNodes xs
        createNodes []              = [none]
        qid_entry name ns   = mkNsName ("a:" ++ name) ns

{- |
Example shader to compute the price of an article independent of the application. The value representing the
product id is configured by /config/@productnode, the value to store the price in is configured by
/config/@pricenode.
-}
shopPrice :: ShaderCreator
shopPrice =
    mkCreator $ \config _ ->
    (proc in_ta -> do
        product_node    <- config >>> getVal "/shader/config/@productnode"              -<  ()
        price_node      <- config >>> getVal "/shader/config/@pricenode"                -<  ()
        -- str             <- xshow (constA in_ta)                                      -<< ()
        arrIO $ putStrLn                                                                -<  show in_ta
        prod            <- getVal product_node                                          -<< in_ta
        arrIO $ putStrLn                                                                -<  prod
        (price :: Int)  <- "local" !*> ("/catalogue/" ++ prod ++ "/price")              -<< ()
        arrIO $ putStrLn                                                                -<  show price
        setVal price_node (show price)                                                  -<< in_ta
        )
