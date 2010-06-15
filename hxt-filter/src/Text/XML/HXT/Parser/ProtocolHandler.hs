{-# LINE 1 "ProtocolHandler.hs" #-}
-- ------------------------------------------------------------
--
-- protocol handler functions
-- configuration file
-- real handler is generated with cpp

module Text.XML.HXT.Parser.ProtocolHandler
    ( getProtocolHandler
    )

where

import Text.XML.HXT.DOM.XmlState

-- ------------------------------------------------------------
--
-- file io and network

import Data.Maybe
    ( fromMaybe
    )

import Network.URI
    ( URI
    , uriScheme
    )

import Text.XML.HXT.Parser.ProtocolHandlerFile
    ( getFileContents
    )


import Text.XML.HXT.Parser.ProtocolHandlerHttpNativeOrCurl
    ( getHttpContentsNativeOrWithCurl
    )





-- ------------------------------------------------------------
--

getProtocolHandler      :: String -> (URI -> XmlStateFilter a)
getProtocolHandler proto
    = fromMaybe getUnsupported handler
      where
      handler  = lookup proto protocolHandler

--
-- the fall back protocol handler

getUnsupported  :: URI -> XmlStateFilter a
getUnsupported uri
    = addFatal ( "unsupported protocol "
                 ++ show (uriScheme uri)
                 ++ " in URI: "
                 ++ show uri
               )

-- ------------------------------------------------------------
--
-- the table of potocol handlers
-- looked up in getProtocolHandler

protocolHandler :: [(String, URI -> XmlStateFilter a)]
protocolHandler
    = [ ("file", getFileContents)
      , ("http",


         getHttpContentsNativeOrWithCurl




        )
      ]

-- ------------------------------------------------------------


