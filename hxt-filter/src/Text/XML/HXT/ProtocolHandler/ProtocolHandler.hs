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

#ifdef ONLY_NATIVE_HTTP

import Text.XML.HXT.Parser.ProtocolHandlerHttpNative
    ( getHttpContentsWithHttp
    )

#else
#ifdef ONLY_CURL_HTTP

import Text.XML.HXT.Parser.ProtocolHandlerHttpCurl
    ( getHttpContentsWithCurl
    )

#else
#ifdef NO_HTTP
#else

import Text.XML.HXT.Parser.ProtocolHandlerHttpNativeOrCurl
    ( getHttpContentsNativeOrWithCurl
    )

#endif
#endif
#endif

-- ------------------------------------------------------------
--

getProtocolHandler	:: String -> (URI -> XmlStateFilter a)
getProtocolHandler proto
    = fromMaybe getUnsupported handler
      where
      handler  = lookup proto protocolHandler

--
-- the fall back protocol handler

getUnsupported	:: URI -> XmlStateFilter a
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

protocolHandler	:: [(String, URI -> XmlStateFilter a)]
protocolHandler
    = [ ("file", getFileContents)
      , ("http",

#ifdef ONLY_NATIVE_HTTP

	 getHttpContentsWithHttp

#else
#ifdef ONLY_CURL_HTTP

	 getHttpContentsWithCurl

#else
#ifdef NO_HTTP

         getUnsupported

#else

	 getHttpContentsNativeOrWithCurl

#endif
#endif
#endif
	)
      ]

-- ------------------------------------------------------------


