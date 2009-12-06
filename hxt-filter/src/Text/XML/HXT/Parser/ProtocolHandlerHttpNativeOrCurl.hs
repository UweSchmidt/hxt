-- ------------------------------------------------------------
--
-- protocol handler functions for native http access

module Text.XML.HXT.Parser.ProtocolHandlerHttpNativeOrCurl
    ( getHttpContentsNativeOrWithCurl
    )

where

import Text.XML.HXT.DOM.XmlKeywords
    ( a_use_curl
    )

import Text.XML.HXT.DOM.XmlState

import Network.URI
    ( URI
    )

import Text.XML.HXT.Parser.ProtocolHandlerHttpNative
    ( getHttpContentsWithHttp
    )

import Text.XML.HXT.Parser.ProtocolHandlerHttpCurl
    ( getHttpContentsWithCurl
    )


-- ------------------------------------------------------------
--
-- the http protocol handler switch for internal / external access

getHttpContentsNativeOrWithCurl		:: URI -> XmlStateFilter a
getHttpContentsNativeOrWithCurl uri n
    = do
      curl <- getSysParamInt a_use_curl 0
      ( if curl /= 0
	then getHttpContentsWithCurl
	else getHttpContentsWithHttp ) uri n


-- ------------------------------------------------------------
