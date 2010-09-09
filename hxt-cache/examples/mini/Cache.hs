module Main
where

import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.Cache
import Codec.Compression.BZip (compress, decompress)

main = runX ( readDocument [ withParseHTML yes
                           , withWarnings no
                           , withRemoveWS yes
                           , withCache "/tmp" 10 False			-- enable /tmp as cache dir
                                                                        -- documents remain valid 10 seconds (for testing)
                                                                        -- no 404 documents are cached
                           , withCompression (compress, decompress)     -- the cached files will be BZip compressed
                           , withTrace 2
                           , withCurl []                                -- curl is taken for HTTP access
                           ] "http://www.fh-wedel.de/"
              >>>
              processChildren (hasName "html" /> hasName "body" //> isText)
              >>>
              writeDocument [] ""
            )
       >> return ()
