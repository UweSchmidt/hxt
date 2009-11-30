module Main		-- TestXmlTreeBinary
where

import Data.Binary
import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.Binary

main		:: IO ()
main		= do
                  runX   ( readDocument [] "t.xml"
                           >>>
                           withTraceLevel 4 (traceDoc "file t.xml read")
                           >>>
                           ( this
                             &&&
                             ( traceMsg 0 "writing file t.bin"
                               >>>
                               arrIO (encodeFile "t.bin")
                               >>>
                               traceMsg 0 "file written, reading t.bin"
                               >>>
                               arrIO0 (decodeFile "t.bin")
                               >>>
                               withTraceLevel 4 (traceDoc "contents of binary file")
                             )
                           )
                           >>>
                           traceMsg 0 "comparing documents"
                           >>>
                           ( ( isA (uncurry (==))
                               >>>
                               traceMsg 0 "test passed: documents are equal"
                             )
                             `orElse`
                             ( traceMsg 0 "test failed: documents differ"
                               >>>
                               none
                             )
                           )
                         )
                  return ()

