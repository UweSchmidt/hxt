-- ------------------------------------------------------------

module Main				-- TestXmlTreeBinary
where

import Text.XML.HXT.DOM.Binary		-- the Binary instance for XmlTree

import Text.XML.HXT.Arrow
import Text.XML.HXT.Arrow.Binary

-- ------------------------------------------------------------

main		:: IO ()
main		= do
                  runX   ( readDocument [] "t.xml"
                           >>>
                           withTraceLevel 4 (traceDoc "file t.xml read")
                           >>>
                           ( this
                             &&&
                             ( traceMsg 0 "writing file t.bin and t.bin.bz"
                               >>>
                               ( writeBinaryValue False "t.bin"
				 &&&
			         writeBinaryValue True "t.bin.bz"
			       )
                               >>>
                               traceMsg 0 "files written, reading t.bin and t.bin.bz"
                               >>>
                               ( readBinaryValue False "t.bin"
				 &&&
			         readBinaryValue True  "t.bin.bz"
			       )
                               >>>
                               withTraceLevel 4 ( (perform $ arr fst >>> traceDoc "contents of binary file")
						  >>>
						  (perform $ arr snd >>> traceDoc "contents of compressed binary file")
						)
                             )
                           )
                           >>>
                           traceMsg 0 "comparing documents"
                           >>>
                           ( ( isA (\ (i, (o1, o2)) -> i == o1 && i == o2)
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

-- ------------------------------------------------------------
