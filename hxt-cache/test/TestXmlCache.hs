module Main		-- TestXmlCache
where

import Text.XML.HXT.Arrow	hiding ( readDocument )
import Text.XML.HXT.Arrow.XmlCache

main			:: IO ()
main			= do
                          runX
                            ( readDocument [ ( a_parse_html,	v_1	  )
                                           , ( a_trace,	v_1	  )
                                           , ( a_cache, 	"./cache" )
                                           , ( a_compress, 	v_1	  )
                                           ] "http://www.haskell.org/"
                              >>>
                              writeDocument [ ( a_indent,	v_1	)
                                            ] ""
                            )
                          return ()

