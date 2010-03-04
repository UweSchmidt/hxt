-- -----------------------------------------------------------------------------

module Main		-- TestXmlCache
where

import Data.Maybe

import System.Environment

import Text.XML.HXT.Arrow	hiding ( readDocument )
import Text.XML.HXT.Arrow.XmlCache

main'			:: String -> IO ()
main' url		= runX
                            ( readDocument [ ( a_trace,		v_1	  )
					   , ( a_parse_html,	v_1	  )
					   , ( a_issue_warnings, v_0	  )
                                           , ( a_cache, 	"./cache" )
					   , ( a_document_age,  "10"      )	-- 10 sec., just for testing
                                           , ( a_compress, 	v_1	  )
                                           ] url
                              >>>
                              writeDocument [ ( a_indent,	v_1	)
                                            ] "t.xml"
                            )
                          >> return ()

main			:: IO ()
main			= do
			  as <- getArgs
			  main' . fromMaybe "http://www.haskell.org/" . listToMaybe $ as

-- -----------------------------------------------------------------------------
