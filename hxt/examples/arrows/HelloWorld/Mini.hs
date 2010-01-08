module Main
where

import Text.XML.HXT.Arrow

main	:: IO()
main
    = runX ( readDocument [ (a_validate, v_0) ] "hello.xml"
	     >>>
	     writeDocument [ ] "bye.xml"
	   )
      >> return ()

      