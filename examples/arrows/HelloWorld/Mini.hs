module Main
where

import Text.XML.HXT.Arrow

main	:: IO()
main
    = do
      runX ( readDocument [ ] "hello.xml"
	     >>>
	     writeDocument [ ] "bye.xml"
	   )
      return ()

      