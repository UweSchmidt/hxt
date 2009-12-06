module Main
where

import Text.XML.HXT.Arrow
import System.Exit

main	:: IO()
main
    = do
      [rc] <- runX ( readDocument [ (a_trace, "1")
				  , (a_validate, v_0)
				  ] "hello.xml"
		     >>>
		     writeDocument [ (a_output_encoding, isoLatin1)
				   ] "-"
		     >>>
		     getErrStatus
		   )
      exitWith ( if rc >= c_err
		 then ExitFailure 1
		 else ExitSuccess
	       )
      
