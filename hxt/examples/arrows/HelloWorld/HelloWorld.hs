module Main
where

import Text.XML.HXT.Core
import System.Exit

main	:: IO()
main
    = do
      [rc] <- runX ( readDocument [ withTrace 1
				  , withValidate no
				  ] "hello.xml"
		     >>>
		     writeDocument [ withOutputEncoding utf8
				   ] "-"
		     >>>
		     getErrStatus
		   )
      exitWith ( if rc >= c_err
		 then ExitFailure 1
		 else ExitSuccess
	       )
      
