module Main
where

import Data.List ( isPrefixOf )
import Text.XML.HXT.Arrow
import System.Log.Logger

main	:: IO ()
main	= do
	  runX ( hxtSetLogLevel INFO
		 >>>
	         hxtSetErrorLog
		 >>>
		 readDocument [ (a_parse_html, v_1)
			      , (a_remove_whitespace, v_1)
			      ] "http://www.haskell.org"
		 >>>
		 processChildren (deep isText)
		 >>>
		 writeDocument [] ""
	       )
	   >> return ()

hxtLoggerName	= "hxt"

hxtLogger :: Int -> String -> IO ()
hxtLogger level msg
    = logM hxtLoggerName priority (show priority ++ "\t" ++ msg')
    where
    msg'
	| "-- (" `isPrefixOf` msg	= drop 7 msg
	| otherwise			= msg
    priority = toPriority level
    toPriority level
	| level <= 0	= WARNING
	| level == 1    = NOTICE
	| level == 2	= INFO
	| level >= 3	= DEBUG

hxtSetLogLevel priority
    = setTraceLevel (fromPriority priority)
      >>>
      setTraceCmd hxtLogger
      >>>
      perform ( arrIO0 $
		updateGlobalLogger hxtLoggerName (setLevel priority)
	      )
    where
    fromPriority NOTICE	 = 1
    fromPriority INFO	 = 2
    fromPriority DEBUG	 = 3
    fromPriority _	 = 0

hxtSetErrorLog	= setErrorMsgHandler False hxtErrorLogger

hxtErrorLogger	:: String -> IO ()
hxtErrorLogger msg
    = logM hxtLoggerName priority (show priority ++ "\t" ++ (drop 2 . dropWhile (/= ':') $ msg)) 
    where
    priority = prio . drop 1 $ msg
    prio m
	| "fatal" `isPrefixOf` m	= CRITICAL
        | "error" `isPrefixOf` m	= ERROR
	| "warning" `isPrefixOf` m	= WARNING
	| otherwise			= NOTICE