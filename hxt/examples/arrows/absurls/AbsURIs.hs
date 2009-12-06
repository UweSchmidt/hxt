-- ------------------------------------------------------------

{- |
   Module     : AbsURIs
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental
   Portability: portable
   Version    : $Id: AbsURIs.hs,v 1.1 2005/05/12 16:41:38 hxml Exp $

AbsURIs - Conversion references into absolute URIs in HTML pages

The commandline interface
-}

-- ------------------------------------------------------------

module Main
where

import Text.XML.HXT.Arrow		-- import all stuff for parsing, validating, and transforming XML

import System.IO			-- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe

import ProcessDocument

-- ------------------------------------------------------------

-- |
-- the main program of the Haskell XML Validating Parser

main :: IO ()
main
    = do
      argv <- getArgs					-- get the commandline arguments
      (al, src) <- cmdlineOpts argv			-- and evaluate them, return a key-value list
      [rc]  <- runX (parser al src)			-- run the parser arrow
      exitProg (rc >= c_err)				-- set return code and terminate

-- ------------------------------------------------------------

exitProg	:: Bool -> IO a
exitProg True	= exitWith (ExitFailure (-1))
exitProg False	= exitWith ExitSuccess

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--
-- get wellformed document, validates document, propagates and check namespaces
-- and controls output

parser	:: Attributes -> String -> IOSArrow b Int
parser al src
    = readDocument ([(a_parse_html, v_1)] ++ al) src
      >>>
      traceMsg 1 "start processing"
      >>>
      processDocument
      >>>
      traceMsg 1 "processing finished"
      >>>
      traceSource
      >>>
      traceTree
      >>>
      writeDocument al "-" `whenNot` hasAttr "no-output"
      >>>
      getErrStatus

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName	:: String
progName	= "AbsURIs"
    
options 	:: [OptDescr (String, String)]
options
    = generalOptions
      ++
      selectOptions [a_trace, a_proxy, a_use_curl, a_issue_warnings, a_do_not_issue_warnings, a_do_not_use_curl, a_options_curl, a_encoding] inputOptions
      ++
      outputOptions
      ++
      showOptions

usage		:: [String] -> IO a
usage errl
    | null errl
	= do
	  hPutStrLn stdout use
	  exitProg False
    | otherwise
	= do
	  hPutStrLn stderr (concat errl ++ "\n" ++ use)
	  exitProg True
    where
    header = progName ++ " - Convert all references in an HTML document into absolute URIs\n\n" ++
             "Usage: " ++ progName ++ " [OPTION...] [URI or FILE]"
    use    = usageInfo header options

cmdlineOpts 	:: [String] -> IO (Attributes, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[])
	  -> do
	     sa <- src n
	     help (lookup a_help ol) sa
	     return (ol, sa)
      (_,_,errs)
	  -> usage errs
    where
    src []	= return []
    src [uri]	= return uri
    src _	= usage ["only one input uri or file allowed\n"]

    help (Just _) _	= usage []
    help Nothing []	= usage ["no input uri or file given\n"]
    help Nothing _	= return ()

-- ------------------------------------------------------------
