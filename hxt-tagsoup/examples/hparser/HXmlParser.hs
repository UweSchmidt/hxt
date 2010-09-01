-- ------------------------------------------------------------

{- |
   Module     : HXmlParser
   Copyright  : Copyright (C) 2005-2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental
   Portability: portable

   HXmlParser - Validating XML Parser of the Haskell XML Toolbox

   XML well-formed checker and validator.

   this program may be used as example main program for the
   arrow API of the Haskell XML Toolbox

   commandline parameter evaluation and
   and return code is the most complicated part
   of this example application

-}

-- ------------------------------------------------------------

module Main
where

import Text.XML.HXT.Arrow		-- import all stuff for parsing, validating, and transforming XML
import Text.XML.HXT.TagSoup		-- import TagSoup parser

import System.IO			-- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe

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

parser	:: SysConfigList -> String -> IOSArrow b Int
parser conf src
    = readDocument conf src
      >>>
      ( ( traceMsg 1 "start processing document"
	  >>>
	  processDocument conf
	  >>>
	  traceMsg 1 "document processing finished"
	)
	`when`
	documentStatusOk
      )
      >>>
      traceSource
      >>>
      traceTree
      >>>             -- TODO vvvvv
      ( writeDocument [] (getSysConfigOption a_output_file $ conf)
        `whenNot`
        ( getParamInt 0 "no-output" >>> isA (== 1) )
      )
      >>>
      getErrStatus

-- simple example of a processing arrow

processDocument	:: SysConfigList -> IOSArrow XmlTree XmlTree
processDocument conf
    | extractText
	= traceMsg 1 "selecting plain text"
	  >>>
	  processChildren (deep isText)
    | otherwise
	= this
    where
    extractText	= (== "1") . getSysConfigOption "show-text" $ conf

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName	:: String
progName	= "HXmlParser"
    
options 	:: [OptDescr SysConfig]
options
    = generalSysConfigOptions
      ++
      inputSysConfigOptions
      ++
      tagSoupSysConfigOptions
      ++
      relaxSysConfigOptions
      ++
      outputSysConfigOptions
      ++
      [ Option "q"	["no-output"]		(NoArg $ optionToSysConfig ("no-output", "1"))		"no output of resulting document"
      , Option "x"	["show-text"]		(NoArg $ optionToSysConfig ("show-text", "1"))		"output only the raw text, remove all markup"
      ]
      ++
      showSysConfigOptions

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
    header = "HXmlParser - Validating XML Parser of the Haskell XML Toolbox with Arrow Interface\n" ++
             "XML well-formed checker, DTD validator, Relax NG validator.\n\n" ++
             "Usage: " ++ progName ++ " [OPTION...] [URI or FILE]"
    use    = usageInfo header options

cmdlineOpts 	:: [String] -> IO (SysConfigList, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (scfg,n,[])
	  -> do
	     sa <- src n
	     help (getSysConfigOption a_help scfg) sa
	     return (scfg, sa)
      (_,_,errs)
	  -> usage errs
    where
    src []	= return []
    src [uri]	= return uri
    src _	= usage ["only one input uri or file allowed\n"]

    help "1" _	= usage []
    help _ []	= usage ["no input uri or file given\n"]
    help _ _	= return ()

-- ------------------------------------------------------------
