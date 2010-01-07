-- ------------------------------------------------------------

{- |
   Module     : HXmlParser
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental
   Portability: portable

   HXmlParser - Validating XML Parser of the Haskell XML Toolbox with XSLT support

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

import Text.XML.HXT.Arrow
import Text.XML.HXT.XSLT               ( xsltApplyStylesheetFromURI )

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

parser	:: Attributes -> String -> IOSArrow b Int
parser al src
    = readDocument al src
      >>>
      ( ( traceMsg 1 "start processing document"
	  >>>
	  processDocument al
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
      >>>
      writeDocument al (fromMaybe "-" . lookup a_output_file $ al) `whenNot` hasAttr "no-output"
      >>>
      getErrStatus

-- simple example of a processing arrow

processDocument	:: Attributes -> IOSArrow XmlTree XmlTree
processDocument al
    | applyXSLT
	= traceMsg 1 ("applying XSLT stylesheet " ++ show xsltUri)
	  >>>
	  xsltApplyStylesheetFromURI xsltUri
    | extractText
	= traceMsg 1 "selecting plain text"
	  >>>
	  processChildren (deep isText)
    | otherwise
	= this
    where
    applyXSLT	= hasEntry "xslt"	  $ al
    extractText	= optionIsSet "show-text" $ al
    xsltUri     = lookup1 "xslt"          $ al

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName	:: String
progName	= "HXmlParser"
    
options 	:: [OptDescr (String, String)]
options
    = generalOptions
      ++
      inputOptions
      ++
      relaxOptions
      ++
      outputOptions
      ++
      [ Option ""	["xslt"]		(ReqArg (att "xslt") "STYLESHEET")	"STYLESHEET is the uri of the XSLT stylesheet to be applied"
      , Option "q"	["no-output"]		(NoArg  ("no-output", "1"))		"no output of resulting document"
      , Option "x"	["show-text"]		(NoArg	("show-text", "1"))		"output only the raw text, remove all markup"
      ]
      ++
      showOptions
    where
    att n v = (n, v)

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
             "XML well-formed checker, DTD validator, Relax NG validator and XSLT transformer.\n\n" ++
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
