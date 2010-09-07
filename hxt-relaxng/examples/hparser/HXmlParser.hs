-- ------------------------------------------------------------

{- |
   Module     : HXmlParser
   Copyright  : Copyright (C) 2005-2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental
   Portability: portable

   HXmlParser - Minimal Validating XML Parser of the Haskell XML Toolbox, no HTTP supported

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

import Text.XML.HXT.Core                -- import all stuff for parsing, validating, and transforming XML
import Text.XML.HXT.Curl		-- import HTTP access
import Text.XML.HXT.RelaxNG             -- import RelaxNG validation

import System.IO                        -- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

-- ------------------------------------------------------------

-- |
-- the main program of the Haskell XML Validating Parser

main :: IO ()
main
    = do
      argv <- getArgs                                   -- get the commandline arguments
      (al, src) <- cmdlineOpts argv                     -- and evaluate them, return a key-value list
      [rc]  <- runX (parser al src)                     -- run the parser arrow
      exitProg (rc >= c_err)                            -- set return code and terminate

-- ------------------------------------------------------------

exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure (-1))
exitProg False  = exitWith ExitSuccess

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--
-- get wellformed document, validates document, propagates and check namespaces
-- and controls output

parser  :: SysConfigList -> String -> IOSArrow b Int
parser config src
    = configSysParams config                            -- set all global config options, the output file and the
      >>>                                               -- other user options are stored as key-value pairs in the stystem state
      readDocument [ withCurl []
		   ] src                                -- no more special read options needed
      >>>
      ( ( traceMsg 1 "start processing document"
          >>>
          ( processDocument $< getSysAttr "action" )    -- ask for the action stored in the key-value list of user defined values 
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
      ( (writeDocument [] $< getSysAttr "output-file")  -- ask for the output file stored in the system configuration
        `whenNot`
        ( getSysAttr "no-output" >>> isA (== "1") )     -- ask for the no-output attr value in the system key-value list
      )
      >>>
      getErrStatus

-- simple example of a processing arrow, selected by a command line option

processDocument :: String -> IOSArrow XmlTree XmlTree
processDocument "only-text"
    = traceMsg 1 "selecting plain text"
      >>>
      processChildren (deep isText)

processDocument "indent"
    = traceMsg 1 "indent document"
      >>>
      indentDoc

processDocument _action
    = traceMsg 1 "default action: do nothing"
      >>>
      this

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName        :: String
progName        = "HXmlParser"
    
options         :: [OptDescr SysConfig]
options
    = generalOptions
      ++
      inputOptions
      ++
      relaxOptions
      ++
      outputOptions
      ++
      showOptions
      ++
      [ Option "f"      ["output-file"] (ReqArg  (withSysAttr "output-file") "FILE") "output file for resulting document (default: stdout)"
      , Option "q"      ["no-output"]   (NoArg $  withSysAttr "no-output"      "1")   "no output of resulting document"
      , Option "x"      ["action"]      (ReqArg  (withSysAttr "action")   "ACTION")   "actions are: only-text, indent, no-op"
      ]
      -- the last 2 option values will be stored by withAttr in the system key-value list
      -- and can be read by getSysAttr key

usage           :: [String] -> IO a
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
             "XML well-formed checker, DTD validator, RelaxNG validator, HTML parser.\n\n" ++
             "Usage: " ++ progName ++ " [OPTION...] [URI or FILE]"
    use    = usageInfo header options

cmdlineOpts     :: [String] -> IO (SysConfigList, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (scfg,n,[])
          -> do
             sa <- src n
             help (getConfigAttr a_help scfg) sa
             return (scfg, sa)
      (_,_,errs)
          -> usage errs
    where
    src []      = return []
    src [uri]   = return uri
    src _       = usage ["only one input uri or file allowed\n"]

    help "1" _  = usage []
    help _ []   = usage ["no input uri or file given\n"]
    help _ _    = return ()

-- ------------------------------------------------------------
