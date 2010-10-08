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

import Text.XML.HXT.Core                -- import all stuff for parsing, validating, and transforming XML
import Text.XML.HXT.Expat               -- import Expat parser
import Text.XML.HXT.Arrow.XmlState.TypeDefs

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
    = configSysVars ((withExpat True) : config)				-- set expat parser
      >>>
      readDocument (withTrace 2 : config) src
      >>>
      ( ( traceMsg 1 "start processing document"
          >>>
          processDocument $< getSysAttr "action"
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
      issueExpatErr	-- this is only of interest when parsing is done lazily, with "withExpat False" config
      >>>
      ( ( writeDocument [] $< getSysAttr "output-file" )
        `whenNot`
        ( getSysAttr "no-output" >>> isA (== "1") )
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

processDocument "show-tree"
    = traceMsg 1 "show-tree document"
      >>>
      writeDocument [withShowTree yes] ""

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
      outputOptions
      ++
      [ Option "q"      ["no-output"]   (NoArg $ withSysAttr "no-output"    "1") "no output of resulting document"
      , Option "x"      ["action"]      (ReqArg (withSysAttr "action") "ACTION") "actions are: only-text, indent, no-op"
      , Option "Z"      ["lazy"]        (NoArg $ withExpat False               ) "errors are not checked during parse"
      , Option "N"      ["not-lazy"]    (NoArg $ withExpat True                ) "errors are checked during parse"
      ]
      ++
      showOptions

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
             "XML well-formed checker, DTD validator, Relax NG validator.\n\n" ++
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
