-- ------------------------------------------------------------

{- |
   Module     : AbsURIs
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental
   Portability: portable

   AbsURIs - Conversion references into absolute URIs in HTML pages

The commandline interface
-}

-- ------------------------------------------------------------

module Main
where

import Text.XML.HXT.Core                -- import all stuff for parsing, validating, and transforming XML

import System.IO                        -- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import ProcessDocument

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
    = configSysVars config                            -- set all global config options
      >>>
      readDocument [withParseHTML yes] src              -- use HTML parser
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
      ( writeDocument [] $< getSysAttr "output-file" )
      >>>
      getErrStatus

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName        :: String
progName        = "AbsURIs"
    
options         :: [OptDescr SysConfig]
options
    = generalOptions
      ++
      inputOptions
      ++
      [ Option "f" ["output-file"] (ReqArg  (withSysAttr "output-file") "FILE")
               "output file for resulting document (default: stdout)"
      ]
      ++
      outputOptions
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
    header = progName ++ " - Convert all references in an HTML document into absolute URIs\n\n" ++
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
