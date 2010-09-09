-- ------------------------------------------------------------

{- |
   Module     : HXmlParser
   Copyright  : Copyright (C) 2005-2010 Uwe Schmidt
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

import Text.XML.HXT.Core
import Text.XML.HXT.XSLT               ( xsltApplyStylesheetFromURI )

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
    = configSysVars config                              -- set all global config options, the output file and all
      >>>                                               -- other user options are stored as key-value pairs in the system state
                                                        -- and can be referenced with "getSysAttr"
      readDocument [] src                               -- no more special read options needed
      >>>
      ( ( traceMsg 1 "start processing document"
          >>>
          ( processDocument $< getSysAttr "xslt" )      -- ask for the xslt schema to be applied
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
      ( writeDocument [] $< getSysAttr "output-file" )  -- ask for the output file stored in the system configuration
      >>>
      getErrStatus

-- simple example of a processing arrow

processDocument :: String -> IOSArrow XmlTree XmlTree
processDocument xsltUri
    = traceMsg 1 ("applying XSLT stylesheet " ++ show xsltUri)
      >>>
      xsltApplyStylesheetFromURI $< getSysAttr xsltUri

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
      [ Option "" ["xslt"] (ReqArg (withSysAttr "xslt") "STYLESHEET") "STYLESHEET is the uri of the XSLT stylesheet to be applied"
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
    header = "HXT XSLT Transformer\n\n" ++
             "Usage: " ++ progName ++ " [OPTION...] [URI or FILE]"
    use    = usageInfo header options

cmdlineOpts     :: [String] -> IO (SysConfigList, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[])
          -> do
             sa <- src n
             help (getConfigAttr a_help ol) sa
             return (ol, sa)
      (_,_,errs)
          -> usage errs
    where
    src []      = return []
    src [uri]   = return uri
    src _       = usage ["only one input uri or file allowed\n"]

    help "1" _     = usage []
    help _  []     = usage ["no input uri or file given\n"]
    help _  _      = return ()

-- ------------------------------------------------------------
