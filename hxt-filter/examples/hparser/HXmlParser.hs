-- |
-- HXmlParser - Validating XML Parser of the Haskell XML Toolbox
-- XML well-formed checker and validator.
--
-- Author : .\\artin Schmidt and Uwe Schmidt
--
-- Version : $Id: HXmlParser.hs,v 1.14 2005/04/14 12:52:51 hxml Exp $
--
-- this program may be used as example main program for the
-- Haskell XML Toolbox

module Main
where

import Text.XML.HXT.Parser              -- import all stuff for parsing, validating, and transforming XML

import System.IO                        -- import the IO and commandline option stuff
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
      argv <- getArgs                                   -- get the commandline arguments
      al   <- cmdlineOpts argv                          -- and evaluate them, return a key-value list
      res  <- run' $ parser al emptyRoot                -- build a XML root from the list and start parsing
      exitProg (null res)                               -- set return code and terminate

-- ------------------------------------------------------------

exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure 1)
exitProg False  = exitWith ExitSuccess

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--
-- get wellformed document, validates document, propagates and check namespaces
-- and controls output

parser  :: Attributes -> XmlStateFilter state
parser al
    = parseDocument al                                  -- read document
      .>>
      traceMsg 1 "start processing"
      .>>
      liftMf processDocument                            -- process document: in this case rather simple, just apply a pure filter
      .>>
      traceMsg 1 "processing finished"
      .>>
      traceSource
      .>>
      traceTree
      .>>
      ( writeDocument []                                -- write result, all options already set with parseDocument
        `whenNotM`
        hasAttr "no-output"
      )
      .>>
      checkStatus                                       -- check status

processDocument :: XmlFilter
processDocument
    = ( processChildren (deep isXText)                  -- process document, just select text when option \"show-text\" is set
        `when`
        hasAttr "show-text"
      )

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName        :: String
progName        = "HXmlParser"

options         :: [OptDescr (String, String)]
options
    = generalOptions
      ++
      inputOptions
      ++
      outputOptions
      ++
      [ Option "q"      ["no-output"]           (NoArg  ("no-output", "1"))     "no output of resulting document"
      , Option "x"      ["show-text"]           (NoArg  ("show-text", "1"))     "output only the raw text, remove all markup"
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
    header = "HXmlParser - Validating XML Parser of the Haskell XML Toolbox\n" ++
             "XML well-formed checker and validator.\n\n" ++
             "Usage: " ++ progName ++ " [OPTION...] [URI or FILE]"
    use    = usageInfo header options

cmdlineOpts     :: [String] -> IO (Attributes)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[])
          -> do
             sa <- src n
             help (lookup a_help ol) sa
             return (ol ++ sa)
      (_,_,errs)
          -> usage errs
    where
    src []      = return []
    src [uri]   = return [("source", uri)]
    src _       = usage ["only one input uri or file allowed\n"]

    help (Just _) _     = usage []
    help Nothing []     = usage ["no input uri or file given\n"]
    help Nothing _      = return ()

-- ------------------------------------------------------------
