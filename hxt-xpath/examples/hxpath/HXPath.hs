-- ------------------------------------------------------------

{- |
   Module     : HXPath
   Copyright  : Copyright (C) 2005 Torbel Kuseler, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental
   Portability: portable

   HXPath - XPath Evaluator of the Haskell XML Toolbox (Arrow version)
-}

-- ------------------------------------------------------------

module Main
where

import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.XPath
import Text.XML.HXT.Arrow.XmlState.TypeDefs

import System.IO                        -- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

-- ------------------------------------------------------------

-- |
-- the main program

main :: IO ()
main
    = do
      argv <- getArgs                                   -- get the commandline arguments
      (al, expr, src) <- cmdlineOpts argv                       -- and evaluate them, return a key-value list
      [rc]  <- runX (xpath al expr src)                 -- run the parser arrow
      exitProg (rc >= c_err)                            -- set return code and terminate

-- ------------------------------------------------------------

exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure (-1))
exitProg False  = exitWith ExitSuccess

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--
-- runs in the trivial XmlState monad (with user state set to ())
-- so IO and access to global options is possible

xpath   :: SysConfigList -> String -> String -> IOSArrow b Int
xpath cf expr src
    = configSysVars cf                                  -- set all global config options, the output file and the
      >>>                                               -- other user options are stored as key-value pairs in the stystem state
      readDocument [withCurl []] src
      >>>
      evalXPathExpr
      >>>
      traceMsg 1 "evaluation finished"
      >>>
      traceSource
      >>>
      traceTree
      >>>
      ( formatXPathResult $< getSysVar theIndent )
      >>>
      writeDocument [] "-"
      >>>
      getErrStatus
    where
    evalXPathExpr       :: IOSArrow XmlTree XmlTree
    evalXPathExpr
        = traceMsg 1 ("evaluating XPath expression: " ++ expr)
          >>>
          replaceChildren ( getXPathTreesInDoc expr
                            >>>
                            filterErrorMsg
                          )

    formatXPathResult   :: Bool -> IOSArrow XmlTree XmlTree
    formatXPathResult indent
        = replaceChildren ( mkelem "xpath-result"
                            [ sattr "expr" expr, sattr "source" src ]
                            [ newline, getChildren >>> (this <+> newline) ]
                          )
        where
        newline
            | indent     = txt "\n"
            | otherwise  = none

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName        :: String
progName        = "HXPath"

options         :: [OptDescr SysConfig]
options
    = generalOptions
      ++
      inputOptions
      ++
      outputOptions

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
    header = "HXPath - XPath Evaluator of the Haskell XML Toolbox (Arrow Version)\n" ++
             "Usage: " ++ progName ++ " [OPTION...] <XPath expr> <URL or FILE>"
    use    = usageInfo header options

cmdlineOpts     :: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (scfg,n,[]  )
          -> do
             (ex, sa) <- src n
             help (getConfigAttr a_help scfg)
             return (scfg, ex, sa)
      (_,_,errs)
          -> usage errs
    where
    src [expr, url]
        = return (expr, url)
    src []
        = usage ["XPath expression and input file/url missing"]
    src [_]
        = usage ["input file/url missing"]
    src _
        = usage ["too many arguments"]

    help "1"            = usage []
    help _              = return ()

-- ------------------------------------------------------------
