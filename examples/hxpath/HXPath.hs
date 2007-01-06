-- |
-- HXPath - XPath Evaluator of the Haskell XML Toolbox
--
-- Author : Torben Kuseler, Uwe Schmidt
--
-- Version : $Id: HXPath.hs,v 1.8 2004/09/02 19:11:53 hxml Exp $

module Main(main)
where

import Text.XML.HXT.Parser		-- import all stuff for parsing, validating, and transforming XML
import Text.XML.HXT.XPath

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
      argv <- getArgs
      al   <- cmdlineOpts argv
      res  <- run'
	      $ xpath
		    $ newDocument' al			-- build a XML root from the list and start parsing
      exitProg (null res)				-- set return code and terminate

-- ------------------------------------------------------------

exitProg	:: Bool -> IO a
exitProg True	= exitWith (ExitFailure (-1))
exitProg False	= exitWith ExitSuccess

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--
-- runs in the trivial XmlState monad (with user state set to ())
-- so IO and access to global options is possible

xpath	:: XmlStateFilter state
xpath t
    = parseDocument []
      .>>
      evalXPathExpr
      .>>
      traceMsg 1 "evaluation finished"
      .>>
      traceSource
      .>>
      traceTree
      .>>
      liftMf formatXPathResult
      .>>
      writeDocument []
      .>>
      checkStatus
      $ t
      where

      evalXPathExpr		:: XmlStateFilter state
      evalXPathExpr t'
	  = let
	    expr = valueOf "expr" t'
	    res1 = getXPath expr  t'
            in
	    do
	    trace 1 ("evaluating XPath expression" ++ show expr)
	    res2 <- issueError $$< res1
	    return ( if length res2 == length res1
		     then replaceChildren res2 t'
		     else replaceChildren []   t'
		   )

      formatXPathResult 	:: XmlFilter
      formatXPathResult t'
	  = replaceChildren (xpRes t') t'
	    where
	    xpRes
		= mkXTag "xpath-result" xpAttr formatRes
	    xpAttr
		= ( getAttrl
		    .> ( isAttr "expr"
			 +++
			 isAttr "source"
		       )
		  )
	    formatRes
		= addFirstNL . (getChildren .> (this +++ nl))
	    addFirstNL
		= (nl t' ++)
	    nl	= if newline
		  then txt "\n"
		  else none
            newline
		= satisfies (hasAttr a_indent) t'

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName	:: String
progName	= "HXPath"
    
options 	:: [OptDescr (String, String)]
options
    = generalOptions
      ++
      inputOptions
      ++
      outputOptions

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
    header = "HXPath - XPath Evaluator of the Haskell XML Toolbox\n" ++
             "Usage: " ++ progName ++ " [OPTION...] <XPath expr> <URL or FILE>"
    use    = usageInfo header options

cmdlineOpts 	:: [String] -> IO (Attributes)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[]  )
	  -> do
	     help (lookup a_help ol)
	     sa <- src n
	     return (ol ++ sa)
      (_,_,errs)
	  -> usage errs
    where
    src [expr, url]
	= return [(a_source, url), ("expr", expr)]
    src []
	= usage ["XPath expression and input file/url missing"]
    src [_]
	= usage ["input file/url missing"]
    src _
	= usage ["too many arguments"]

    help Nothing	= return ()
    help (Just _)	= usage []

-- ------------------------------------------------------------
