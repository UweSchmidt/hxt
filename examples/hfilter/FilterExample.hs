-- |
-- HXPath - XPath Evaluator of the Haskell XML Toolbox
--
-- Author : Torben Kuseler, Uwe Schmidt
--
-- Version : $Id: FilterExample.hs,v 1.9 2004/09/02 19:11:50 hxml Exp $

module Main(main)
where

import Text.XML.HXT.Parser		-- import all stuff for parsing, validating, and transforming XML

import System.IO			-- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Network.URI

import Data.Maybe


-- ------------------------------------------------------------

-- |
-- the main program of the Filter example 

main :: IO ()
main
    = do
      argv <- getArgs
      al   <- cmdlineOpts argv
      res  <- run' $ filterEx al emptyRoot
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

filterEx	:: Attributes -> XmlStateFilter state
filterEx al
    = parseDocument al
      .>>
      liftMf filterDocument
      .>>
      writeDocument []
      .>>
      checkStatus

-- ------------------------------------------------------------
--
-- some simple selection and editing filter

filterDocument	:: XmlFilter		-- the main processing filter
filterDocument t
    = processChildren (filterexamples xf) t
      where
      xf = xshow . getValue "filter" $ t

filterexamples	:: String -> XmlFilter
filterexamples "all-text"				= deep isXText
filterexamples "all-body-text-without-whitespace"	= getAllBodyText
filterexamples "all-a-tags-with-href-attr"		= getAllTagsWithAttr "a" "href"
filterexamples "all-href-values-of-a-tags"		= getAllAttrValues   "a" "href"
filterexamples "change-img-src-to-abs-uri"		= changeToAbsolutURIs "http://localhost/images/" "img" "src"
filterexamples f					= cmt ("error: " ++ f ++ " not defined") +++ this

getAllBodyText		:: XmlFilter
getAllBodyText
    = isTag "html" .> getChildren .> isTag "body" .> getChildren .> deep (isOfText noWhiteSpace)
      where
      noWhiteSpace = not . all (`elem` " \t\n")

getAllTagsWithAttr	:: String -> String -> XmlFilter
getAllTagsWithAttr tn an
    = multi (isTag tn .> hasAttr an)

getAllAttrValues	:: String -> String -> XmlFilter
getAllAttrValues tn an
    = multi (isTag tn .> getValue an)

changeToAbsolutURIs	:: String -> String -> String -> XmlFilter
changeToAbsolutURIs base tn an
    = processTopDown (changeURI `when` isTag tn)
      where
      changeURI    = modifyAttr an (toAbsURI base)

toAbsURI	:: String -> String -> String
toAbsURI base uri
    = fromMaybe "" $ expand
    where
    expand = do
	     base' <- parseURI base
	     uri'  <- parseURI uri
	     abs'  <- relativeTo uri' base'
	     return $ show abs'

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName	:: String
progName	= "HXmlFilter"
    
options 	:: [OptDescr (String, String)]
options
    = generalOptions
      ++
      inputOptions
      ++
      outputOptions
      ++
      [ Option "f"	["filter"]		(ReqArg flt "FILTER")			( "filter to be applied, one of: " ++ nl ++
											  "\"all-text\"," ++ nl ++
											  "\"all-body-text-without-whitespace\"," ++ nl ++
											  "\"all-a-tags-with-href-attr\"," ++ nl ++
											  "\"all-href-values-of-a-tags\"," ++ nl ++
											  "\"change-img-src-to-abs-uri\"" )
      ]
      ++
      showOptions
    where
    nl = "\n\t\t\t\t\t  "
    att n v	= (n, v)
    flt = att "filter"


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
    header = "FilterExamples - Examples for applying XML filter from the Haskell XML Toolbox\n" ++
             "Usage: " ++ progName ++ " [OPTION...] [URI or FILE]"
    use    = usageInfo header options

cmdlineOpts 	:: [String] -> IO (Attributes)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[]  )
	  -> do
	     sa <- src n
	     help (lookup a_help ol)
	     return (ol ++ sa)
      (_,_,errs)
	  -> usage errs
    where
    src []	= return [("source", "")]
    src [uri]	= return [("source", uri)]
    src _	= usage ["only one input uri or file allowed\n"]

    help Nothing	= return ()
    help (Just _)	= usage []

-- ------------------------------------------------------------
