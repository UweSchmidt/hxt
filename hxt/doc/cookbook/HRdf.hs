{- |
   Module     : Main
   Author     : Manuel Ohlendorf
   
   Maintainer : Manuel Ohlendorf
   Stability  : experimental
   Portability: portable
   Version    : 

The main programme, using the RDF\/XML parser and the SPARQL query language parser.
-}

module Main
where

import Text.XML.HXT.Arrow
import System.IO      -- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Char
import Data.Maybe
import RDF
-- ------------------------------------------------------------

-- |
-- the main programme,

main :: IO ()
main
    = do
      argv <- getArgs         -- get the commandline arguments
      (al, qr, src) <- cmdlineOpts argv       -- and evaluate them, return a key-value list
      [rc] <- runX (processDocument al qr src)
      exitProg (rc >= c_err)
-- ------------------------------------------------------------

exitProg  :: Bool -> IO a
exitProg True = exitWith (ExitFailure 1)
exitProg False  = exitWith ExitSuccess

-- ------------------------------------------------------------

processDocument :: Attributes -> String -> String -> IOSArrow b Int
processDocument al qr src
    =   readDocument al src 
        >>> removeAllWhiteSpace >>> propagateNamespaces 
        >>>
        ifA normaliseOption 
            (normalizeRDF >>> indentDoc >>> writeDocument al outputFile) 
            (replaceChildren (parseRDF >>> processQuery qr)
             >>> writeDocument ((a_output_xml,v_0):al) outputFile)
        >>>
        getErrStatus
   where
    
    
   normaliseOption = getParamString a_normalise >>> isA (== "1")
    
   outputFile = (fromMaybe "" (lookup a_output_file al))

processQuery :: String -> IOSArrow RDFStore XmlTree 
processQuery qr = ifP (const (qr == ""))
                      (arr showRDFStore)
                      ((getSPARQLQuery qr &&& this) 
                        >>> arr2 evalQuery 
                        >>> arr showResult)
                  >>> mkText



-- ------------------------------------------------------------
--
-- the boring option definition and evaluation part
--
-- see doc for System.Console.GetOpt

a_normalise :: String
a_normalise = "normaliseRDF"

rdfOptions :: [OptDescr (String, String)]
rdfOptions = [Option "n" [a_normalise] (NoArg (a_normalise, "1")) "generate output in normalised RDF, SPARQL not supported here"
			]

progName	:: String
progName	= "RDFParser"
    
options 	:: [OptDescr (String, String)]
options
    = selectOptions [ a_help
		    ] generalOptions
      ++
      selectOptions [ a_trace
		    , a_proxy
		    , a_use_curl
		    , a_options_curl
		    , a_encoding
		    ] inputOptions
      ++
      selectOptions [ a_output_file
		    ] outputOptions
      ++
      rdfOptions
      ++
      showOptions

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
    header = "RDFParser - a program to parse simple RDF/XML documents and search for triples with SPARQL\n"++ 
             "Usage: " ++ progName ++ " [OPTION...] <SPARQL query> [URI or FILE]"
    use    = usageInfo header options


cmdlineOpts 	:: [String] -> IO (Attributes,String, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[]  )
	  -> do
	     (qr, sa) <- src n
	     help (lookup a_help ol)
	     return (ol, qr, sa)
      (_,_,errs)
	  -> usage errs
    where
    src [qr, i]	= return (qr, i)
    src []	= usage ["input file/uri missing"]
    src [u]	= return ("", u)
    src _ = usage ["only one input url or file allowed\n"]

    help Nothing	= return ()
    help (Just _)	= usage []

-- ------------------------------------------------------------
