-- |
-- Citations - a program for selecting all citations from www.blutgraetsche.de
--
-- Author : Uwe Schmidt

module Main
where

import Text.XML.HXT.Arrow		-- import all stuff for parsing, validating, and transforming XML

import System.IO			-- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe
import Data.Char
import Data.List

baseURL	:: String
baseURL	= "http://www.blutgraetsche.de/dkg/sk/"
{- baseURL	= "./" -}

pages	:: [String]
pages	= [ "zitate1.php"
	  , "zitate2.php"
	  , "zitate3.php"
	  , "zitate4.php"
	  , "zitate6.php"
	  , "zitateneu.php"
	  ]

{- pages = ["zitate5.php"] -}

-- ------------------------------------------------------------

-- |
-- the main program

main :: IO ()
main
    = do
      argv <- getArgs					-- get the commandline arguments
      al   <- cmdlineOpts argv				-- and evaluate them, return a key-value list
      [rc] <- runX (process al)
      exitProg (rc >= c_err)

-- ------------------------------------------------------------

exitProg	:: Bool -> IO a
exitProg True	= exitWith (ExitFailure 1)
exitProg False	= exitWith ExitSuccess

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--

process	:: Attributes -> IOSArrow b Int
process al
    = root [] [ listA ( catA (map readPage pages)
			>>>
			selectCitations
		      )
		>>>
		arr formatCitations
		>>>
		mkText
	      ]
      >>>
      writeDocument [ (a_output_encoding, isoLatin1)
		    , (a_output_xml, "0")
		    ] (fromMaybe "-" . lookup a_output_file $ al)
      >>>
      getErrStatus
    where
    readPage	:: String -> IOSArrow b XmlTree
    readPage p
	= readDocument ( al 
			 ++
			 [ (a_parse_html,	"1")
			 , (a_encoding,		isoLatin1 )
			 , (a_issue_warnings,	"0"	  )
			 ]
		       ) (baseURL ++ p)

    selectCitations	:: IOSArrow XmlTree String
    selectCitations
	= getChildren >>> hasName "html"
	  >>>
	  getChildren >>> hasName "body"
	  >>>
	  getChildren >>> deep (hasName "table")
	  >>>
	  getChildren >>> deep (hasName "table")
	  >>>
	  getChildren >>> deep (hasName "table")
	  >>>
	  getChildren >>> deep (hasName "table")
	  >>>
	  getChildren >>> hasName "tr" >>> hasAttr "id"
	  >>>
	  xshow ( ( ( (getChildren >>> hasName "td") >>. take 1)
		    >>>
		    deep (isText >>> removeWhiteSpace)
		  )
		  <+>
		  txt ":\n\t"
		  <+>
		  ( ( (getChildren >>> hasName "td") >>. drop 1)
		    >>>
		    deep (isText >>> removeWhiteSpace)
		  )
		)

    formatCitations	:: [String] -> String
    formatCitations
	= ( "module Zitate\nwhere\n\nzitate :: [(Int, String)]\nzitate =\n    [" ++ )
	  .
	  ( ++ "\n    ]\n" )
	  .
	  ( \ l -> if null l then "" else foldr1 (\ s1 s2 -> s1 ++ "\n    ," ++ s2) l)
	  .
	  zipWith (\ i c -> "( " ++ show i ++ ", " ++ show c ++ " )") [1::Int ..]
          .
	  nub
	  .
	  sort

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName	:: String
progName	= "Citations"
    
options 	:: [OptDescr (String, String)]
options
    = generalOptions
      ++
      inputOptions
      ++
      outputOptions
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
    header = progName ++ " - Get all Citations from www.blutgraetsche.de" ++
             "Usage: " ++ progName ++ " [OPTION...]"
    use    = usageInfo header options

cmdlineOpts 	:: [String] -> IO (Attributes)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,_,[]  )
	  -> do
	     help (lookup "usage" ol)
	     return ol
      (_,_,errs)
	  -> usage errs
    where
    help Nothing	= return ()
    help (Just _)	= usage []

-- ------------------------------------------------------------
