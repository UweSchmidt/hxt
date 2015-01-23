-- |
-- Citations - a program for selecting all citations from www.blutgraetsche.de
--
-- Author : Uwe Schmidt
--
-- Version : $Id: Citations.hs,v 1.6 2004/09/02 19:11:49 hxml Exp $

module Main
where

import Text.XML.HXT.Parser              -- import all stuff for parsing, validating, and transforming XML

import System.IO                        -- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe
import Data.Char
import Data.List

baseURL :: String
baseURL = "http://www.blutgraetsche.de/dkg/sk/"

pages   :: [String]
pages   = [ "zitate1.php"
          , "zitate2.php"
          , "zitate3.php"
          , "zitate4.php"
          , "zitate6.php"
          , "zitateneu.php"
          ]

-- ------------------------------------------------------------

-- |
-- the main program

main :: IO ()
main
    = do
      argv <- getArgs                                   -- get the commandline arguments
      al   <- cmdlineOpts argv                          -- and evaluate them, return a key-value list
      res  <- run'                                      -- build a XML root from the list and start parsing
              $ getDocumentAndGenerateHaskellCode
                    $ newDocument' ( al
                                     ++
                                     [ (a_source,               baseURL   )
                                     , (a_encoding,             isoLatin1 )
                                     , (a_issue_warnings,       "0"       )
                                     ]
                                   )
      exitProg (null res)                               -- set return code and terminate

-- ------------------------------------------------------------

exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure 1)
exitProg False  = exitWith ExitSuccess

-- ------------------------------------------------------------

-- |
-- the /real/ main program
--

getDocumentAndGenerateHaskellCode       :: XmlStateFilter state
getDocumentAndGenerateHaskellCode t
    = liftMf editRoot
      .>>
      processChildrenM getHtmlDoc
      .>>
      liftMf (substChildren (formatCitations . selectCitations))
      .>>
      outTree
      .>>
      checkStatus
      $
      t
      where

      editRoot          :: XmlFilter
      editRoot
          = substChildren chl
            where
            chl = cat [ modifyAttr a_source (++ pg) | pg <- pages ]

      outTree           :: XmlStateFilter state
      outTree
          = liftMf
            ( choice
              [ hasAttr "show-haskell"  :-> haskellRepOfXmlDoc
              , hasAttr "show-tree"     :-> treeRepOfXmlDoc
              , this                    :-> this
              ]
            )
            .>>
            writeXmlDoc

      selectCitations   :: XmlFilter
      selectCitations
          = ( deep (isTag "html")
              .>
              getChildren .> deep (isTag "body")
              .>
              getChildren .> deep (isTag "table")
              .>
              getChildren .> deep (isTag "table")
              .>
              getChildren .> deep (isTag "table")
              .>
              getChildren .> deep (isTag "table")
            )
            .>
            getChildren .> isTag "tr" .> hasAttr "id"
            .>
            ( xtext
              .
              xshow
              .
              cat
              [ (take 1 . (getChildren .> isTag "td")) .> deep (isXText .> neg isWhiteSpace)
              , txt ":\n\t"
              , (drop 1 . (getChildren .> isTag "td")) .> deep (isXText .> neg isWhiteSpace)
              ]
            )
      formatCitations   :: XmlTrees -> XmlTrees
      formatCitations cl
          = xtext
            .
            ( "module Zitate\nwhere\n\nzitate :: [(Int, String)]\nzitate =\n    [" ++ )
            .
            ( ++ "\n    ]\n" )
            .
            ( \ l -> if null l then "" else foldr1 (\ s1 s2 -> s1 ++ "\n    ," ++ s2) l)
            .
            zipWith (\ i c -> "( " ++ show i ++ ", " ++ (show . xshow . this) c ++ " )") [1::Int ..]
            .
            nub
            .
            sort
            $ cl

      extractText       :: XmlTree -> String
      extractText
          = xshow . deep isXText

      writeXmlDoc       :: XmlStateFilter state
      writeXmlDoc t'
          = ( if null fn || fn == "-"
              then performAction
                   (\ page -> io $ putStrLn (extractText page))
              else ( writePage fn
                     .>>
                     traceMsg 1 ("document written to file: " ++ fn)
                   )
            )
            $ t'
          where
          fn = xshow . getValue a_output_file $ t'

      writePage :: String -> XmlStateFilter a
      writePage fn
          = performAction
            ( \ page ->
              io $ do
                   h <- openBinaryFile fn WriteMode
                   hPutStr h (extractText page)
                   hClose h
            )

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName        :: String
progName        = "Citations"

options         :: [OptDescr (String, String)]
options
    = [ Option "v"      ["verbose"]             (NoArg  (att "verbose" "1"))            "display a message if document is well-formed or valid"
      , Option "h?"     ["help"]                (NoArg  (att "usage"   "1"))            "this message"
      , Option "f"      [a_output_file]         (ReqArg (att a_output_file) "FILE")     "output file for resulting document (default is stdout)"
      , Option "p"      ["proxy"]               (ReqArg prx "PROXY")                    "proxy for http access (e.g. \"www-cache:3128\")"
      , Option ""       [a_use_curl]            (NoArg  (att a_use_curl "1"))           "HTTP access via external program \"curl\", more stable, more general, less efficient"
      , Option ""       [a_options_curl]        (ReqArg (att a_options_curl) "STR")     "additional curl options, e.g. for timeout, ..."
      , Option "x"      ["show-text"]           (NoArg  (att "show-text"      "1"))     "output only the raw text, remove all markup"
      , Option "T"      ["show-tree"]           (NoArg  (att "show-tree"      "1"))     "output tree representation instead of document source"
      , Option "S"      ["show-haskell"]        (NoArg  (att "show-haskell"   "1"))     "output internal Haskell representation instead of document source"
      , Option "t"      [a_trace]               (OptArg trc "LEVEL")                    "trace level (0-4), default 1"
      ]
    where
    att n v     = (n, v)
    prx = att "proxy"
    trc = att a_trace . show . max 0 . min 9 . (read :: String -> Int) . ('0':) . filter (`elem` "0123456789") . fromMaybe "1"

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
    header = progName ++ " - Get all Citations from www.blutgraetsche.de" ++
             "Usage: " ++ progName ++ " [OPTION...]"
    use    = usageInfo header options

cmdlineOpts     :: [String] -> IO (Attributes)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,_,[]  )
          -> do
             help (lookup "usage" ol)
             return ol
      (_,_,errs)
          -> usage errs
    where
    help Nothing        = return ()
    help (Just _)       = usage []

-- ------------------------------------------------------------
