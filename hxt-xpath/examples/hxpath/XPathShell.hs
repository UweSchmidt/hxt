-- ------------------------------------------------------------

{- |
   Module     : XPathShell
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   XPath example program for testing xpath evaluation
   with both evaluation stategies, the full XPath functionality
   and the limited but faster one for simple XPath queries

-}

-- ------------------------------------------------------------


module Main
where

import qualified Control.Monad as M

import Data.Maybe

import Text.XML.HXT.Core
import Text.XML.HXT.XPath
import Text.XML.HXT.Curl

import Text.XML.HXT.Parser.XmlCharParser( withNormNewline )

import System.Console.Haskeline
import System.Console.Haskeline.IO
import System.Environment

import Text.ParserCombinators.Parsec    ( runParser )

type NsEnv'     = AssocList String String

main    :: IO()
main
    = do
      args <- getArgs
      (path, env, doc) <- evalArgs args
      if not (null path) && not (null doc)
         then evalXPath path env (head doc)
         else startEvalLoop env doc

evalArgs                        :: [String] -> IO (String, NsEnv', XmlTrees)
evalArgs []                     = evalArgs (""   : "[]" : ""  : [])
evalArgs [doc]                  = evalArgs (""   : "[]" : doc : [])
evalArgs [path, doc]            = evalArgs (path : "[]" : doc : [])
evalArgs [path, env, ""]        = return (path, buildEnv env, [])
evalArgs [path, env, doc]       = do
                                  (d, ne) <- loadDoc doc
                                  return (path, addEntries ne . buildEnv $ env, d)
evalArgs al                     = evalArgs (take 3 al)

buildEnv                        :: String -> NsEnv'
buildEnv env                    = (addEntries . read $ env) $ defaultEnv

loadDoc         :: String -> IO ([XmlTree], NsEnv')
loadDoc doc
    = do
      d <- runX ( readDocument [ withParseByMimeType yes
                               , withCheckNamespaces yes
                               , withRemoveWS yes
                               , withValidate no
                               , withCanonicalize yes
                               , withCurl []
                               ] doc
                  >>>
                  (documentStatusOk `guards` this)
                )
      let env = runLA (unlistA >>> collectNamespaceDecl) d
      return (d, env)

showDoc         :: XmlTree -> IO ()
showDoc doc
    = runX ( constA doc
             >>>
             writeDocument [ withIndent yes
                           , withXmlPi no
                           ] ""
           )
      >> return ()

showTree                :: XmlTree -> IO ()
showTree doc
    = runX ( constA doc
             >>>
             writeDocument [ withShowTree yes
                           , withXmlPi no
                           ] ""
           )
      >> return ()

evalXPath       :: String -> NsEnv' -> XmlTree -> IO()
evalXPath path env doc
    = putStrLn . unlines $
      [ "start xpath evaluation: " ++ pathS
      , "          parsed xpath: " ++ pathString
      , "  parsed xpath as tree:"
      , pathTree
      , "xpath result:"
      ] ++ xr ++
      [ "end   xpath evaluation: " ++ pathS
      ]
    where
    pathS       = show                                  $ path
    pathEx      = runParser parseXPath (withNormNewline (toNsEnv env)) "" $ path
    pathString  = either show show                      $ pathEx
    pathTree    = either show formatXPathTree           $ pathEx
    xr          = runLA ( xshow $ getXPathTreesWithNsEnv env path) doc

startEvalLoop        :: NsEnv' -> XmlTrees -> IO ()
startEvalLoop env doc
    = do is0 <- initializeInput defaultSettings
         evalLoop0 (readCmdLine is0 "xpath> ") env doc
         closeInput is0
         return ()

readCmdLine     :: InputState -> String -> IO String
readCmdLine is0 prompt
  = do
    line <- queryInput is0 (getInputLine prompt)
    let line' = stringTrim . fromMaybe "" $ line
    if null line'
      then readCmdLine is0 prompt
      else return line'

evalLoop0        :: IO String -> NsEnv' -> XmlTrees -> IO ()
evalLoop0 readCmdLine' env doc
    = do
      line <- readCmdLine'
      case line of
        "" -> return () -- EOF / control-d
        ":q" -> return ()
        _ -> do
                     let ws = words line
                     if null ws
                        then evalLoop env doc
                        else do
                             evalCmd (words line)
    where
    evalLoop = evalLoop0 readCmdLine'

    evalCmd []          = evalLoop env doc
    evalCmd [":ns",uri] = evalCmd [":ns", "", uri]
    evalCmd [":ns", ns, uri]
                        = evalLoop (addEntry ns uri env) doc
    evalCmd (":?":_)    = do
                          putStrLn . unlines $
                                       [ "XPath Tester"
                                       , "Commands:"
                                       , ":l <document>\tload a document"
                                       , ":ns <uri>\tset default namespace"
                                       , ":ns <px> <uri>\tset namespace"
                                       , ":q\t\tquit"
                                       , ":s\t\tshow current document"
                                       , ":t\t\tshow current document in tree format"
                                       , ":x\t\tshow current namespace environment"
                                       , ":?\t\tthis message"
                                       , "<xpath-expr>\tevaluate XPath expression"
                                       ]
                          evalLoop env doc
    evalCmd [":x"]      = do
                          putStrLn . unlines . map show $ env
                          evalLoop env doc
    evalCmd [":s"]      = do
                          M.when (not . null $ doc) (showDoc . head $ doc)
                          evalLoop env doc
    evalCmd [":t"]      = do
                          M.when (not . null $ doc) (showTree . head $ doc)
                          evalLoop env doc
    evalCmd [":l",n]    = do
                          (nd, nv) <- loadDoc n
                          if null nd
                             then do
                                  putStrLn ("error when loading " ++ show n)
                                  evalLoop env doc
                             else evalLoop (addEntries nv env) nd
    evalCmd ws@((':':_):_)
                        = do
                          putStrLn ("unknown command (:? for help): " ++ (show . unwords $ ws))
                          evalLoop env doc
    evalCmd ws          = do
                          let path = unwords ws
                          if null doc
                             then putStrLn "no document loaded"
                             else evalXPath path env (head doc)
                          evalLoop env doc

defaultEnv              :: NsEnv'
defaultEnv              = [ ("xml",xmlNamespace)
                          , ("xmlns",xmlnsNamespace)
                          ]

-- ----------------------------------------
