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

import Text.XML.HXT.Arrow
import Text.XML.HXT.XPath

import System.Console.Editline.Readline
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
         else evalLoop env doc

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
      d <- runX ( readDocument [ (a_tagsoup, v_0)
                               , (a_parse_by_mimetype, v_1)
                               , (a_check_namespaces, v_1)
                               , (a_remove_whitespace, v_1)
                               , (a_validate, v_0)
                               , (a_canonicalize, v_1)
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
             writeDocument [ (a_indent, v_1)
                           , (a_no_xml_pi, v_1)
                           ] ""
           )
      >> return ()

showTree                :: XmlTree -> IO ()
showTree doc
    = runX ( constA doc
             >>>
             writeDocument [ (a_show_tree, v_1)
                           , (a_no_xml_pi, v_1)
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
    pathEx      = runParser parseXPath (toNsEnv env) "" $ path
    pathString  = either show show                      $ pathEx
    pathTree    = either show formatXPathTree           $ pathEx
    xr          = runLA ( xshow $ getXPathTreesWithNsEnv env path) doc

evalLoop        :: NsEnv' -> XmlTrees -> IO ()
evalLoop env doc
    = do
      maybeLine <- readline "xpath> "
      case maybeLine of
        Nothing -> return () -- EOF / control-d
        Just ":q"       -> return ()
        Just line -> do
                     let ws = words line
                     if null ws
                        then evalLoop env doc
                        else do
                             addHistory line
                             evalCmd (words line)
    where
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
