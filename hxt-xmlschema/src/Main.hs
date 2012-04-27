{- |
   Module     : Main
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   The main module for the validateWithSchema executable.
-}

module Main

where

import Text.XML.HXT.Core                 ( withTrace )
import Text.XML.HXT.Curl                 ( withCurl )

import Text.XML.HXT.XMLSchema.Validation ( validateWithSchema
                                         , printSValResult
                                         )

import Text.XML.HXT.XMLSchema.TestSuite  ( runTestSuite )

import System.Environment                ( getArgs )

-- ----------------------------------------

-- | Prints usage text
printUsage :: IO ()
printUsage
  = do
    putStrLn $ "\nUsage:\n\n"
            ++ "validateWithSchema -runTestSuite\n"
            ++ "> Run the hxt-xmlschema test suite (for development purposes).\n\n"
            ++ "validateWithSchema <schemaFileURI> <instanceFileURI>\n"
            ++ "> Test an instance file against a schema file.\n"
    return ()

-- ----------------------------------------

-- | Starts the hxt-xmlschema validator
main :: IO ()
main
  = do
    argv <- getArgs
    case length argv of
      1 -> if argv !! 0 == "-runTestSuite"
             then runTestSuite
             else printUsage
      2 -> validateWithSchema [ withCurl []
                              , withTrace 2
                              ] (argv !! 0) (argv !! 1) >>= printSValResult
      _ -> printUsage

-- ----------------------------------------
