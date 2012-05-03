{- |
   Module     : Main
   Copyright  : Copyright (C) 2012 Thorben Guelck, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   A test prog for the XML schema validation
-}

module Main

where

import Text.XML.HXT.Core                 -- ( withTrace )
import Text.XML.HXT.Curl                 ( withCurl )
import Text.XML.HXT.XMLSchema.Validation ( validateWithXmlSchema )
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
      2 -> runX ( validateDoc [ withCurl []
                              , withTrace 2
                              ] (argv !! 0) (argv !! 1) ) >> return ()
      _ -> printUsage

validateDoc :: SysConfigList -> String -> String -> IOSArrow a XmlTree
validateDoc config schemaUri docUri
    = readDocument ( config ++
                     [ withValidate yes        -- validate source
                     , withRemoveWS yes        -- remove redundant whitespace
                     , withPreserveComment no  -- keep comments
                     , withCheckNamespaces yes -- check namespaces
                     ]
                   ) docUri
      >>>
      validateWithXmlSchema config schemaUri

-- ----------------------------------------
{-
test1 :: IO [XmlTree]
test1
    = runX $ validateDoc [] "../tests/simpleTypesElems.xsd" "../tests/simpleTypesElemsOk.xml"

test2 :: IO [XmlTree]
test2
    = runX $ validateDoc [] "../tests/simpleTypesElems.xsd" "../tests/simpleTypesElemsErrors.xml"
-}