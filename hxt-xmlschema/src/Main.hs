{- |
   Module     : Main
   Copyright  : Copyright (C) 2012 Thorben Guelck, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   A test prog for the XML schema validation
-}

module Main

where

import Text.XML.HXT.Core                 -- ( withTrace )
import Text.XML.HXT.Curl                 ( withCurl )
import Text.XML.HXT.XMLSchema.Validation ( validateDocumentWithXmlSchema )
import Text.XML.HXT.XMLSchema.TestSuite  ( runTestSuite )

import System.Environment                ( getArgs )

-- ----------------------------------------

-- | Prints usage text
printUsage :: IO ()
printUsage
  = do
    putStrLn $ "\nUsage:\n\n"
            ++ "validateWithSchema -runTestSuite\n"
            ++ "> Run the hxt-xmlschema test suite (cwd must be the hxt-xmlschema dir).\n\n"
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
                              , withTrace 1
                              ] (argv !! 0) (argv !! 1)
                  >>>
                  writeDocument [ withIndent yes ] ""
                ) >> return ()
      _ -> printUsage

validateDoc :: SysConfigList -> String -> String -> IOSArrow a XmlTree
validateDoc config schemaUri docUri
    = readDocument ( config ++
                     [ withValidate yes        -- validate source
                     , withRemoveWS yes        -- remove redundant whitespace
                     , withPreserveComment no  -- do'nt keep comments
                     , withCheckNamespaces yes -- check namespaces
                     ]
                   ) docUri
      >>>
      validateDocumentWithXmlSchema config schemaUri

-- ----------------------------------------
