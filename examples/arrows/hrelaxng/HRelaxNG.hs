-- |
-- HRelaxNG - Relax NG Validator of the Haskell XML Toolbox.
-- RELAX NG is a simpler schema language for XML.
--
-- Author : Torben Kuseler
--
-- Version : $Id: HRelaxNG.hs,v 1.2 2005/09/30 14:41:44 hxml Exp $
--
-- This program may be used as example main program for the
-- Relax NG Validator.
--

module Main
where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Text.XML.HXT.Arrow 
import Text.XML.HXT.RelaxNG

-- ------------------------------------------------------------


main :: IO ()
main
  = do
    argv <- getArgs                       -- get the commandline arguments
    (al, xml, schema) <- cmdlineOpts argv -- and evaluate them, return a key-value list
    [rc]  <- runX (relax al xml schema)   -- run the Relax NG validator 
    exitProg (rc >= c_err)                -- set return code and terminate

{-
relax :: Attributes -> String -> String -> IOSArrow b Int
relax al xml schema
    = readDocument al schema
      >>>
      createSimpleForm
      >>>
      ( getErrors		-- compute errors in schema
        `orElse`
        validateXMLDoc [] xml	-- compute errors in document
        `orElse`
        root [] [txt "valid"]	-- or document is valid
      )
      >>>
      writeDocument [(a_show_tree, "1")] "-"
      >>>
      getErrStatus
-}

relax :: Attributes -> String -> String -> IOSArrow b Int
relax al xml schema
    = readDocument ( [ (a_check_namespaces, v_1)
		     , (a_validate, v_0)
		     ] ++ al
		   ) xml
      >>>
      traceState
      >>>
      validateDocumentWithRelaxSchema al schema
      >>>
      traceState
      >>>
      writeDocument al "-"
      >>>
      getErrStatus


exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure (-1))
exitProg False  = exitWith ExitSuccess



-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName    :: String
progName    = "HRelaxNGValidator"

options     :: [OptDescr (String, String)]
options
    = generalOptions
      ++
      inputOptions
      ++
      outputOptions
      ++
      relaxOptions

usage :: [String] -> IO a
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
    header = "HRelaxNGValidator - Relax NG schema validator of the " ++
             "Haskell XML Toolbox with Arrow Interface\n\n" ++
             "Usage: " ++ progName ++ " [OPTION...] (XML file URI/FILE) (Relax NG Schema URI/FILE)"
    use    = usageInfo header options

cmdlineOpts :: [String] -> IO (Attributes, String, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[])
        -> do
           (xml, schema) <- src n
           help (lookup a_help ol)
           return (ol, xml, schema)
      (_,_,errs)
        -> usage errs
    where
    src [xml, schema] = return (xml, schema)
    src []  = usage ["XML file and Relax NG schema input file/url missing"]
    src [_] = usage ["Relax NG schema input file/url missing"]
    src _   = usage ["too many arguments"]

    help (Just _) = usage []
    help Nothing = return ()


-- ------------------------------------------------------------
-- test environment
--
{-
xmlFile :: String
xmlFile = "testCases/213/1.v.xml"

schema :: [String]
schema = ["testCases/213/c.rng"]

testValidator :: Attributes -> String -> String -> IOSArrow b Int
testValidator al src xmlFile
    = readDocument al src
      >>> propagateNamespaces
      >>> simplificationStep1
      >>> simplificationStep2 [] []
      >>> simplificationStep3
      >>> simplificationStep4
      >>> simplificationStep5
      >>> simplificationStep6
      >>> simplificationStep7
      >>> simplificationStep8
      >>> cleanUp
      >>> resetStates
--       >>> perform (getErrors >>> writeDocument [(a_show_tree, "1")] "-")
--       >>> perform (fromLA xmlTreeToPatternString >>> arrIO (\s -> hPutStrLn stdout (s ++ "\n\n")))
--       >>> perform (fromLA xmlTreeToPatternFormatedString >>> arrIO (\s -> hPutStrLn stdout (s ++ "\n\n")))
--       >>> perform (fromLA xmlTreeToPatternStringTree >>> arrIO (hPutStrLn stdout))
      >>>
      ( getErrors
        `orElse`
        (validateXMLDoc [] xmlFile)
        `orElse`
        (root [] [(constA $ mkXTextTree "valid")])
      )
--       >>> writeDocument [(a_show_tree, "1")] "-"
--       >>> writeDocument [(a_show_haskell, "1")] "-"      
      >>> getErrStatus
-}
