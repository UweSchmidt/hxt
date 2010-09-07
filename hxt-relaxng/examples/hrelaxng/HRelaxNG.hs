-- |
-- HRelaxNG - Relax NG Validator of the Haskell XML Toolbox.
-- RELAX NG is a simpler schema language for XML.
--
-- Author : Torben Kuseler
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

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG

-- ------------------------------------------------------------


main :: IO ()
main
  = do
    argv <- getArgs                       -- get the commandline arguments
    (al, xml, schema) <- cmdlineOpts argv -- and evaluate them, return a key-value list
    [rc]  <- runX (relax al xml schema)   -- run the Relax NG validator 
    exitProg (rc >= c_err)                -- set return code and terminate

relax :: SysConfigList -> String -> String -> IOSArrow b Int
relax al xml schema
    = configSysParams (al ++ [withRelaxNG schema])
      >>>
      readDocument [] xml
      >>>
      writeDocument [] "-"
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

options     :: [OptDescr SysConfig]
options
    = generalOptions
      ++
      inputOptions
      ++
      relaxOptions
      ++
      outputOptions

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

cmdlineOpts :: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[])
        -> do
           (xml, schema) <- src n
           help (getConfigAttr a_help ol)
           return (ol, xml, schema)
      (_,_,errs)
        -> usage errs
    where
    src [xml, schema] = return (xml, schema)
    src []  = usage ["XML file and Relax NG schema input file/url missing"]
    src [_] = usage ["Relax NG schema input file/url missing"]
    src _   = usage ["too many arguments"]

    help "1" = usage []
    help _   = return ()


-- ------------------------------------------------------------
