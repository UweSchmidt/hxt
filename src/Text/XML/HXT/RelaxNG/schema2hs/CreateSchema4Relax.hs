-- |
-- Creates the simple form of the Relax NG specification grammar
-- and prints it to stdout.
--

module Main
where

import Text.XML.HXT.RelaxNG
import Text.XML.HXT.Arrow 

import System.Exit
import System.Environment

-- ------------------------------------------------------------

main :: IO String
main
  = do
    argv <- getArgs
    [rc]  <- runX $ writeSpecification $ if length argv > 0 
                                         then head argv
                                         else relaxSchemaFile
    exitProg (rc >= c_err)



writeSpecification :: String -> IOSArrow b Int
writeSpecification schemaFile
  = readDocument [] schemaFile
    >>>
    createSimpleForm
    >>>
    writeDocument [(a_show_haskell, "1")] "-"
    >>>
    getErrStatus


exitProg        :: Bool -> IO a
exitProg True   = exitWith (ExitFailure (-1))
exitProg False  = exitWith ExitSuccess
