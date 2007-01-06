-- |
-- Simple example program to run the Relax NG testsuite.
-- The program argument must be the absolute!!! path to the directory
-- containing the testcases.
--
-- $Id: RunTestCases.hs,v 1.1 2005/09/02 17:09:38 hxml Exp $

module Main
where

import TestCases

import System.Environment

-- ------------------------------------------------------------

main :: IO ()
main = do
       argv <- getArgs
       res  <- runTest $ if length argv > 0 
                         then head argv
                         else "./testCases"
       putStrLn $ show res

-- ------------------------------------------------------------
