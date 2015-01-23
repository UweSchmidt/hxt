-- |
-- This is a simple example for parsing and validating an XML file and printing the generated
-- XmlTree to standard out.
--
-- Author : .\\artin Schmidt
-- Version : $Id: ParseExample.hs,v 1.4 2004/09/02 19:11:53 hxml Exp $

module Main(main)
where

import Text.XML.HXT.Parser

import System.IO
import System

--
-- call this program with a single parameter for the file or uri to be processed
-- e.g. ParseExample example1.xml

main :: IO ()
main
    = do
      argv <- getArgs
                              -- take the first cmdline argument as source name
                              -- start the XState monad with the runParser command
      res  <- run' $ parser [(a_source, head argv)] emptyRoot
      exitProg res

-- run a command in the XState monad.
-- this monad includes the IO monad, a system state for global options
-- and a user definable state (type parameter state). This user state is
-- set to () (the empty state) by the run' command.
-- There are more general run commands: run, run0 in case of a proccessing
-- function, that requires e.g. an environment

parser  :: Attributes -> XmlStateFilter ()
parser al
    = parseDocument al
      .>>
      writeDocument [(a_indent, v_1)]   -- indent document and output to stdout
      .>>
      checkStatus                       -- check status

-- ------------------------------------------------------------

exitProg        :: [a] -> IO ()
exitProg []     = exitWith (ExitFailure 1)
exitProg _      = exitWith ExitSuccess

-- ------------------------------------------------------------
