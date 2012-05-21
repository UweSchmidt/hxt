-- ------------------------------------------------------------

{- |
   Module     : System.PipeOpen
   Copyright  : Copyright (C) 2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: none portable

   pipe open implemented with System.Process functions

-}

-- ------------------------------------------------------------

module System.PipeOpen
    ( popen )
where

import System.IO
    ( hGetContents
    , hClose
    )

import System.Exit
    ( ExitCode(..)
    )

import System.Process
    ( runInteractiveProcess
    , waitForProcess
    )

-- | call an external program with a list of command line arguments
-- and return content of stdout, content of stderr and return code

popen   :: String -> [String] -> IO (String, String, Int)
popen prg argl
    = do
      (inpH, outH, errH, pH) <- runInteractiveProcess prg argl Nothing Nothing
      hClose inpH
      res  <- hGetContents outH
      errs <- hGetContents errH

      -- hack: stdout and stderr must be read completely, otherwise waitForProcess blocks
      if (length res  == 0) then return () else return ()
      if (length errs == 0) then return () else return ()

      rc <- waitForProcess pH
      return ( res
             , errs
             , case rc of
               ExitSuccess -> 0
               ExitFailure i -> i
             )

-- eof ------------------------------------------------------------
