{-# OPTIONS -fglasgow-exts #-}
-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module Network.Server.HWS.Util where

import Char
import Control.Concurrent
import Control.Exception -- (tryJust, ioErrors)
import Data.IORef
-- import Debug.Trace
import Foreign.C.Error
import IO
import Locale
import Network.Socket   hiding (accept)
import qualified Network.Socket
-- import System.Posix
import System.Time

-- TODO #if __GLASGOW_HASKELL__ < 504
-- import PrelGHC
-- import PrelIOBase
-- import PrelConc
-- #else
import GHC.Base
import GHC.Conc
import GHC.IOBase
-- #endif

-- TODO #ifdef DEBUG
-- import qualified IOExts
-- #endif

-----------------------------------------------------------------------------
-- Utils

-- TODO #ifdef DEBUG
-- trace s e = IOExts.trace s e
-- #else
trace s e = e
-- #endif

crlf = "\r\n"

emptyLine "\r" = True
emptyLine _    = False

stripWS :: String -> String
stripWS = stripLeadingWS . reverse . stripLeadingWS . reverse

stripLeadingWS :: String -> String
stripLeadingWS = dropWhile isSpace

data E b a = Ok a | Bad b
instance Monad (E b) where
   m >>= k = case m of
	        Ok  a -> k a
		Bad b -> Bad b
   return a = Ok a

failE :: b -> E b a
failE b = Bad b

maybeE :: b -> Maybe a -> E b a
maybeE _ (Just a) = return a
maybeE b Nothing  = failE b

commaSep :: String -> [String]
commaSep s = go (dropWhile isSpace s)
  where go "" = []
	go s  = word : case rest of ',':rest -> go rest; _ -> go rest
	  where (word,rest) = break (==',') s

-- ToDo: deHex is supposed to remove the '%'-encoding
deHex :: String -> String
deHex s = s

hPutStrCrLf h s = hPutStr h s >> hPutChar h '\r' >> hPutChar h '\n'

-----------------------------------------------------------------------------
-- Time utils

formatTimeSensibly :: CalendarTime -> String
formatTimeSensibly time
   = formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" time

-- epochTimeToClockTime :: EpochTime -> ClockTime
-- epochTimeToClockTime epoch_time = TOD (fromIntegral epoch_time) 0

-----------------------------------------------------------------------------
-- concurrency utilities

-- timeout

-- Time-outs are implemented by having another thread wait for the
-- specified period of time before sending an exception to the
-- original thread.  We have to be extremely careful about race
-- conditions here: we don't want the timeout thread raising an
-- exception outside of our handler, so we must arrange that the
-- timeout exception can only be raised when we're ready for it.  This
-- is implemented using a semaphore to indicate that the thread is
-- ready to handle the timeout exception.
--
-- Things get hairy when we consider that the action being run may
-- generate its own exceptions.

timeout
   :: Int	-- secs
   -> IO a	-- action to run
   -> IO a	-- action to run on timeout
   -> IO a

timeout secs action on_timeout 
  = do
    threadid <- myThreadId
    timeout <- forkIOIgnoreExceptions (
			    do threadDelay (secs * 1000000)
			       throwTo threadid (ErrorCall "__timeout")
			  )
    ( do result <- action
	 killThread timeout
	 return result
      ) 
      `Control.Exception.catch`
      ( \exception -> case exception of
		       ErrorCall "__timeout" -> on_timeout		       
		       _other               -> do
						killThread timeout
						throw exception )

forkIOIgnoreExceptions :: IO () -> IO ThreadId
forkIOIgnoreExceptions action = IO $ \ s -> 
   case (fork# action s) of (# s1, id #) -> (# s1, ThreadId id #)

-----------------------------------------------------------------------------
-- networking utils

accept :: Socket 		-- Listening Socket
       -> IO (Handle,SockAddr)	-- StdIO Handle for read/write
accept sock = do
 (sock', addr) <- Network.Socket.accept sock
 handle	<- socketToHandle sock' ReadWriteMode
 return (handle,addr)


-----------------------------------------------------------------------------
-- "pattern matching" utils

data PatternItem a = Delim
                   | Str [a]
        deriving (Show)

splitter :: Eq a => a -> [a] -> [PatternItem a]
splitter a xs = case span (/=a) xs of
                  (ys, _:zs) -> Str ys : Delim : splitter a zs
                  (ys, [])   -> [Str ys]

-- Match a pattern string containing *'s against a concrete string
-- E.g. match "*/*" "text/plain" == True, but
--      match "*/*" "cgi-script" == False.
match :: String -> String -> Bool
match ps xs = match' (splitter '*' ps) xs
  where match' [] "" = True
        match' [] _  = False
        match' (Delim:ps) xs = or [ match' ps zs | (ys,zs) <- part xs ]
        match' (Str s:ps) xs = or [ match' ps zs | (ys,zs) <- part xs, s == ys ]

        part []     = [([],[])]
        part (x:xs) = ([],x:xs) : [ (x:a, b) | (a, b) <- part xs ]


-----------------------------------------------------------------------------
-- File utils

-- statMaybe filename = do
--    maybe_stat <- tryJust ioErrors (getFileStatus filename)
--    case maybe_stat of
--        Left e -> do
--            errno <- getErrorCode
--            if errno == eNOENT
--                then return Nothing
--                else ioError e
--        Right stat ->
--            return (Just stat)

