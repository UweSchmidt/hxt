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

module Network.Server.HWS.MimeTypes (
       MimeType(..),
       initMimeTypes,	-- :: IO ()
       mimeTypeOf,	-- :: String -> MimeType
       ) where

import Data.Map as Map
import Data.IORef
import IO
import System.IO.Unsafe
import Text.Regex

data MimeType = MimeType String String
instance Show MimeType where
   showsPrec _ (MimeType part1 part2) = showString (part1 ++ '/':part2)

mime_types_ref :: IORef (Map String MimeType)
mime_types_ref = unsafePerformIO (newIORef (error "no mime types"))

mimeTypeOf :: String -> Maybe MimeType
mimeTypeOf filename = unsafePerformIO (do
  mime_types <- readIORef mime_types_ref
  let ext = extension filename
  if Prelude.null ext 
     then return Nothing 
     else return (Map.lookup ext mime_types)
  )

extension :: String -> String
extension fn = go (reverse fn) ""
  where go []      _   = ""
	go ('.':_) ext = ext
	go (x:s)   ext = go s (x:ext)

initMimeTypes :: String -> IO ()
initMimeTypes mime_types_file = do
   h <- openFile mime_types_file ReadMode
   stuff <- hGetContents h
   let mime_types = fromList (parseMimeTypes stuff)
   writeIORef mime_types_ref mime_types

parseMimeTypes :: String -> [(String, MimeType)]
parseMimeTypes file =
  [ (ext,val) 
  | Just (val,exts) <- Prelude.map (parseMimeLine . takeWhile (/= '#')) (lines file)
  , ext <- exts
  ]

mimeRegex :: Regex
mimeRegex = mkRegex "^([^/]+)/([^ \t]+)[ \t]+(.*)$"

parseMimeLine :: String -> Maybe (MimeType, [String])
parseMimeLine l = 
  case matchRegex mimeRegex l of
      Just (part1:part2:extns:_) -> Just (MimeType part1 part2, words extns)
      _ -> Nothing
