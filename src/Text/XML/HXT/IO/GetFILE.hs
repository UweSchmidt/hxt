-- ------------------------------------------------------------
--
-- GET for local file access
--
-- Version : $Id: GetFILE.hs,v 1.6 2006/09/04 06:03:03 hxml Exp $

module Text.XML.HXT.IO.GetFILE
    ( module Text.XML.HXT.IO.GetFILE
    )

where

import System.IO
    ( IOMode(..)
    , openFile
    -- , getContents  is defined in the prelude
    , hGetContents
    )

import System.IO.Error
    ( ioeGetErrorString
    , try
    )

import System.Directory
    ( doesFileExist
    , getPermissions
    , readable
    )

-- ------------------------------------------------------------

getStdinCont		:: IO (Either String String)
getStdinCont
    = do
      c <- try ( do
		 getContents
	       )
      return (either readErr Right c)
    where
    readErr e
	= Left ( "system error when reading from stdin: "
		 ++ ioeGetErrorString e
	       )

getCont		:: String -> IO (Either String String)
getCont source
    = do			-- preliminary
      exists <- doesFileExist source'
      if not exists
	 then return (Left ("file " ++ show source' ++ " not found"))
	 else do
	      perm <- getPermissions source'
	      if not (readable perm)
	         then return (Left ("file " ++ show source' ++ " not readable"))
	         else do
		      c <- try ( do
				 h <- openFile source' ReadMode
				 hGetContents h
			       )
		      return (either readErr Right c)
    where
    -- please NO call of unEscapeString for file names, NOT: source' = drivePath . unEscapeString $ source
    source' = drivePath $ source
    readErr e
	= Left ( "system error when reading file "
		 ++ show source
		 ++ ": "
		 ++ ioeGetErrorString e
	       )

    -- remove leading / if file starts with windows drive letter, e.g. /c:/windows -> c:/windows
    drivePath ('/' : file@(d : ':' : _more))
	| d `elem` ['A'..'Z'] || d `elem` ['a'..'z']
	    = file
    drivePath file
	= file


-- ------------------------------------------------------------
