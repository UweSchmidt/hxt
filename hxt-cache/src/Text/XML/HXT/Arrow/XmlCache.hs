{-# OPTIONS -fno-warn-unused-imports #-}

-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlCache
   Copyright  : Copyright (C) 2009 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Caching of XML document trees and other binary data
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlCache
    ( readDocument
    , a_cache
    , a_compress
    , a_document_age
    , lookupCache
    , writeCache
    , sha1HashValue
    , sha1HashString
    , CacheConfig(..)
    )
where

import 		 Control.Exception	( SomeException	, try )

import 		 Data.Binary
import qualified Data.ByteString.Lazy	as B
import           Data.Char
import           Data.Either
import           Data.Maybe
import		 Data.Digest.Pure.SHA

import           System.FilePath
import		 System.Directory
import           System.IO
import           System.Time

import           Text.XML.HXT.Arrow	hiding	( readDocument )
import qualified Text.XML.HXT.Arrow 	as	X
import           Text.XML.HXT.Arrow.Binary

import           Text.XML.HXT.DOM.Binary

-- ------------------------------------------------------------

a_cache			:: String
a_cache			= "document-cache"

a_compress		:: String
a_compress		= "compress"

a_document_age		:: String
a_document_age		= "document-age"

-- ------------------------------------------------------------

-- | This readDocument is a wrapper for the 'Text.XML.HXT.Arrow.ReadDocument.readDocument' function.
-- The function is controlled by the options 'a_cache', 'a_compress' and 'a_document_age'.
--
-- * 'a_cache': the document tree of the document read is cached in the directory given by this option,
--              or, if it is read before and it is not too old, see 'a_document_age', it is read from the
--              cache. The document is stored in binary format (used package: binary).
--
-- - 'a_compress' : controls whether the cache contents is compressed with the bzip2 lib for saving space
--
-- - 'a_document_age': determines the maximum age of the document in seconds. If this time is exceeded, the cache entry
--                     is ignored, the original is re-read and cached again. Default for the document age is 1 day.

readDocument		:: Attributes -> String -> IOStateArrow s b XmlTree
readDocument userOptions src
			= maybe rd (\ l -> withTraceLevel (read l) rd) $
                          lookup a_trace userOptions
    where
    rd                  = readDocument' userOptions src
    
readDocument'		:: Attributes -> String -> IOStateArrow s b XmlTree
readDocument' userOptions src
    | withCache		= ( traceMsg 1 ("looking up document " ++ show src ++ " in cache")
                            >>>
                            lookupCache cacheConfig src
                            >>>
                            traceMsg 1 "cache hit"
                          )
                          `orElse`
                          ( traceMsg 1 "cache miss, reading original document"
                            >>>
                            X.readDocument userOptions src
                            >>>
                            perform ( ( writeCache cacheConfig src
                                        >>>
                                        none
                                      )
                                      `when`
                                      documentStatusOk
                                    )
                          )
    | otherwise		= X.readDocument userOptions src

      where

      options			= addEntries userOptions defaultOptions
      defaultOptions		= [ ( a_compress,	v_0	   )
				  , ( a_cache,		"./.cache" )
				  , ( a_document_age,   ""         )
				  ]
      compr			= optionIsSet a_compress options
      withCache			= isJust . lookup a_cache $ userOptions
      cacheDir			= lookup1 a_cache options
      cacheAge			= readInteger . lookup1 a_document_age $ options
      readInteger s
	  | null s || not (all isDigit s)
				= 60 * 60 * 24				-- default age: 1 day
	  | otherwise		= read s
      cacheConfig		= CC { c_dir      = cacheDir
				     , c_compress = compr
				     , c_age      = cacheAge
				     }

-- ------------------------------------------------------------

data CacheConfig	= CC { c_dir		:: FilePath
                             , c_compress	:: Bool
                             , c_age		:: Integer
                             }

lookupCache		:: (Binary b) => CacheConfig -> String -> IOStateArrow s a b
lookupCache cc f	= isIOA (const $ cacheHit cc cf)
			  `guards`
			  readBinaryValue (c_compress cc) cf
    where
    cf			= uncurry (</>) $ cacheFile cc f

writeCache		:: (Binary b) => CacheConfig -> String -> IOStateArrow s b ()
writeCache cc f		= perform (arrIO0 createDir)
                          >>>
                          writeBinaryValue (c_compress cc) hf
			  >>>
			  perform (arrIO0 $ writeIndex cc f hf)
    where
    hf                  = dir </> file
    (dir, file)		= cacheFile cc f
    createDir		= createDirectoryIfMissing True dir

cacheFile		:: CacheConfig -> String -> (FilePath, FilePath)
cacheFile cc f		= (c_dir cc </> fd, fn)
    where
    (fd, fn)		= splitAt 2 . sha1HashString $ f
    
cacheHit		:: CacheConfig -> FilePath -> IO Bool
cacheHit cc hf		= ( try' $
			    do
			    e <- doesFileExist hf
			    if not e
			      then return False
			      else do
			           mt <- getModificationTime hf
				   ct <- getClockTime
				   return $ (dt `addToClockTime` mt) >= ct
			  ) >>= return . either (const False) id
    where	  
    age			= c_age cc
    seconds		= fromInteger $ age `mod` 60
    minutes		= fromInteger $ age `div` 60
    dt			= normalizeTimeDiff $ TimeDiff 0 0 0 0 minutes seconds 0

try'			:: IO a -> IO (Either SomeException a)
try'			= try

writeIndex		:: CacheConfig -> String -> FilePath -> IO ()
writeIndex cc f hf	= ( try' $
			    do
			    h <- openBinaryFile (c_dir cc </> "index") AppendMode
			    hPutStrLn h $ show (hf, f)
			    hClose h
			    return ()
			  ) >> return ()

-- ------------------------------------------------------------

-- | Compute the SHA1 hash is hexadecimal format for an arbitray serializable value

sha1HashValue		:: (Arrow a, Binary b) => a b Integer
sha1HashValue		= arr $ integerDigest . sha1 . encode

sha1HashString		:: (Arrow a, Binary b) => a b String
sha1HashString		= arr $ showDigest . sha1 . encode

-- ------------------------------------------------------------
