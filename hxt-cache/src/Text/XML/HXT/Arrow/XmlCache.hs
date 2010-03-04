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

import           Control.DeepSeq
import           Control.Concurrent.ResourceTable
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
import           System.Locale
import           System.Posix		( touchFile )
import           System.Time
import           System.IO.Unsafe               ( unsafePerformIO )

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
--              or, if it is read before and it is not out of date, see 'a_document_age', it is read from the
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
    | withCache		= applyA $ arrIO0 (lookupCache' cacheConfig userOptions src)
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

lookupCache'		:: CacheConfig -> Attributes -> String -> IO (IOStateArrow s a XmlTree)
lookupCache' cc os src	= do
                          ch <- cacheHit cc cf
                          return $
                                 case ch of
                                 Nothing        -> readAndCacheDocument
                                 Just Nothing   -> readDocumentFromCache
                                 Just (Just mt) -> readDocumentCond mt
    where
    cf			= uncurry (</>) $ cacheFile cc src

    readDocumentFromCache
			= traceMsg 1 ("cache hit, reading from cache file " ++ show cf)
                          >>>
                          ( ( readCache cc cf
                              >>>
                              traceMsg 1 "cache read"
                            )
                            `orElse`
                            ( traceMsg 1 "cache file was corrupted, reading original"
                              >>>
                              readAndCacheDocument
                            )
                          )
    readAndCacheDocument
			= traceMsg 1 ("cache miss, reading original document" ++ show src)
                          >>>
                          X.readDocument os src
                          >>>
                          perform ( (writeCache cc src >>> none)
                                    `when`
                                    documentStatusOk
                                  )
    readDocumentCond mt
			= traceMsg 1 ("cache out of date, read original document if modified" ++ show src)
                          >>>
                          X.readDocument (addEntries (condOpts mt) os) src
                          >>>
                          choiceA
                          [ is304            :-> ( traceMsg 1 ("document not modified, using cache data from " ++ show cf)
                                                   >>>
                                                   perform (arrIO0 $ touchFile cf)
                                                   >>>
                                                   readCache cc cf
                                                 )
                          , documentStatusOk :-> ( traceMsg 1 "document read and cache updated"
                                                   >>>
                                                   perform (writeCache cc src)
                                                 )
                          , this             :-> traceMsg 1 "document read without caching"
                          ]
        where
        is304           = hasAttrValue transferStatus (== "304")
        condOpts t	= [("curl--header", "If-Modified-Since: " ++ fmtTime t)]
                          -- [(a_if_modified_since, fmtTime t)]
        fmtTime		= formatCalendarTime defaultTimeLocale rfc822DateFormat . toUTCTime

-- ------------------------------------------------------------

lookupCache		:: (NFData b, Binary b) => CacheConfig -> String -> IOStateArrow s a b
lookupCache cc f	= isIOA (const $ hit)
			  `guards`
			  readCache cc cf
    where
    cf			= uncurry (</>) $ cacheFile cc f
    hit			= do
                          ch <- cacheHit cc cf
                          return $ case ch of
                                   Just Nothing -> True
                                   _            -> False

-- ------------------------------------------------------------

readCache		:: (NFData c, Binary c) => CacheConfig -> String -> IOStateArrow s b c
readCache cc cf		= withLock cf $ readBinaryValue (c_compress cc) cf

writeCache		:: (Binary b) => CacheConfig -> String -> IOStateArrow s b ()
writeCache cc f		= perform (arrIO0 createDir)
                          >>>
                          withLock hf (writeBinaryValue (c_compress cc) hf)
			  >>>
			  perform (withLock ixf (arrIO0 $ writeIndex ixf f hf))
    where
    hf                  = dir </> file
    ixf 		= c_dir cc </> "index"
    (dir, file)		= cacheFile cc f
    createDir		= createDirectoryIfMissing True dir

-- ------------------------------------------------------------

cacheFile		:: CacheConfig -> String -> (FilePath, FilePath)
cacheFile cc f		= (c_dir cc </> fd, fn)
    where
    (fd, fn)		= splitAt 2 . sha1HashString $ f

-- result interpretation for cacheHit
--
-- Nothing       : cache miss: get document
-- Just Nothing  : cache hit, cache data valid: use cache data
-- Just (Just t) : cache hit, but cache data out of date: get document conditionally with if-modified-since t
    
cacheHit		:: CacheConfig -> FilePath -> IO (Maybe (Maybe ClockTime))
cacheHit cc hf		= ( try' $
			    do
			    e <- doesFileExist hf
			    if not e
			      then return Nothing
			      else do
			           mt <- getModificationTime hf
				   ct <- getClockTime
				   return . Just $ if (dt `addToClockTime` mt) >= ct
                                                   then Nothing
                                                   else Just mt
			  ) >>= return . either (const Nothing) id
    where	  
    age			= c_age cc
    seconds		= fromInteger $ age `mod` 60
    minutes		= fromInteger $ age `div` 60
    dt			= normalizeTimeDiff $ TimeDiff 0 0 0 0 minutes seconds 0

try'			:: IO a -> IO (Either SomeException a)
try'			= try

writeIndex		:: String -> String -> FilePath -> IO ()
writeIndex ixf f hf	= ( try' $
			    do
			    h <- openBinaryFile ixf AppendMode
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


-- | the internal table of file locks

theLockedFiles		:: ResourceTable String
theLockedFiles		= unsafePerformIO newResourceTable
{-# NOINLINE theLockedFiles #-}

lockFile, unlockFile	:: String -> IO ()
lockFile		= requestResource theLockedFiles
unlockFile		= releaseResource theLockedFiles

withLock		:: String -> IOStateArrow s b c -> IOStateArrow s b c
withLock l a		= ( perform (arrIO0 $ lockFile l)
			    >>>
			    listA a
			    >>>
			    perform (arrIO0 $ unlockFile l)
			  )
			  >>>
			  unlistA
		  
-----------------------------------------------------------------------------
