{-# LANGUAGE CPP #-}
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
    ( withCache
    , withoutCache
    , isInCache
    , lookupCache
    , readCache
    , writeCache
    , sha1HashValue
    , sha1HashString
    )
where

import           Control.Concurrent.ResourceTable
import           Control.DeepSeq
import           Control.Exception                    (SomeException, try)

import           Data.Binary
import qualified Data.ByteString.Lazy                 as B
import           Data.Char
import           Data.Digest.Pure.SHA
import           Data.Either
import           Data.Maybe
import           Data.Time                            (UTCTime, addUTCTime,
                                                       formatTime,
                                                       getCurrentTime)

import           System.Directory                     (createDirectoryIfMissing,
                                                       doesFileExist,
                                                       getModificationTime,
                                                       removeFile)
import           System.FilePath                      ((</>))
import           System.IO                            (IOMode (AppendMode),
                                                       hClose, hPutStrLn,
                                                       openBinaryFile)
#if MIN_VERSION_time(1,5,0)
import           Data.Time                            (defaultTimeLocale,
                                                       rfc822DateFormat)
#else
import           System.Locale                        (defaultTimeLocale,
                                                       rfc822DateFormat)
#endif
import           System.IO.Unsafe                     (unsafePerformIO)
import           System.Posix                         (touchFile)

import           Text.XML.HXT.Arrow.Binary
import           Text.XML.HXT.Arrow.XmlState.TypeDefs
import           Text.XML.HXT.Core

-- ------------------------------------------------------------

-- | withCache enables reading documents with caching.
--
-- When the cache is configured and enabled, every document read and parsed is serialized and stored in binary
-- form in the cache. When reading the same document again, it is just deserialized, no parsing is performed.
--
-- The cache is configured by a path pointing to a directory for storing the documents,
-- by a maximal time span in second for valid documents. After that time span, the documents are read again
-- and the cache is updated.
-- The flag contols, whether documents returning 404 or other errors will be cached.
-- If set, the cache is even activated for 404 (not found) responses, default is false.
--
-- The serialized documents can be compressed, e.g. with bzip, to save disk space and IO time.
-- The compression can be configured by 'Text.XML.HXT.Arrow.XmlState.withCompression'
--
-- example:
--
-- > import Text.XML.HXT.Core
-- > import Text.XML.HXT.Cache
-- > import Codec.Compression.BZip (compress, decompress)
-- > ...
-- > readDocument [ withCache "/tmp/cache" 3600 False
-- >              , withCompression (compress, decompress)
-- >              , ....
-- >              ] "http://www.haskell.org/"
-- >
--
-- In the example the document is read and stored in binary serialized form under \/tmp\/cache.
-- The cached document remains valid for the next hour.
-- It is compressed, before written to disk.

withCache               :: String -> Int -> Bool -> SysConfig
withCache cachePath documentAge cache404
                        = setS (theWithCache   .&&&.
                                theCacheDir    .&&&.
                                theDocumentAge .&&&.
                                theCache404Err .&&&.
                                theCacheRead
                               ) (True, (cachePath, (documentAge, (cache404, readDocCache))))

-- | Disable use of cache
withoutCache            :: SysConfig
withoutCache            = setS theWithCache False

-- ------------------------------------------------------------

readDocCache              :: String -> IOStateArrow s b XmlTree
readDocCache src          = localSysVar theWithCache
                            $
                            configSysVar withoutCache
                            >>>
                            ( flip readDocCache' src
                              $< getSysVar (theCacheDir    .&&&.
                                            theDocumentAge .&&&.
                                            theCache404Err
                                           )
                            )
    where
    readDocCache' config src'
                          = applyA $ arrIO0 (lookupCache' config src')

-- ------------------------------------------------------------

-- | Predicate arrow for checking if a document is in the cache.
-- The arrow fails if document not there or is not longer valid, else the file name is returned.

isInCache               :: IOStateArrow s String String
isInCache               = uncurry isInC $< getSysVar (theDocumentAge .&&&. theCacheDir)
    where
    isInC age cdir      = ( traceValue 2 (\ x -> "isInCache: file=" ++ show x ++ " age=" ++ show age ++ " cache dir=" ++ show cdir)
                            >>>
                            arrIO (isInCache' age cdir)
                            >>>
                            arrL ( \ x ->
                                   case x of
                                   Just Nothing -> [x]
                                   _            -> []
                                 )
                          ) `guards` this

    isInCache' age cdir f
                        = cacheHit age cf
        where
        cf              = uncurry (</>) $ cacheFile cdir f

-- ------------------------------------------------------------

lookupCache'            :: (FilePath, (Int, Bool)) -> String -> IO (IOStateArrow s a XmlTree)
lookupCache' (dir, (age, e404)) src
                        = do
                          ch <- cacheHit age cf
                          return $
                                 case ch of
                                 Nothing        -> readAndCacheDocument
                                 Just Nothing   -> readDocumentFromCache
                                 Just (Just mt) -> readDocumentCond mt
    where
    cf                  = uncurry (</>) $ cacheFile dir src

    is200
        | e404          = hasAttrValue transferStatus (`elem` ["200", "404"])
        | otherwise     = hasAttrValue transferStatus (== "200")

    is304               = hasAttrValue transferStatus (== "304")

    readDocumentFromCache
                        = traceMsg 1 ("cache hit for " ++ show src ++ " reading " ++ show cf)
                          >>>
                          ( readCache' cf
                            >>>
                            traceMsg 2 "cache read"
                          )
                          `orElse`
                          ( clearErrStatus
                            >>>
                            traceMsg 1 "cache file was corrupted, reading original"
                            >>>
                            readAndCacheDocument
                          )
    readAndCacheDocument
                        = traceMsg 1 ("cache miss, reading original document " ++ show src)
                          >>>
                          readDocument [] src
                          >>>
                          perform ( choiceA
                                    [ is200 :-> ( writeCache src >>> none )
                                    , this  :-> traceMsg 1 "transfer status /= 200, page not cached"
                                    ]
                                  )

    readDocumentCond mt
                        = traceMsg 1 ("cache out of date, read original document if modified " ++ show src)
                          >>>
                          readDocument [withInputOption a_if_modified_since (fmtTime mt)] src
                          >>>
                          choiceA
                          [ is304            :-> ( traceMsg 1 ("document not modified, using cache data from " ++ show cf)
                                                   >>>
                                                   perform (arrIO0 $ touchFile cf)
                                                   >>>
                                                   readDocumentFromCache
                                                 )
                          , is200            :-> ( traceMsg 1 "document read, cache will be updated"
                                                   >>>
                                                   perform (writeCache src
                                                            >>>
                                                            traceMsg 2 "cache is updated"
                                                           )
                                                 )
                          , this             :-> ( traceMsg 1 "document read without caching"
                                                   >>>
                                                   perform ( arrIO0 $ remFile cf )
                                                 )
                          ]
        where
        fmtTime         = formatTime defaultTimeLocale rfc822DateFormat

-- ------------------------------------------------------------

lookupCache             :: (NFData b, Binary b) => String -> IOStateArrow s a b
lookupCache f           = uncurry lookupC $< getSysVar (theDocumentAge .&&&. theCacheDir)
    where
    lookupC age cdir    = isIOA (const $ hit)
                          `guards`
                          readCache' cf
        where
        cf              = uncurry (</>) $ cacheFile cdir f
        hit             = do
                          ch <- cacheHit age cf
                          return $ case ch of
                                   Just Nothing -> True
                                   _            -> False

-- ------------------------------------------------------------

readCache               :: (NFData c, Binary c) => String -> IOStateArrow s b c
readCache f             = readC $< getSysVar theCacheDir
    where
    readC cdir          = readCache' $ uncurry (</>) $ cacheFile cdir f

readCache'              :: (NFData c, Binary c) => String -> IOStateArrow s b c
readCache' cf           = rnfA $ withLock cf $ readBinaryValue cf

writeCache              :: (Binary b) => String -> IOStateArrow s b ()
writeCache f            = writeC $< getSysVar theCacheDir
    where
    writeC cdir         = traceMsg 1 ("writing cache file " ++ show cf ++ " for document " ++ show f)
                          >>>
                          perform (arrIO0 createDir)
                          >>>
                          withLock cf (writeBinaryValue cf)
                          >>>
                          perform (withLock ixf (arrIO0 $ writeIndex ixf f cf))
        where
        cf              = dir </> file
        ixf             = cdir </> "index"
        (dir, file)     = cacheFile cdir f
        createDir       = createDirectoryIfMissing True dir

-- ------------------------------------------------------------

remFile                 :: FilePath -> IO ()
remFile f               = ( try' $ do ex <- doesFileExist f
                                      if ex
                                        then removeFile f
                                        else return ()
                          ) >> return ()

-- ------------------------------------------------------------

cacheFile               :: FilePath -> String -> (FilePath, FilePath)
cacheFile dir f          = (dir </> fd, fn)
    where
    (fd, fn)            = splitAt 2 . sha1HashString $ f

-- ------------------------------------------------------------

-- result interpretation for cacheHit
--
-- Nothing       : cache miss: get document
-- Just Nothing  : cache hit, cache data valid: use cache data
-- Just (Just t) : cache hit, but cache data out of date: get document conditionally with if-modified-since t

cacheHit                :: Int -> FilePath -> IO (Maybe (Maybe UTCTime))
cacheHit age cf         = ( try' $
                            do
                            e <- doesFileExist cf
                            if not e
                              then return Nothing
                              else do
                                   mt <- getModificationTime cf
                                   ct <- getCurrentTime
                                   return . Just $ if (dt `addUTCTime` mt) >= ct
                                                   then Nothing
                                                   else Just mt
                          ) >>= return . either (const Nothing) id
    where
    dt                  = fromInteger . toInteger $ age

try'                    :: IO a -> IO (Either SomeException a)
try'                    = try

writeIndex              :: String -> String -> FilePath -> IO ()
writeIndex ixf f cf     = ( try' $
                            do
                            h <- openBinaryFile ixf AppendMode
                            hPutStrLn h $ show (cf, f)
                            hClose h
                            return ()
                          ) >> return ()

-- ------------------------------------------------------------

-- | Compute the SHA1 hash is hexadecimal format for an arbitray serializable value

sha1HashValue           :: (Arrow a, Binary b) => a b Integer
sha1HashValue           = arr $ integerDigest . sha1 . encode

sha1HashString          :: (Arrow a, Binary b) => a b String
sha1HashString          = arr $ showDigest . sha1 . encode

-- ------------------------------------------------------------


-- | the internal table of file locks

theLockedFiles          :: ResourceTable String
theLockedFiles          = unsafePerformIO newResourceTable
{-# NOINLINE theLockedFiles #-}

lockFile, unlockFile    :: String -> IO ()
lockFile                = requestResource theLockedFiles
unlockFile              = releaseResource theLockedFiles

withLock                :: String -> IOStateArrow s b c -> IOStateArrow s b c
withLock l a            = ( perform (arrIO0 $ lockFile l)
                            >>>
                            listA a
                            >>>
                            perform (arrIO0 $ unlockFile l)
                          )
                          >>>
                          unlistA

-----------------------------------------------------------------------------
