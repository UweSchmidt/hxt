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
    )
where

import qualified Data.ByteString.Lazy	as B
import           Data.Maybe
import		 Data.Digest.Pure.SHA

import           System.FilePath
import		 System.Directory

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

readDocument		:: Attributes -> String -> IOStateArrow s b XmlTree
readDocument userOptions src
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
      options		= addEntries userOptions defaultOptions
      defaultOptions	= [ ( a_compress,	v_0	   )
                          , ( a_cache,		"./.cache" )
                          ]
      compr		= optionIsSet a_compress options
      withCache		= isJust . lookup a_cache $ userOptions
      cachedir		= lookup1 a_cache options
      cacheConfig	= CC { c_dir      = cachedir
                             , c_compress = compr
                             , c_age      = undefined
                             }

-- ------------------------------------------------------------

data CacheConfig	= CC { c_dir		:: FilePath
                             , c_compress	:: Bool
                             , c_age		:: Integer
                             }

lookupCache		:: CacheConfig -> String -> IOStateArrow s a XmlTree
lookupCache cc f	= readBinaryValue (c_compress cc) (uncurry (</>) $ cacheFile cc f)

writeCache		:: CacheConfig -> String -> IOStateArrow s XmlTree ()
writeCache cc f		= perform (arrIO0 createDir)
                          >>>
                          writeBinaryValue (c_compress cc) (dir </> file)
    where
    (dir, file)		= cacheFile cc f
    createDir		= createDirectoryIfMissing True dir

cacheFile		:: CacheConfig -> String -> (FilePath, FilePath)
cacheFile cc f		= (c_dir cc </> fd, fn)
    where
    (fd, fn)		= splitAt 2 . showDigest . sha1 . B.pack . map (toEnum . fromEnum) $ f
    

-- ------------------------------------------------------------
