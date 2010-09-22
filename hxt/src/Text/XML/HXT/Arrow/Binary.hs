-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.Binary
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   De-/Serialisation arrows for XmlTrees and other arbitrary values with a Binary instance
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.Binary
    ( readBinaryValue
    , writeBinaryValue
    )
where

import           Control.Arrow          ()
import           Control.Arrow.ArrowExc
import           Control.Arrow.ArrowList
import           Control.Arrow.ArrowIO

import           Control.DeepSeq

import           Data.Binary
import qualified Data.ByteString.Lazy   as B

import           Text.XML.HXT.Arrow.XmlState.ErrorHandling
import           Text.XML.HXT.Arrow.XmlState.TypeDefs

-- ------------------------------------------------------------

readBinaryValue         :: (NFData a, Binary a) => String -> IOStateArrow s b a
readBinaryValue file    = flip decodeBinaryValue file $< getSysVar theBinaryDeCompression

-- | Read a serialied value from a file, optionally decompress it and decode the value
-- In case of an error, the error message is issued and the arrow fails

decodeBinaryValue         :: (NFData a, Binary a) => DeCompressionFct -> String -> IOStateArrow s b a
decodeBinaryValue decompress file
                          = arrIO0 ( do
                                     r <- dec
                                     rnf r `seq` return r
                                   )
                            `catchA`
                            issueExc "readBinaryValue"
    where
    dec                 = B.readFile file >>= return . decode . decompress

-- | Serialize a value, optionally compress it, and write it to a file.
-- In case of an error, the error message is issued and the arrow fails

writeBinaryValue        :: (Binary a) => String -> IOStateArrow s a ()
writeBinaryValue file   = flip encodeBinaryValue file $< getSysVar theBinaryCompression

encodeBinaryValue        :: (Binary a) => CompressionFct -> String -> IOStateArrow s a ()
encodeBinaryValue compress file
                         = arrIO enc
                           `catchA`
                           issueExc "writeBinaryXmlTree"
    where
    enc                  = B.writeFile file . compress . encode

-- ------------------------------------------------------------
