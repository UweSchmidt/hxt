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

import           Control.DeepSeq
import           Control.Exception      ( SomeException
                                        , try
                                        )
import           Codec.Compression.BZip ( compress
                                        , decompress
                                        )
import           Data.Binary
import qualified Data.ByteString.Lazy   as B

import           Text.XML.HXT.Arrow

-- ------------------------------------------------------------

-- | Read a serialied value from a file. The the flag indicates uncompressing.
-- In case of an error, the error message is issued and the arrow fails

readBinaryValue         :: (NFData a, Binary a) => Bool -> String -> IOStateArrow s b a
readBinaryValue c f     = arrIO0 (try' $ do
                                         r <- dec c
                                         rnf r `seq` return r
                                 )
                          >>>
                          issueExc "readBinaryValue"
    where
    dec False           = decodeFile f
    dec True            = B.readFile f >>= return . decode . decompress

-- | Serialize a value, optionally compress it, and write it to a file.
-- In case of an error, the error message is issued and the arrow fails

writeBinaryValue        :: (Binary a) => Bool -> String -> IOStateArrow s a ()
writeBinaryValue c f    = arrIO (\ x -> try' $ enc c x)
                          >>>
                          issueExc "writeBinaryXmlTree"
    where
    enc False           = encodeFile f
    enc True            = B.writeFile f . compress . encode


issueExc                :: String -> IOStateArrow s (Either SomeException a) a
issueExc s              = ( ( issueFatal $< arr  ((("Exception in " ++ s ++ ": ") ++) . show)
                              >>>
                              none
                            )
                            |||
                            this
                          )

try'                    :: IO a -> IO (Either SomeException a)
try'                    = try

-- ------------------------------------------------------------
