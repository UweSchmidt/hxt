-- ------------------------------------------------------------

{- |
   Module     : Data.String.UTF8Decoding
   Copyright  : Copyright (C) 2010- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Interface for Data.Char.UTF8 funtions

-}

-- ------------------------------------------------------------

module Data.String.UTF8Decoding (
   decodeUtf8,
   decodeUtf8EmbedErrors,
   decodeUtf8IgnoreErrors,
   )
where

import qualified Data.String.UTF8 as UTF8
import           Data.Word (Word8)

-- | calls 'Data.Char.UTF8.decode' for parsing and decoding UTF-8

decodeUtf8      :: String -> (String, [String])
decodeUtf8 str
    = (res, map (uncurry toErrStr) errs)
    where
    (res, errs) = UTF8.decode . stringToByteString $ str

decodeUtf8IgnoreErrors  :: String -> String
decodeUtf8IgnoreErrors
    = fst . decodeUtf8

decodeUtf8EmbedErrors   :: String -> [Either String Char]
decodeUtf8EmbedErrors str
    = map (either (Left . uncurry toErrStr) Right) $
      UTF8.decodeEmbedErrors $ stringToByteString $ str

stringToByteString :: String -> [Word8]
stringToByteString = map (toEnum . fromEnum)

toErrStr :: UTF8.Error -> Int -> String
toErrStr err pos
        = " at input position " ++ show pos ++ ": " ++ show err

-- ------------------------------------------------------------
