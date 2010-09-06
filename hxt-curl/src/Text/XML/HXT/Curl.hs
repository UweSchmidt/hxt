-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Curl
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   libcurl input
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Curl
    ( getLibCurlContents
    , a_use_curl
    , withCurl
    , curlOptions
    )
where

import           Text.XML.HXT.Arrow.LibCurlInput

-- ----------------------------------------------------------

