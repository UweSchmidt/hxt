-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DOM.UTF8Decoding
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable
   Version    : $Id$

   Interface for Data.Char.UTF8 funtions

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DOM.UTF8Decoding
    ( decodeUtf8 )
where

import Data.Char.UTF8

-- | calls 'Data.Char.UTF8.decode' for parsing and decoding UTF-8

decodeUtf8	:: String -> (String, [String])
decodeUtf8 str
    = (res, map (uncurry toErrStr) errs)
    where
    (res, errs) = decode . map (toEnum . fromEnum) $ str
    toErrStr err pos
	= " at input position " ++ show pos ++ ": " ++ show err

-- ------------------------------------------------------------
