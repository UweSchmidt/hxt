module Text.XML.HXT.DOM.UTF8Decoding
    ( decodeUtf8 )
where

import Data.Char.UTF8

decodeUtf8	:: String -> (String, [String])
decodeUtf8 str
    = (res, map (uncurry toErrStr) errs)
    where
    (res, errs) = decode . map (toEnum . fromEnum) $ str
    toErrStr err pos
	= " at input position " ++ show pos ++ ": " ++ show err
