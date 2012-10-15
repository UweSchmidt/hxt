module Henry where

import Text.XML.HXT.Core
-- import Data.String.Unicode (unicodeToUtf8) -- workaround for hxt < 9.3.1.1
import qualified System.IO.UTF8 as U

isoFile = "/usr/share/xml/iso-codes/iso_3166_2.xml"

countZerosInLines = length . filter (\x -> x == '0') . concat

utf8Copy = do
  xml <- U.readFile isoFile
  -- let doc = readString [withValidate no] xml -- function "doc" never executed
  U.writeFile "iso_written.xml" xml
  putStrLn "Ran utf8Copy" 

noCrash ::  IO ()
noCrash = do
  lines <- runX (readDocument [withValidate no] isoFile
                 >>> writeDocumentToString [withShowTree yes]
                )
  -- mapM_ putStrLn lines            
  print $ countZerosInLines lines
  putStrLn "Ran noCrash" 


crash :: IO ()
crash = do
  xml <- U.readFile isoFile
  src <- runX (readString [withValidate no] xml -- (unicodeToUtf8 xml) -- workaround
               >>> writeDocumentToString [withShowTree yes]
              )
  print $ countZerosInLines src
  putStrLn "Ran crash (didn't)"


{-
Ran utf8Copy
1236
Ran noCrash

error: UTF-8 encoding error at input position 2640: InvalidLaterByte 1

error: UTF-8 encoding error at input position 2646: InvalidLaterByte 1
*** Exception: Enum.toEnum{Word8}: tag (363) is outside of bounds (0,255)
-}

main = do
  utf8Copy
  noCrash
  crash
