module Main
where

import Text.XML.HXT.Core

main :: IO ()
main = runX
       ( readBinaryValue "emil"
         >>>
         xshow this
       ) >> return ()
             