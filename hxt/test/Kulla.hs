{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Text.XML.HXT.Core

parseXml0 :: IOSArrow XmlTree XmlTree
parseXml0 = getChildren >>> getChildren >>>
  proc x -> do
    _ <- hasName "item" -< x
    returnA -< x

parseXml1 :: IOSArrow XmlTree XmlTree
parseXml1 = getChildren >>> getChildren >>>
            (hasName "item" `guards` this)

main1 :: Show c => IOSArrow XmlTree c -> IO ()
main1 parseXml = do
    person <- runX (readString [withValidate no]
                    "<xml><item>John</item><item2>Smith</item2></xml>"
                    >>> parseXml)
    putStrLn $ show person
    return ()

main :: IO ()
main = main1 parseXml0 >> main1 parseXml1
