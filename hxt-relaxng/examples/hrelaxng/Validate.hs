module Validate where

import Data.Maybe

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG

loadSchema	:: String -> IO (Maybe XmlTree)
loadSchema schema =
    runX ( validateSchemaWithRelax schema ) >>= return . listToMaybe

validateWithSchema	:: XmlTree -> XmlTree -> Maybe String
validateWithSchema schema doc
    = listToMaybe $
      runLA
      ( validateRelax' schema
        >>>
        getErrorMsg
      ) doc

main1    :: String -> String -> IO ()
main1 sf df
    = do
      schema <- loadSchema sf >>= return . fromJust
      doc    <- runX ( readDocument [ withValidate no ] df ) >>= return . head
      case validateWithSchema schema doc of
        Just e  -> putStrLn $ "Document " ++ show df ++ " is not valid for schema " ++ show sf ++ ": " ++ e
        Nothing -> putStrLn $ "Document " ++ show df ++ " is valid for schema " ++ show sf

main :: IO ()
main
    = do
      main1 "valid1.rng" "valid1.xml"
      main1 "valid1.rng" "invalid1.xml"
