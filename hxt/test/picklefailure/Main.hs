{-# LANGUAGE RecordWildCards #-}

module Main
where
import Data.Maybe
import Data.Tuple.Curry ( uncurryN )
import Text.XML.HXT.Core

data WeatherListing =
  WeatherListing {
    db_teams :: String,
    db_weather :: String }
  deriving (Eq, Show)


data WeatherLeague =
  WeatherLeague {
    league_name     :: Maybe String,
    listings :: [WeatherListing] }
  deriving (Eq, Show)


data WeatherForecastXml =
  WeatherForecastXml {
    xml_game_date :: String,  -- ^ This is a 'String' instead of 'UTCTime'
                              --   because they don't use a standard
                              --   time format for the day of the month.
    xml_league :: WeatherLeague }
  deriving (Eq, Show)


data Message =
  Message {
    xml_xml_file_id :: Int,
    xml_heading :: String,
    xml_category :: String,
    xml_sport :: String,
    xml_title :: String,
    xml_forecasts :: [WeatherForecastXml],
    xml_time_stamp :: String }
  deriving (Eq, Show)


pickle_listing :: PU WeatherListing
pickle_listing =
  xpElem "listing" $
    xpWrap (from_pair, to_pair) $
      xpPair
        (xpElem "teams" xpText)
        (xpElem "weather" xpText)
  where
    from_pair = uncurry WeatherListing
    to_pair WeatherListing{..} = (db_teams, db_weather)

pickle_league :: PU WeatherLeague
pickle_league =
  xpElem "league" $
    xpWrap (from_pair, to_pair) $
      xpPair
        (xpAttr "name" $
                xpWrap (\ x -> if null x then Nothing else Just x
                       , fromMaybe ""
                       ) xpText0
        )
        (xpList pickle_listing)
  where
    from_pair = uncurry WeatherLeague
    to_pair WeatherLeague{..} = (league_name, listings)

pickle_forecast :: PU WeatherForecastXml
pickle_forecast =
  xpElem "forecast" $
    xpWrap (from_pair, to_pair) $
      xpPair
        (xpAttr "gamedate" xpText)
        pickle_league
  where
    from_pair = uncurry WeatherForecastXml
    to_pair WeatherForecastXml{..} = (xml_game_date,
                                      xml_league)

pickle_message :: PU Message
pickle_message =
  xpElem "message" $
    xpWrap (from_tuple, to_tuple) $
      xp7Tuple
        (xpElem "XML_File_ID" xpInt)
        (xpElem "heading" xpText)
        (xpElem "category" xpText)
        (xpElem "sport" xpText)
        (xpElem "title" xpText)
        (xpList pickle_forecast)
        (xpElem "time_stamp" xpText)
  where
    from_tuple = uncurryN Message
    to_tuple Message{..} = (xml_xml_file_id,
                            xml_heading,
                            xml_category,
                            xml_sport,
                            xml_title,
                            xml_forecasts,
                            xml_time_stamp)


main :: IO ()
main = do
  with_validation <- runX $ xunpickleDocument
                              pickle_message
                              [withRemoveWS yes, withTrace 3]
                              "weatherxml.xml"
  print with_validation

  without_validation <- runX $ xunpickleDocument
                                 pickle_message
                                 [withRemoveWS yes, withValidate no, withTrace 3]
                                 "weatherxml.xml"
  print without_validation
