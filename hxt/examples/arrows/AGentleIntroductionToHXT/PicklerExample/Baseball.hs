{- |
  Example for usage of pickler functions
  to de-/serialise from/to XML

  Example data is taken from haskell wiki
  http://www.haskell.org/haskellwiki/HXT/Practical/Simple2
-}

module Main
where

import           Data.Map (Map, fromList, toList)

import Text.XML.HXT.Arrow

-- Example data taken from:
-- http://www.ibiblio.org/xml/books/bible/examples/05/5-1.xml

-- ------------------------------------------------------------
-- the data modell

data Season = Season
    { sYear    :: Int
    , sLeagues :: Leagues
    }
	      deriving (Show, Eq)

type Leagues   = Map String Divisions

type Divisions = Map String [Team]

data Team = Team
    { teamName :: String
    , city     :: String
    , players  :: [Player]
    }
	    deriving (Show, Eq)
	     
data Player = Player
    { firstName :: String
    , lastName  :: String
    , position  :: String
    , atBats    :: Maybe Int
    , hits      :: Maybe Int
    , era       :: Maybe Float
    }
	      deriving (Show, Eq)

-- ------------------------------------------------------------
-- the pickler instance declarations
-- in this case just for uniform naming

instance XmlPickler Season where
    xpickle = xpSeason

instance XmlPickler Team where
    xpickle = xpTeam

instance XmlPickler Player where
    xpickle = xpPlayer

-- ------------------------------------------------------------
-- for every data type there is a pickler

-- the XML root element

xpSeason	:: PU Season
xpSeason
    = xpElem "SEASON" $
      xpWrap ( uncurry Season
	     , \ s -> (sYear s, sLeagues s)) $
      xpPair (xpAttr "YEAR" xpickle) xpLeagues

xpLeagues	:: PU Leagues
xpLeagues
    = xpWrap ( fromList
	     , toList ) $
      xpList $
      xpElem "LEAGUE" $
      xpPair (xpAttr "NAME" xpText) xpDivisions

xpDivisions	:: PU Divisions
xpDivisions
    = xpWrap ( fromList
	     , toList
	     ) $
      xpList $
      xpElem "DIVISION" $
      xpPair (xpAttr "NAME" xpText) xpickle

xpTeam	:: PU Team
xpTeam
    = xpElem "TEAM" $
      xpWrap ( uncurry3 Team
	     , \ t -> (teamName t, city t, players t)) $
      xpTriple (xpAttr "NAME" xpText) (xpAttr "CITY" xpText) (xpList xpickle)

xpPlayer	:: PU Player
xpPlayer
    = xpElem "PLAYER" $
      xpWrap ( \ ((f,l,p),(a,h,e)) -> Player f l p a h e
	     , \ t -> ((firstName t, lastName t, position t),(atBats t, hits t, era t))) $
      xpPair (xpTriple (xpAttr "GIVEN_NAME" xpText)
	               (xpAttr "SURNAME"    xpText)
	               (xpAttr "POSITION"   xpText))
             (xpTriple (xpOption (xpAttr "AT_BATS" xpickle))
	               (xpOption (xpAttr "HITS"    xpickle))
	               (xpOption (xpAttr "ERA"     xpPrim )))


-- ------------------------------------------------------------

-- a simple pickle/unpickle application

main	:: IO ()
main
    = do
      runX ( xunpickleDocument xpSeason [ (a_validate,v_0)
					, (a_trace, v_1)
					, (a_remove_whitespace,v_1)
					, (a_preserve_comment, v_0)
					] "simple2.xml"
	     >>>
	     processSeason
	     >>>
	     xpickleDocument xpSeason [ (a_indent, v_1)
				      ] "new-simple2.xml"
	   )
      return ()

-- the dummy for processing the unpickled data

processSeason	:: IOSArrow Season Season
processSeason
    = arrIO ( \ x -> do {print x ; return x})

-- ------------------------------------------------------------

-- the internal data of "simple2.xml"

season1998	:: Season
season1998
    = Season
      { sYear = 1998
      , sLeagues = fromList
	[ ( "American League"
	  , fromList
	    [ ( "Central"
	      , [ Team { teamName = "White Sox"
		       , city = "Chicago"
		       , players = []}
		, Team { teamName = "Royals"
		       , city = "Kansas City"
		       , players = []}
		, Team { teamName = "Tigers"
		       , city = "Detroit"
		       , players = []}
		, Team { teamName = "Indians"
		       , city = "Cleveland"
		       , players = []}
		, Team { teamName = "Twins"
		       , city = "Minnesota"
		       , players = []}
		])
	    , ( "East"
	      , [ Team { teamName = "Orioles"
		       , city = "Baltimore"
		       , players = []}
		, Team { teamName = "Red Sox"
		       , city = "Boston"
		       , players = []}
		, Team { teamName = "Yankees"
		       , city = "New York"
		       , players = []}
		, Team { teamName = "Devil Rays"
		       , city = "Tampa Bay"
		       , players = []}
		, Team { teamName = "Blue Jays"
		       , city = "Toronto"
		       , players = []}
		])
	    , ( "West"
	      , [ Team { teamName = "Angels"
		       , city = "Anaheim"
		       , players = []}
		, Team { teamName = "Athletics"
		       , city = "Oakland"
		       , players = []}
		, Team { teamName = "Mariners"
		       , city = "Seattle"
		       , players = []}
		, Team { teamName = "Rangers"
		       , city = "Texas"
		       , players = []}
		])
	    ])
	, ( "National League"
	  , fromList
	    [ ( "Central"
	      , [ Team { teamName = "Cubs"
		       , city = "Chicago"
		       , players = []}
		, Team { teamName = "Reds"
		       , city = "Cincinnati"
		       , players = []}
		, Team { teamName = "Astros"
		       , city = "Houston"
		       , players = []}
		, Team { teamName = "Brewers"
		       , city = "Milwaukee"
		       , players = []}
		, Team { teamName = "Pirates"
		       , city = "Pittsburgh"
		       , players = []}
		, Team { teamName = "Cardinals"
		       , city = "St. Louis"
		       , players = []}
		])
	    , ( "East"
	      , [ Team { teamName = "Braves"
		       , city = "Atlanta"
		       , players =
			 [ Player { firstName = "Marty"
				  , lastName = "Malloy"
				  , position = "Second Base"
				  , atBats = Just 28
				  , hits = Just 5
				  , era = Nothing}
			 , Player { firstName = "Ozzie"
				  , lastName = "Guillen"
				  , position = "Shortstop"
				  , atBats = Just 264
				  , hits = Just 73
				  , era = Nothing}
			 , Player { firstName = "Danny"
				  , lastName = "Bautista"
				  , position = "Outfield"
				  , atBats = Just 144
				  , hits = Just 36
				  , era = Nothing}
			 , Player { firstName = "Gerald"
				  , lastName = "Williams"
				  , position = "Outfield"
				  , atBats = Just 266
				  , hits = Just 81
				  , era = Nothing}
			 , Player { firstName = "Tom"
				  , lastName = "Glavine"
				  , position = "Starting Pitcher"
				  , atBats = Nothing
				  , hits = Nothing
				  , era = Just 2.47}
			 , Player { firstName = "Javier"
				  , lastName = "Lopez"
				  , position = "Catcher"
				  , atBats = Just 489
				  , hits = Just 139
				  , era = Nothing}
			 , Player { firstName = "Ryan"
				  , lastName = "Klesko"
				  , position = "Outfield"
				  , atBats = Just 427
				  , hits = Just 117
				  , era = Nothing}
			 , Player { firstName = "Andres"
				  , lastName = "Galarraga"
				  , position = "First Base"
				  , atBats = Just 555
				  , hits = Just 169
				  , era = Nothing}
			 , Player { firstName = "Wes"
				  , lastName = "Helms"
				  , position = "Third Base"
				  , atBats = Just 13
				  , hits = Just 4
				  , era = Nothing}
			 ]}
		, Team { teamName = "Marlins"
		       , city = "Florida"
		       , players = []}
		, Team { teamName = "Expos"
		       , city = "Montreal"
		       , players = []}
		, Team { teamName = "Mets"
		       , city = "New York"
		       , players = []}
		, Team { teamName = "Phillies"
		       , city = "Philadelphia"
		       , players = []}
		])
	    , ( "West"
	      , [ Team { teamName = "Diamondbacks"
		       , city = "Arizona"
		       , players = []}
		, Team { teamName = "Rockies"
		       , city = "Colorado"
		       , players = []}
		, Team { teamName = "Dodgers"
		       , city = "Los Angeles"
		       , players = []}
		, Team { teamName = "Padres"
		       , city = "San Diego"
		       , players = []}
		, Team { teamName = "Giants"
		       , city = "San Francisco"
		       , players = []}
		])
	    ])
	]
      }

-- ------------------------------------------------------------
