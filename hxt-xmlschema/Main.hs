import Text.XML.HXT.Core

import Data.Map (Map, fromList, toList)

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

instance XmlPickler Season where
    xpickle = xpSeason
 
instance XmlPickler Team where
    xpickle = xpTeam
 
instance XmlPickler Player where
    xpickle = xpPlayer

-- root pickler for root node
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
	     , \ t -> ( teamName t
                      , city t
                      , players t
                      )
             ) $
      xpTriple (xpAttr "NAME" xpText)
               (xpAttr "CITY" xpText)
               (xpList xpickle)

xpPlayer        :: PU Player
xpPlayer
    = xpElem "PLAYER" $
      xpWrap ( \ ((f,l,p,a,h,e)) -> Player f l p a h e
             , \ t -> (firstName t, lastName t
                      , position t, atBats t
                      , hits t, era t
                      )
             ) $
      xp6Tuple (xpAttr           "GIVEN_NAME" xpText  )
               (xpAttr           "SURNAME"    xpText  )
               (xpAttr           "POSITION"   xpText  )
               (xpOption (xpAttr "AT_BATS"    xpickle))
               (xpOption (xpAttr "HITS"       xpickle))
               (xpOption (xpAttr "ERA"        xpPrim ))

loadSeason :: IO Season
loadSeason
  = do
    [s] <- runX
            ( 
              xunpickleDocument xpSeason
                                [ withValidate yes       -- validate source
                                , withTrace 1            -- trace processing steps
                                , withRemoveWS yes       -- remove redundant whitespace
                                , withPreserveComment no -- remove comments
                                ] "example.xml"
            )
    return s

storeSeason :: Season -> IO ()
storeSeason s
  = do
    _ <- runX ( constA s
                >>>
	        xpickleDocument   xpSeason
                                  [ withIndent yes       -- indent generated xml
                                  ] "new-example.xml"
         )
    return ()

main	:: IO ()
main
    = do
      season <- loadSeason
      print season
      storeSeason season
      return ()


