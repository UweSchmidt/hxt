-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.Pickle.Schema
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

Datatypes and functions for building a content model
for XML picklers. A schema is part of every pickler
and can be used to derive a corrensponding DTD (or Relax NG schema).
This schema further enables checking the picklers.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.Pickle.Schema
where

import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.XMLSchema.DataTypeLibW3CNames

import Data.List
    ( sort )

-- ------------------------------------------------------------

-- | The datatype for modelling the structure of an

data Schema                     = Any
                                | Seq           { sc_l  :: [Schema]
                                                }
                                | Alt           { sc_l  :: [Schema]
                                                }
                                | Rep           { sc_lb :: Int
                                                , sc_ub :: Int
                                                , sc_1  :: Schema
                                                }
                                | Element       { sc_n  :: Name
                                                , sc_1  :: Schema
                                                }
                                | Attribute     { sc_n  :: Name
                                                , sc_1  :: Schema
                                                }
                                | ElemRef       { sc_n  :: Name
                                                }
                                | CharData      { sc_dt :: DataTypeDescr
                                                }
                                  deriving (Eq, Show)

type Name                       = String
type Schemas                    = [Schema]

data DataTypeDescr              = DTDescr { dtLib    :: String
                                          , dtName   :: String
                                          , dtParams :: Attributes
                                          }
                                  deriving (Show)

instance Eq DataTypeDescr where
    x1 == x2 = dtLib x1 == dtLib x2
               &&
               dtName x1 == dtName x2
               &&
               sort (dtParams x1) == sort (dtParams x2)

-- ------------------------------------------------------------

-- | test: is schema a simple XML Schema datatype

isScXsd                 :: (String -> Bool) -> Schema -> Bool

isScXsd p (CharData (DTDescr lib n _ps))
                        = lib == w3cNS
                          &&
                          p n
isScXsd _ _             = False

-- | test: is type a fixed value attribute type

isScFixed               :: Schema -> Bool
isScFixed sc            = isScXsd (== xsd_string) sc
                          &&
                          ((== 1) . length . words . xsdParam xsd_enumeration) sc

isScEnum                :: Schema -> Bool
isScEnum sc             = isScXsd (== xsd_string) sc
                          &&
                          (not . null . xsdParam xsd_enumeration) sc

isScElem                :: Schema -> Bool
isScElem (Element _ _)  = True
isScElem _              = False

isScAttr                :: Schema -> Bool
isScAttr (Attribute _ _)= True
isScAttr _              = False

isScElemRef             :: Schema -> Bool
isScElemRef (ElemRef _) = True
isScElemRef _           = False

isScCharData            :: Schema -> Bool
isScCharData (CharData _)= True
isScCharData _          = False

isScSARE                :: Schema -> Bool
isScSARE (Seq _)        = True
isScSARE (Alt _)        = True
isScSARE (Rep _ _ _)    = True
isScSARE (ElemRef _)    = True
isScSARE _              = False

isScList                :: Schema -> Bool
isScList (Rep 0 (-1) _) = True
isScList _              = False

isScOpt                 :: Schema -> Bool
isScOpt (Rep 0 1 _)     = True
isScOpt _               = False

-- | access an attribute of a descr of an atomic type

xsdParam                :: String -> Schema -> String
xsdParam n (CharData dtd)
                        = lookup1 n (dtParams dtd)
xsdParam _ _            = ""

-- ------------------------------------------------------------

-- smart constructors for Schema datatype

-- ------------------------------------------------------------
--
-- predefined xsd data types for representation of DTD types

scDT            :: String -> String -> Attributes -> Schema
scDT l n rl     = CharData $ DTDescr l n rl

scDTxsd         :: String -> Attributes -> Schema
scDTxsd         = scDT w3cNS

scString        :: Schema
scString        = scDTxsd xsd_string []

scString1       :: Schema
scString1       = scDTxsd xsd_string [(xsd_minLength, "1")]

scFixed         :: String -> Schema
scFixed v       = scDTxsd xsd_string [(xsd_enumeration, v)]

scEnum          :: [String] -> Schema
scEnum vs       = scFixed (unwords vs)

scNmtoken       :: Schema
scNmtoken       = scDTxsd xsd_NCName []

scNmtokens      :: Schema
scNmtokens      = scList scNmtoken

-- ------------------------------------------------------------

scEmpty                         :: Schema
scEmpty                         = Seq []

scSeq                           :: Schema -> Schema -> Schema
scSeq (Seq [])   sc2            = sc2
scSeq sc1        (Seq [])       = sc1
scSeq (Seq scs1) (Seq scs2)     = Seq (scs1 ++ scs2)    -- prevent nested Seq expr
scSeq (Seq scs1) sc2            = Seq (scs1 ++ [sc2])
scSeq sc1        (Seq scs2)     = Seq (sc1  :  scs2)
scSeq sc1        sc2            = Seq [sc1,sc2]

scSeqs                          :: [Schema] -> Schema
scSeqs                          = foldl scSeq scEmpty

scNull                          :: Schema
scNull                          = Alt []

scAlt                           :: Schema -> Schema -> Schema
scAlt (Alt [])   sc2            = sc2
scAlt sc1        (Alt [])       = sc1
scAlt (Alt scs1) (Alt scs2)     = Alt (scs1 ++ scs2)    -- prevent nested Alt expr
scAlt (Alt scs1) sc2            = Alt (scs1 ++ [sc2])
scAlt sc1        (Alt scs2)     = Alt (sc1  :  scs2)
scAlt sc1        sc2            = Alt [sc1,sc2]

scAlts          :: [Schema] -> Schema
scAlts          = foldl scAlt scNull

scOption        :: Schema -> Schema
scOption     (Seq [])           = scEmpty
scOption (Attribute n sc2)      = Attribute n (scOption sc2)
scOption sc1
    | sc1 == scString1          = scString
    | otherwise                 = scOpt sc1

scList          :: Schema -> Schema
scList          = scRep 0 (-1)

scList1         :: Schema -> Schema
scList1         = scRep 1 (-1)

scOpt           :: Schema -> Schema
scOpt           = scRep 0 1

scRep           :: Int -> Int -> Schema -> Schema
scRep l u sc1  = Rep l u sc1

scElem          :: String -> Schema -> Schema
scElem n sc1    = Element n sc1

scAttr          :: String -> Schema -> Schema
scAttr n sc1    = Attribute n sc1

-- ------------------------------------------------------------
