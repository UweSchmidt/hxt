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

import Text.XML.HXT.DOM.XmlKeywords (k_required, k_implied, k_fixed)

-- ------------------------------------------------------------

-- | The datatype for modelling the structure of an

data Schema			= Any
				| PCData	SchemaRestriction
				| CData		Kind	SchemaRestriction
				| Option	Schema
				| Seq		[Schema]
				| Alt		[Schema]
				| Rep	Int Int Schema
				| Element	Name	Schema
				| Attribute	Name	Schema
				| ElemRef	Name
				  deriving (Show)

type Name			= String
type Kind               	= String	-- k_fixed, k_implies, k_required
type Schemas			= [Schema]

data SchemaRestriction		= FixedValue	String
				| DTDAttrType	String
				| ValEnum	[String]
				| RegEx		String
				| XmlSchemaType	String
				  deriving (Eq, Show)

isScElem		:: Schema -> Bool
isScElem (Element _ _)	= True
isScElem _		= False

isScElemRef		:: Schema -> Bool
isScElemRef (ElemRef _)	= True
isScElemRef _		= False

isScPCData		:: Schema -> Bool
isScPCData (PCData _)	= True
isScPCData _		= False

isScCData		:: Schema -> Bool
isScCData (CData _ _)	= True
isScCData _		= False

isScSARE		:: Schema -> Bool
isScSARE (Seq _)	= True
isScSARE (Alt _)	= True
isScSARE (Rep _ _ _)	= True
isScSARE (ElemRef _)	= True
isScSARE _		= False

emptyText			:: SchemaRestriction
emptyText			= restrictRegEx ".*"

noneEmptyText			:: SchemaRestriction
noneEmptyText			= restrictRegEx ".+"

restrictRegEx			:: String -> SchemaRestriction
restrictRegEx			= RegEx

restrictDTDAttrType		:: String -> SchemaRestriction
restrictDTDAttrType		= DTDAttrType

restrictEnum			:: [String] -> SchemaRestriction
restrictEnum			= ValEnum

restrictOption			:: SchemaRestriction -> SchemaRestriction
restrictOption (RegEx re)	= restrictRegEx $ "(" ++ re ++ ")?"
restrictOption r		= r

-- ------------------------------------------------------------

-- smart constructors for Schema datatype

scEmpty				:: Schema
scEmpty				= Seq []

scRequiredAttr			:: SchemaRestriction -> Schema
scRequiredAttr re		= CData k_required re

scImpliedAttr			:: SchemaRestriction -> Schema
scImpliedAttr re		= CData k_implied re

scFixedCData			:: String -> Schema
scFixedCData v			= CData k_fixed (FixedValue v)

scRestrict			:: SchemaRestriction -> Schema -> Schema
scRestrict re (CData k _)	= CData k re
scRestrict re (PCData _)	= PCData re
scRestrict _ sc			= sc

scSeq				:: Schema -> Schema -> Schema
scSeq (Seq [])   sc2		= sc2
scSeq sc1        (Seq [])	= sc1
scSeq (Seq scs1) (Seq scs2)	= Seq (scs1 ++ scs2)	-- prevent nested Seq expr
scSeq (Seq scs1) sc2		= Seq (scs1 ++ [sc2])
scSeq sc1        (Seq scs2)	= Seq (sc1  :  scs2)
scSeq sc1        sc2		= Seq [sc1,sc2]

scSeqs				:: [Schema] -> Schema
scSeqs				= foldl scSeq scEmpty

scNull				:: Schema
scNull				= Alt []

scAlt				:: Schema -> Schema -> Schema
scAlt (Alt [])   sc2		= sc2
scAlt sc1        (Alt [])	= sc1
scAlt (Alt scs1) (Alt scs2)	= Alt (scs1 ++ scs2)	-- prevent nested Alt expr
scAlt (Alt scs1) sc2		= Alt (scs1 ++ [sc2])
scAlt sc1        (Alt scs2)	= Alt (sc1  :  scs2)
scAlt sc1        sc2		= Alt [sc1,sc2]

scAlts		:: [Schema] -> Schema
scAlts		= foldl scAlt scNull

scOption	:: Schema -> Schema
scOption     (Seq [])		= scEmpty

scOption (PCData re)
    | re == noneEmptyText	= PCData emptyText
    | otherwise			= PCData (restrictOption re)

scOption (Attribute n sc2)	= Attribute n (scOptionAttr sc2)
    where
    scOptionAttr (CData _ re)	= scImpliedAttr re
    scOptionAttr sc'		= sc'

scOption sc1			= Rep 0 1 sc1

scList		:: Int -> Int -> Schema -> Schema
scList l u sc1  = Rep l u sc1

scElem		:: String -> Schema -> Schema
scElem n sc1	= Element n sc1

scAttr		:: String -> Schema -> Schema
scAttr n sc1	= Attribute n (scAttrCont sc1)
    where
    scAttrCont (PCData re)	= scRequiredAttr re
    scAttrCont sc'		= sc'

-- ------------------------------------------------------------
