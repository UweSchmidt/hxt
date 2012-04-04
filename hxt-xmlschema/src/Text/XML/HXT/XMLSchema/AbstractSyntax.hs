{- |
   Module     : Text.XML.HXT.XMLSchema.AbstractSyntax
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

-}

module Text.XML.HXT.XMLSchema.AbstractSyntax

where

import Text.XML.HXT.Core ( QName )

import Data.Map          ( Map )

-- ----------------------------------------

-- | ...
data XmlSchema         = XmlSchema
                       { sTargetNS        :: Maybe Namespace
                       , sIncludes        :: Includes 
                       , sSimpleTypes     :: SimpleTypeMap
                       , sComplexTypes    :: ComplexTypeMap
                       , sElements        :: ElementMap
                       , sGroups          :: GroupMap
                       , sAttributes      :: AttributeMap
                       , sAttributeGroups :: AttributeGroupMap
                       }
                       deriving (Show, Eq)

type Namespace         = String
type Includes          = [Include]
type SimpleTypeMap     = Map QName SimpleType
type ComplexTypeMap    = Map QName ComplexType
type ElementMap        = Map QName Element
type GroupMap          = Map QName Group
type AttributeMap      = Map QName Attribute
type AttributeGroupMap = Map QName AttributeGroup

data Include           = Incl  {unIncl  :: Location}
                       | Imp   {unImp   :: (Location, Namespace)}
                       | Redef {unRedef :: (Location, Redefinitions)}
                       deriving (Show, Eq)
type Location          = String
type Redefinitions     = [Redefinition]
data Redefinition      = RedefSt {unRedefSt :: (QName, SimpleType)}
                       | RedefCt {unRedefCt :: (QName, ComplexType)}
                       | RedefGr {unRedefGr :: (QName, Group)}
                       | RedefAg {unRedefAg :: (QName, AttributeGroup)}
                       deriving (Show, Eq)

data SimpleType        = Restr {unRestr :: STRestriction}
                       | Lst   {unLst   :: STList}
                       | Un    {unUn    :: STUnion}
                       deriving (Show, Eq)
type STRestriction     = (SimpleTypeRef, RestrAttrs)
data SimpleTypeRef     = BaseAttr        {unBaseAttr        :: QName}
                       | STRAnonymStDecl {unSTRAnonymStDecl :: SimpleType}
                       deriving (Show, Eq)
type RestrAttrs        = [RestrAttr]
data RestrAttr         = MinIncl        {unMinIncl        :: Value}
                       | MaxIncl        {unMaxIncl        :: Value}
                       | MinExcl        {unMinExcl        :: Value}
                       | MaxExcl        {unMaxExcl        :: Value}
                       | TotalDigits    {unTotalDigits    :: Value}
                       | FractionDigits {unFractionDigits :: Value}
                       | Length         {unLength         :: Value}
                       | MinLength      {unMinLength      :: Value}
                       | MaxLength      {unMaxLength      :: Value}
                       | Pattern        {unPattern        :: Value}
                       | Enumeration    {unEnumeration    :: Value}
                       | WhiteSpace     {unWhiteSpace     :: Value}
                       deriving (Show, Eq)
type Value             = String
data STList            = ItemTypeAttr    {unItemTypeAttr    :: QName}
                       | STLAnonymStDecl {unSTLAnonymStDecl :: SimpleType}
                       deriving (Show, Eq)
data STUnion           = STUnion
                       { memberTypes :: Maybe QNames
                       , anonymDecls :: [SimpleType]
                       }
                       deriving (Show, Eq)
type QNames            = [QName]

data ComplexType       = ComplexType
                       { ctMixed :: Maybe String
                       , ctDef   :: CTDef
                       }
                       deriving (Show, Eq)
data CTDef             = SCont {unSCont :: SimpleContent}
                       | CCont {unCCont :: ComplexContent}
                       | NewCT {unNewCT :: CTModel}
                       deriving (Show, Eq)

data SimpleContent     = SCExt   {unSCExt   :: SCExtension}
                       | SCRestr {unSCRestr :: SCRestriction}
                       deriving (Show, Eq)
type SCExtension       = (QName, AttrList)
type SCRestriction     = (STRestriction, AttrList)

data ComplexContent    = ComplexContent
                       { ccMixed :: Maybe String
                       , ccDef   :: CCDef
                       }
                       deriving (Show, Eq)
data CCDef             = CCExt   {unCCExt   :: CCExtension}
                       | CCRestr {unCCRestr :: CCRestriction}
                       deriving (Show, Eq)
type CCExtension       = (QName, CTModel)
type CCRestriction     = (QName, CTModel)

type CTModel           = (Maybe CTCompositor, AttrList)
data CTCompositor      = CompGr {unCompGr :: (MinMaxOcc, Group)}
                       | CompAl {unCompAl :: (MinMaxOcc, All)}
                       | CompCh {unCompCh :: (MinMaxOcc, Choice)}
                       | CompSq {unCompSq :: (MinMaxOcc, Sequence)}
                       deriving (Show, Eq)
data MinMaxOcc         = MinMaxOcc
                       { minOcc :: Maybe String
                       , maxOcc :: Maybe String
                       }
                       deriving (Show, Eq)
type All               = [(MinMaxOcc, Element)]
type Choice            = [ChSeqContent]
type Sequence          = [ChSeqContent]
data ChSeqContent      = ChSeqEl {unChSeqEl :: (MinMaxOcc, Element)}
                       | ChSeqGr {unChSeqGr :: (MinMaxOcc, Group)}
                       | ChSeqCh {unChSeqCh :: (MinMaxOcc, Choice)}
                       | ChSeqSq {unChSeqSq :: (MinMaxOcc, Sequence)}
                       | ChSeqAn {unChSeqAn :: (MinMaxOcc, Any)}
                       deriving (Show, Eq)
data Any               = Any
                       { namespace       :: Maybe Namespace
                       , processContents :: Maybe String
                       }
                       deriving (Show, Eq)
type AttrList          = [AttrListElem]
data AttrListElem      = Attr    {unAttr    :: Attribute}
                       | AttrGrp {unAttrGrp :: AttributeGroup}
                       | AnyAttr {unAnyAttr :: AnyAttribute}
                       deriving (Show, Eq)
type AnyAttribute      = Any

data Element           = ElRef {unElRef :: QName}
                       | ElDef {unElDef :: ElementDef}
                       deriving (Show, Eq)
data ElementDef        = ElementDef
                       { elemName    :: QName
                       , elemTypeDef :: ElemTypeDef
                       }
                       deriving (Show, Eq)
data ElemTypeDef       = ETDTypeAttr     {unETDTypeAttr     :: QName}
                       | ETDAnonymStDecl {unETDAnonymStDecl :: SimpleType}
                       | ETDAnonymCtDecl {unETDAnonymCtDecl :: ComplexType}
                       deriving (Show, Eq)

data Group             = GrpRef {unGrpRef :: QName}
                       | GrpDef {unGrpDef :: Maybe GroupContDef}
                       deriving (Show, Eq)
data GroupContDef      = Al {unAl :: All}
                       | Ch {unCh :: Choice}
                       | Sq {unSq :: Sequence}
                       deriving (Show, Eq)

data Attribute         = AttrRef {unAttrRef :: QName}
                       | AttrDef {unAttrDef :: AttributeDef}
                       deriving (Show, Eq)
data AttributeDef      = AttributeDef
                       { attrName    :: QName
                       , attrTypeDef :: AttrTypeDef
                       , attrUse     :: Maybe String
                       }
                       deriving (Show, Eq)
data AttrTypeDef       = ATDTypeAttr   {unATDTypeAttr   :: QName}
                       | ATDAnonymDecl {unATDAnonymDecl :: SimpleType}
                       deriving (Show, Eq)

data AttributeGroup    = AttrGrpRef {unAttrGrpRef :: QName}
                       | AttrGrpDef {unAttrGrpDef :: AttrList}
                       deriving (Show, Eq)

