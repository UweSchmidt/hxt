{- |
   Module     : Text.XML.HXT.XMLSchema.AbstractSyntax
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Contains the basic datatypes to represent a schema description.
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
type Location          = String
type Redefinitions     = [Redefinition]
data Redefinition      = RedefSt {unRedefSt :: (QName, SimpleType)}
                       | RedefCt {unRedefCt :: (QName, ComplexType)}
                       | RedefGr {unRedefGr :: (QName, Group)}
                       | RedefAg {unRedefAg :: (QName, AttributeGroup)}

data SimpleType        = Restr {unRestr :: STRestriction}
                       | Lst   {unLst   :: STList}
                       | Un    {unUn    :: STUnion}
type STRestriction     = (SimpleTypeRef, RestrAttrs)
data SimpleTypeRef     = BaseAttr        {unBaseAttr        :: QName}
                       | STRAnonymStDecl {unSTRAnonymStDecl :: SimpleType}
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
type Value             = String
data STList            = ItemTypeAttr    {unItemTypeAttr    :: QName}
                       | STLAnonymStDecl {unSTLAnonymStDecl :: SimpleType}
data STUnion           = STUnion
                       { memberTypes :: Maybe QNames
                       , anonymDecls :: [SimpleType]
                       }
type QNames            = [QName]

data ComplexType       = ComplexType
                       { ctMixed :: Maybe String
                       , ctDef   :: CTDef
                       }
data CTDef             = SCont {unSCont :: SimpleContent}
                       | CCont {unCCont :: ComplexContent}
                       | NewCT {unNewCT :: CTModel}

data SimpleContent     = SCExt   {unSCExt   :: SCExtension}
                       | SCRestr {unSCRestr :: SCRestriction}
type SCExtension       = (QName, AttrList)
type SCRestriction     = (STRestriction, AttrList)

data ComplexContent    = ComplexContent
                       { ccMixed :: Maybe String
                       , ccDef   :: CCDef
                       }
data CCDef             = CCExt   {unCCExt   :: CCExtension}
                       | CCRestr {unCCRestr :: CCRestriction}
type CCExtension       = (QName, CTModel)
type CCRestriction     = (QName, CTModel)

type CTModel           = (Maybe CTCompositor, AttrList)
data CTCompositor      = CompGr {unCompGr :: (MinMaxOcc, Group)}
                       | CompAl {unCompAl :: (MinMaxOcc, All)}
                       | CompCh {unCompCh :: (MinMaxOcc, Choice)}
                       | CompSq {unCompSq :: (MinMaxOcc, Sequence)}
data MinMaxOcc         = MinMaxOcc
                       { minOcc :: Maybe String
                       , maxOcc :: Maybe String
                       }
type All               = [(MinMaxOcc, Element)]
type Choice            = [ChSeqContent]
type Sequence          = [ChSeqContent]
data ChSeqContent      = ChSeqEl {unChSeqEl :: (MinMaxOcc, Element)}
                       | ChSeqGr {unChSeqGr :: (MinMaxOcc, Group)}
                       | ChSeqCh {unChSeqCh :: (MinMaxOcc, Choice)}
                       | ChSeqSq {unChSeqSq :: (MinMaxOcc, Sequence)}
                       | ChSeqAn {unChSeqAn :: (MinMaxOcc, Any)}
data Any               = Any
                       { namespace       :: Maybe Namespace
                       , processContents :: Maybe String
                       }
type AttrList          = [AttrListElem]
data AttrListElem      = Attr    {unAttr    :: Attribute}
                       | AttrGrp {unAttrGrp :: AttributeGroup}
                       | AnyAttr {unAnyAttr :: AnyAttribute}
type AnyAttribute      = Any

data Element           = ElRef {unElRef :: QName}
                       | ElDef {unElDef :: ElementDef}
data ElementDef        = ElementDef
                       { elemName    :: QName
                       , elemTypeDef :: ElemTypeDef
                       }
data ElemTypeDef       = ETDTypeAttr     {unETDTypeAttr     :: QName}
                       | ETDAnonymStDecl {unETDAnonymStDecl :: SimpleType}
                       | ETDAnonymCtDecl {unETDAnonymCtDecl :: ComplexType}

data Group             = GrpRef {unGrpRef :: QName}
                       | GrpDef {unGrpDef :: Maybe GroupContDef}
data GroupContDef      = Al {unAl :: All}
                       | Ch {unCh :: Choice}
                       | Sq {unSq :: Sequence}

data Attribute         = AttrRef {unAttrRef :: QName}
                       | AttrDef {unAttrDef :: AttributeDef}
data AttributeDef      = AttributeDef
                       { attrName    :: QName
                       , attrTypeDef :: AttrTypeDef
                       , attrUse     :: Maybe String
                       }
data AttrTypeDef       = ATDTypeAttr   {unATDTypeAttr   :: QName}
                       | ATDAnonymDecl {unATDAnonymDecl :: SimpleType}

data AttributeGroup    = AttrGrpRef {unAttrGrpRef :: QName}
                       | AttrGrpDef {unAttrGrpDef :: AttrList}

