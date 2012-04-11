{- |
   Module     : Text.XML.HXT.XMLSchema.AbstractSyntax
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Contains the basic datatypes to represent a schema definition.
-}

module Text.XML.HXT.XMLSchema.AbstractSyntax

where

import Text.XML.HXT.Core ( QName )

import Data.Map          ( Map )

-- ----------------------------------------

-- | The Schema is represented by several tables
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

-- | Namespace datatype
type Namespace         = String
-- | List of external references
type Includes          = [Include]
-- | Table for SimpleTypes
type SimpleTypeMap     = Map QName SimpleType
-- | Table for ComplexTypes
type ComplexTypeMap    = Map QName ComplexType
-- | Table for elements
type ElementMap        = Map QName Element
-- | Table for groups
type GroupMap          = Map QName Group
-- | Table for attributes
type AttributeMap      = Map QName Attribute
-- | Table for attribute groups
type AttributeGroupMap = Map QName AttributeGroup

-- | External references can be includes, imports or redefines
data Include           = Incl  {unIncl  :: Location}
                       | Imp   {unImp   :: (Location, Namespace)}
                       | Redef {unRedef :: (Location, Redefinitions)}
-- | The referenced schema's location
type Location          = String
-- | List of redefinitions inside a redefine
type Redefinitions     = [Redefinition]
-- | Redefinition of a SimpleType, ComplexType, group or attribute group
data Redefinition      = RedefSt {unRedefSt :: (QName, SimpleType)}
                       | RedefCt {unRedefCt :: (QName, ComplexType)}
                       | RedefGr {unRedefGr :: (QName, Group)}
                       | RedefAg {unRedefAg :: (QName, AttributeGroup)}

-- | SimpleTypes can be restrictions, lists or unions
data SimpleType        = Restr {unRestr :: STRestriction}
                       | Lst   {unLst   :: STList}
                       | Un    {unUn    :: STUnion}
-- | A SimpleType restriction is a type reference and a list of restriction attributes
type STRestriction     = (SimpleTypeRef, RestrAttrs)
-- | A SimpleType reference can be a reference by name or an anonymous declaration
data SimpleTypeRef     = BaseAttr        {unBaseAttr        :: QName}
                       | STRAnonymStDecl {unSTRAnonymStDecl :: SimpleType}
-- | A list of restriction attributes
type RestrAttrs        = [RestrAttr]
-- | Possible restriction attributes (facets)
data RestrAttr         = MinIncl        {unMinIncl        :: String}
                       | MaxIncl        {unMaxIncl        :: String}
                       | MinExcl        {unMinExcl        :: String}
                       | MaxExcl        {unMaxExcl        :: String}
                       | TotalDigits    {unTotalDigits    :: String}
                       | FractionDigits {unFractionDigits :: String}
                       | Length         {unLength         :: String}
                       | MinLength      {unMinLength      :: String}
                       | MaxLength      {unMaxLength      :: String}
                       | Pattern        {unPattern        :: String}
                       | Enumeration    {unEnumeration    :: String}
                       | WhiteSpace     {unWhiteSpace     :: String}
-- | A list of a referenced SimpleType (by name or anonymous declaration)
data STList            = ItemTypeAttr    {unItemTypeAttr    :: QName}
                       | STLAnonymStDecl {unSTLAnonymStDecl :: SimpleType}
-- | An union of referenced SimpleTypes (by names and anonymous declarations)
data STUnion           = STUnion
                       { memberTypes :: Maybe QNames
                       , anonymDecls :: [SimpleType]
                       }
-- | A list of QNames
type QNames            = [QName]

-- | ComplexTypes consist of a mixed-flag and a definition
data ComplexType       = ComplexType
                       { ctMixed :: Maybe String
                       , ctDef   :: CTDef
                       }
-- | A ComplexType definition uses a simple content, complex content or a new ComplexType model
data CTDef             = SCont {unSCont :: SimpleContent}
                       | CCont {unCCont :: ComplexContent}
                       | NewCT {unNewCT :: CTModel}

-- | A simple content can either be an extension or a restriction of an existing type
data SimpleContent     = SCExt   {unSCExt   :: SCExtension}
                       | SCRestr {unSCRestr :: SCRestriction}
-- | The extension of a referenced type with a list of new attributes
type SCExtension       = (QName, AttrList)
-- | The restriction of a referenced types's base type and attributes
type SCRestriction     = (STRestriction, AttrList)

-- | A complex content consists of a mixed-flag and a definition
data ComplexContent    = ComplexContent
                       { ccMixed :: Maybe String
                       , ccDef   :: CCDef
                       }
-- | A complex content definition can either be an extension or a restriction of an existing ComplexType
data CCDef             = CCExt   {unCCExt   :: CCExtension}
                       | CCRestr {unCCRestr :: CCRestriction}
-- | The extension of a referenced ComplexType with an additional ComplexType model
type CCExtension       = (QName, CTModel)
-- | The restriction of a referenced ComplexType on a given ComplexType model
type CCRestriction     = (QName, CTModel)

-- | A ComplexType model can contain a ComplexType compositor and a list of attributes
type CTModel           = (Maybe CTCompositor, AttrList)
-- | Compositors can be group, all, choice or sequence with restriction on the number occurrences
data CTCompositor      = CompGr {unCompGr :: (MinMaxOcc, Group)}
                       | CompAl {unCompAl :: (MinMaxOcc, All)}
                       | CompCh {unCompCh :: (MinMaxOcc, Choice)}
                       | CompSq {unCompSq :: (MinMaxOcc, Sequence)}
-- | Restriction on minimum and maximum number of occurrences
data MinMaxOcc         = MinMaxOcc
                       { minOcc :: Maybe String
                       , maxOcc :: Maybe String
                       }
-- | All is a list of elements and restrictions on the number of occurrences
type All               = [(MinMaxOcc, Element)]
-- | Choice is a list of elements, groups, choices, sequences and element wildcards
type Choice            = [ChSeqContent]
-- | Sequence is a list of elements, groups, choices, sequences and element wildcards
type Sequence          = [ChSeqContent]
-- | Contents of choice and sequence with restrictions on the number of occurrences
data ChSeqContent      = ChSeqEl {unChSeqEl :: (MinMaxOcc, Element)}
                       | ChSeqGr {unChSeqGr :: (MinMaxOcc, Group)}
                       | ChSeqCh {unChSeqCh :: (MinMaxOcc, Choice)}
                       | ChSeqSq {unChSeqSq :: (MinMaxOcc, Sequence)}
                       | ChSeqAn {unChSeqAn :: (MinMaxOcc, Any)}
-- | A wildcard for elements or attributes matching a given namespace
data Any               = Any
                       { namespace       :: Maybe Namespace
                       , processContents :: Maybe String
                       }
-- | A list of attributes
type AttrList          = [AttrListElem]
-- | An attribute list element can be a single attribute, attribute group reference or attribute wildcard
data AttrListElem      = Attr    {unAttr    :: Attribute}
                       | AttrGrp {unAttrGrp :: AttributeGroup}
                       | AnyAttr {unAnyAttr :: Any}

-- | An element is defined by a reference by name or an element definition
data Element           = ElRef {unElRef :: QName}
                       | ElDef {unElDef :: ElementDef}
-- | An element definition consists of the element's name and a type definition
data ElementDef        = ElementDef
                       { elemName    :: QName
                       , elemTypeDef :: ElemTypeDef
                       }
-- | The element's type is defined by a reference by name or an anonymous SimpleType or ComplexType declaration
data ElemTypeDef       = ETDTypeAttr     {unETDTypeAttr     :: QName}
                       | ETDAnonymStDecl {unETDAnonymStDecl :: SimpleType}
                       | ETDAnonymCtDecl {unETDAnonymCtDecl :: ComplexType}

-- | A group is defined by a reference by name or a group definition 
data Group             = GrpRef {unGrpRef :: QName}
                       | GrpDef {unGrpDef :: Maybe GroupDef}
-- | A group definition can be made using all, choice or sequence
data GroupDef          = Al {unAl :: All}
                       | Ch {unCh :: Choice}
                       | Sq {unSq :: Sequence}

-- | An attribute is defined by a reference by name or an attribute definition
data Attribute         = AttrRef {unAttrRef :: QName}
                       | AttrDef {unAttrDef :: AttributeDef}
-- | An attribute definition consists of the attribute's name, type definition and the use-field
data AttributeDef      = AttributeDef
                       { attrName    :: QName
                       , attrTypeDef :: AttrTypeDef
                       , attrUse     :: Maybe String
                       }
-- | The attribute type definition can be a reference by name or an anonymous SimpleType declaration
data AttrTypeDef       = ATDTypeAttr   {unATDTypeAttr   :: QName}
                       | ATDAnonymDecl {unATDAnonymDecl :: SimpleType}

-- | An attribute group is defined by a reference by name or an attribute list
data AttributeGroup    = AttrGrpRef {unAttrGrpRef :: QName}
                       | AttrGrpDef {unAttrGrpDef :: AttrList}

