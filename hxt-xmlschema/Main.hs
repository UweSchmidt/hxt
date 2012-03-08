import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import Data.Map (Map, lookup, keys, elems, fromList, toList, empty, insert, union)
import Prelude hiding (lookup)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer hiding (Any, All)

-- Type definitions

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
type SimpleTypeMap     = Map Name SimpleType
type ComplexTypeMap    = Map Name ComplexType
type ElementMap        = Map Name Element
type GroupMap          = Map Name Group
type AttributeMap      = Map Name Attribute
type AttributeGroupMap = Map Name AttributeGroup
type Name              = String

data XmlSchema'        = XmlSchema'
                       { targetNS :: Maybe Namespace
                       , parts    :: [XmlSchemaPart]
                       }
data XmlSchemaPart     = In {unIn :: Include}
                       | St {unSt :: (Name, SimpleType)}
                       | Ct {unCt :: (Name, ComplexType)}
                       | El {unEl :: Element}
                       | Gr {unGr :: (Name, Group)}
                       | At {unAt :: Attribute}
                       | Ag {unAg :: (Name, AttributeGroup)}

data Include           = Incl  {unIncl  :: Location}
                       | Imp   {unImp   :: (Location, Namespace)}
                       | Redef {unRedef :: (Location, Redefinitions)}
                       deriving (Show, Eq)
type Location          = String
type Redefinitions     = [Redefinition]
data Redefinition      = RedefSt {unRedefSt :: (Name, SimpleType)}
                       | RedefCt {unRedefCt :: (Name, ComplexType)}
                       | RedefGr {unRedefGr :: (Name, Group)}
                       | RedefAg {unRedefAg :: (Name, AttributeGroup)}
                       deriving (Show, Eq)

data SimpleType        = Restr {unRestr :: STRestriction}
                       | Lst   {unLst   :: STList}
                       | Un    {unUn    :: STUnion}
                       deriving (Show, Eq)
type STRestriction     = (SimpleTypeRef, RestrAttrs)
data SimpleTypeRef     = BaseAttr        {unBaseAttr        :: Name}
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
                       | Enumeration    {unEnumeration    :: Value}
                       | Pattern        {unPattern        :: Value}
                       | WhiteSpace     {unWhiteSpace     :: Value}
                       deriving (Show, Eq)
type Value             = String
data STList            = ItemTypeAttr    {unItemTypeAttr    :: Name}
                       | STLAnonymStDecl {unSTLAnonymStDecl :: SimpleType}
                       deriving (Show, Eq)
data STUnion           = STUnion
                       { memberTypes :: Maybe String -- space separated list
                       , anonymDecls :: [SimpleType]
                       }
                       deriving (Show, Eq)

data ComplexType       = ComplexType
                       { ctMixed :: Maybe String
                       , ctDef   :: CTDef
                       }
                       deriving (Show, Eq)
data CTDef             = SCont {unSCont :: SimpleContent}
                       | CCont {unCCont :: ComplexContent} -- TODO: Prüffunktionen logisch verknüpfen für Ableitungen
                       | NewCT {unNewCT :: CTModel}    -- dynamic Programming, Werte aus Map abfragen während sie konstr. wird
                       deriving (Show, Eq)

data SimpleContent     = SCExt   {unSCExt   :: SCExtension}
                       | SCRestr {unSCRestr :: SCRestriction}
                       deriving (Show, Eq)
type SCExtension       = (Name, AttrList)
type SCRestriction     = (STRestriction, AttrList)

data ComplexContent    = ComplexContent
                       { ccMixed :: Maybe String
                       , ccDef :: CCDef
                       }
                       deriving (Show, Eq)
data CCDef             = CCExt   {unCCExt   :: CCExtension}
                       | CCRestr {unCCRestr :: CCRestriction}
                       deriving (Show, Eq)
type CCExtension       = (Name, CTModel)
type CCRestriction     = (Name, CTModel)

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

data Element           = ElRef {unElRef :: Name}
                       | ElDef {unElDef :: ElementDef}
                       deriving (Show, Eq)
data ElementDef        = ElementDef
                       { elemName        :: Name
                       , elemTypeDef     :: ElemTypeDef
                       , elemDefaultVal  :: Maybe String -- TODO: sense?
                       }
                       deriving (Show, Eq)
data ElemTypeDef       = ETDTypeAttr     {unETDTypeAttr     :: Name}
                       | ETDAnonymStDecl {unETDAnonymStDecl :: SimpleType}
                       | ETDAnonymCtDecl {unETDAnonymCtDecl :: ComplexType}
                       deriving (Show, Eq)

data Group             = GrpRef {unGrpRef :: Name}
                       | GrpDef {unGrpDef :: Maybe GroupContDef}
                       deriving (Show, Eq)
data GroupContDef      = Al {unAl :: All}
                       | Ch {unCh :: Choice}
                       | Sq {unSq :: Sequence}
                       deriving (Show, Eq)

data Attribute         = AttrRef {unAttrRef :: Name}
                       | AttrDef {unAttrDef :: AttributeDef}
                       deriving (Show, Eq)
data AttributeDef      = AttributeDef
                       { attrName       :: Name
                       , attrTypeDef    :: AttrTypeDef
                       , attrDefaultVal :: Maybe String
                       , attrUse        :: Maybe String -- pendant to minMaxOcc
                       }
                       deriving (Show, Eq)
data AttrTypeDef       = ATDTypeAttr   {unATDTypeAttr   :: Name}
                       | ATDAnonymDecl {unATDAnonymDecl :: SimpleType}
                       deriving (Show, Eq)

data AttributeGroup    = AttrGrpRef {unAttrGrpRef :: Name}
                       | AttrGrpDef {unAttrGrpDef :: AttrList}
                       deriving (Show, Eq)

-- Namespace handling

nsUri :: String
nsUri    = "http://www.w3.org/2001/XMLSchema"

nsPrefix :: String
nsPrefix = "xs"

xpElem' :: String -> PU a -> PU a
xpElem' name
    = xpElemNS nsUri nsPrefix name

xpSchemaElem :: String -> PU a -> PU a
xpSchemaElem name
    = xpElem' name . xpFilterSchema

xpFilterSchema :: PU a -> PU a
xpFilterSchema
    = -- keep elems from xs namespace that are not blacklisted
      xpFilterCont (isXmlSchemaElem           >>> neg (   hasQName annotationName
                                                      <+> hasQName notationName
                                                      <+> hasQName uniqueName
                                                      <+> hasQName keyName
                                                      <+> hasQName keyrefName
                                                      )) .
      -- keep attrs without namespace that are not blacklisted
      xpFilterAttr (isAttrWithoutNamespaceUri >>> neg (   hasQName idName
                                                      <+> hasQName attributeFormDefaultName
                                                      <+> hasQName blockDefaultName
                                                      <+> hasQName elementFormDefaultName
                                                      <+> hasQName finalDefaultName
                                                      <+> hasQName versionName
                                                      <+> hasQName langName
                                                      <+> hasQName finalName
                                                      <+> hasQName fixedName
                                                      <+> hasQName abstractName
                                                      <+> hasQName blockName
                                                      <+> hasQName formName
                                                      <+> hasQName nillableName
                                                      <+> hasQName substitutionGroupName
                                                      ))
      where
      -- element blacklist
      isXmlSchemaElem           = hasNameWith ((== nsUri) . namespaceUri)
      annotationName            = mkQName nsUri nsPrefix "annotation"
      notationName              = mkQName nsUri nsPrefix "notation"
      -- irrelevant content for element
      uniqueName                = mkQName nsUri nsPrefix "unique"
      keyName                   = mkQName nsUri nsPrefix "key"
      keyrefName                = mkQName nsUri nsPrefix "keyref"
      -- attribute blacklist
      isAttrWithoutNamespaceUri = hasNameWith (null       . namespaceUri)
      idName                    = mkQName ""    ""       "id"
      -- irrelevant attributes for schema
      attributeFormDefaultName  = mkQName ""    ""       "attributeFormDefault"
      blockDefaultName          = mkQName ""    ""       "blockDefault"
      elementFormDefaultName    = mkQName ""    ""       "elementFormDefault"
      finalDefaultName          = mkQName ""    ""       "finalDefault"
      versionName               = mkQName ""    ""       "version"
      langName                  = mkQName ""    ""       "lang"
      -- irrelevant attributes for simpleType, complexType and element
      finalName                 = mkQName ""    ""       "final"
      -- irrelevant attributes for simpleType restrictions, complexType, element and attribute
      fixedName                 = mkQName ""    ""       "fixed"
      -- irrelevant attributes for complexType and element
      abstractName              = mkQName ""    ""       "abstract"
      blockName                 = mkQName ""    ""       "block"
      -- irrelevant attributes for element and attribute
      formName                  = mkQName ""    ""       "form"
      -- irrelevant attributes for element
      nillableName              = mkQName ""    ""       "nillable"
      substitutionGroupName     = mkQName ""    ""       "substitutionGroup"


-- Conversion between Schema and Schema'

toSchema :: XmlSchema' -> XmlSchema
toSchema s
  = toSchemaRec s [] empty empty empty empty empty empty
    where
    toSchemaRec (XmlSchema' tns [])                ins sts cts els grs ats ags
      = XmlSchema tns ins sts cts els grs ats ags
    toSchemaRec (XmlSchema' tns ((In incl)   :xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) (ins ++ [incl]) sts cts els grs ats ags
    toSchemaRec (XmlSchema' tns ((St (k, st)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins (insert k st sts) cts els grs ats ags
    toSchemaRec (XmlSchema' tns ((Ct (k, ct)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts (insert k ct cts) els grs ats ags
    toSchemaRec (XmlSchema' tns ((El el)     :xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts (insert (elemName (unElDef el)) el els) grs ats ags
    toSchemaRec (XmlSchema' tns ((Gr (k, gr)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els (insert k gr grs) ats ags
    toSchemaRec (XmlSchema' tns ((At at)     :xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els grs (insert (attrName (unAttrDef at)) at ats) ags
    toSchemaRec (XmlSchema' tns ((Ag (k, ag)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els grs ats (insert k ag ags)

fromSchema :: XmlSchema -> XmlSchema'
fromSchema s
  = XmlSchema' (sTargetNS s) $ concat [ins, sts, cts, els, grs, ats, ags]
    where
    ins = map In $ sIncludes s
    sts = map St $ toList $ sSimpleTypes s
    cts = map Ct $ toList $ sComplexTypes s
    els = map El $ elems  $ sElements s
    grs = map Gr $ toList $ sGroups s 
    ats = map At $ elems  $ sAttributes s
    ags = map Ag $ toList $ sAttributeGroups s

-- Pickler definitions

xpXmlSchema' :: PU XmlSchema'
xpXmlSchema'
  = xpSchemaElem "schema" $
    xpAddNSDecl nsPrefix nsUri $
    xpFilterSchema $
    xpWrap (\ (a, b) -> XmlSchema' a b , \ t -> (targetNS t, parts t)) $
    xpPair (xpOption $ xpAttr "targetNamespace" xpText) $
    xpList $ xpSchemaPart

xpSchemaPart :: PU XmlSchemaPart
xpSchemaPart
  = xpAlt tag ps
    where
    tag (In _) = 0
    tag (St _) = 1
    tag (Ct _) = 2
    tag (El _) = 3
    tag (Gr _) = 4
    tag (At _) = 5
    tag (Ag _) = 6
    ps = [ xpWrap (In, unIn) $ xpInclude
         , xpWrap (St, unSt) $ xpSchemaElem "simpleType"     $ xpPair (xpAttr "name" xpText) xpSimpleType
         , xpWrap (Ct, unCt) $ xpSchemaElem "complexType"    $ xpPair (xpAttr "name" xpText) xpComplexType
         , xpWrap (El, unEl) $ xpSchemaElem "element"        $ xpElement
         , xpWrap (Gr, unGr) $ xpSchemaElem "group"          $ xpPair (xpAttr "name" xpText) xpGroup
         , xpWrap (At, unAt) $ xpSchemaElem "attribute"      $ xpAttribute
         , xpWrap (Ag, unAg) $ xpSchemaElem "attributeGroup" $ xpPair (xpAttr "name" xpText) xpAttributeGroup
         ]

xpInclude :: PU Include
xpInclude
  = xpAlt tag ps
    where
    tag (Incl _)  = 0
    tag (Imp _)   = 1
    tag (Redef _) = 2
    ps = [ xpWrap (Incl,  unIncl)  $ xpSchemaElem "include"  $ xpAttr "schemaLocation" xpText
         , xpWrap (Imp,   unImp)   $ xpSchemaElem "import"   $ xpPair (xpAttr "schemaLocation" xpText) (xpAttr "namespace" xpText)
         , xpWrap (Redef, unRedef) $ xpSchemaElem "redefine" $ xpPair (xpAttr "schemaLocation" xpText) (xpList xpRedefinition)
         ]

xpRedefinition :: PU Redefinition
xpRedefinition
  = xpAlt tag ps
    where
    tag (RedefSt _) = 0
    tag (RedefCt _) = 1
    tag (RedefGr _) = 2
    tag (RedefAg _) = 3
    ps = [ xpWrap (RedefSt, unRedefSt) $ xpSchemaElem "simpleType"     $ xpPair (xpAttr "name" xpText) xpSimpleType
         , xpWrap (RedefCt, unRedefCt) $ xpSchemaElem "complexType"    $ xpPair (xpAttr "name" xpText) xpComplexType
         , xpWrap (RedefGr, unRedefGr) $ xpSchemaElem "group"          $ xpPair (xpAttr "name" xpText) xpGroup
         , xpWrap (RedefAg, unRedefAg) $ xpSchemaElem "attributeGroup" $ xpPair (xpAttr "name" xpText) xpAttributeGroup
         ]

xpSimpleType :: PU SimpleType
xpSimpleType
  = xpAlt tag ps
    where
    tag (Restr _) = 0
    tag (Lst _)   = 1
    tag (Un _)    = 2
    ps = [ xpWrap (Restr, unRestr) $ xpSchemaElem "restriction" $ xpSTRestriction
         , xpWrap (Lst,   unLst)   $ xpSchemaElem "list"        $ xpSTList
         , xpWrap (Un,    unUn)    $ xpSchemaElem "union"       $ xpSTUnion
         ]

xpSTRestriction :: PU STRestriction
xpSTRestriction
  = xpPair xpSimpleTypeRef $ xpList $ xpRestrAttr

xpSimpleTypeRef :: PU SimpleTypeRef
xpSimpleTypeRef
  = xpAlt tag ps
    where
    tag (BaseAttr _)        = 0
    tag (STRAnonymStDecl _) = 1
    ps = [ xpWrap (BaseAttr,        unBaseAttr)        $ xpAttr "base" xpText -- TODO: pickler der Fehlermeldung erzeugt, falls simpleType als Kind gefunden
         , xpWrap (STRAnonymStDecl, unSTRAnonymStDecl) $ xpSchemaElem "simpleType" $ xpSimpleType -- TODO: only works if first child?
         ]

xpRestrAttr :: PU RestrAttr
xpRestrAttr
  = xpAlt tag ps
    where
    tag (MinIncl _)        = 0
    tag (MaxIncl _)        = 1
    tag (MinExcl _)        = 2
    tag (MaxExcl _)        = 3
    tag (TotalDigits _)    = 4
    tag (FractionDigits _) = 5
    tag (Length _)         = 6
    tag (MinLength _)      = 7
    tag (MaxLength _)      = 8
    tag (Enumeration _)    = 9
    tag (Pattern _)        = 10
    tag (WhiteSpace _)     = 11
    ps = [ xpWrap (MinIncl,        unMinIncl)        $ xpSchemaElem "minInclusive"   $ xpAttr "value" xpText
         , xpWrap (MaxIncl,        unMaxIncl)        $ xpSchemaElem "maxInclusive"   $ xpAttr "value" xpText
         , xpWrap (MinExcl,        unMinExcl)        $ xpSchemaElem "minExclusive"   $ xpAttr "value" xpText
         , xpWrap (MaxExcl,        unMaxExcl)        $ xpSchemaElem "maxExclusive"   $ xpAttr "value" xpText
         , xpWrap (TotalDigits,    unTotalDigits)    $ xpSchemaElem "totalDigits"    $ xpAttr "value" xpText
         , xpWrap (FractionDigits, unFractionDigits) $ xpSchemaElem "fractionDigits" $ xpAttr "value" xpText
         , xpWrap (Length,         unLength)         $ xpSchemaElem "length"         $ xpAttr "value" xpText
         , xpWrap (MinLength,      unMinLength)      $ xpSchemaElem "minLength"      $ xpAttr "value" xpText
         , xpWrap (MaxLength,      unMaxLength)      $ xpSchemaElem "maxLength"      $ xpAttr "value" xpText
         , xpWrap (Enumeration,    unEnumeration)    $ xpSchemaElem "enumeration"    $ xpAttr "value" xpText
         , xpWrap (Pattern,        unPattern)        $ xpSchemaElem "pattern"        $ xpAttr "value" xpText
         , xpWrap (WhiteSpace,     unWhiteSpace)     $ xpSchemaElem "whiteSpace"     $ xpAttr "value" xpText
         ]

xpSTList :: PU STList
xpSTList
  = xpAlt tag ps
    where
    tag (ItemTypeAttr _)    = 0
    tag (STLAnonymStDecl _) = 1
    ps = [ xpWrap (ItemTypeAttr,    unItemTypeAttr)    $ xpAttr "itemType" xpText
         , xpWrap (STLAnonymStDecl, unSTLAnonymStDecl) $ xpSchemaElem "simpleType" $ xpSimpleType
         ]

xpSTUnion :: PU STUnion
xpSTUnion
  = xpWrap (\ (a, b) -> STUnion a b , \ t -> (memberTypes t, anonymDecls t)) $
    xpPair (xpOption $ xpAttr "memberTypes" xpText) $
    xpList $ xpSchemaElem "simpleType" $ xpSimpleType

xpComplexType :: PU ComplexType
xpComplexType
  = xpWrap (\ (a, b) -> ComplexType a b , \ t -> (ctMixed t, ctDef t)) $
    xpPair (xpOption $ xpAttr "mixed" xpText) xpCTDef

xpCTDef :: PU CTDef
xpCTDef
  = xpAlt tag ps
    where
    tag (SCont _) = 0
    tag (CCont _) = 1
    tag (NewCT _) = 2
    ps = [ xpWrap (SCont, unSCont) $ xpSchemaElem "simpleContent"  $ xpSimpleContent
         , xpWrap (CCont, unCCont) $ xpSchemaElem "complexContent" $ xpComplexContent
         , xpWrap (NewCT, unNewCT) $ xpCTModel
         ]

xpSimpleContent :: PU SimpleContent
xpSimpleContent
  = xpAlt tag ps
    where
    tag (SCExt _)   = 0
    tag (SCRestr _) = 1
    ps = [ xpWrap (SCExt,   unSCExt)   $ xpSchemaElem "extension"   $ xpPair (xpAttr "base" xpText) xpAttrList
         , xpWrap (SCRestr, unSCRestr) $ xpSchemaElem "restriction" $ xpPair xpSTRestriction xpAttrList 
         ]

xpComplexContent :: PU ComplexContent
xpComplexContent
  = xpWrap (\ (a, b) -> ComplexContent a b , \ t -> (ccMixed t, ccDef t)) $
    xpPair (xpOption $ xpAttr "mixed" xpText) xpCCDef

xpCCDef :: PU CCDef
xpCCDef
  = xpAlt tag ps
    where
    tag (CCExt _)   = 0
    tag (CCRestr _) = 1
    ps = [ xpWrap (CCExt,   unCCExt)   $ xpSchemaElem "extension"   $ xpPair (xpAttr "base" xpText) $ xpCTModel
         , xpWrap (CCRestr, unCCRestr) $ xpSchemaElem "restriction" $ xpPair (xpAttr "base" xpText) $ xpCTModel
         ]

xpCTModel :: PU CTModel
xpCTModel
  = xpPair (xpOption xpCTCompositor) xpAttrList -- TODO: only works if first child?

xpCTCompositor :: PU CTCompositor
xpCTCompositor
  = xpAlt tag ps
    where
    tag (CompGr _) = 0
    tag (CompAl _) = 1
    tag (CompCh _) = 2
    tag (CompSq _) = 3
    ps = [ xpWrap (CompGr, unCompGr) $ xpSchemaElem "group"    $ xpPair xpMinMaxOcc xpGroup
         , xpWrap (CompAl, unCompAl) $ xpSchemaElem "all"      $ xpPair xpMinMaxOcc xpAll
         , xpWrap (CompCh, unCompCh) $ xpSchemaElem "choice"   $ xpPair xpMinMaxOcc xpSchemaChoice
         , xpWrap (CompSq, unCompSq) $ xpSchemaElem "sequence" $ xpPair xpMinMaxOcc xpSequence
         ]

xpMinMaxOcc :: PU MinMaxOcc
xpMinMaxOcc
  = xpWrap (\ (a, b) -> MinMaxOcc a b , \ t -> (minOcc t, maxOcc t)) $
    xpPair (xpOption $ xpAttr "minOccurs" xpText) (xpOption $ xpAttr "maxOccurs" xpText)

xpAll :: PU All
xpAll
  = xpList $ xpSchemaElem "element" $ xpPair xpMinMaxOcc xpElement

xpSchemaChoice :: PU Choice
xpSchemaChoice
  = xpList xpChSeqContent

xpSequence :: PU Sequence
xpSequence
  = xpList xpChSeqContent

xpChSeqContent :: PU ChSeqContent
xpChSeqContent
  = xpAlt tag ps
    where
    tag (ChSeqEl _) = 0
    tag (ChSeqGr _) = 1
    tag (ChSeqCh _) = 2
    tag (ChSeqSq _) = 3
    tag (ChSeqAn _) = 4
    ps = [ xpWrap (ChSeqEl, unChSeqEl) $ xpSchemaElem "element"  $ xpPair xpMinMaxOcc xpElement
         , xpWrap (ChSeqGr, unChSeqGr) $ xpSchemaElem "group"    $ xpPair xpMinMaxOcc xpGroup
         , xpWrap (ChSeqCh, unChSeqCh) $ xpSchemaElem "choice"   $ xpPair xpMinMaxOcc xpSchemaChoice
         , xpWrap (ChSeqSq, unChSeqSq) $ xpSchemaElem "sequence" $ xpPair xpMinMaxOcc xpSequence
         , xpWrap (ChSeqAn, unChSeqAn) $ xpSchemaElem "any"      $ xpPair xpMinMaxOcc xpAny
         ]

xpAny :: PU Any
xpAny
  = xpWrap (\ (a, b) -> Any a b , \ t -> (namespace t, processContents t)) $
    xpPair (xpOption $ xpAttr "namespace" xpText) (xpOption $ xpAttr "processContents" xpText)

xpAttrList :: PU AttrList
xpAttrList
  = xpList $
    xpAlt tag ps
    where
    tag (Attr _)    = 0
    tag (AttrGrp _) = 1
    tag (AnyAttr _) = 2
    ps = [ xpWrap (Attr,    unAttr)    $ xpSchemaElem "attribute"      $ xpAttribute
         , xpWrap (AttrGrp, unAttrGrp) $ xpSchemaElem "attributeGroup" $ xpAttributeGroup
         , xpWrap (AnyAttr, unAnyAttr) $ xpSchemaElem "anyAttribute"   $ xpAny
         ]

xpElement :: PU Element
xpElement
  = xpAlt tag ps
    where
    tag (ElRef _) = 0
    tag (ElDef _) = 1
    ps = [ xpWrap (ElRef, unElRef) $ xpAttr "ref" xpText -- TODO: name, type etc. wegwerfen -> weiter arbeiten
         , xpWrap (ElDef, unElDef) $ xpElementDef
         ]

xpElementDef :: PU ElementDef
xpElementDef
  = xpWrap (\ (a, b, c) -> ElementDef a b c, \ t -> (elemName t, elemTypeDef t, elemDefaultVal t)) $
    xpTriple (xpAttr "name" xpText) xpElemTypeDef (xpOption $ xpAttr "default" xpText)

xpElemTypeDef :: PU ElemTypeDef
xpElemTypeDef
  = xpAlt tag ps
    where
    tag (ETDTypeAttr _)     = 0
    tag (ETDAnonymStDecl _) = 1
    tag (ETDAnonymCtDecl _) = 2
    ps = [ xpWrap (ETDTypeAttr,     unETDTypeAttr)     $ xpAttr "type" xpText
         , xpWrap (ETDAnonymStDecl, unETDAnonymStDecl) $ xpSchemaElem "simpleType"  $ xpSimpleType
         , xpWrap (ETDAnonymCtDecl, unETDAnonymCtDecl) $ xpSchemaElem "complexType" $ xpComplexType
         ]

xpGroup :: PU Group
xpGroup
  = xpAlt tag ps
    where
    tag (GrpRef _) = 0
    tag (GrpDef _) = 1
    ps = [ xpWrap (GrpRef, unGrpRef) $ xpAttr "ref" xpText
         , xpWrap (GrpDef, unGrpDef) $ xpOption $ xpGroupContDef
         ]

xpGroupContDef :: PU GroupContDef
xpGroupContDef
  = xpAlt tag ps
    where
    tag (Al _) = 0
    tag (Ch _) = 1
    tag (Sq _) = 2
    ps = [ xpWrap (Al, unAl) $ xpSchemaElem "all"      $ xpAll
         , xpWrap (Ch, unCh) $ xpSchemaElem "choice"   $ xpSchemaChoice
         , xpWrap (Sq, unSq) $ xpSchemaElem "sequence" $ xpSequence
         ]

xpAttribute :: PU Attribute
xpAttribute
  = xpAlt tag ps
    where
    tag (AttrRef _) = 0
    tag (AttrDef _) = 1
    ps = [ xpWrap (AttrRef, unAttrRef) $ xpAttr "ref" xpText
         , xpWrap (AttrDef, unAttrDef) $ xpAttributeDef
         ]

xpAttributeDef :: PU AttributeDef
xpAttributeDef
  = xpWrap (\ (a, b, c, d) -> AttributeDef a b c d, \ t -> (attrName t, attrTypeDef t, attrDefaultVal t, attrUse t)) $
    xp4Tuple (xpAttr "name" xpText) xpAttrTypeDef (xpOption $ xpAttr "default" xpText) (xpOption $ xpAttr "use" xpText)

xpAttrTypeDef :: PU AttrTypeDef
xpAttrTypeDef
  = xpAlt tag ps
    where
    tag (ATDTypeAttr _)   = 0
    tag (ATDAnonymDecl _) = 1
    ps = [ xpWrap (ATDTypeAttr,   unATDTypeAttr)   $ xpAttr "type" xpText
         , xpWrap (ATDAnonymDecl, unATDAnonymDecl) $ xpSchemaElem "simpleType" $ xpSimpleType
         ]

xpAttributeGroup :: PU AttributeGroup
xpAttributeGroup
  = xpAlt tag ps
    where
    tag (AttrGrpRef _) = 0
    tag (AttrGrpDef _) = 1
    ps = [ xpWrap (AttrGrpRef, unAttrGrpRef) $ xpAttr "ref" xpText
         , xpWrap (AttrGrpDef, unAttrGrpDef) $ xpAttrList
         ]

-- Load schema from given url

loadXmlSchema :: String -> IO XmlSchema
loadXmlSchema uri
  = do
    s' <- runX ( 
                xunpickleDocument xpXmlSchema'
                                  [ withValidate yes        -- validate source
                                  , withTrace 1             -- trace processing steps
                                  , withRemoveWS yes        -- remove redundant whitespace
                                  , withPreserveComment no  -- keep comments
                                  , withCheckNamespaces yes -- check namespaces
                                  , withCurl []             -- use libCurl for http access
                                  ] uri
               )
    s <- return $ toSchema $ head s' 
    resolveIncls s (sIncludes s) -- TODO: remove includes from list?
    -- TODO: further normalisation:
    --       default values of minOcc..
    --       apply namespaces (full qualified names)
    --       resolve anonymous types

resolveIncls :: XmlSchema -> Includes -> IO XmlSchema
resolveIncls s []     = return s
resolveIncls s (x:xs) = do
                        incl <- resolveIncl x
                        resolveIncls (mergeSchemata s incl) xs

-- TODO: It is no error, if the referenced schema cannot be loaded
-- TODO: Do not resolve the same include multiple times (allowed in XML Schema)

-- Include:
-- Inclusion of a schema for the same target namespace
-- (or which has no namespace -> conversion to including document's targetNamespace if it has one)

-- Import:
-- Nothing to do: Import of a schema for another target namespace 
-- (or which has no namespace -> conversion to including document's targetNamespace if it has one)

-- Redefine:
-- Same as include but apply redefinitions on referenced schema before merging

resolveIncl :: Include -> IO XmlSchema
resolveIncl (Incl loc)            = loadXmlSchema loc
resolveIncl (Imp (loc, _))        = loadXmlSchema loc
resolveIncl (Redef (loc, redefs)) = do
                                    s <- loadXmlSchema loc
                                    return $ applyRedefs s redefs

-- Assuming no name collisions in a valid xml schema file

applyRedefs :: XmlSchema -> Redefinitions -> XmlSchema
applyRedefs s []
  = s
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefSt (k, st)):xs)
  = applyRedefs (XmlSchema tns ins (insert k st sts) cts els grs ats ags) xs -- TODO: Apply redefinition to existing ST
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefCt (k, ct)):xs)
  = applyRedefs (XmlSchema tns ins sts (insert k ct cts) els grs ats ags) xs -- TODO: Apply redefinition to existing CT
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefGr (k, gr)):xs)
  = applyRedefs (XmlSchema tns ins sts cts els (insert k gr grs) ats ags) xs
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefAg (k, ag)):xs)
  = applyRedefs (XmlSchema tns ins sts cts els grs ats (insert k ag ags)) xs

mergeSchemata :: XmlSchema -> XmlSchema -> XmlSchema
mergeSchemata (XmlSchema tns _ sts cts els grs ats ags) (XmlSchema _ _ sts' cts' els' grs' ats' ags')
  = XmlSchema tns [] (union sts sts') (union cts cts') (union els els') (union grs grs') (union ats ats') (union ags ags')

-- Save schema to given target file

storeXmlSchema :: XmlSchema -> String -> IO ()
storeXmlSchema s t
  = do
    _ <- runX ( constA (fromSchema s)
                >>>
                xpickleDocument   xpXmlSchema'
                                  [ withIndent yes          -- indent generated xml
                                  ] t
              )
    return ()

-- Environment aufbauen:

-- createSValEnv :: Schema -> SValEnv
-- createSValEnv s
--  =

-- Testfunktionen anwenden:

-- testXmlTree :: XmlTree -> SVal Bool
-- testXmlTree
--   = do
--     env <- ask

-- Monad Transformer Test

type Env = Map String String

-- Schema Validation type
type SVal a = ReaderT Env (WriterT [String] Identity) a

runSVal :: Env -> SVal a -> (a, [String])
runSVal env val = runIdentity (runWriterT (runReaderT val env))

testSVal :: String -> SVal Bool
testSVal s
  = do
    env <- ask
    case lookup s env of
      Nothing  -> do
                  tell ["not in map: " ++ s]
                  return False
      Just val -> do
                  tell [val]
                  return True

-- Test Xml Processing with HXT

readDoc :: String -> IO XmlTree
readDoc uri
  = do
    s <- runX ( readDocument [ withValidate yes        -- validate source
                             , withTrace 1             -- trace processing steps
                             , withRemoveWS yes        -- remove redundant whitespace TODO: ??
                             , withPreserveComment no  -- keep comments               TODO: ??
                             -- , withCheckNamespaces yes -- check namespaces
                             , withCurl []             -- use libCurl for http access
                             ] uri
                >>>
                getChildren
              )
    return $ head s

selectFromTree :: XmlTree -> IOSArrow XmlTree a -> IO [a]
selectFromTree t arrow
  = do
    res <- runX ( constA t
                  >>>
                  arrow
              )
    return res

getNodeName :: XmlTree -> IO String
getNodeName t
  = do
    l <- selectFromTree t (isElem >>> getLocalPart)
    return $ if (null l)
             then ""
             else head l

getNodeAttrs :: XmlTree -> IO [(String, String)]
getNodeAttrs t
  = selectFromTree t (getAttrl >>> getName &&& (getChildren >>> getText))

getNodeChildren :: XmlTree -> IO XmlTrees
getNodeChildren t
  = selectFromTree t (getChildren >>> (isElem <+> isText))

-- Test setup

main :: IO ()
main
  = do
    t <- readDoc "example.xsd"

    n <- getNodeName t
    putStrLn $ n

    l <- getNodeAttrs t
    mapM_ (\ (a, b) -> putStrLn $ a ++ " = " ++ b) l

    c <- getNodeChildren t
    c' <- mapM getNodeName c

    mapM_ putStrLn c'

    let res = runSVal (fromList [("foo","bar"), ("hallo","welt")]) (testSVal "foo")
    putStrLn $ if (fst res) then "Klappt!" else "Fehler!"
    mapM_ putStrLn $ snd res

    -- putStrLn "\n--------------------------------------------- Pickling ---------------------------------------------\n"
    -- xmlschema <- loadXmlSchema "example.xsd"
    -- putStrLn "\n-------------------------------------------- RE Testing --------------------------------------------\n"
    -- putStrLn $ show $ schemaREs xmlschema
    -- putStrLn $ fst $ elementREs $ xmlschema $ Just $ lookup "quark" $ sElements xmlschema
    -- putStrLn $ fst $ elementREs $ xmlschema $ Just $ lookup "title" $ sElements xmlschema
    -- putStrLn $ fst $ elementREs $ xmlschema $ Just $ lookup "html"  $ sElements xmlschema
    -- putStrLn "\n------------------------------------------- Simple Types -------------------------------------------"
    -- putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sSimpleTypes xmlschema
    -- putStrLn "------------------------------------------- Complex Types ------------------------------------------"
    -- putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sComplexTypes xmlschema
    -- putStrLn "--------------------------------------------- Elements ---------------------------------------------"
    -- putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sElements xmlschema
    -- putStrLn "---------------------------------------------- Groups ----------------------------------------------"
    -- putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sGroups xmlschema
    -- putStrLn "-------------------------------------------- Attributes --------------------------------------------"
    -- putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sAttributes xmlschema
    -- putStrLn "------------------------------------------ AttributeGroups -----------------------------------------"
    -- putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sAttributeGroups xmlschema
    -- storeXmlSchema xmlschema "new-example.xsd"
    return ()

