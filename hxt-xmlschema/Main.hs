import Text.XML.HXT.Core hiding (getElemName, isElem, isText, getAttrName, getAttrValue, getText)
import Text.XML.HXT.Curl
import Text.XML.HXT.Arrow.XmlRegex

import Data.Tree.NTree.TypeDefs

import Data.Map (Map, lookup, fromList, toList, keys, elems, empty, insert, union)
-- import qualified Data.Map as M
-- M.lookup
import Prelude hiding (lookup)

import Data.List (partition)

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
                       { memberTypes :: Maybe String
                       , anonymDecls :: [SimpleType]
                       }
                       deriving (Show, Eq)

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
type SCExtension       = (Name, AttrList)
type SCRestriction     = (STRestriction, AttrList)

data ComplexContent    = ComplexContent
                       { ccMixed :: Maybe String
                       , ccDef   :: CCDef
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
                       , attrDefaultVal :: Maybe String -- sense?
                       , attrUse        :: Maybe String
                       }
                       deriving (Show, Eq)
data AttrTypeDef       = ATDTypeAttr   {unATDTypeAttr   :: Name}
                       | ATDAnonymDecl {unATDAnonymDecl :: SimpleType}
                       deriving (Show, Eq)

data AttributeGroup    = AttrGrpRef {unAttrGrpRef :: Name}
                       | AttrGrpDef {unAttrGrpDef :: AttrList}
                       deriving (Show, Eq)

knownW3CTypes :: Map String STTF
knownW3CTypes = fromList
  [ ("xs:string",             \ _ -> return True)
  , ("xs:normalizedString",   \ _ -> return True)
  , ("xs:token",              \ _ -> return True)
  , ("xs:language",           \ _ -> return True)
  , ("xs:NMTOKEN",            \ _ -> return True)
  , ("xs:NMTOKENS",           \ _ -> return True)
  , ("xs:Name",               \ _ -> return True)
  , ("xs:NCName",             \ _ -> return True)
  , ("xs:ID",                 \ _ -> return True)
  , ("xs:IDREF",              \ _ -> return True)
  , ("xs:IDREFS",             \ _ -> return True)
  , ("xs:ENTITY",             \ _ -> return True)
  , ("xs:ENTITIES",           \ _ -> return True)
  , ("xs:anyURI",             \ _ -> return True)
  , ("xs:QName",              \ _ -> return True)
  , ("xs:NOTATION",           \ _ -> return True)
  , ("xs:hexBinary",          \ _ -> return True)
  , ("xs:base64Binary",       \ _ -> return True)
  , ("xs:decimal",            \ _ -> return True)
  , ("xs:integer",            \ _ -> return True)
  , ("xs:nonPositiveInteger", \ _ -> return True)
  , ("xs:negativeInteger",    \ _ -> return True)
  , ("xs:nonNegativeInteger", \ _ -> return True)
  , ("xs:positiveInteger",    \ _ -> return True)
  , ("xs:long",               \ _ -> return True)
  , ("xs:int",                \ _ -> return True)
  , ("xs:short",              \ _ -> return True)
  , ("xs:byte",               \ _ -> return True)
  , ("xs:unsignedLong",       \ _ -> return True)
  , ("xs:unsignedInt",        \ _ -> return True)
  , ("xs:unsignedShort",      \ _ -> return True)
  , ("xs:unsignedByte",       \ _ -> return True)
  -- TODO: not implemented yet in DataTypeLibW3C
  , ("xs:boolean",            \ _ -> return True)
  , ("xs:float",              \ _ -> return True)
  , ("xs:double",             \ _ -> return True)
  , ("xs:time",               \ _ -> return True)
  , ("xs:duration",           \ _ -> return True)
  , ("xs:date",               \ _ -> return True)
  , ("xs:dateTime",           \ _ -> return True)
  , ("xs:gDay",               \ _ -> return True)
  , ("xs:gMonth",             \ _ -> return True)
  , ("xs:gMonthDay",          \ _ -> return True)
  , ("xs:gYear",              \ _ -> return True)
  , ("xs:gYearMonth",         \ _ -> return True)
  ]

-- Types for Validation

data ElemDesc = ElemDesc
              { errmsg       :: Maybe String 
              , attrMap      :: AttrMap
              , contentModel :: XmlRegex
              , subElemDesc  :: SubElemDesc
              , sttf         :: STTF
              }

type AttrMap = Map String AttrMapVal
type AttrMapVal = (Bool, STTF)
type SubElemDesc = Map String ElemDesc
type STTF = String -> SVal Bool

type CountingTable = Map String Int

type XSC a = ReaderT XmlSchema Identity a

runXSC :: XmlSchema -> XSC a -> a
runXSC schema xsc = runIdentity $ runReaderT xsc schema

-- Create Element Description for Validation

createRootDesc :: XSC ElemDesc -- TODO: Verify root element interpretation
createRootDesc
  = do
    s <- ask
    am' <- mapM createAttrMapEntry $ elems $ sAttributes s
    let am = fromList am'
    let cm = mkStar $ mkAlts $ map mkElemRE $ keys $ sElements s
    se' <- mapM createElemDesc $ elems $ sElements s
    let se = fromList $ zip (keys $ sElements s) se'
    tf <- mkNoTextSTTF 
    return $ ElemDesc Nothing am cm se tf

createAttrMapEntry :: Attribute -> XSC (Name, AttrMapVal)
createAttrMapEntry (AttrRef n)
  = do
    s <- ask
    case lookup n (sAttributes s) of
           Just a  -> createAttrMapEntry a
           Nothing -> do
                      errorSTTF <- mkErrorSTTF "attribute validation error: illegal attribute reference in schema file"
                      return (n, (False, errorSTTF))
createAttrMapEntry (AttrDef (AttributeDef n tdef _ use)) -- TODO: sense of (attrDefaultVal :: Maybe String) ?
  = do
    let req = case use of
                Nothing -> False -- default "optional"
                Just s  -> case s of
                             "required" -> True
                             _          -> False
    tf <- case tdef of
            ATDTypeAttr r   -> lookupSTTF r
            ATDAnonymDecl t -> stToSTTF t    
    return (n, (req, tf))

lookupSTTF :: Name -> XSC STTF
lookupSTTF n
  = do
    s <- ask
    case lookup n knownW3CTypes of
      Just tf -> return tf
      Nothing -> case lookup n (sSimpleTypes s) of
                   Nothing -> mkErrorSTTF "type validation error: illegal type reference in schema file"
                   Just t  -> stToSTTF t -- TODO: cache sttf? prevent infinite recursion?

mkNoTextSTTF :: XSC STTF
mkNoTextSTTF
  = return $ \ s -> do
                    env <- ask
                    if not $ null (unwords $ words s)
                      then do
                           tell [(xpath env, "no text allowed here.")]
                           return False
                      else return True

mkErrorSTTF :: String -> XSC STTF
mkErrorSTTF s
  = return $ \ _ -> do
                    env <- ask
                    tell [(xpath env, s)]
                    return False

rlistToSTTF :: RestrAttrs -> XSC STTF
rlistToSTTF _ = return $ \ _ -> return True -- TODO: implement restriction checks
-- MinIncl
-- MaxIncl
-- MinExcl
-- MaxExcl
-- TotalDigits
-- FractionDigits
-- Length
-- MinLength
-- MaxLength
-- Enumeration
-- Pattern
-- WhiteSpace

stToSTTF :: SimpleType -> XSC STTF
stToSTTF (Restr (tref, rlist)) = do
                                 baseTF  <- case tref of
                                              BaseAttr n        -> lookupSTTF n
                                              STRAnonymStDecl t -> stToSTTF t
                                 restrTF <- rlistToSTTF rlist
                                 return $ \ x -> do
                                                 baseCheck <- baseTF x
                                                 restrCheck <- restrTF x
                                                 return (baseCheck && restrCheck)
stToSTTF (Lst tref)            = do
                                 baseTF  <- case tref of
                                              ItemTypeAttr n    -> lookupSTTF n
                                              STLAnonymStDecl t -> stToSTTF t
                                 return $ \ x -> do
                                                 env <- ask
                                                 let (checks, _) = runSVal env $ mapM baseTF $ words x
                                                 if not (foldr (&&) True checks)
                                                   then do
                                                        tell [(xpath env, "value does not match list type")]
                                                        return False
                                                   else return True
stToSTTF (Un ts)               = do
                                 trefTFs <- case memberTypes ts of
                                              Nothing    -> return []
                                              Just trefs -> mapM lookupSTTF $ words trefs
                                 tdefTFs <- mapM stToSTTF $ anonymDecls ts
                                 return $ \ x -> do
                                                 env <- ask
                                                 let (checks, _) = runSVal env $ mapM (\ f -> f x) (trefTFs ++ tdefTFs)
                                                 if not (foldr (||) False checks)
                                                   then do
                                                        tell [(xpath env, "value does not match union type")]
                                                        return False
                                                   else return True

ctToElemDesc :: ComplexType -> XSC ElemDesc
ctToElemDesc ct
  = return $ ElemDesc Nothing empty mkTextRE empty (\ _ -> return True)





-- TODO: RE for empty ComplexType ?
-- default values of minmaxOcc..

createElemDesc :: Element -> XSC ElemDesc
createElemDesc (ElRef n)
  = do
    s <- ask
    case lookup n (sElements s) of
           Just e  -> createElemDesc e
           Nothing -> do
                      let msg = Just "element validation error: illegal element reference in schema file"
                      return $ ElemDesc msg empty mkUnit empty (\ _ -> return True)
createElemDesc (ElDef (ElementDef _ tdef _)) -- TODO: sense of (elemDefaultVal  :: Maybe String) ?
  = do
    s <- ask
    t <- case tdef of
           ETDTypeAttr r      -> case lookup r (sComplexTypes s) of
                                   Nothing  -> do
                                               tf <- lookupSTTF r
                                               return $ Left tf
                                   Just ctr -> return $ Right $ ctr
           ETDAnonymStDecl st -> do
                                 tf <- stToSTTF st
                                 return $ Left tf
           ETDAnonymCtDecl ct -> return $ Right ct
    case t of
      Left tf -> return $ ElemDesc Nothing empty mkTextRE empty tf 
      Right ct -> ctToElemDesc ct

---------------------------------------------------------

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
    = -- keep elems from xs namespace which are not blacklisted
      xpFilterCont (isXmlSchemaElem           >>> neg elementBlacklist) .
      -- keep attrs without namespace which are not blacklisted
      xpFilterAttr (isAttrWithoutNamespaceUri >>> neg attributeBlacklist)
      where
      -- element blacklist
      isXmlSchemaElem           = hasNameWith ((== nsUri) . namespaceUri)
      elementBlacklist          = foldr (<+>) none $ map hasQName $
                                  [ mkQName nsPrefix "annotation"           nsUri
                                  , mkQName nsPrefix "notation"             nsUri
                                  -- irrelevant content for element
                                  , mkQName nsPrefix "unique"               nsUri
                                  , mkQName nsPrefix "key"                  nsUri
                                  , mkQName nsPrefix "keyref"               nsUri
                                  ]
      -- attribute blacklist
      isAttrWithoutNamespaceUri = hasNameWith (null       . namespaceUri)
      attributeBlacklist        = foldr (<+>) none $ map hasQName $
                                  [ mkQName ""       "id"                   ""
                                  -- irrelevant attributes for schema
                                  , mkQName ""       "attributeFormDefault" ""
                                  , mkQName ""       "blockDefault"         ""
                                  , mkQName ""       "elementFormDefault"   ""
                                  , mkQName ""       "finalDefault"         ""
                                  , mkQName ""       "version"              ""
                                  , mkQName ""       "lang"                 ""
                                  -- irrelevant attributes for simpleType, complexType and element
                                  , mkQName ""       "final"                ""
                                  -- irrelevant attributes for simpleType restrictions, complexType, element and attribute
                                  , mkQName ""       "fixed"                ""
                                  -- irrelevant attributes for complexType and element
                                  , mkQName ""       "abstract"             ""
                                  , mkQName ""       "block"                ""
                                  -- irrelevant attributes for element and attribute
                                  , mkQName ""       "form"                 ""
                                  -- irrelevant attributes for element
                                  , mkQName ""       "nillable"             ""
                                  , mkQName ""       "substitutionGroup"    ""
                                  ]


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

-- fromSchema :: XmlSchema -> XmlSchema'
-- fromSchema s
--   = XmlSchema' (sTargetNS s) $ concat [ins, sts, cts, els, grs, ats, ags]
--     where
--     ins = map In $ sIncludes s
--     sts = map St $ toList $ sSimpleTypes s
--     cts = map Ct $ toList $ sComplexTypes s
--     els = map El $ elems  $ sElements s
--     grs = map Gr $ toList $ sGroups s 
--     ats = map At $ elems  $ sAttributes s
--     ags = map Ag $ toList $ sAttributeGroups s

-- Pickler definitions

xpXmlSchema' :: PU XmlSchema'
xpXmlSchema'
  = xpSchemaElem "schema" $
    -- xpAddNSDecl nsPrefix nsUri $
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
    ps = [ xpWrap (BaseAttr,        unBaseAttr)        $ xpAttr "base" xpText -- TODO: error msg and cleanup if simpleType
         , xpWrap (STRAnonymStDecl, unSTRAnonymStDecl) $ xpSchemaElem "simpleType" $ xpSimpleType -- TODO:what if not 1st child?
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
    ps = [ xpWrap (ElRef, unElRef) $ xpAttr "ref" xpText -- TODO: error msg and cleanup if more attributes (name, type ...)
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
                                  -- , withTrace 1             -- trace processing steps
                                  , withRemoveWS yes        -- remove redundant whitespace
                                  , withPreserveComment no  -- keep comments
                                  , withCheckNamespaces yes -- check namespaces
                                  , withCurl []             -- use libCurl for http access
                                  ] uri
               )
    s <- return $ toSchema $ head s' 
    resolveIncls s (sIncludes s)

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

-- storeXmlSchema :: XmlSchema -> String -> IO ()
-- storeXmlSchema s t
--   = do
--     _ <- runX ( constA (fromSchema s)
--                 >>>
--                 xpickleDocument   xpXmlSchema'
--                                   [ withIndent yes          -- indent generated xml
--                                   ] t
--               )
--     return ()

-- Validation

getReqAttrNames :: AttrMap -> [String]
getReqAttrNames m = map (\ (n, _) -> n) $ filter (\ (_, (req, _)) -> req) (toList m)

hasReqAttrs :: [String] -> [String] -> SVal Bool
hasReqAttrs [] _
  = return True
hasReqAttrs (x:xs) attrs
  = do
    env <- ask
    if x `notElem` attrs
      then do
           tell [((xpath env) ++ "/@" ++ x, "required attribute is missing.")]
           res <- hasReqAttrs xs attrs
           return (res && False)
      else hasReqAttrs xs attrs

checkAllowedAttrs :: [(String, String)] -> SVal Bool
checkAllowedAttrs []
  = return True
checkAllowedAttrs ((n, val):xs)
  = do
    env <- ask
    let m = attrMap $ elemDesc env
    res <- case lookup n m of
             Nothing      -> do
                             tell [((xpath env) ++ "/@" ++ n, "attribute not allowed here.")]
                             return False
             Just (_, tf) -> do
                             tfRes <- local (const (appendXPath ("/@" ++ n) env)) (tf val)
                             if not tfRes
                               then do
                                    tell [((xpath env) ++ "/@" ++ n, "value does not match type.")]
                                    return tfRes                                    
                               else return tfRes
    rest <- checkAllowedAttrs xs
    return (res && rest)

testAttrs :: XmlTree -> SVal Bool
testAttrs e
  = do
    env <- ask
    let attrl = getElemAttrs e
    allowedAttrsRes <- checkAllowedAttrs attrl
    reqAttrsRes <- hasReqAttrs (getReqAttrNames (attrMap $ elemDesc env)) (map fst attrl)
    return (allowedAttrsRes && reqAttrsRes)

testContentModel :: XmlTrees -> SVal Bool
testContentModel t
  = do
    env <- ask
    case matchXmlRegex (contentModel $ elemDesc env) t of
      Nothing -> return True
      Just _  -> do
                 tell [(xpath env ++ "/*", "elements do not match content model.")] -- TODO: regex message
                 return False

appendXPath :: String -> SValEnv -> SValEnv
appendXPath s env
  = SValEnv ((xpath env) ++ s) $ elemDesc env

newDesc :: ElemDesc -> SValEnv -> SValEnv
newDesc d env
  = SValEnv (xpath env) d

testElemChildren :: CountingTable -> XmlTrees -> SVal Bool
testElemChildren _ []
  = return True
testElemChildren t (x:xs)
  = do
    env <- ask
    let n = getElemName x
    let c = case lookup n t of
              Nothing -> 1
              Just v  -> v+1
    let elemXPath = "/" ++ n ++ "[" ++ (show c) ++ "]"
    let m = subElemDesc $ elemDesc env
    res <- case lookup n m of
             Nothing -> do
                        tell [((xpath env) ++ elemXPath, "element not allowed here.")]
                        return False
             Just d  -> local (const (appendXPath elemXPath (newDesc d env))) (testElem x)
    rest <- testElemChildren (insert n c t) xs
    return (res && rest)

testElemText :: XmlTrees -> SVal Bool
testElemText t
  = do
    env <- ask
    local (const (appendXPath "/child::text()" env)) $ (sttf $ elemDesc env) $ getCombinedText t

extractElems :: XmlTrees -> (XmlTrees, XmlTrees)
extractElems = partition isElem

testElem :: XmlTree -> SVal Bool
testElem e
  = do
    env <- ask
    case (errmsg $ elemDesc env) of
      Just msg -> do
                  tell [(xpath env, msg)]
                  return False
      Nothing  -> do
                  attrRes <- testAttrs e
                  let content = getElemChildren e -- Text and Tag nodes
                  contModelRes <- testContentModel content
                  let (tags, text) = extractElems content
                  textRes <- testElemText text
                  tagsRes <- testElemChildren empty tags
                  return (attrRes && contModelRes && textRes && tagsRes)

-- Main

main :: IO ()
main
  = do
    -- argv <- getArgs
    -- (schemaurl, docurl) <- return (argv!!0, argv!!1)

    xmlschema <- loadXmlSchema "example.xsd"
    doc <- readDoc "example.xml"
    let res = runSVal (SValEnv "" (runXSC xmlschema createRootDesc)) (testElem doc)
    if (fst res)
      then putStrLn $ "\nok.\n"
      else putStrLn $ "\nerrors were found:\n"
    mapM_ (\ (a, b) -> putStrLn $ a ++ "\n" ++ b ++ "\n") $ snd res

    -- Text.XML.HXT.XPath.XPathEval
    -- getXPath :: String -> XmlTree -> XmlTrees
    -- Postprocess: take XPath and add error msg as comment in document

    return ()

-- Schema Validation type

type SValLog = [(String, String)]

type SVal a = ReaderT SValEnv (WriterT SValLog Identity) a

runSVal :: SValEnv -> SVal a -> (a, SValLog)
runSVal env val = runIdentity $ runWriterT $ runReaderT val env

data SValEnv = SValEnv
             { xpath :: String
             , elemDesc :: ElemDesc
             }

-- XML Processing with HXT

readDoc :: String -> IO XmlTree
readDoc uri
  = do
    s <- runX ( readDocument [ withValidate yes        -- validate source
                             -- , withTrace 1             -- trace processing steps
                             , withRemoveWS yes        -- remove redundant whitespace -- TODO: necessary?
                             , withPreserveComment no  -- keep comments               -- TODO: necessary?
                             -- , withCheckNamespaces yes -- check namespaces
                             , withCurl []             -- use libCurl for http access
                             ] uri
                >>>
                getChildren
              )
    return $ head s

getElemName :: XmlTree -> String
getElemName (NTree (XTag n _) _) = localPart n
getElemName _                    = ""

getElemAttrs :: XmlTree -> [(String, String)]
getElemAttrs (NTree (XTag _ attrs) _) = map (\ x -> (getAttrName x, getAttrValue x)) attrs
getElemAttrs _                        = []

getElemChildren :: XmlTree -> XmlTrees
getElemChildren (NTree (XTag _ _) c) = filter isRelevant c
getElemChildren _                    = []

isElem :: XmlTree -> Bool
isElem (NTree (XTag _ _) _) = True
isElem _                    = False

isText :: XmlTree -> Bool -- TODO: Handle character entity references
isText (NTree (XText _) _) = True
isText _                   = False

isRelevant :: XmlTree -> Bool -- TODO: Handle character entity references
isRelevant (NTree (XTag _ _) _) = True
isRelevant (NTree (XText _) _)  = True
isRelevant _                    = False

getAttrName :: XmlTree -> String
getAttrName (NTree (XAttr n) _) = localPart n
getAttrName _                   = ""

getAttrValue :: XmlTree -> String
getAttrValue (NTree (XAttr _) c) = getCombinedText c
getAttrValue _                   = ""

getCombinedText :: XmlTrees -> String
getCombinedText t = concat $ map getText t

getText :: XmlTree -> String -- TODO: Handle character entity references
getText (NTree (XText t) _) = t
getText _                   = ""

mkTextRE :: XmlRegex
mkTextRE = mkPrim $ isText

mkElemRE :: String -> XmlRegex
mkElemRE s = mkPrim $ (== s) . getElemName

