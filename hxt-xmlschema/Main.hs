import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import Data.Map (Map, elems, toList, empty, insert)

data XmlSchema       = XmlSchema
                     { sTargetNS        :: Maybe Namespace
                     , sIncludes        :: Includes 
                     , sSimpleTypes     :: SimpleTypes
                     , sComplexTypes    :: ComplexTypes
                     , sElements        :: Elements
                     , sGroups          :: Groups
                     , sAttributes      :: Attributes
                     , sAttributeGroups :: AttributeGroups
                     }
                     deriving (Show, Eq)

type Namespace       = String
type Includes        = [Include]
type SimpleTypes     = Map Name SimpleType
type ComplexTypes    = Map Name ComplexType
type Elements        = Map Name Element
type Groups          = Map Name Group
type Attributes      = Map Name Attribute
type AttributeGroups = Map Name AttributeGroups
type Name            = String

data XmlSchema'      = XmlSchema'
                     { targetNS :: Maybe Namespace
                     , parts    :: [XmlSchemaPart]
                     }
data XmlSchemaPart   = In {unIn :: Include}
                     | St {unSt :: (Name, SimpleType)}
                     | Ct {unCt :: (Name, ComplexType)}
                     | El {unEl :: Element}
                     | Gr {unGr :: (Name, Group)}
                     | At {unAt :: Attribute}
                     | Ag {unAg :: (Name, AttributeGroup)}

data Include         = Incl  {unIncl  :: Location}
                     | Imp   {unImp   :: (Location, Namespace)}
                     | Redef {unRedef :: (Location, Redefinitions)}
                     deriving (Show, Eq)
type Location        = String
type Redefinitions   = [Redefinition]
data Redefinition    = StRestr   {unStRestr   :: Restriction}
                     | CtSCExt   {unCtSCExt   :: TODO:}
                     | CtSCRestr {unCtSCRestr :: TODO:}
                     | CtCCExt   {unCtCCExt   :: TODO:}
                     | CtCCRestr {unCtCCRestr :: TODO:}
                     | Grp       {unGrp       :: TODO:}
                     | AttrGrp   {unAttrGrp   :: TODO:}

data SimpleType      = Restr {unRestr :: Restriction}
                     | Lst   {unLst   :: List}
                     | Un    {unUn    :: Union}
                     deriving (Show, Eq)
type Restriction     = (Base, RestrAttrs)
type Base            = String
type RestrAttrs      = [RestrAttr]
data RestrAttr       = MinIncl        {unMinIncl        :: Value}
                     | MaxIncl        {unMaxIncl        :: Value}
                     | MinExcl        {unMinExcl        :: Value}
                     | MaxExcl        {unMaxExcl        :: Value}
                     | TotalDigits    {unTotalDigits    :: Value}
                     | FractionDigits {unFractionDigits :: Value}
                     | Enumeration    {unEnumeration    :: Value}
                     | Pattern        {unPattern        :: Value}
                     | MinLength      {unMinLength      :: Value}
                     | MaxLength      {unMaxLength      :: Value}
                     | Length         {unLength         :: Value}
                     deriving (Show, Eq)
type Value           = String
-- type RestrFunc       = String -> Maybe String -- Nothing: true, Just x: error with message x
type List            = ItemType
type ItemType        = String
type Union           = MemberTypes
type MemberTypes     = String

data ComplexType     = ComplexType
                     { mixed   :: Maybe String
                     , ctelems :: [CTElems]
                     }
                     deriving (Show, Eq)
data CTElems         = Sq    {unSq    :: Sequence}
                     | Ch    {unCh    :: Choice}
                     | Al    {unAl    :: All}
                     | Attr  {unAttr  :: Attribute}
                     | SCont {unSCont :: SimpleContent}
                     | CCont {unCCont :: ComplexContent}
                     deriving (Show, Eq)
type Sequence        = [Element]
type Choice          = [Element]
type All             = [Element]
type Attribute       = (Name, Type)
type Type            = String
data SimpleContent   = SCExt   {unSCExt   :: SCExtension}
                     | SCRestr {unSCRestr :: SCRestriction}
                     deriving (Show, Eq)
type SCExtension     = 
type SCRestriction   = 
data ComplexContent  = CCExt   {unCCExt   :: CCExtension}
                     | CCRestr {unCCRestr :: CCRestriction}
                     deriving (Show, Eq)
type CCExtension     = (Base, Sequence)
type CCRestriction   = (Base, Sequence)

data Element         = ElRef {unElRef :: ElementRef}
                     | ElDef {unElDef :: ElementDef}
                     deriving (Show, Eq)
type ElementRef      = String
data ElementDef      = ElementDef
                     { name      :: Name
                     , elTypeDef :: ElTypeDef
                     , minOcc    :: Maybe String
                     , maxOcc    :: Maybe String
                     }
                     deriving (Show, Eq)
data ElTypeDef       = TRef  {unTRef  :: Name}
                     | CTDef {unCTDef :: ComplexType}
                     deriving (Show, Eq)

data Group           = TODO:
data Attribute       = TODO: @see type Attribute
data AttributeGroup  = 

-- Namespace handling

nsPrefix :: String
nsPrefix = "xs"

nsUri :: String
nsUri    = "http://www.w3.org/2001/XMLSchema"

xpElem' :: String -> PU a -> PU a
xpElem' = xpElemNS nsUri nsPrefix

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
      = toSchemaRec (XmlSchema' tns xs) ins sts cts (insert (name (unElDef el)) el els) grs ats ags
    toSchemaRec (XmlSchema' tns ((Gr (k, gr)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els (insert k gr grs) ats ags
    toSchemaRec (XmlSchema' tns ((At at)     :xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els grs (insert (name (unAttrDef at)) at ats) ags
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
  = xpElem' "schema" $ xpAddNSDecl nsPrefix nsUri $ 
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
         , xpWrap (St, unSt) $ xpElem' "simpleType"     $ xpPair (xpAttr "name" xpText) xpSimpleType
         , xpWrap (Ct, unCt) $ xpElem' "complexType"    $ xpPair (xpAttr "name" xpText) xpComplexType
         , xpWrap (El, unEl) $ xpElem' "element"        $ xpElement
         , xpWrap (Gr, unGr) $ xpElem' "group"          $ xpPair (xpAttr "name" xpText) xpGroup
         , xpWrap (At, unAt) $ xpElem' "attribute"      $ xpAttribute
         , xpWrap (Ag, unAg) $ xpElem' "attributeGroup" $ xpPair (xpAttr "name" xpText) xpAttributeGroup
         ]

xpInclude :: PU Include
xpInclude
  = xpAlt tag ps
    where
    tag (Incl _)  = 0
    tag (Imp _)   = 1
    tag (Redef _) = 2
    ps = [ xpElem' "include"  $ xpWrap (Incl,  unIncl)  $ xpAttr "schemaLocation" xpText
         , xpElem' "import"   $ xpWrap (Imp,   unImp)   $ xpPair (xpAttr "schemaLocation" xpText) (xpAttr "namespace" xpText)
         , xpElem' "redefine" $ xpWrap (Redef, unRedef) $ xpPair (xpAttr "schemaLocation" xpText) (xpList xpRedefinition)
         ]
xpRedefinition :: PU Redefinition
xpRedefinition
  = xpAlt tag ps
    where
    tag (StRestr _)   = 0
    tag (CtSCExt _)   = 1
    tag (CtSCRestr _) = 2
    tag (CtCCExt _)   = 3
    tag (CtCCRestr _) = 4
    tag (Grp _)       = 5
    tag (AttrGrp _)   = 6
    ps = [ xpElem' "simpleType"     $ xpElem' "restriction"    $ xpWrap (StRestr,  unStRestr) $ xpRestriction
         , xpElem' "complexType"    $ xpElem' "simpleContent"  $ xpElem' "extension" $ TODO:
         , xpElem' "complexType"    $ xpElem' "simpleContent"  $ xpElem' "restriction" $ TODO:
         , xpElem' "complexType"    $ xpElem' "complexContent" $ xpElem' "extension" $ TODO:
         , xpElem' "complexType"    $ xpElem' "complexContent" $ xpElem' "restriction" $ TODO:
         , xpElem' "group"          $ TODO:
         , xpElem' "attributeGroup" $ TODO:
         ]

xpSimpleType :: PU SimpleType
xpSimpleType
  = xpAlt tag ps
    where
    tag (Restr _) = 0
    tag (Lst _)   = 1
    tag (Un _)    = 2
    ps = [ xpWrap (Restr, unRestr) $ xpElem' "restriction" $ xpRestriction
         , xpWrap (Lst,   unLst)   $ xpElem' "list"        $ xpAttr "itemType" xpText
         , xpWrap (Un,    unUn)    $ xpElem' "union"       $ xpAttr "memberTypes" xpText
         ]
xpRestriction :: PU Restriction
xpRestriction
  = xpPair (xpAttr "base" xpText) $ xpList $ xpRestrAttr
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
    tag (Enumeration _)    = 6
    tag (Pattern _)        = 7
    tag (MinLength _)      = 8
    tag (MaxLength _)      = 9
    tag (Length _)         = 10
    ps = [ xpWrap (MinIncl,        unMinIncl)        $ xpElem' "minInclusive"   $ xpAttr "value" xpText
         , xpWrap (MaxIncl,        unMaxIncl)        $ xpElem' "maxInclusive"   $ xpAttr "value" xpText
         , xpWrap (MinExcl,        unMinExcl)        $ xpElem' "minExclusive"   $ xpAttr "value" xpText
         , xpWrap (MaxExcl,        unMaxExcl)        $ xpElem' "maxExclusive"   $ xpAttr "value" xpText
         , xpWrap (TotalDigits,    unTotalDigits)    $ xpElem' "totalDigits"    $ xpAttr "value" xpText
         , xpWrap (FractionDigits, unFractionDigits) $ xpElem' "fractionDigits" $ xpAttr "value" xpText
         , xpWrap (Enumeration,    unEnumeration)    $ xpElem' "enumeration"    $ xpAttr "value" xpText
         , xpWrap (Pattern,        unPattern)        $ xpElem' "pattern"        $ xpAttr "value" xpText
         , xpWrap (MinLength,      unMinLength)      $ xpElem' "minLength"      $ xpAttr "value" xpText
         , xpWrap (MaxLength,      unMaxLength)      $ xpElem' "maxLength"      $ xpAttr "value" xpText
         , xpWrap (Length,         unLength)         $ xpElem' "length"         $ xpAttr "value" xpText
         ]

xpComplexType :: PU ComplexType
xpComplexType
  = xpWrap (\ (a, b) -> ComplexType a b , \ t -> (mixed t, ctelems t)) $
    xpPair (xpOption $ xpAttr "mixed" xpText) $
    xpList $ 
    xpAlt tag ps
    where
    tag (Sq _)    = 0
    tag (Ch _)    = 1
    tag (Al _)    = 2
    tag (Attr _)  = 3
    tag (SCont _) = 4
    tag (CCont _) = 5
    ps = [ xpWrap (Sq,    unSq)    $ xpElem' "sequence"       $ xpSequence
         , xpWrap (Ch,    unCh)    $ xpElem' "choice"         $ xpList $ xpElem' "element" $ xpElement
         , xpWrap (Al,    unAl)    $ xpElem' "all"            $ xpList $ xpElem' "element" $ xpElement
         , xpWrap (Attr,  unAttr)  $ xpElem' "attribute"      $ xpPair (xpAttr "name" xpText) (xpAttr "type" xpText)
         , xpWrap (SCont, unSCont) $ xpElem' "simpleContent"  $ xpSimpleContent
         , xpWrap (CCont, unCCont) $ xpElem' "complexContent" $ xpComplexContent
         ]
xpSequence :: PU Sequence
xpSequence
  = xpList $ xpElem' "element" $ xpElement
xpSimpleContent :: PU SimpleContent
xpSimpleContent
  = xpAlt tag ps
    where
    tag (SCExt _)   = 0
    tag (SCRestr _) = 1
    ps = [ xpWrap (SCExt,   unSCExt)   $ xpElem' "extension"   $ xpPair (xpAttr "base" xpText) $ TODO:
         , xpWrap (SCRestr, unSCRestr) $ xpElem' "restriction" $ xpPair (xpAttr "base" xpText) $ TODO:
         ]
xpComplexContent :: PU ComplexContent
xpComplexContent
  = xpAlt tag ps
    where
    tag (CCExt _)   = 0
    tag (CCRestr _) = 1
    ps = [ xpWrap (CCExt,   unCCExt)   $ xpElem' "extension"   $ xpPair (xpAttr "base" xpText) $ xpElem' "sequence" $ xpSequence
         , xpWrap (CCRestr, unCCRestr) $ xpElem' "restriction" $ xpPair (xpAttr "base" xpText) $ xpElem' "sequence" $ xpSequence
         ]

xpElement :: PU Element
xpElement
  = xpAlt tag ps
    where
    tag (ElRef _) = 0
    tag (ElDef _) = 1
    ps = [ xpWrap (ElRef, unElRef) $ xpAttr "ref" xpText
         , xpWrap (ElDef, unElDef) $ xpElementDef
         ]
xpElementDef :: PU ElementDef
xpElementDef
  = xpWrap (\ (a, b, c, d) -> ElementDef a b c d, \ t -> (name t, elTypeDef t, minOcc t, maxOcc t)) $
    xp4Tuple (xpAttr "name" xpText) (xpElTypeDef) (xpOption $ xpAttr "minOccurs" xpText) (xpOption $ xpAttr "maxOccurs" xpText)
xpElTypeDef :: PU ElTypeDef
xpElTypeDef
  = xpAlt tag ps
    where
    tag (TRef _)  = 0
    tag (CTDef _) = 1
    ps = [ xpWrap (TRef,  unTRef)  $ xpAttr "type" xpText
         , xpWrap (CTDef, unCTDef) $ xpElem' "complexType" $ xpComplexType
         ]

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
    s <- return $ toSchema $ head s' -- TODO: remove includes from list? get includes inside resolveIncls
    resolveIncls s $ sIncludes s

resolveIncls :: XmlSchema -> Includes -> IO XmlSchema
resolveIncls s []     = return s
resolveIncls s (x:xs) 
  = do
    incl <- resolveIncl x
    resolveIncls (mergeSchemata s incl) xs

-- TODO: It is no error, if the referenced schema cannot be loaded
-- TODO: Not resolving the same include multiple times (allowed in XML Schema)
resolveIncl :: Include -> IO XmlSchema
resolveIncl (Incl loc)       = loadXmlSchema loc -- Inclusion of a schema for the same target namespace (or which has no namespace -> conversion to including document's targetNamespace if it has one)
resolveIncl (Imp (loc, _))   = loadXmlSchema loc -- Nothing to do: Import of a schema for another target namespace (or which has no namespace -> conversion to including document's targetNamespace if it has one)
resolveIncl (Redef (loc, _)) = loadXmlSchema loc -- Same as include but TODO: apply redefinitions
-- It's not an error to have two elements in the same symbol space, provided they have the same type.
-- However, if they have a different type then it is an error, i.e., name collision.

mergeSchemata :: XmlSchema -> XmlSchema -> XmlSchema
mergeSchemata a _ = a -- TODO: implement merge rules

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

main :: IO ()
main
  = do
    putStrLn "\n--------------------------------------------- Pickling ---------------------------------------------\n"
    xmlschema <- loadXmlSchema "example.xsd"
    putStrLn "\n------------------------------------------- Simple Types -------------------------------------------"
    putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sSimpleTypes xmlschema
    putStrLn "------------------------------------------- Complex Types ------------------------------------------"
    putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sComplexTypes xmlschema
    putStrLn "--------------------------------------------- Elements ---------------------------------------------"
    putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sElements xmlschema
    storeXmlSchema xmlschema "new-example.xsd"
    return ()

