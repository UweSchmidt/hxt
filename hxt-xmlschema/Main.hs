{-# LANGUAGE TypeSynonymInstances #-}

import Text.XML.HXT.Core

import Data.Map (Map, toList, empty, insert)

data XmlSchema      = XmlSchema
                    { sSimpleTypes  :: SimpleTypes
                    , sComplexTypes :: ComplexTypes
                    , sElements     :: Elements
                    , sIncludes     :: Includes
                    }
                    deriving (Show, Eq)

type SimpleTypes    = Map Name SimpleType
type ComplexTypes   = Map Name ComplexType
type Elements       = Map Name Element
type Includes       = [Include]
type Name           = String

type XmlSchema'     = [XmlSchemaPart]
data XmlSchemaPart  = St {unSt :: (Name, SimpleType)}
                    | Ct {unCt :: (Name, ComplexType)}
                    | El {unEl :: (Name, Element)}
                    | In {unIn :: Include}

data SimpleType     = Restr {unRestr :: Restriction}
                    | Lst   {unLst   :: List}
                    | Un    {unUn    :: Union}
                    deriving (Show, Eq)
type Restriction    = (Base, RestrAttrs)
type Base           = String
type RestrAttrs     = [RestrAttr]
data RestrAttr      = MinIncl        {unMinIncl        :: Value}
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
type Value          = String
-- type RestrFunc      = String -> Maybe String -- Nothing: true, Just x: error with message x
type List           = ItemType
type ItemType       = String
type Union          = MemberTypes
type MemberTypes    = String

type ComplexType    = [CTElems]
data CTElems        = Sq    {unSq    :: Sequence}
                    | Ch    {unCh    :: Choice}
                    | Al    {unAl    :: All}
                    | Attr  {unAttr  :: Attribute}
                    | CCont {unCCont :: ComplexContent}
                    deriving (Show, Eq)
type Sequence       = [Element]
type Choice         = [Element]
type All            = [Element]
type Attribute      = (Name, Type)
type Type           = String
data ComplexContent = CCExt   {unCCExt   :: CCExtension}
                    | CCRestr {unCCRestr :: CCRestriction}
                    deriving (Show, Eq)
type CCExtension    = (Base, Sequence)
type CCRestriction  = (Base, Sequence)

data Element        = ElRef {unElRef :: ElementRef}
                    | ElDef {unElDef :: ElementDef}
                    deriving (Show, Eq)
type ElementRef     = String
data ElementDef     = ElementDef
                    { name      :: Name
                    , elTypeDef :: ElTypeDef
                    , minOcc    :: Maybe String
                    , maxOcc    :: Maybe String
                    }
                    deriving (Show, Eq)
data ElTypeDef      = TRef  {unTRef  :: Name}
                    | CTDef {unCTDef :: ComplexType}
                    deriving (Show, Eq)

data Include        = Incl {unIncl :: String}
                    | Imp  {unImp  :: String}
                    | Redef {unRedef :: (String, XmlTrees)}
                    deriving (Show, Eq)

-- Conversion between Schema and Schema'

toSchema :: XmlSchema' -> XmlSchema
toSchema s
  = toSchemaRec s empty empty empty []
    where
    toSchemaRec []                sts cts els ins = XmlSchema { sSimpleTypes  = sts
                                                              , sComplexTypes = cts
                                                              , sElements     = els
                                                              , sIncludes     = ins
                                                              }
    toSchemaRec ((St (k, st)):xs) sts cts els ins = toSchemaRec xs (insert k st sts) cts els ins
    toSchemaRec ((Ct (k, ct)):xs) sts cts els ins = toSchemaRec xs sts (insert k ct cts) els ins
    toSchemaRec ((El (k, el)):xs) sts cts els ins = toSchemaRec xs sts cts (insert k el els) ins
    toSchemaRec ((In incl)   :xs) sts cts els ins = toSchemaRec xs sts cts els (ins ++ [incl]) -- keep ordering

fromSchema :: XmlSchema -> XmlSchema'
fromSchema s
  = concat [sts, cts, els, ins]
    where 
    sts = map St $ toList $ sSimpleTypes s
    cts = map Ct $ toList $ sComplexTypes s
    els = map El $ toList $ sElements s
    ins = map In $ sIncludes s

-- Pickler definitions

instance XmlPickler XmlSchema' where
  xpickle = xpXmlSchema'

xpXmlSchema' :: PU XmlSchema'
xpXmlSchema'
  = xpElem "xs:schema" $ xpList $ xpSchemaPart

xpSchemaPart :: PU XmlSchemaPart
xpSchemaPart
  = xpAlt tag ps
    where
    tag (St _) = 0
    tag (Ct _) = 1
    tag (El _) = 2
    tag (In _) = 3
    ps = [ xpWrap (St, unSt) $ xpElem "xs:simpleType"  $ xpPair (xpAttr "name" xpText) xpSimpleType
         , xpWrap (Ct, unCt) $ xpElem "xs:complexType" $ xpPair (xpAttr "name" xpText) xpComplexType
         , xpWrap (El, unEl) $ xpElem "xs:element"     $ xpPair (xpAttr "name" xpText) xpElement
         , xpWrap (In, unIn) $ xpInclude
         ]

xpSimpleType :: PU SimpleType
xpSimpleType
  = xpAlt tag ps
    where
    tag (Restr _) = 0
    tag (Lst _)   = 1
    tag (Un _)    = 2
    ps = [ xpWrap (Restr, unRestr) $ xpElem "xs:restriction" $ xpRestriction
         , xpWrap (Lst,   unLst)   $ xpElem "xs:list"        $ xpAttr "itemType" xpText
         , xpWrap (Un,    unUn)    $ xpElem "xs:union"       $ xpAttr "memberTypes" xpText
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
    ps = [ xpWrap (MinIncl,        unMinIncl)        $ xpElem "xs:minInclusive"   $ xpAttr "value" xpText
         , xpWrap (MaxIncl,        unMaxIncl)        $ xpElem "xs:maxInclusive"   $ xpAttr "value" xpText
         , xpWrap (MinExcl,        unMinExcl)        $ xpElem "xs:minExclusive"   $ xpAttr "value" xpText
         , xpWrap (MaxExcl,        unMaxExcl)        $ xpElem "xs:maxExclusive"   $ xpAttr "value" xpText
         , xpWrap (TotalDigits,    unTotalDigits)    $ xpElem "xs:totalDigits"    $ xpAttr "value" xpText
         , xpWrap (FractionDigits, unFractionDigits) $ xpElem "xs:fractionDigits" $ xpAttr "value" xpText
         , xpWrap (Enumeration,    unEnumeration)    $ xpElem "xs:enumeration"    $ xpAttr "value" xpText
         , xpWrap (Pattern,        unPattern)        $ xpElem "xs:pattern"        $ xpAttr "value" xpText
         , xpWrap (MinLength,      unMinLength)      $ xpElem "xs:minLength"      $ xpAttr "value" xpText
         , xpWrap (MaxLength,      unMaxLength)      $ xpElem "xs:maxLength"      $ xpAttr "value" xpText
         , xpWrap (Length,         unLength)         $ xpElem "xs:length"         $ xpAttr "value" xpText
         ]

xpComplexType :: PU ComplexType
xpComplexType
  = xpList $ 
    xpAlt tag ps
    where
    tag (Sq _)    = 0
    tag (Ch _)    = 1
    tag (Al _)    = 2
    tag (Attr _)  = 3
    tag (CCont _) = 4
    ps = [ xpWrap (Sq,    unSq)    $ xpElem "xs:sequence"       $ xpSequence
         , xpWrap (Ch,    unCh)    $ xpElem "xs:choice"         $ xpList $ xpElem "xs:element" $ xpElement
         , xpWrap (Al,    unAl)    $ xpElem "xs:all"            $ xpList $ xpElem "xs:element" $ xpElement
         , xpWrap (Attr,  unAttr)  $ xpElem "xs:attribute"      $ xpPair (xpAttr "name" xpText) (xpAttr "type" xpText)
         , xpWrap (CCont, unCCont) $ xpElem "xs:complexContent" $ xpComplexContent
         ]
xpSequence :: PU Sequence
xpSequence
  = xpList $ xpElem "xs:element" $ xpElement
xpComplexContent :: PU ComplexContent
xpComplexContent
  = xpAlt tag ps
    where
    tag (CCExt _)   = 0
    tag (CCRestr _) = 1
    ps = [ xpWrap (CCExt,   unCCExt)   $ xpElem "xs:extension"   $ xpPair (xpAttr "base" xpText) $ xpElem "xs:sequence" $ xpSequence
         , xpWrap (CCRestr, unCCRestr) $ xpElem "xs:restriction" $ xpPair (xpAttr "base" xpText) $ xpElem "xs:sequence" $ xpSequence
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
         , xpWrap (CTDef, unCTDef) $ xpElem "xs:complexType" $ xpComplexType
         ]

xpInclude :: PU Include
xpInclude
  = xpAlt tag ps
    where
    tag (Incl _)   = 0
    tag (Imp _)    = 1
    tag (Redef _)  = 2
    ps = [ xpElem "xs:include"  $ xpWrap (Incl,  unIncl)  $ xpAttr "schemaLocation" xpText
         , xpElem "xs:import"   $ xpWrap (Imp,   unImp)   $ xpAttr "namespace" xpText
         , xpElem "xs:redefine" $ xpWrap (Redef, unRedef) $ xpPair (xpAttr "schemaLocation" xpText) $ xpTrees
         ]

loadXmlSchema :: IO XmlSchema
loadXmlSchema
  = do
    s <- runX ( 
                xunpickleDocument xpXmlSchema'
                                  [ withValidate yes       -- validate source
                                  , withTrace 1            -- trace processing steps
                                  , withRemoveWS yes       -- remove redundant whitespace
                                  , withPreserveComment no -- keep comments
                                  ] "example.xsd"
              )
    return $ toSchema $ head s

storeXmlSchema :: XmlSchema -> IO ()
storeXmlSchema s
  = do
    _ <- runX ( constA (fromSchema s)
                >>>
                xpickleDocument   xpXmlSchema'
                                  [ withIndent yes         -- indent generated xml
                                  ] "new-example.xsd"
              )
    return ()

main :: IO ()
main
  = do
    putStrLn "\n--------------------------------------------- Pickling ---------------------------------------------\n"
    xmlschema <- loadXmlSchema
    putStrLn "\n------------------------------------------- Simple Types -------------------------------------------"
    putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sSimpleTypes xmlschema
    putStrLn "------------------------------------------- Complex Types ------------------------------------------"
    putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sComplexTypes xmlschema
    putStrLn "--------------------------------------------- Elements ---------------------------------------------"
    putStrLn $ concat $ map (\ (k, s) -> "\n" ++ k ++ ":\n" ++ (show s) ++ "\n") $ toList $ sElements xmlschema
    storeXmlSchema xmlschema
    return ()

