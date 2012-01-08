{-# LANGUAGE TypeSynonymInstances #-}

import Text.XML.HXT.Core

import Data.Map (Map, toList, empty, insert)

data XmlSchema = XmlSchema
  { sSimpleTypes    :: SimpleTypes
  , sComplexTypes   :: ComplexTypes
  , sElements       :: Elements
  -- , sIncludes ...
  }
  deriving (Show, Eq)

type Name           = String

type SimpleTypes    = Map Name SimpleType
type ComplexTypes   = Map Name ComplexType
type Elements       = Map Name Element

type XmlSchema'     = [XmlSchemaPart]
data XmlSchemaPart  = St {unSt :: (Name, SimpleType)}
                    | Ct {unCt :: (Name, ComplexType)}
                    | El {unEl :: (Name, Element)}

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
                    | Length         {unLength         :: Value} -- TODO: more possible restriction attributes?
                    deriving (Show, Eq)
type Value          = String
-- type RestrFunc      = String -> Maybe String -- Nothing: true, Just x: error with message x

type List           = ItemType
type ItemType       = String

type Union          = MemberTypes
type MemberTypes    = String

type ComplexType    = [CTElems] -- mixed-attribute?
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
type CCRestriction  = (Base, Sequence) -- re-use Restriction type?

type Element        = XmlTrees -- Either (Name (Either Type ComplexType) (Maybe MinOcc) (Maybe MaxOcc)) Ref 

-- Conversion between Schema and Schema'

toSchema :: XmlSchema' -> XmlSchema
toSchema s
  = toSchemaRec s empty empty empty
    where
    toSchemaRec []                sts cts els = XmlSchema {sSimpleTypes  = sts,
                                                           sComplexTypes = cts,
                                                           sElements     = els}
    -- Error message if value already exists in map? Valid XML Schema input file?
    toSchemaRec ((St (k, st)):xs) sts cts els = toSchemaRec xs (insert k st sts) cts els
    toSchemaRec ((Ct (k, ct)):xs) sts cts els = toSchemaRec xs sts (insert k ct cts) els
    toSchemaRec ((El (k, el)):xs) sts cts els = toSchemaRec xs sts cts (insert k el els)

fromSchema :: XmlSchema -> XmlSchema'
fromSchema s
  = concat [sts, cts, els]
    where 
    sts = map St $ toList $ sSimpleTypes s
    cts = map Ct $ toList $ sComplexTypes s
    els = map El $ toList $ sElements s

-- Pickler definitions

instance XmlPickler XmlSchema' where
  xpickle = xpXmlSchema'

-- TODO: Namespace-Handling: xpElem "schema" + ns seperate test

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
    ps = [ xpWrap (St, unSt) $ xpElem "xs:simpleType"  $ xpPair (xpAttr "name" xpText) xpSimpleType
         , xpWrap (Ct, unCt) $ xpElem "xs:complexType" $ xpPair (xpAttr "name" xpText) xpComplexType
         , xpWrap (El, unEl) $ xpElem "xs:element"     $ xpPair (xpAttr "name" xpText) xpElement
         ]

xpSimpleType :: PU SimpleType
xpSimpleType
  = xpAlt tag ps
    where
    tag (Restr _) = 0
    tag (Lst _)   = 1
    tag (Un _)    = 2
    ps = [ xpWrap (Restr, unRestr) $ xpElem "xs:restriction" $ xpRestriction
         , xpWrap (Lst,   unLst)   $ xpElem "xs:list"        $ xpAttr "itemType" xpText    -- xpListType?
         , xpWrap (Un,    unUn)    $ xpElem "xs:union"       $ xpAttr "memberTypes" xpText -- xpUnionType?
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
  = xpTrees

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

