{-# LANGUAGE TypeSynonymInstances #-}

import Text.XML.HXT.Core

import Data.Map (Map, toList, keys, empty, insert)

data XmlSchema = XmlSchema
  { sSimpleTypes   :: SimpleTypes
  , sComplexTypes  :: ComplexTypes
  , sElements      :: Elements
  -- , sIncludes ...
  }
  deriving (Show, Eq)

type Name          = String

type SimpleTypes   = Map Name SimpleType
type ComplexTypes  = Map Name ComplexType
type Elements      = Map Name Element

type XmlSchema'    = [XmlSchemaPart]
data XmlSchemaPart = St {unSt :: (Name, SimpleType)}
                   | Ct {unCt :: (Name, ComplexType)}
                   | El {unEl :: (Name, Element)}

data SimpleType    = Restr {unRestr :: Restriction}
                   | Lst   {unLst   :: List}
                   | Un    {unUn    :: Union}
                   deriving (Show, Eq)

type Restriction   = (Base, RestrAttrs)
type Base          = String
type RestrAttrs    = [RestrAttr]
data RestrAttr     = MinIncl        {unMinIncl        :: Value}
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
type Value         = String
-- type RestrFunc     = String -> Maybe String -- Nothing: true, Just x: error with message x

type List          = XmlTrees
type Union         = XmlTrees

type ComplexType   = XmlTrees
type Element       = XmlTrees

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
         , xpWrap (Ct, unCt) $ xpElem "xs:complexType" $ xpPair (xpAttr "name" xpText) xpTrees
         , xpWrap (El, unEl) $ xpElem "xs:element"     $ xpPair (xpAttr "name" xpText) xpTrees
         ]

xpSimpleType :: PU SimpleType
xpSimpleType
  = xpAlt tag ps
    where
    tag (Restr _) = 0
    tag (Lst _)   = 1
    tag (Un _)    = 2
    ps = [ xpWrap (Restr, unRestr) $ xpElem "xs:restriction" $ xpRestriction
         , xpWrap (Lst,   unLst)   $ xpElem "xs:list"        $ xpTrees
         , xpWrap (Un,    unUn)    $ xpElem "xs:union"       $ xpTrees
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
         , xpWrap (Length,         unLength)         $ xpElem "xs:Length"         $ xpAttr "value" xpText
         ]

loadXmlSchema :: IO XmlSchema
loadXmlSchema
  = do
    s <- runX ( 
                xunpickleDocument xpXmlSchema'
                                  [ withValidate no        -- validate source
                                  , withTrace 1            -- trace processing steps
                                  , withRemoveWS yes       -- remove redundant whitespace
                                  , withPreserveComment no -- remove comments
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
    xmlschema <- loadXmlSchema
    putStrLn $ "SimpleTypes:\t"  ++ (concat $ map (\ (k, s) -> k ++ ":\n\t\t" ++ (show s) ++ "\n\n\t\t") $ toList  $ sSimpleTypes xmlschema)
    putStrLn $ "ComplexTypes:\t" ++ (concat $ map (\ s -> s ++ " ") $ keys  $ sComplexTypes xmlschema)
    putStrLn $ "Elements:\t"     ++ (concat $ map (\ s -> s ++ " ") $ keys  $ sElements xmlschema)
    storeXmlSchema xmlschema
    return ()

