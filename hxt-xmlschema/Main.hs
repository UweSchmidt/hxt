{-# LANGUAGE TypeSynonymInstances #-}

import Text.XML.HXT.Core

import Data.Map (Map, fromList, toList, keys, empty, insert)

data XmlSchema = XmlSchema
  { sSimpleTypes   :: SimpleTypes
  , sComplexTypes  :: ComplexTypes
  , sElements      :: Elements
  -- , sIncludes ...
  }
  deriving (Show, Eq)

type SimpleTypes   = Map String SimpleType
type ComplexTypes  = Map String ComplexType
type Elements      = Map String Element

type XmlSchema'    = [XmlSchemaPart]
type XmlSchemaPart = (String, Part)
data Part          = St SimpleType
                   | Ct ComplexType
                   | El Element

type SimpleType    = XmlTree
type ComplexType   = XmlTree
type Element       = XmlTree

-- Conversion between XmlSchemaPart and (String, SimpleType) etc.

toSchemaPartSt :: (String, SimpleType) -> (String, Part)
toSchemaPartSt (s, st) = (s, St st)

toSchemaPartCt :: (String, ComplexType) -> (String, Part)
toSchemaPartCt (s, ct) = (s, Ct ct)

toSchemaPartEl :: (String, Element) -> (String, Part)
toSchemaPartEl (s, el) = (s, El el)

fromSchemaPartSt :: (String, Part) -> (String, SimpleType)
fromSchemaPartSt (s, St st) = (s, st)
-- Handling of Ct and El?

fromSchemaPartCt :: (String, Part) -> (String, ComplexType)
fromSchemaPartCt (s, Ct ct) = (s, ct)
-- Handling of St and El?

fromSchemaPartEl :: (String, Part) -> (String, Element)
fromSchemaPartEl (s, El el) = (s, el)
-- Handling of St and Ct?

-- Conversion between Schema and Schema'

toSchema :: XmlSchema' -> XmlSchema
toSchema s
  = toSchemaRec s empty empty empty
    where
    toSchemaRec []              sts cts els = XmlSchema {sSimpleTypes = sts,
                                                         sComplexTypes = cts,
                                                         sElements = els}
    -- Error message if value already exists in map?
    toSchemaRec ((k, St st):xs) sts cts els = toSchemaRec xs (insert k st sts) cts els
    toSchemaRec ((k, Ct ct):xs) sts cts els = toSchemaRec xs sts (insert k ct cts) els
    toSchemaRec ((k, El el):xs) sts cts els = toSchemaRec xs sts cts (insert k el els)

fromSchema :: XmlSchema -> XmlSchema'
fromSchema s
  = concat [sts, cts, els]
    where 
    sts = map toSchemaPartSt $ toList $ sSimpleTypes s
    cts = map toSchemaPartCt $ toList $ sComplexTypes s
    els = map toSchemaPartEl $ toList $ sElements s

-- Pickler definitions

instance XmlPickler XmlSchema' where
  xpickle = xpXmlSchema'

xpXmlSchema' :: PU XmlSchema'
xpXmlSchema'
  = xpElem "xs:schema" $ -- xpElem "schema" $ + ns einzeln testen
    xpList $ xpSchemaPart

xpSchemaPart :: PU XmlSchemaPart
xpSchemaPart
  = xpAlt tag ps
    where
    tag (_, St _) = 0
    tag (_, Ct _) = 1
    tag (_, El _) = 2
    ps = [ xpWrap (toSchemaPartSt, fromSchemaPartSt) $ xpElem "xs:simpleType"  $ xpPair (xpAttr "name" xpText) xpTree
         , xpWrap (toSchemaPartCt, fromSchemaPartCt) $ xpElem "xs:complexType" $ xpPair (xpAttr "name" xpText) xpTree
         , xpWrap (toSchemaPartEl, fromSchemaPartEl) $ xpElem "xs:element"     $ xpPair (xpAttr "name" xpText) xpTree
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
    putStrLn $ "SimpleTypes:\t" ++ (concat $ map (\ s -> s ++ " ") $ keys (sSimpleTypes xmlschema))
    putStrLn $ "ComplexTypes:\t" ++ (concat $ map (\ s -> s ++ " ") $ keys (sComplexTypes xmlschema))
    putStrLn $ "Elements:\t" ++ (concat $ map (\ s -> s ++ " ") $ keys (sElements xmlschema))
    storeXmlSchema xmlschema
    return ()

