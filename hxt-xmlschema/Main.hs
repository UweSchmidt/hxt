import Text.XML.HXT.Core

import Data.Map (Map, fromList, toList, keys)

data XmlSchema = XmlSchema
  { sSimpleTypes  :: SimpleTypes
  , sComplexTypes :: ComplexTypes
  , sElements     :: Elements
  }
  deriving (Show, Eq)

type SimpleTypes  = Map String SimpleType
type ComplexTypes = Map String ComplexType
type Elements     = Map String Element

type SimpleType   = XmlTree
type ComplexType  = XmlTree
type Element      = XmlTree

instance XmlPickler XmlSchema where
  xpickle = xpXmlSchema

-- root pickler for root node
xpXmlSchema :: PU XmlSchema
xpXmlSchema
  = xpElem "xs:schema" $
    xpWrap ( uncurry3 XmlSchema
           , \ s -> (sSimpleTypes s, sComplexTypes s, sElements s) ) $
    xpTriple xpSimpleTypes xpComplexTypes xpElements

xpSimpleTypes :: PU SimpleTypes
xpSimpleTypes
  = xpWrap ( fromList
           , toList
           ) $
    xpList $
    xpElem "xs:simpleType" $
    xpPair (xpAttr "name" xpText) xpTree

xpComplexTypes :: PU SimpleTypes
xpComplexTypes
  = xpWrap ( fromList
           , toList
           ) $
    xpList $
    xpElem "xs:complexType" $
    xpPair (xpAttr "name" xpText) xpTree

xpElements :: PU Elements
xpElements
  = xpWrap ( fromList
           , toList
           ) $
    xpList $
    xpElem "xs:element" $
    xpPair (xpAttr "name" xpText) xpTree

loadXmlSchema :: IO XmlSchema
loadXmlSchema
  = do
    s <- runX ( 
                xunpickleDocument xpXmlSchema
                                  [ withValidate no        -- validate source
                                  , withTrace 1            -- trace processing steps
                                  , withRemoveWS yes       -- remove redundant whitespace
                                  , withPreserveComment no -- remove comments
                                  ] "example.xsd"
              )
    return $ head s

storeXmlSchema :: XmlSchema -> IO ()
storeXmlSchema s
  = do
    _ <- runX ( constA s
                >>>
                xpickleDocument   xpXmlSchema
                                  [ withIndent yes         -- indent generated xml
                                  ] "new-example.xsd"
              )
    return ()

main :: IO ()
main
  = do
    xmlschema <- loadXmlSchema
    putStrLn $ concat $ map (\ s -> s ++ " ") $ keys (sSimpleTypes xmlschema)
    putStrLn $ concat $ map (\ s -> s ++ " ") $ keys (sComplexTypes xmlschema)
    putStrLn $ concat $ map (\ s -> s ++ " ") $ keys (sElements xmlschema)
    storeXmlSchema xmlschema
    return ()

