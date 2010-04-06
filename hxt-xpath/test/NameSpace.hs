module NameSpace
where

import Text.XML.HXT.Arrow
import Text.XML.HXT.XPath

simpleXml :: String
simpleXml = "<soap:Body xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\"/>"

wrongXml :: String
wrongXml = "<???>"

nsEnv :: [(String, String)]
nsEnv = [ ("s"    , "http://www.w3.org/2003/05/soap-envelope") ]

evalXPath :: String -> String -> [XmlTree]
evalXPath xpath xml =
  runLA ( xread
          >>>
          ( ( propagateNamespaces
              >>>
              getXPathTreesWithNsEnv nsEnv xpath
            )
            `whenNot` isError
          )
        ) xml

t1 = evalXPath "//s:Body"    simpleXml
t2 = evalXPath "//soap:Body" simpleXml
t3 = evalXPath "???"         simpleXml
t4 = evalXPath "//soap:Body" wrongXml
t5 = runLA xread wrongXml

