module NameSpace
where

import Text.XML.HXT.Arrow
import Text.XML.HXT.XPath

simpleXml :: String
simpleXml = "<soap:Body xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\"/>"

wrongXml :: String
wrongXml = "<???>"

nsEnv :: [(String, String)]
nsEnv = [ ("s"    , "http://www.w3.org/2003/05/soap-envelope")
        , ("soap" , "http://www.w3.org/2003/05/soap-envelope")
        , (""     , "the-default-namespace")
        ]

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

t1 = evalXPath "//s:Body"    simpleXml          -- o.k.
t2 = evalXPath "//soap:Body" simpleXml          -- o.k.
t3 = evalXPath "???"         simpleXml          -- syntax error in xpath
t4 = evalXPath "//xxx:Body"  simpleXml          -- syntax error in xpath: no namespace given for xxx
t5 = evalXPath "//soap:Body" wrongXml           -- syntax error in doc

t6 = runLA xread wrongXml

