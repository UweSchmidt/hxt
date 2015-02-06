-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XmlSchema.SystemConfig
   Copyright  : Copyright (C) 2012 Uwe Schmidt, Thorben Guelck
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   This helper module exports elements from the basic XML schema libraries:
   Validator, CreatePattern, PatternToString and DataTypes.
   It is the main entry point to the XML schema validator of the Haskell
   XML Toolbox.

-}

-- ------------------------------------------------------------

module   Text.XML.HXT.XMLSchema.SystemConfig
where

import           Text.XML.HXT.Arrow.XmlState.TypeDefs
-- import Text.XML.HXT.Arrow.XmlOptions

import           Text.XML.HXT.XMLSchema.Validation    (validateDocumentWithXmlSchema)


import           System.Console.GetOpt

-- ------------------------------------------------------------

withXmlSchema                   :: String -> SysConfig
withXmlSchema s                 = setS (theXmlSchemaValidate
                                        .&&&. theXmlSchemaSchema
                                        .&&&. theXmlSchemaValidator
                                       ) ( not (null s)         -- null s turns off validation
                                         , ( s
                                           , validateDocumentWithXmlSchema [] s
                                           )
                                         )

-- ------------------------------------------------------------

-- | available XML schema validation options

xmlSchemaOptions :: [OptDescr SysConfig]
xmlSchemaOptions
    = [ Option "Y" [a_xml_schema]   (ReqArg withXmlSchema "SCHEMA")  "validation with XML schema, SCHEMA is the URI for the XML schema document"
      ]

-- ------------------------------------------------------------
-- option for Relax NG

a_xml_schema :: String
a_xml_schema = "xml-schema"

-- ------------------------------------------------------------
