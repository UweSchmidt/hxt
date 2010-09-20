-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XSLT.Application
   Copyright  : Copyright (C) 2006 Tim Walkenhorst, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Names and constants for HXSLT

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XSLT.Names
where

import Text.XML.HXT.XSLT.Common

xsltPrefix      :: String
xsltPrefix      = "xsl"

xsltUri         :: String
xsltUri         = "http://www.w3.org/1999/XSL/Transform"

mkXsltName :: String -> QName
mkXsltName name = mkQName xsltPrefix name xsltUri

mkXsltAttribName :: String -> QName
mkXsltAttribName name = mkQName "" name ""

-- XSLT-Element QNames
xsltTransform
  , xsltStylesheet
  , xsltMessage
  , xsltForEach
  , xsltChoose
  , xsltWhen
  , xsltOtherwise
  , xsltIf
  , xsltElement
  , xsltAttribute
  , xsltText
  , xsltValueOf
  , xsltComment
  , xsltProcInstr
  , xsltInclude
  , xsltImport
  , xsltTemplate
  , xsltApplyTemplates
  , xsltApplyImports
  , xsltCallTemplate
  , xsltVariable
  , xsltParam
  , xsltWithParam
  , xsltAttributeSet
  , xsltCopy
  , xsltCopyOf
  , xsltSort
  , xsltStripSpace
  , xsltPreserveSpace
  , xsltNamespaceAlias  :: QName

xsltTransform                   = mkXsltName "transform"
xsltStylesheet                  = mkXsltName "stylesheet"
xsltMessage                     = mkXsltName "message"
xsltForEach                     = mkXsltName "for-each"
xsltChoose                      = mkXsltName "choose"
xsltWhen                        = mkXsltName "when"
xsltOtherwise                   = mkXsltName "otherwise"
xsltIf                          = mkXsltName "if"
xsltElement                     = mkXsltName "element"
xsltAttribute                   = mkXsltName "attribute"
xsltText                        = mkXsltName "text"
xsltValueOf                     = mkXsltName "value-of"
xsltComment                     = mkXsltName "comment"
xsltProcInstr                   = mkXsltName "processing-instruction"
xsltInclude                     = mkXsltName "include"
xsltImport                      = mkXsltName "import"
xsltTemplate                    = mkXsltName "template"
xsltApplyTemplates              = mkXsltName "apply-templates"
xsltApplyImports                = mkXsltName "apply-imports"
xsltCallTemplate                = mkXsltName "call-template"
xsltVariable                    = mkXsltName "variable"
xsltParam                       = mkXsltName "param"
xsltWithParam                   = mkXsltName "with-param"
xsltAttributeSet                = mkXsltName "attribute-set"
xsltCopy                        = mkXsltName "copy"
xsltCopyOf                      = mkXsltName "copy-of"
xsltSort                        = mkXsltName "sort"
xsltStripSpace                  = mkXsltName "strip-space"
xsltPreserveSpace               = mkXsltName "preserve-space"
xsltNamespaceAlias              = mkXsltName "namespace-alias"

-- XSLT-Attribute QNames
xsltTerminate
  , xsltSelect
  , xsltTest
  , xsltName
  , xsltNamespace
  , xsltUseAttributeSets
  , xsltHRef
  , xsltMatch
  , xsltPriority
  , xsltMode
  , xsltDataType
  , xsltOrder
  , xsltElements
  , xsltStylesheetPrefix
  , xsltResultPrefix
  , xsltVersion
  , xsltExlcudeResultPrefixes
  , xsltExtensionElementPrefixes        :: QName

xsltTerminate                   = mkXsltAttribName "terminate"
xsltSelect                      = mkXsltAttribName "select"
xsltTest                        = mkXsltAttribName "test"
xsltName                        = mkXsltAttribName "name"
xsltNamespace                   = mkXsltAttribName "namespace"
xsltUseAttributeSets            = mkXsltAttribName "use-attribute-sets"
xsltHRef                        = mkXsltAttribName "href"
xsltMatch                       = mkXsltAttribName "match"
xsltPriority                    = mkXsltAttribName "priority"
xsltMode                        = mkXsltAttribName "mode"
xsltDataType                    = mkXsltAttribName "data-type"
xsltOrder                       = mkXsltAttribName "order"
xsltElements                    = mkXsltAttribName "elements"
xsltStylesheetPrefix            = mkXsltAttribName "stylesheet-prefix"
xsltResultPrefix                = mkXsltAttribName "result-prefix"
xsltVersion                     = mkXsltAttribName "version"
xsltExlcudeResultPrefixes       = mkXsltAttribName "exclude-result-prefixes"
xsltExtensionElementPrefixes    = mkXsltAttribName "extension-element-prefixes"

-- XSLT-Attribute QNames for special Literal result element attributes
xsltUseAttributeSetsLRE
  , xsltVersionLRE
  , xsltExlcudeResultPrefixesLRE
  , xsltExtensionElementPrefixesLRE     :: QName

xsltUseAttributeSetsLRE         = mkXsltName "use-attribute-sets"
xsltVersionLRE                  = mkXsltName "version"
xsltExlcudeResultPrefixesLRE    = mkXsltName "exclude-result-prefixes"
xsltExtensionElementPrefixesLRE = mkXsltName "extension-element-prefixes"

-- xml:space attribute-name
xmlSpace        :: QName

xmlSpace                        = mkQName "xml" "space" xmlNamespace
