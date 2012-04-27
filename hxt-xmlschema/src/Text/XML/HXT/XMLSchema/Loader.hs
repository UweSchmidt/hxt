{- |
   Module     : Text.XML.HXT.XMLSchema.Loader
   Copyright  : Copyright (C) 2012 Thorben Guelck, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Contains functions to load schema definition and instance documents
   and provide an internal representation.
-}

module Text.XML.HXT.XMLSchema.Loader
  ( loadDefinition
  , loadInstance
  )
where

import Text.XML.HXT.XMLSchema.AbstractSyntax

import Text.XML.HXT.Core                       ( PU
                                               , xpText
                                               , xpAttr
                                               , xpWrap
                                               , xpAlt
                                               , xpList
                                               , xpOption
                                               , xpPair
                                               , xpTriple
                                               , xpElemNS
                                               , xpFilterCont
                                               , xpFilterAttr

                                               , QName
                                               , mkQName
                                               , localPart
                                               , setLocalPart'
                                               , qualifiedName
                                               , namespaceUri
                                               , newXName

                                               , neg
                                               , none
                                               , (<+>)
                                               , (>>>)
                                               , hasNameWith
                                               , hasQName

                                               , runX
                                               , withValidate
                                               , withRemoveWS
                                               , withPreserveComment
                                               , withCheckNamespaces
                                               , yes
                                               , no
                                               , traceMsg
                                               , SysConfigList

                                               , LA
                                               , ($<)
                                               , unXN
                                               , fromLA
                                               , when
                                               , isElem
                                               , hasName
                                               , processTopDown
                                               , processAttrl
                                               , processWithNsEnvWithoutAttrl
                                               , changeAttrValue
                                               , xmlXName
                                               , xmlNamespaceXName
                                               , xmlnsXName
                                               , xmlnsNamespaceXName
                                               , xunpickleVal

                                               , XmlTree
                                               , readDocument
                                               , documentStatusOk
                                               , (&&&)
                                               , getAttrValue
                                               , getChildren
                                               )

import Text.XML.HXT.Arrow.XmlState.URIHandling ( expandURIString )

import Control.Applicative                     ( (<$>) )

import Data.List                               ( isPrefixOf
                                               , findIndex
                                               )

import Data.Map                                ( empty
                                               , lookup
                                               , insert
                                               , union
                                               , fromList
                                               )

import Prelude hiding (lookup)

-- ----------------------------------------

-- | Helper type for unpickling a schema definition
data XmlSchema'        = XmlSchema'
                       { targetNS :: Maybe Namespace
                       , parts    :: [XmlSchemaPart]
                       }

-- | Helper type for unpickling schema definition parts
data XmlSchemaPart     = In {unIn :: Include}
                       | St {unSt :: (QName, SimpleType)}
                       | Ct {unCt :: (QName, ComplexType)}
                       | El {unEl :: Element}
                       | Gr {unGr :: (QName, Group)}
                       | At {unAt :: Attribute}
                       | Ag {unAg :: (QName, AttributeGroup)}

-- ----------------------------------------

-- | The XML Schema namespace
nsUri :: String
nsUri = "http://www.w3.org/2001/XMLSchema"

-- | The XML Schema namespace prefix
nsPrefix :: String
nsPrefix = "xs"

-- | Basic namespace-aware element pickler
xpElem' :: String -> PU a -> PU a
xpElem' name
  = xpElemNS nsUri nsPrefix name

-- | Basic schema element pickler
xpSchemaElem :: String -> PU a -> PU a
xpSchemaElem name
  = xpElem' name . xpFilterSchema

-- | Filter for inessential schema definition parts
xpFilterSchema :: PU a -> PU a
xpFilterSchema
  = -- keep elems from xs namespace which are not blacklisted
    xpFilterCont (isXmlSchemaElem           >>> neg elementBlacklist) .
    -- keep attrs without namespace which are not blacklisted
    xpFilterAttr (isAttrWithoutNamespaceUri >>> neg attributeBlacklist)
    where
    -- element blacklist
    isXmlSchemaElem           = hasNameWith $ (== nsUri) . namespaceUri
    elementBlacklist          = foldr (<+>) none $ map hasQName $
                                [ mkQName nsPrefix "annotation"           nsUri
                                , mkQName nsPrefix "notation"             nsUri
                                -- skipped content for element
                                , mkQName nsPrefix "unique"               nsUri
                                , mkQName nsPrefix "key"                  nsUri
                                , mkQName nsPrefix "keyref"               nsUri
                                ]
    -- attribute blacklist
    isAttrWithoutNamespaceUri = hasNameWith $ null       . namespaceUri
    attributeBlacklist        = foldr (<+>) none $ map hasQName $
                                [ mkQName ""       "id"                   ""
                                -- skipped attributes for schema
                                , mkQName ""       "attributeFormDefault" ""
                                , mkQName ""       "blockDefault"         ""
                                , mkQName ""       "elementFormDefault"   ""
                                , mkQName ""       "finalDefault"         ""
                                , mkQName ""       "version"              ""
                                , mkQName ""       "lang"                 ""
                                -- skipped attributes for simpleType, complexType and element
                                , mkQName ""       "final"                ""
                                -- skipped attributes for simpleType restrictions, complexType, element and attribute
                                , mkQName ""       "fixed"                ""
                                -- skipped attributes for complexType and element
                                , mkQName ""       "abstract"             ""
                                , mkQName ""       "block"                ""
                                -- skipped attributes for element and attribute
                                , mkQName ""       "default"              ""
                                , mkQName ""       "form"                 ""
                                -- skipped attributes for element
                                , mkQName ""       "nillable"             ""
                                , mkQName ""       "substitutionGroup"    ""
                                ]

-- | Split a string at a given delimiter
splitWith :: Char -> String -> (String, String)
splitWith del s
  = case mn of
      Nothing -> ("", s)
      Just n  -> (take n s, drop (n+1) s)
    where
    mn = findIndex (== del) s

-- | Parse a QName from a string in format "{uri}prefix:localPart"
parseQName :: String -> QName
parseQName s
  = mkQName pf lp uri 
    where
    (pf, lp) = splitWith ':' n
    (uri, n) = if isPrefixOf "{" s
                 then splitWith '}' $ tail s
                 else ("", s)

-- | Basic QName pickler
xpQName :: PU QName
xpQName
  = xpWrap (parseQName, qualifiedName) xpText -- TODO: namespaces / target namespace

-- | Basic pickler for a list of QNames
xpQNames :: PU QNames
xpQNames
  = xpWrap ( map parseQName . words
           , unwords . map qualifiedName
           ) xpText

-- | Entry point to unpickle a schema definition into an internal representation
xpXmlSchema' :: PU XmlSchema'
xpXmlSchema'
  = xpSchemaElem "schema" $
    xpFilterSchema $
    xpWrap (\ (a, b) -> XmlSchema' a b , \ t -> (targetNS t, parts t)) $
    xpPair (xpOption $ xpAttr "targetNamespace" xpText) $
    xpList $ xpSchemaPart

-- | Pickler for schema definition parts
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
         , xpWrap (St, unSt) $ xpSchemaElem "simpleType"     $ xpPair (xpAttr "name" xpQName) xpSimpleType
         , xpWrap (Ct, unCt) $ xpSchemaElem "complexType"    $ xpPair (xpAttr "name" xpQName) xpComplexType
         , xpWrap (El, unEl) $ xpSchemaElem "element"        $ xpElement
         , xpWrap (Gr, unGr) $ xpSchemaElem "group"          $ xpPair (xpAttr "name" xpQName) xpGroup
         , xpWrap (At, unAt) $ xpSchemaElem "attribute"      $ xpAttribute
         , xpWrap (Ag, unAg) $ xpSchemaElem "attributeGroup" $ xpPair (xpAttr "name" xpQName) xpAttributeGroup
         ]

-- | Pickler for external references
xpInclude :: PU Include
xpInclude
  = xpAlt tag ps
    where
    tag (Incl _)  = 0
    tag (Imp _)   = 1
    tag (Redef _) = 2
    ps = [ xpWrap (Incl,  unIncl)  $ xpSchemaElem "include"  $ xpAttr "schemaLocation" xpText
         , xpWrap (Imp,   unImp)   $ xpSchemaElem "import"   $ xpPair (xpAttr "schemaLocation" xpText) $
                                                                      (xpAttr "namespace" xpText)
         , xpWrap (Redef, unRedef) $ xpSchemaElem "redefine" $ xpPair (xpAttr "schemaLocation" xpText) $
                                                                      (xpList xpRedefinition)
         ]

-- | Pickler for redefinitions
xpRedefinition :: PU Redefinition
xpRedefinition
  = xpAlt tag ps
    where
    tag (RedefSt _) = 0
    tag (RedefCt _) = 1
    tag (RedefGr _) = 2
    tag (RedefAg _) = 3
    ps = [ xpWrap (RedefSt, unRedefSt) $ xpSchemaElem "simpleType"     $ xpPair (xpAttr "name" xpQName) xpSimpleType
         , xpWrap (RedefCt, unRedefCt) $ xpSchemaElem "complexType"    $ xpPair (xpAttr "name" xpQName) xpComplexType
         , xpWrap (RedefGr, unRedefGr) $ xpSchemaElem "group"          $ xpPair (xpAttr "name" xpQName) xpGroup
         , xpWrap (RedefAg, unRedefAg) $ xpSchemaElem "attributeGroup" $ xpPair (xpAttr "name" xpQName) xpAttributeGroup
         ]

-- | Pickler for SimpleTypes
xpSimpleType :: PU SimpleType
xpSimpleType
  = xpAlt tag ps
    where
    tag (Restr _) = 0
    tag (Lst _)   = 1
    tag (Un _)    = 2
    ps = [ xpWrap (Restr, unRestr) $ xpSchemaElem "restriction" $ xpSTRestriction
         , xpWrap (Lst,   unLst)   $ xpSchemaElem "list"        $ xpSTList
         , xpWrap (Un,    unUn)    $ xpSchemaElem "union"       $ xpSTUnion
         ]

-- | Pickler for SimpleType restrictions
xpSTRestriction :: PU STRestriction
xpSTRestriction
  = xpPair xpSimpleTypeRef $ xpList $ xpRestrAttr

-- | Pickler for SimpleType references
xpSimpleTypeRef :: PU SimpleTypeRef
xpSimpleTypeRef
  = xpAlt tag ps
    where
    tag (BaseAttr _)        = 0
    tag (STRAnonymStDecl _) = 1
    ps = [ xpWrap (BaseAttr,        unBaseAttr)        $ xpAttr "base" xpQName
         , xpWrap (STRAnonymStDecl, unSTRAnonymStDecl) $ xpSchemaElem "simpleType" $ xpSimpleType
         ]

-- | Pickler for restriction attributes
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
    tag (Length _)         = 6
    tag (MinLength _)      = 7
    tag (MaxLength _)      = 8
    tag (Pattern _)        = 9
    tag (Enumeration _)    = 10
    tag (WhiteSpace _)     = 11
    ps = [ xpWrap (MinIncl,        unMinIncl)        $ xpSchemaElem "minInclusive"   $ xpAttr "value" xpText
         , xpWrap (MaxIncl,        unMaxIncl)        $ xpSchemaElem "maxInclusive"   $ xpAttr "value" xpText
         , xpWrap (MinExcl,        unMinExcl)        $ xpSchemaElem "minExclusive"   $ xpAttr "value" xpText
         , xpWrap (MaxExcl,        unMaxExcl)        $ xpSchemaElem "maxExclusive"   $ xpAttr "value" xpText
         , xpWrap (TotalDigits,    unTotalDigits)    $ xpSchemaElem "totalDigits"    $ xpAttr "value" xpText
         , xpWrap (FractionDigits, unFractionDigits) $ xpSchemaElem "fractionDigits" $ xpAttr "value" xpText
         , xpWrap (Length,         unLength)         $ xpSchemaElem "length"         $ xpAttr "value" xpText
         , xpWrap (MinLength,      unMinLength)      $ xpSchemaElem "minLength"      $ xpAttr "value" xpText
         , xpWrap (MaxLength,      unMaxLength)      $ xpSchemaElem "maxLength"      $ xpAttr "value" xpText
         , xpWrap (Pattern,        unPattern)        $ xpSchemaElem "pattern"        $ xpAttr "value" xpText
         , xpWrap (Enumeration,    unEnumeration)    $ xpSchemaElem "enumeration"    $ xpAttr "value" xpText
         , xpWrap (WhiteSpace,     unWhiteSpace)     $ xpSchemaElem "whiteSpace"     $ xpAttr "value" xpText
         ]

-- | Pickler for a list of a referenced SimpleType
xpSTList :: PU STList
xpSTList
  = xpAlt tag ps
    where
    tag (ItemTypeAttr _)    = 0
    tag (STLAnonymStDecl _) = 1
    ps = [ xpWrap (ItemTypeAttr,    unItemTypeAttr)    $ xpAttr "itemType" xpQName
         , xpWrap (STLAnonymStDecl, unSTLAnonymStDecl) $ xpSchemaElem "simpleType" $ xpSimpleType
         ]

-- | Pickler for an union of referenced SimpleTypes
xpSTUnion :: PU STUnion
xpSTUnion
  = xpWrap (\ (a, b) -> STUnion a b , \ t -> (memberTypes t, anonymDecls t)) $
    xpPair (xpOption $ xpAttr "memberTypes" xpQNames) $
    xpList $ xpSchemaElem "simpleType" $ xpSimpleType

-- | Pickler for a ComplexType
xpComplexType :: PU ComplexType
xpComplexType
  = xpWrap (\ (a, b) -> ComplexType a b , \ t -> (ctMixed t, ctDef t)) $
    xpPair (xpOption $ xpAttr "mixed" xpText) xpCTDef

-- | Pickler for a ComplexType definition
xpCTDef :: PU CTDef
xpCTDef
  = xpAlt tag ps
    where
    tag (SCont _) = 0
    tag (CCont _) = 1
    tag (NewCT _) = 2
    ps = [ xpWrap (SCont, unSCont) $ xpSchemaElem "simpleContent"  $ xpSimpleContent
         , xpWrap (CCont, unCCont) $ xpSchemaElem "complexContent" $ xpComplexContent
         , xpWrap (NewCT, unNewCT) $ xpCTModel
         ]

-- | Pickler for simple content
xpSimpleContent :: PU SimpleContent
xpSimpleContent
  = xpAlt tag ps
    where
    tag (SCExt _)   = 0
    tag (SCRestr _) = 1
    ps = [ xpWrap (SCExt,   unSCExt)   $ xpSchemaElem "extension"   $ xpPair (xpAttr "base" xpQName) xpAttrList
         , xpWrap (SCRestr, unSCRestr) $ xpSchemaElem "restriction" $ xpPair xpSTRestriction xpAttrList 
         ]

-- | Pickler for complex content
xpComplexContent :: PU ComplexContent
xpComplexContent
  = xpWrap (\ (a, b) -> ComplexContent a b , \ t -> (ccMixed t, ccDef t)) $
    xpPair (xpOption $ xpAttr "mixed" xpText) xpCCDef

-- | Pickler for a complex content definition
xpCCDef :: PU CCDef
xpCCDef
  = xpAlt tag ps
    where
    tag (CCExt _)   = 0
    tag (CCRestr _) = 1
    ps = [ xpWrap (CCExt,   unCCExt)   $ xpSchemaElem "extension"   $ xpPair (xpAttr "base" xpQName) $ xpCTModel
         , xpWrap (CCRestr, unCCRestr) $ xpSchemaElem "restriction" $ xpPair (xpAttr "base" xpQName) $ xpCTModel
         ]

-- | Pickler for a complex type model
xpCTModel :: PU CTModel
xpCTModel
  = xpPair (xpOption xpCTCompositor) xpAttrList

-- | Pickler for a complex type compositor
xpCTCompositor :: PU CTCompositor
xpCTCompositor
  = xpAlt tag ps
    where
    tag (CompGr _) = 0
    tag (CompAl _) = 1
    tag (CompCh _) = 2
    tag (CompSq _) = 3
    ps = [ xpWrap (CompGr, unCompGr) $ xpSchemaElem "group"    $ xpPair xpMinMaxOcc xpGroup
         , xpWrap (CompAl, unCompAl) $ xpSchemaElem "all"      $ xpPair xpMinMaxOcc xpAll
         , xpWrap (CompCh, unCompCh) $ xpSchemaElem "choice"   $ xpPair xpMinMaxOcc xpSchemaChoice
         , xpWrap (CompSq, unCompSq) $ xpSchemaElem "sequence" $ xpPair xpMinMaxOcc xpSequence
         ]

-- | Pickler for restrictions on the number of occurrences
xpMinMaxOcc :: PU MinMaxOcc
xpMinMaxOcc
  = xpWrap (\ (a, b) -> MinMaxOcc a b , \ t -> (minOcc t, maxOcc t)) $
    xpPair (xpOption $ xpAttr "minOccurs" xpText) $ xpOption $ xpAttr "maxOccurs" xpText

-- | Pickler for all
xpAll :: PU All
xpAll
  = xpList $ xpSchemaElem "element" $ xpPair xpMinMaxOcc xpElement

-- | Pickler for choice
xpSchemaChoice :: PU Choice
xpSchemaChoice
  = xpList xpChSeqContent

-- | Pickler for sequence
xpSequence :: PU Sequence
xpSequence
  = xpList xpChSeqContent

-- | Pickler for contents of choice and sequence
xpChSeqContent :: PU ChSeqContent
xpChSeqContent
  = xpAlt tag ps
    where
    tag (ChSeqEl _) = 0
    tag (ChSeqGr _) = 1
    tag (ChSeqCh _) = 2
    tag (ChSeqSq _) = 3
    tag (ChSeqAn _) = 4
    ps = [ xpWrap (ChSeqEl, unChSeqEl) $ xpSchemaElem "element"  $ xpPair xpMinMaxOcc xpElement
         , xpWrap (ChSeqGr, unChSeqGr) $ xpSchemaElem "group"    $ xpPair xpMinMaxOcc xpGroup
         , xpWrap (ChSeqCh, unChSeqCh) $ xpSchemaElem "choice"   $ xpPair xpMinMaxOcc xpSchemaChoice
         , xpWrap (ChSeqSq, unChSeqSq) $ xpSchemaElem "sequence" $ xpPair xpMinMaxOcc xpSequence
         , xpWrap (ChSeqAn, unChSeqAn) $ xpSchemaElem "any"      $ xpPair xpMinMaxOcc xpAny
         ]

-- | Pickler for wildcards
xpAny :: PU Any
xpAny
  = xpWrap (\ (a, b) -> Any a b , \ t -> (namespace t, processContents t)) $
    xpPair (xpOption $ xpAttr "namespace" xpText) $ xpOption $ xpAttr "processContents" xpText

-- | Pickler for a list of attributes
xpAttrList :: PU AttrList
xpAttrList
  = xpList $
    xpAlt tag ps
    where
    tag (Attr _)    = 0
    tag (AttrGrp _) = 1
    tag (AnyAttr _) = 2
    ps = [ xpWrap (Attr,    unAttr)    $ xpSchemaElem "attribute"      $ xpAttribute
         , xpWrap (AttrGrp, unAttrGrp) $ xpSchemaElem "attributeGroup" $ xpAttributeGroup
         , xpWrap (AnyAttr, unAnyAttr) $ xpSchemaElem "anyAttribute"   $ xpAny
         ]

-- | Pickler for an element
xpElement :: PU Element
xpElement
  = xpAlt tag ps
    where
    tag (ElRef _) = 0
    tag (ElDef _) = 1
    ps = [ xpWrap (ElRef, unElRef) $ xpAttr "ref" xpQName
         , xpWrap (ElDef, unElDef) $ xpElementDef
         ]

-- | Pickler for an element definition
xpElementDef :: PU ElementDef
xpElementDef
  = xpWrap (\ (a, b) -> ElementDef a b, \ t -> (elemName t, elemTypeDef t)) $
    xpPair (xpAttr "name" xpQName) xpElemTypeDef

-- | Pickler for an element's type
xpElemTypeDef :: PU ElemTypeDef
xpElemTypeDef
  = xpAlt tag ps
    where
    tag (ETDTypeAttr _)     = 0
    tag (ETDAnonymStDecl _) = 1
    tag (ETDAnonymCtDecl _) = 2
    ps = [ xpWrap (ETDTypeAttr,     unETDTypeAttr)     $ xpAttr "type" xpQName
         , xpWrap (ETDAnonymStDecl, unETDAnonymStDecl) $ xpSchemaElem "simpleType"  $ xpSimpleType
         , xpWrap (ETDAnonymCtDecl, unETDAnonymCtDecl) $ xpSchemaElem "complexType" $ xpComplexType
         ]

-- | Pickler for group
xpGroup :: PU Group
xpGroup
  = xpAlt tag ps
    where
    tag (GrpRef _) = 0
    tag (GrpDef _) = 1
    ps = [ xpWrap (GrpRef, unGrpRef) $ xpAttr "ref" xpQName
         , xpWrap (GrpDef, unGrpDef) $ xpOption $ xpGroupDef
         ]

-- | Pickler for a group definition
xpGroupDef :: PU GroupDef
xpGroupDef
  = xpAlt tag ps
    where
    tag (Al _) = 0
    tag (Ch _) = 1
    tag (Sq _) = 2
    ps = [ xpWrap (Al, unAl) $ xpSchemaElem "all"      $ xpAll
         , xpWrap (Ch, unCh) $ xpSchemaElem "choice"   $ xpSchemaChoice
         , xpWrap (Sq, unSq) $ xpSchemaElem "sequence" $ xpSequence
         ]

-- | Pickler for an attribute
xpAttribute :: PU Attribute
xpAttribute
  = xpAlt tag ps
    where
    tag (AttrRef _) = 0
    tag (AttrDef _) = 1
    ps = [ xpWrap (AttrRef, unAttrRef) $ xpAttr "ref" xpQName
         , xpWrap (AttrDef, unAttrDef) $ xpAttributeDef
         ]

-- | Pickler for an attribute definition
xpAttributeDef :: PU AttributeDef
xpAttributeDef
  = xpWrap (\ (a, b, c) -> AttributeDef a b c, \ t -> (attrName t, attrTypeDef t, attrUse t)) $
    xpTriple (xpAttr "name" xpQName) xpAttrTypeDef $ xpOption $ xpAttr "use" xpText

-- | Pickler for an attribute type definition
xpAttrTypeDef :: PU AttrTypeDef
xpAttrTypeDef
  = xpAlt tag ps
    where
    tag (ATDTypeAttr _)   = 0
    tag (ATDAnonymDecl _) = 1
    ps = [ xpWrap (ATDTypeAttr,   unATDTypeAttr)   $ xpAttr "type" xpQName
         , xpWrap (ATDAnonymDecl, unATDAnonymDecl) $ xpSchemaElem "simpleType" $ xpSimpleType
         ]

-- | Pickler for an attribute group
xpAttributeGroup :: PU AttributeGroup
xpAttributeGroup
  = xpAlt tag ps
    where
    tag (AttrGrpRef _) = 0
    tag (AttrGrpDef _) = 1
    ps = [ xpWrap (AttrGrpRef, unAttrGrpRef) $ xpAttr "ref" xpQName
         , xpWrap (AttrGrpDef, unAttrGrpDef) $ xpAttrList
         ]

-- ----------------------------------------

-- | Conversion from XmlSchema' helper type to XmlSchema type
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
      = toSchemaRec (XmlSchema' tns xs) ins sts cts (insert (elemName $ unElDef el) el els) grs ats ags
    toSchemaRec (XmlSchema' tns ((Gr (k, gr)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els (insert k gr grs) ats ags
    toSchemaRec (XmlSchema' tns ((At at)     :xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els grs (insert (attrName $ unAttrDef at) at ats) ags
    toSchemaRec (XmlSchema' tns ((Ag (k, ag)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els grs ats (insert k ag ags)

-- ----------------------------------------

-- | Applies a list of redefinitions
applyRedefs :: XmlSchema -> Redefinitions -> XmlSchema
applyRedefs s []
  = s
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefSt (k, st')):xs)
  = applyRedefs (XmlSchema tns ins sts'' cts els grs ats ags) xs
    where
    k'    = setLocalPart' (newXName $ (localPart k) ++ "_redef") k
    sts'  = case lookup k sts of
              Just st -> insert k' st sts
              Nothing -> sts
    sts'' = case st' of
              (Restr (BaseAttr _, rlist)) -> insert k (Restr (BaseAttr k', rlist)) sts'
              _                           -> sts
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefCt (k, ct')):xs)
  = applyRedefs (XmlSchema tns ins sts cts'' els grs ats ags) xs
    where
    k'    = setLocalPart' (newXName $ (localPart k) ++ "_redef") k
    cts'  = case lookup k cts of
              Just ct -> insert k' ct cts
              Nothing -> cts
    cts'' = case ct' of
              (ComplexType ctm (CCont (ComplexContent ccm (CCExt   (_, m))))) ->
                insert k (ComplexType ctm $ CCont $ ComplexContent ccm $ CCExt   (k', m)) cts'
              (ComplexType ctm (CCont (ComplexContent ccm (CCRestr (_, m))))) ->
                insert k (ComplexType ctm $ CCont $ ComplexContent ccm $ CCRestr (k', m)) cts'
              _                                                               -> cts
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefGr (k, gr')):xs)
  = applyRedefs (XmlSchema tns ins sts cts els (insert k gr' grs) ats ags) xs
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefAg (k, ag')):xs)
  = applyRedefs (XmlSchema tns ins sts cts els grs ats (insert k ag' ags)) xs

-- | Combines two given schema definitions
mergeSchemata :: XmlSchema -> XmlSchema -> XmlSchema
mergeSchemata (XmlSchema tns _ sts cts els grs ats ags) (XmlSchema _ _ sts' cts' els' grs' ats' ags')
  = XmlSchema tns [] (union sts sts') (union cts cts') (union els els') (union grs grs') (union ats ats') $ union ags ags'

-- ----------------------------------------

-- | Transforms the location of an external reference into an absolute path
mkAbsPath :: String -> Include -> Include
mkAbsPath anchor incl
  = case incl of
      (Incl loc)            -> Incl  $ modLoc loc
      (Imp (loc, ns))       -> Imp   $ (modLoc loc, ns)
      (Redef (loc, redefs)) -> Redef $ (modLoc loc, redefs)
    where
    modLoc l = case expandURIString l anchor of
                 Nothing -> l
                 Just l' -> l'

-- | Loads a schema definition from a given url
loadDefinition :: SysConfigList -> String -> IO (Maybe XmlSchema)
loadDefinition config uri
  = do
    s' <- runX (
                readDocument ( config ++
                               [ withValidate yes        -- validate source
                               , withRemoveWS yes        -- remove redundant whitespace
                               , withPreserveComment no  -- keep comments
                               , withCheckNamespaces yes -- check namespaces
                               ]
                             ) uri
                >>>
                traceMsg 2 "propagating namespaces into XMLSchema attribute values"
                >>>
                fromLA propagateTargetNamespace
                >>>
                fromLA propagateXmlSchemaNamespaces
                >>>
                traceMsg 2 ("start unpickling schema for " ++ show uri) 
                >>>
                (getAttrValue "transfer-URI" &&& xunpickleVal xpXmlSchema')
                >>>
                traceMsg 2 "unpickling schema done"
               )
    if null s'
      then return Nothing
      else do
           let h = head s'
           let s = toSchema $ snd h
           Just <$> (resolveIncls s $ map (mkAbsPath $ fst h) $ sIncludes s)
  where
    loadDefinition' = loadDefinition config

    -- | Processes a list of external references
    resolveIncls :: XmlSchema -> Includes -> IO XmlSchema
    resolveIncls s []
        = return s
    resolveIncls s (x:xs)
        = do incl' <- resolveIncl x
             case incl' of
               Nothing   -> resolveIncls s xs
               Just incl -> resolveIncls (mergeSchemata s incl) xs

    -- | Processes a single external reference
    resolveIncl :: Include -> IO (Maybe XmlSchema) -- TODO: apply namespaces
    resolveIncl (Incl loc)
        = loadDefinition' loc
    resolveIncl (Imp (loc, _)) -- second param: ns      
        = loadDefinition' loc
    resolveIncl (Redef (loc, redefs))
        = do s' <- loadDefinition' loc
             case s' of
               Nothing -> return Nothing
               Just s  -> return $ Just $ applyRedefs s redefs


-- | Propagates the target namespace
propagateTargetNamespace :: LA XmlTree XmlTree
propagateTargetNamespace
  = propagate $< getTargetNS
    where
    getTargetNS
      = getChildren >>> isSchema >>> getAttrValue "targetNamespace"
    propagate tns
      = processTopDown $
        addTns `when` (isSimpleType <+> isComplexType <+> isElement <+> isAttribute <+> isGroup <+> isAttributeGroup)
        where
          addUri
            | null tns  = id
            | otherwise = (("{" ++ tns ++ "}") ++)
          addTns
            = processAttrl $ changeAttrValue addUri `when` hasName "name"

-- | Checks whether a given XmlTree is a xs:schema tag
isSchema :: LA XmlTree XmlTree
isSchema = isXSElem "schema"

-- | Checks whether a given XmlTree is a xs:simpleType tag
isSimpleType :: LA XmlTree XmlTree
isSimpleType = isXSElem "simpleType"

-- | Checks whether a given XmlTree is a xs:complexType tag
isComplexType :: LA XmlTree XmlTree
isComplexType = isXSElem "complexType"

-- | Checks whether a given XmlTree is a xs:element tag
isElement :: LA XmlTree XmlTree
isElement = isXSElem "element"

-- | Checks whether a given XmlTree is a xs:attribute tag
isAttribute :: LA XmlTree XmlTree
isAttribute = isXSElem "attribute"

-- | Checks whether a given XmlTree is a xs:group tag
isGroup :: LA XmlTree XmlTree
isGroup = isXSElem "group"

-- | Checks whether a given XmlTree is a xs:attributeGroup tag
isAttributeGroup :: LA XmlTree XmlTree
isAttributeGroup = isXSElem "attributeGroup"

-- | Checks whether a given XmlTree is a xs tag with a given name
isXSElem :: String -> LA XmlTree XmlTree
isXSElem name
  = isElem >>> hasXSName name

-- | Checks whether a given XmlTree is from xs namespace and has a given name
hasXSName :: String -> LA XmlTree XmlTree
hasXSName lp
  = hasNameWith $
    \ qn -> namespaceUri qn == nsUri
            &&
            localPart qn == lp

-- | Checks whether a given XmlTree is a xs:restriction tag
isRestriction :: LA XmlTree XmlTree
isRestriction = isXSElem "restriction"

-- | Checks whether a given XmlTree is a xs:list tag
isList :: LA XmlTree XmlTree
isList = isXSElem "list"

-- | Checks whether a given XmlTree is a xs:union tag
isUnion :: LA XmlTree XmlTree
isUnion = isXSElem "union" -- TODO: memberTypes: List of QNames

-- | Checks whether a given XmlTree is a xs:extension tag
isExtension :: LA XmlTree XmlTree
isExtension = isXSElem "extension"

-- | Propagates namespaces which are defined by xmlns attributes
propagateXmlSchemaNamespaces :: LA XmlTree XmlTree
propagateXmlSchemaNamespaces
  = processWithNsEnvWithoutAttrl propagate
    [ (xmlXName,   xmlNamespaceXName)
    , (xmlnsXName, xmlnsNamespaceXName)
    ]
    where
    hasPropAttr = isRestriction <+> isList <+> isUnion <+> isExtension <+> isElement <+> isAttribute <+> isGroup <+> isAttributeGroup
    isPropAttr  = hasName "base" <+> hasName "itemType" <+> hasName "type" <+> hasName "ref"
    propagate env
      = addXns `when` hasPropAttr
        where
        addXns = processAttrl $ changeAttrValue addUri `when` isPropAttr
        addUri n
          = maybe n (\ u -> "{" ++ unXN u ++ "}" ++ n) $ lookup (newXName px) $ fromList env
            where
            (px', lp') = span (/= ':') n
            px
              | null lp'  = ""
              | otherwise = px'

-- ----------------------------------------

-- | Loads a schema instance from a given url
loadInstance :: SysConfigList -> String -> IO (Maybe XmlTree)
loadInstance config uri
  = do
    s <- runX ( readDocument ( config ++
                               -- these options can't are mandatory
                               [ withValidate yes        -- validate source
                               , withRemoveWS yes        -- remove redundant whitespace
                               , withPreserveComment no  -- remove comments
                               , withCheckNamespaces yes -- check namespaces
                               ]
                             ) uri
                >>>
                documentStatusOk
                >>>
                getChildren
                >>>
                isElem
              )
    case s of
      [] -> return Nothing
      (t : _) ->  return $ Just t
{-
    if (fst $ head s) == ""
      then return $ Just $ snd $ head s
      else return Nothing
-}
-- ----------------------------------------

