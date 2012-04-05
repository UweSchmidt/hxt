{- |
   Module     : Text.XML.HXT.XMLSchema.Loader
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Contains functions to load schema definition and instance documents
   and provide an internal representation.
-}

module Text.XML.HXT.XMLSchema.Loader

  ( loadDefinition
  , loadInstance
  )

where

import Text.XML.HXT.XMLSchema.AbstractSyntax

import Text.XML.HXT.Core   ( PU
                           , xpText
                           , xpAttr
                           , xpWrap
                           , xpAlt
                           , xpList
                           , xpOption
                           , xpPair
                           , xpTriple
                           , xpElemNS
                           -- , xpAddNSDecl
                           , xpFilterCont
                           , xpFilterAttr

                           , QName
                           , mkQName
                           , mkName
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
                           , xunpickleDocument
                           , withValidate
                           , withTrace
                           , withRemoveWS
                           , withPreserveComment
                           , withCheckNamespaces
                           , yes
                           , no

                           , XmlTree
                           , readDocument
                           , (&&&)
                           , getAttrValue
                           , getChildren
                           )

import Text.XML.HXT.Curl   ( withCurl )

import Control.Applicative ( (<$>) )

import Data.Map            ( empty
                           , lookup
                           , insert
                           , union
                           )

import Prelude hiding (lookup)

-- ----------------------------------------

data XmlSchema'        = XmlSchema'
                       { targetNS :: Maybe Namespace
                       , parts    :: [XmlSchemaPart]
                       }

data XmlSchemaPart     = In {unIn :: Include}
                       | St {unSt :: (QName, SimpleType)}
                       | Ct {unCt :: (QName, ComplexType)}
                       | El {unEl :: Element}
                       | Gr {unGr :: (QName, Group)}
                       | At {unAt :: Attribute}
                       | Ag {unAg :: (QName, AttributeGroup)}

-- ----------------------------------------

-- | ...
nsUri :: String
nsUri = "http://www.w3.org/2001/XMLSchema"

nsPrefix :: String
nsPrefix = "xs"

xpElem' :: String -> PU a -> PU a
xpElem' name
  = xpElemNS nsUri nsPrefix name

xpSchemaElem :: String -> PU a -> PU a
xpSchemaElem name
  = xpElem' name . xpFilterSchema

xpFilterSchema :: PU a -> PU a
xpFilterSchema
  = -- keep elems from xs namespace which are not blacklisted
    xpFilterCont (isXmlSchemaElem           >>> neg elementBlacklist) .
    -- keep attrs without namespace which are not blacklisted
    xpFilterAttr (isAttrWithoutNamespaceUri >>> neg attributeBlacklist)
    where
    -- element blacklist
    isXmlSchemaElem           = hasNameWith ((== nsUri) . namespaceUri)
    elementBlacklist          = foldr (<+>) none $ map hasQName $
                                [ mkQName nsPrefix "annotation"           nsUri
                                , mkQName nsPrefix "notation"             nsUri
                                -- skipped content for element
                                , mkQName nsPrefix "unique"               nsUri
                                , mkQName nsPrefix "key"                  nsUri
                                , mkQName nsPrefix "keyref"               nsUri
                                ]
    -- attribute blacklist
    isAttrWithoutNamespaceUri = hasNameWith (null       . namespaceUri)
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

xpQName :: PU QName
xpQName
  = xpWrap (mkName, qualifiedName) xpText -- TODO: namespaces / target namespace

xpQNames :: PU QNames
xpQNames
  = xpWrap ( \ x -> map mkName $ words x
           , \ x -> (qualifiedName $ head x) ++ (concat $ map (\ y -> ' ':(qualifiedName y)) (tail x))
           ) xpText

xpXmlSchema' :: PU XmlSchema'
xpXmlSchema'
  = xpSchemaElem "schema" $
    -- TODO: xpAddNSDecl nsPrefix nsUri $
    xpFilterSchema $
    xpWrap (\ (a, b) -> XmlSchema' a b , \ t -> (targetNS t, parts t)) $
    xpPair (xpOption $ xpAttr "targetNamespace" xpText) $
    xpList $ xpSchemaPart

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

xpInclude :: PU Include
xpInclude
  = xpAlt tag ps
    where
    tag (Incl _)  = 0
    tag (Imp _)   = 1
    tag (Redef _) = 2
    ps = [ xpWrap (Incl,  unIncl)  $ xpSchemaElem "include"  $ xpAttr "schemaLocation" xpText
         , xpWrap (Imp,   unImp)   $ xpSchemaElem "import"   $ xpPair (xpAttr "schemaLocation" xpText) (xpAttr "namespace" xpText)
         , xpWrap (Redef, unRedef) $ xpSchemaElem "redefine" $ xpPair (xpAttr "schemaLocation" xpText) (xpList xpRedefinition)
         ]

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

xpSTRestriction :: PU STRestriction
xpSTRestriction
  = xpPair xpSimpleTypeRef $ xpList $ xpRestrAttr

xpSimpleTypeRef :: PU SimpleTypeRef
xpSimpleTypeRef
  = xpAlt tag ps
    where
    tag (BaseAttr _)        = 0
    tag (STRAnonymStDecl _) = 1
    ps = [ xpWrap (BaseAttr,        unBaseAttr)        $ xpAttr "base" xpQName
         , xpWrap (STRAnonymStDecl, unSTRAnonymStDecl) $ xpSchemaElem "simpleType" $ xpSimpleType
         ]

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

xpSTList :: PU STList
xpSTList
  = xpAlt tag ps
    where
    tag (ItemTypeAttr _)    = 0
    tag (STLAnonymStDecl _) = 1
    ps = [ xpWrap (ItemTypeAttr,    unItemTypeAttr)    $ xpAttr "itemType" xpQName
         , xpWrap (STLAnonymStDecl, unSTLAnonymStDecl) $ xpSchemaElem "simpleType" $ xpSimpleType
         ]

xpSTUnion :: PU STUnion
xpSTUnion
  = xpWrap (\ (a, b) -> STUnion a b , \ t -> (memberTypes t, anonymDecls t)) $
    xpPair (xpOption $ xpAttr "memberTypes" xpQNames) $
    xpList $ xpSchemaElem "simpleType" $ xpSimpleType

xpComplexType :: PU ComplexType
xpComplexType
  = xpWrap (\ (a, b) -> ComplexType a b , \ t -> (ctMixed t, ctDef t)) $
    xpPair (xpOption $ xpAttr "mixed" xpText) xpCTDef

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

xpSimpleContent :: PU SimpleContent
xpSimpleContent
  = xpAlt tag ps
    where
    tag (SCExt _)   = 0
    tag (SCRestr _) = 1
    ps = [ xpWrap (SCExt,   unSCExt)   $ xpSchemaElem "extension"   $ xpPair (xpAttr "base" xpQName) xpAttrList
         , xpWrap (SCRestr, unSCRestr) $ xpSchemaElem "restriction" $ xpPair xpSTRestriction xpAttrList 
         ]

xpComplexContent :: PU ComplexContent
xpComplexContent
  = xpWrap (\ (a, b) -> ComplexContent a b , \ t -> (ccMixed t, ccDef t)) $
    xpPair (xpOption $ xpAttr "mixed" xpText) xpCCDef

xpCCDef :: PU CCDef
xpCCDef
  = xpAlt tag ps
    where
    tag (CCExt _)   = 0
    tag (CCRestr _) = 1
    ps = [ xpWrap (CCExt,   unCCExt)   $ xpSchemaElem "extension"   $ xpPair (xpAttr "base" xpQName) $ xpCTModel
         , xpWrap (CCRestr, unCCRestr) $ xpSchemaElem "restriction" $ xpPair (xpAttr "base" xpQName) $ xpCTModel
         ]

xpCTModel :: PU CTModel
xpCTModel
  = xpPair (xpOption xpCTCompositor) xpAttrList

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

xpMinMaxOcc :: PU MinMaxOcc
xpMinMaxOcc
  = xpWrap (\ (a, b) -> MinMaxOcc a b , \ t -> (minOcc t, maxOcc t)) $
    xpPair (xpOption $ xpAttr "minOccurs" xpText) (xpOption $ xpAttr "maxOccurs" xpText)

xpAll :: PU All
xpAll
  = xpList $ xpSchemaElem "element" $ xpPair xpMinMaxOcc xpElement

xpSchemaChoice :: PU Choice
xpSchemaChoice
  = xpList xpChSeqContent

xpSequence :: PU Sequence
xpSequence
  = xpList xpChSeqContent

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

xpAny :: PU Any
xpAny
  = xpWrap (\ (a, b) -> Any a b , \ t -> (namespace t, processContents t)) $
    xpPair (xpOption $ xpAttr "namespace" xpText) (xpOption $ xpAttr "processContents" xpText)

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

xpElement :: PU Element
xpElement
  = xpAlt tag ps
    where
    tag (ElRef _) = 0
    tag (ElDef _) = 1
    ps = [ xpWrap (ElRef, unElRef) $ xpAttr "ref" xpQName
         , xpWrap (ElDef, unElDef) $ xpElementDef
         ]

xpElementDef :: PU ElementDef
xpElementDef
  = xpWrap (\ (a, b) -> ElementDef a b, \ t -> (elemName t, elemTypeDef t)) $
    xpPair (xpAttr "name" xpQName) xpElemTypeDef

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

xpGroup :: PU Group
xpGroup
  = xpAlt tag ps
    where
    tag (GrpRef _) = 0
    tag (GrpDef _) = 1
    ps = [ xpWrap (GrpRef, unGrpRef) $ xpAttr "ref" xpQName
         , xpWrap (GrpDef, unGrpDef) $ xpOption $ xpGroupDef
         ]

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

xpAttribute :: PU Attribute
xpAttribute
  = xpAlt tag ps
    where
    tag (AttrRef _) = 0
    tag (AttrDef _) = 1
    ps = [ xpWrap (AttrRef, unAttrRef) $ xpAttr "ref" xpQName
         , xpWrap (AttrDef, unAttrDef) $ xpAttributeDef
         ]

xpAttributeDef :: PU AttributeDef
xpAttributeDef
  = xpWrap (\ (a, b, c) -> AttributeDef a b c, \ t -> (attrName t, attrTypeDef t, attrUse t)) $
    xpTriple (xpAttr "name" xpQName) xpAttrTypeDef (xpOption $ xpAttr "use" xpText)

xpAttrTypeDef :: PU AttrTypeDef
xpAttrTypeDef
  = xpAlt tag ps
    where
    tag (ATDTypeAttr _)   = 0
    tag (ATDAnonymDecl _) = 1
    ps = [ xpWrap (ATDTypeAttr,   unATDTypeAttr)   $ xpAttr "type" xpQName
         , xpWrap (ATDAnonymDecl, unATDAnonymDecl) $ xpSchemaElem "simpleType" $ xpSimpleType
         ]

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

-- | Conversion from Schema' to Schema
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
      = toSchemaRec (XmlSchema' tns xs) ins sts cts (insert (elemName (unElDef el)) el els) grs ats ags
    toSchemaRec (XmlSchema' tns ((Gr (k, gr)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els (insert k gr grs) ats ags
    toSchemaRec (XmlSchema' tns ((At at)     :xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els grs (insert (attrName (unAttrDef at)) at ats) ags
    toSchemaRec (XmlSchema' tns ((Ag (k, ag)):xs)) ins sts cts els grs ats ags
      = toSchemaRec (XmlSchema' tns xs) ins sts cts els grs ats (insert k ag ags)

-- ----------------------------------------

-- Processing of includes

resolveIncls :: XmlSchema -> Includes -> IO XmlSchema
resolveIncls s []
  = return s
resolveIncls s (x:xs)
  = do
    incl' <- resolveIncl x
    case incl' of
      Nothing   -> resolveIncls s xs
      Just incl -> resolveIncls (mergeSchemata s incl) xs

resolveIncl :: Include -> IO (Maybe XmlSchema) -- TODO: apply namespaces
resolveIncl (Incl loc)
  = loadDefinition loc
resolveIncl (Imp (loc, _))       
  = loadDefinition loc
resolveIncl (Redef (loc, redefs))
  = do
    s' <- loadDefinition loc
    case s' of
      Nothing -> return Nothing
      Just s  -> return $ Just $ applyRedefs s redefs

applyRedefs :: XmlSchema -> Redefinitions -> XmlSchema
applyRedefs s []
  = s
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefSt (k, st')):xs)
  = applyRedefs (XmlSchema tns ins sts'' cts els grs ats ags) xs
    where
    k'    = setLocalPart' (newXName ((localPart k) ++ "_redef")) k
    sts'  = case lookup k sts of
              Just st -> insert k' st sts
              Nothing -> sts
    sts'' = case st' of
              (Restr (BaseAttr _, rlist)) -> insert k (Restr (BaseAttr k', rlist)) sts'
              _                           -> sts
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefCt (k, ct')):xs)
  = applyRedefs (XmlSchema tns ins sts cts'' els grs ats ags) xs
    where
    k'    = setLocalPart' (newXName ((localPart k) ++ "_redef")) k
    cts'  = case lookup k cts of
              Just ct -> insert k' ct cts
              Nothing -> cts
    cts'' = case ct' of
              (ComplexType ctm (CCont (ComplexContent ccm (CCExt   (_, m))))) ->
                insert k (ComplexType ctm (CCont (ComplexContent ccm (CCExt   (k', m))))) cts'
              (ComplexType ctm (CCont (ComplexContent ccm (CCRestr (_, m))))) ->
                insert k (ComplexType ctm (CCont (ComplexContent ccm (CCRestr (k', m))))) cts'
              _                                                               -> cts
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefGr (k, gr')):xs)
  = applyRedefs (XmlSchema tns ins sts cts els (insert k gr' grs) ats ags) xs
applyRedefs (XmlSchema tns ins sts cts els grs ats ags) ((RedefAg (k, ag')):xs)
  = applyRedefs (XmlSchema tns ins sts cts els grs ats (insert k ag' ags)) xs

mergeSchemata :: XmlSchema -> XmlSchema -> XmlSchema
mergeSchemata (XmlSchema tns _ sts cts els grs ats ags) (XmlSchema _ _ sts' cts' els' grs' ats' ags')
  = XmlSchema tns [] (union sts sts') (union cts cts') (union els els') (union grs grs') (union ats ats') (union ags ags')

-- ----------------------------------------

-- Load schema from given url
loadDefinition :: String -> IO (Maybe XmlSchema)
loadDefinition uri
  = do
    s' <- runX (
                -- TODO: readDocument, add namespaces to each node, finally unpickle
                xunpickleDocument xpXmlSchema'
                                  [ withValidate yes        -- validate source
                                  , withTrace 0             -- trace processing steps?
                                  , withRemoveWS yes        -- remove redundant whitespace
                                  , withPreserveComment no  -- keep comments
                                  , withCheckNamespaces yes -- check namespaces
                                  , withCurl []             -- use libCurl for http access
                                  ] uri
               )
    if null s'
      then return Nothing
      else do
           let s = toSchema $ head s'
           Just <$> resolveIncls s (sIncludes s)

-- ----------------------------------------

loadInstance :: String -> IO (Maybe XmlTree)
loadInstance uri
  = do
    s <- runX ( readDocument [ withValidate yes        -- validate source
                             , withTrace 0             -- trace processing steps?
                             , withRemoveWS yes        -- remove redundant whitespace
                             , withPreserveComment no  -- remove comments
                             , withCheckNamespaces yes -- check namespaces
                             , withCurl []             -- use libCurl for http access
                             ] uri
                >>>
                ((getAttrValue "status") &&& getChildren)
              )
    if (fst $ head s) == ""
      then return $ Just (snd $ head s)
      else return Nothing

