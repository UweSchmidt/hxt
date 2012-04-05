{- |
   Module     : Text.XML.HXT.XMLSchema.Transformation
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

-}

module Text.XML.HXT.XMLSchema.Transformation

  ( createRootDesc )

where

import Text.XML.HXT.XMLSchema.XmlUtils
import Text.XML.HXT.XMLSchema.AbstractSyntax
import Text.XML.HXT.XMLSchema.W3CDataTypeCheck
import Text.XML.HXT.XMLSchema.ValidationTypes

import Text.XML.HXT.Core           ( QName
                                   , mkName
                                   , localPart
                                   , namespaceUri
                                   )

import Text.XML.HXT.Arrow.XmlRegex ( XmlRegex
                                   , mkZero
                                   , mkUnit
                                   , mkPrim
                                   , mkAlt
                                   , mkAlts
                                   , mkSeq
                                   , mkSeqs
                                   , mkRep
                                   , mkRng
                                   , mkPerms
                                   )

import Control.Monad.Identity      ( Identity
                                   , runIdentity
                                   )

import Control.Monad.Reader        ( ReaderT
                                   , runReaderT
                                   , ask
                                   )

import Control.Monad.Writer        ( tell )

import Control.Applicative         ( (<$>) )

import Data.Maybe                  ( fromMaybe )

import Data.Map                    ( empty
                                   , lookup
                                   , union
                                   , fromList
                                   , keys
                                   , elems
                                   )

import Prelude hiding ( lookup )

-- ----------------------------------------

-- | ...
type XSC a        = ReaderT XmlSchema Identity a

runXSC :: XmlSchema -> XSC a -> a
runXSC schema xsc = runIdentity $ runReaderT xsc schema

-- ----------------------------------------

-- Create SimpleType test functions

checkBothSTTF :: STTF -> STTF -> STTF
checkBothSTTF tf1 tf2
  = \ v -> do
           tf1res <- tf1 v
           tf2res <- tf2 v
           return (tf1res && tf2res)

mkNoTextSTTF :: STTF
mkNoTextSTTF
  = \ s -> do
           env <- ask
           if not $ null $ unwords $ words s
             then do
                  tell [(xpath env, "no text allowed here.")]
                  return False
             else return True

mkPassThroughSTTF :: STTF
mkPassThroughSTTF
  = \ _ -> return True

mkWarnSTTF :: String -> STTF
mkWarnSTTF s
  = \ _ -> do
           env <- ask
           tell [(xpath env, s)]
           return True

mkErrorSTTF :: String -> STTF
mkErrorSTTF s
  = \ _ -> do
           env <- ask
           tell [(xpath env, s)]
           return False

mkW3CCheckSTTF :: QName -> ParamList -> STTF
mkW3CCheckSTTF n p
  = if n `elem` [ mkName "xs:boolean" -- TODO: extend W3CDataTypeCheck
                , mkName "xs:float"
                , mkName "xs:double"
                , mkName "xs:time"
                , mkName "xs:duration"
                , mkName "xs:date"
                , mkName "xs:dateTime"
                , mkName "xs:gDay"
                , mkName "xs:gMonth"
                , mkName "xs:gMonthDay"
                , mkName "xs:gYear"
                , mkName "xs:gYearMonth"
                ]
    then mkWarnSTTF $ "no check implemented for W3C type " ++ (localPart n) ++ "."
    else \ v -> case datatypeAllowsW3C (localPart n) p v of
                  Nothing  -> return True
                  Just msg -> do
                              env <- ask
                              tell [(xpath env, msg)]
                              return False

lookupSTTF :: QName -> XSC STTF
lookupSTTF n
  = do
    s <- ask
    case lookup n (sSimpleTypes s) of
      Just t  -> stToSTTF t
      Nothing -> return $ mkW3CCheckSTTF n []

mergeRestrAttrs :: RestrAttrs -> RestrAttrs -> RestrAttrs
mergeRestrAttrs rlist rlist'
  = rlist ++ rlist'

restrAttrsToParamList :: RestrAttrs -> ParamList
restrAttrsToParamList rlist
  = concat $ map (\ x -> case x of
                           MinIncl v        -> [(xsd_minInclusive,   v)]
                           MaxIncl v        -> [(xsd_maxInclusive,   v)]
                           MinExcl v        -> [(xsd_minExclusive,   v)]
                           MaxExcl v        -> [(xsd_maxExclusive,   v)]
                           TotalDigits v    -> [(xsd_totalDigits,    v)]
                           FractionDigits v -> [(xsd_fractionDigits, v)]
                           Length v         -> [(xsd_length,         v)]
                           MinLength v      -> [(xsd_minLength,      v)]
                           MaxLength v      -> [(xsd_maxLength,      v)]
                           Pattern v        -> [(xsd_pattern,        v)]
                           -- Enumeration v    -> [(xsd_enumeration,    v)] -- TODO: extend W3CDataTypeCheck
                           -- WhiteSpace v     -> [(xsd_whiteSpace,     v)] -- TODO: extend W3CDataTypeCheck
                           _                -> []

                 ) rlist

rstrToSTTF :: STRestriction -> XSC STTF
rstrToSTTF (tref, rlist)
  = do
    s <- ask
    let t' = case tref of
               BaseAttr n        -> case lookup n (sSimpleTypes s) of
                                      Just t  -> Left t
                                      Nothing -> Right $ mkW3CCheckSTTF n $ restrAttrsToParamList rlist
               STRAnonymStDecl t -> Left t
    case t' of
      Left t   -> case t of
                    (Restr (tref', rlist')) -> rstrToSTTF (tref', mergeRestrAttrs rlist rlist')
                    (Lst _)                 -> checkBothSTTF (mkWarnSTTF "no restriction checks implemented for lists.") <$> stToSTTF t
                                               -- TODO: allowed: length, minLength, maxLength, pattern, enumeration, whiteSpace
                    (Un _)                  -> checkBothSTTF (mkWarnSTTF "no restriction checks implemented for unions.") <$> stToSTTF t
                                               -- TODO: allowed: pattern, enumeration
      Right tf -> return tf

stToSTTF :: SimpleType -> XSC STTF
stToSTTF (Restr rstr)
  = rstrToSTTF rstr
stToSTTF (Lst tref)
  = do
    baseTF  <- case tref of
                 ItemTypeAttr n    -> lookupSTTF n
                 STLAnonymStDecl t -> stToSTTF t
    return $ \ x -> do
                    env <- ask
                    if not $ foldr (&&) True $ fst $ runSVal env $ mapM baseTF $ words x
                      then do
                           tell [(xpath env, "value does not match list type.")]
                           return False
                      else return True
stToSTTF (Un ts)
  = do
    trefTFs <- case memberTypes ts of
                 Nothing    -> return []
                 Just trefs -> mapM lookupSTTF trefs
    tdefTFs <- mapM stToSTTF $ anonymDecls ts
    return $ \ x -> do
                    env <- ask
                    if not $ foldr (||) False $ fst $ runSVal env $ mapM (\ f -> f x) (trefTFs ++ tdefTFs)
                      then do
                           tell [(xpath env, "value does not match union type.")]
                           return False
                      else return True

-- ----------------------------------------

-- Create attribute descriptions

createAttrMapEntry :: Attribute -> XSC (QName, AttrMapVal)
createAttrMapEntry (AttrRef n)
  = do
    s <- ask
    case lookup n (sAttributes s) of
           Just a  -> createAttrMapEntry a
           Nothing -> do
                      let errorSTTF = mkErrorSTTF "attribute validation error: illegal attribute reference in schema file"
                      return (n, (False, errorSTTF))
createAttrMapEntry (AttrDef (AttributeDef n tdef use))
  = do
    let req = case use of
                Nothing -> False
                Just s  -> case s of
                             "required" -> True
                             _          -> False
    tf <- case tdef of
            ATDTypeAttr r   -> lookupSTTF r
            ATDAnonymDecl t -> stToSTTF t
    return (n, (req, tf))

attrGrpToAttrList :: AttributeGroup -> XSC AttrList
attrGrpToAttrList g
  = do
    s <- ask
    case g of
      AttrGrpRef n -> case lookup n $ sAttributeGroups s of
                        Nothing -> return []
                        Just g' -> attrGrpToAttrList g'
      AttrGrpDef l -> return l

attrListToAttrDesc :: AttrList -> XSC AttrDesc
attrListToAttrDesc l
  = do
    attrMap <- fromList <$> attrListToAttrMap l
    mkPair attrMap <$> attrListToAttrWildcards l

box :: a -> [a]
box x = [x]

attrListToAttrMap :: AttrList -> XSC [(QName, AttrMapVal)]
attrListToAttrMap l
  = concat <$> mapM (\ x -> case x of
                              Attr    a -> box <$> createAttrMapEntry a
                              AttrGrp g -> attrGrpToAttrList g >>= attrListToAttrMap
                              _         -> return []
                    ) l

attrListToAttrWildcards :: AttrList -> XSC AttrWildcards
attrListToAttrWildcards l
  = concat <$> mapM (\ x -> case x of
                              AnyAttr a -> box <$> anyToPredicate a
                              _         -> return []
                    ) l

-- ----------------------------------------

-- Regex constructors

mkElemNameRE :: QName -> XmlRegex
mkElemNameRE s = mkPrim $ (== s) . getElemName

mkElemNamespaceRE :: (QName -> Bool) -> XmlRegex
mkElemNamespaceRE p = mkPrim $ p . getElemName

mkTextRE :: XmlRegex
mkTextRE = mkPrim $ isText

mkMixedRE :: Bool -> XmlRegex -> XmlRegex
mkMixedRE mixed re
  = if mixed
      then mkAlt re mkTextRE -- TODO: regex for mixed
      else re

-- ----------------------------------------

-- Create element descriptions

mkErrorElemDesc :: String -> ElemDesc
mkErrorElemDesc s
  = ElemDesc (Just s) (empty, []) mkUnit empty mkPassThroughSTTF

mkSimpleElemDesc :: AttrDesc -> STTF -> ElemDesc
mkSimpleElemDesc ad tf
  = ElemDesc Nothing ad mkTextRE empty tf

mkComposeElemDesc :: XmlRegex -> SubElemDesc -> ElemDesc
mkComposeElemDesc cm se
  = ElemDesc Nothing (empty, []) cm se mkNoTextSTTF

mkElemDesc :: AttrDesc -> XmlRegex -> SubElemDesc -> STTF -> ElemDesc
mkElemDesc ad cm se tf
  = ElemDesc Nothing ad cm se tf

groupToElemDesc :: Group -> XSC ElemDesc
groupToElemDesc (GrpRef r)
  = do
    s <- ask
    case lookup r $ sGroups s of
      Nothing -> return $ mkErrorElemDesc "element validation error: illegal group reference in schema file"
      Just g  -> groupToElemDesc g
groupToElemDesc (GrpDef d)
  = case d of
      Nothing      -> return $ mkComposeElemDesc mkUnit empty
      Just (Al al) -> allToElemDesc al
      Just (Ch ch) -> choiceToElemDesc ch
      Just (Sq sq) -> sequenceToElemDesc sq

elementToName :: Element -> QName
elementToName e
  = case e of
      ElRef r -> r
      ElDef d -> elemName d

combineElemDescs :: ([XmlRegex] -> XmlRegex) -> [ElemDesc] -> XSC ElemDesc
combineElemDescs mkRE eds
  = do
    let re = mkRE $ map contentModel eds
    let se = foldr union empty $ map subElemDesc eds
    return $ mkComposeElemDesc re se

allToElemDesc :: All -> XSC ElemDesc
allToElemDesc l
  = do
    eds <- mapM (\ (occ, el) -> do 
                                ed <- createElemDesc el
                                let n = elementToName el
                                return $ mkComposeElemDesc (mkMinMaxRE occ $ mkElemNameRE n) (fromList [(n, ed)])
                ) l
    combineElemDescs mkPerms eds

anyToPredicate :: Any -> XSC (QName -> Bool)
anyToPredicate an
  = do
    s <- ask
    let ns = fromMaybe "##any" $ namespace an
    let p = case ns of
              "##any"   -> const True
              "##other" -> case sTargetNS s of
                             Nothing  -> (/= "")
                             Just tns -> (/= tns)
              _         -> (`elem` (map (\ x -> case x of
                                                  "##targetNamespace" -> fromMaybe "" $ sTargetNS s
                                                  "##local"           -> ""
                                                  _                   -> x
                                        ) $ words ns)
                           )
    return $ p . namespaceUri

anyToElemDesc :: Any -> XSC ElemDesc
anyToElemDesc an
  = do
    re <- mkElemNamespaceRE <$> anyToPredicate an
    return $ mkComposeElemDesc re empty

mkPair :: a -> b -> (a, b)
mkPair x y = (x, y)

chSeqContToElemDesc :: ChSeqContent -> XSC ElemDesc
chSeqContToElemDesc c
  = do
    (occ, ed) <- case c of
                   ChSeqEl (occ, el) -> mkPair occ <$> createElemDesc el
                   ChSeqGr (occ, gr) -> mkPair occ <$> groupToElemDesc gr
                   ChSeqCh (occ, ch) -> mkPair occ <$> choiceToElemDesc ch
                   ChSeqSq (occ, sq) -> mkPair occ <$> sequenceToElemDesc sq
                   ChSeqAn (occ, an) -> mkPair occ <$> anyToElemDesc an
    case c of
      ChSeqEl (_, el) -> do
                         let n = elementToName el
                         return $ mkComposeElemDesc (mkMinMaxRE occ $ mkElemNameRE n) (fromList [(n, ed)])
      _               -> return $ mkComposeElemDesc (mkMinMaxRE occ $ contentModel ed) (subElemDesc ed)

choiceToElemDesc :: Choice -> XSC ElemDesc
choiceToElemDesc l
  = mapM chSeqContToElemDesc l >>= combineElemDescs mkAlts

sequenceToElemDesc :: Sequence -> XSC ElemDesc
sequenceToElemDesc l
  = mapM chSeqContToElemDesc l >>= combineElemDescs mkSeqs

readMaybeInt :: String -> Maybe Int
readMaybeInt str
  = val $ reads str
    where
    val [(x, "")] = Just x
    val _         = Nothing

mkMinMaxRE :: MinMaxOcc -> XmlRegex -> XmlRegex
mkMinMaxRE occ re
  = case readMaybeInt minOcc' of
      Nothing       -> mkZero $ "element validation error: illegal minOccurs in schema file"
      Just minOcc'' -> if maxOcc' == "unbounded"
                         then mkRep minOcc'' re
                         else case readMaybeInt maxOcc' of
                                Nothing       -> mkZero $ "element validation error: illegal maxOccurs in schema file"
                                Just maxOcc'' -> mkRng minOcc'' maxOcc'' re
    where
    minOcc' = case minOcc occ of
                Nothing -> "1"
                Just i  -> i
    maxOcc' = case maxOcc occ of
                Nothing -> "1"
                Just i  -> i

compToElemDesc :: CTCompositor -> XSC ElemDesc
compToElemDesc c
  = do
    (occ, ed) <- case c of
                   CompGr (occ, gr) -> mkPair occ <$> groupToElemDesc gr
                   CompAl (occ, al) -> mkPair occ <$> allToElemDesc al
                   CompCh (occ, ch) -> mkPair occ <$> choiceToElemDesc ch
                   CompSq (occ, sq) -> mkPair occ <$> sequenceToElemDesc sq
    return $ mkElemDesc (attrDesc ed) (mkMinMaxRE occ $ contentModel ed) (subElemDesc ed) (sttf ed)

mergeAttrDescs :: AttrDesc -> AttrDesc -> AttrDesc
mergeAttrDescs ad ad'
  = (union (fst ad) $ fst ad', snd ad ++ snd ad')

ctModelToElemDesc :: CTModel -> XSC ElemDesc
ctModelToElemDesc (comp, attrs)
  = do
    ad <- attrListToAttrDesc attrs
    case comp of
      Nothing -> return $ mkElemDesc ad mkUnit empty mkNoTextSTTF
      Just c  -> do
                 ed <- compToElemDesc c
                 return $ mkElemDesc (mergeAttrDescs ad $ attrDesc ed) (contentModel ed) (subElemDesc ed) (sttf ed)

simpleContentToElemDesc :: SimpleContent -> AttrDesc -> RestrAttrs -> XSC ElemDesc
simpleContentToElemDesc (SCExt (n, attrs)) ad rlist
  = do
    s <- ask
    ad' <- mergeAttrDescs ad <$> attrListToAttrDesc attrs
    case lookup n $ sComplexTypes s of
      Nothing -> mkSimpleElemDesc ad' <$> rstrToSTTF (BaseAttr n, rlist)
      Just ct -> case ctDef ct of
                   SCont sc -> simpleContentToElemDesc sc ad' rlist
                   _        -> return $ mkErrorElemDesc "element validation error: illegal type reference in schema file"
simpleContentToElemDesc (SCRestr ((tref, rlist'), attrs)) ad rlist
  = do
    s <- ask
    ad' <- mergeAttrDescs ad <$> attrListToAttrDesc attrs
    let mergedRlist = mergeRestrAttrs rlist rlist'
    case tref of
      BaseAttr n -> case lookup n $ sComplexTypes s of
                      Nothing -> mkSimpleElemDesc ad' <$> rstrToSTTF (BaseAttr n, mergedRlist)
                      Just ct -> case ctDef ct of
                                   SCont sc -> simpleContentToElemDesc sc ad' mergedRlist
                                   _        -> return $ mkErrorElemDesc "element validation error: illegal type reference in schema file"
      STRAnonymStDecl _ -> mkSimpleElemDesc ad' <$> rstrToSTTF (tref, mergedRlist)

ctToElemDesc :: ComplexType -> XSC ElemDesc
ctToElemDesc ct
  = do
    s <- ask
    case ctDef ct of
      SCont sc      -> simpleContentToElemDesc sc (empty, []) []
      CCont cc      -> do
                       let mixed = case ccMixed cc of
                                     Just "true"  -> True
                                     Just "false" -> False
                                     _            -> case ctMixed ct of
                                                       Just "true" -> True
                                                       _           -> False
                       case ccDef cc of
                         CCExt   (n, m) -> case lookup n $ sComplexTypes s of
                                             Nothing  -> return $ mkErrorElemDesc
                                                         "element validation error: illegal type reference in schema file"
                                             Just ct' -> do
                                                         base <- ctToElemDesc ct'
                                                         ed <- ctModelToElemDesc m
                                                         return $ mkElemDesc (mergeAttrDescs (attrDesc ed) $ attrDesc base)
                                                                             (mkMixedRE mixed $
                                                                              mkSeq (contentModel base) $ contentModel ed)
                                                                             (union (subElemDesc ed) $ subElemDesc base)
                                                                             (sttf base)

                         CCRestr (n, m) -> case lookup n $ sComplexTypes s of
                                             Nothing  -> return $ mkErrorElemDesc
                                                         "element validation error: illegal type reference in schema file"
                                             Just ct' -> do
                                                         base <- ctToElemDesc ct'
                                                         ed <- ctModelToElemDesc m
                                                         return $ mkElemDesc (mergeAttrDescs (attrDesc ed) $ attrDesc base)
                                                                             (mkMixedRE mixed $ contentModel ed)
                                                                             (subElemDesc ed)
                                                                             (sttf ed)
      NewCT m       -> do
                       ed <- ctModelToElemDesc m
                       let mixed = case ctMixed ct of
                                     Just "true" -> True
                                     _           -> False
                       return $ mkElemDesc (attrDesc ed)
                                           (mkMixedRE mixed $ contentModel ed)
                                           (subElemDesc ed)
                                           (sttf ed)

createElemDesc :: Element -> XSC ElemDesc
createElemDesc (ElRef n)
  = do
    s <- ask
    case lookup n (sElements s) of
           Just e  -> createElemDesc e
           Nothing -> return $ mkErrorElemDesc "element validation error: illegal element reference in schema file"
createElemDesc (ElDef (ElementDef _ tdef))
  = do
    s <- ask
    t <- case tdef of
           ETDTypeAttr r      -> case lookup r (sComplexTypes s) of
                                   Nothing  -> Left <$> lookupSTTF r
                                   Just ctr -> return $ Right ctr
           ETDAnonymStDecl st -> Left <$> stToSTTF st
           ETDAnonymCtDecl ct -> return $ Right ct
    case t of
      Left tf  -> return $ mkSimpleElemDesc (empty, []) tf 
      Right ct -> ctToElemDesc ct

-- ----------------------------------------

createRootDesc' :: XSC ElemDesc
createRootDesc'
  = do
    s <- ask
    let cm = mkAlts $ map mkElemNameRE $ keys $ sElements s
    se <- fromList <$> zip (keys (sElements s)) <$> (mapM createElemDesc $ elems $ sElements s)
    return $ mkComposeElemDesc cm se

createRootDesc :: XmlSchema -> ElemDesc
createRootDesc schema
  = runXSC schema createRootDesc'

