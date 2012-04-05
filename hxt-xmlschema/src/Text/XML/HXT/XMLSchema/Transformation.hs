{- |
   Module     : Text.XML.HXT.XMLSchema.Transformation
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Contains functions to transform the internal schema representation
   into structures which will be used during validation.
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

-- | Schema transformation monad
type ST a = ReaderT XmlSchema Identity a

-- | Runs a computation in the schema transformation monad
runST :: XmlSchema -> ST a -> a
runST schema st = runIdentity $ runReaderT st schema

-- ----------------------------------------

-- | Creates a SimpleType test function which creates a warning and always succeeds
mkWarnSTTF :: String -> STTF
mkWarnSTTF s
  = \ _ -> do
           env <- ask
           tell [(xpath env, s)]
           return True

-- | Creates a SimpleType test function which creates an error and always fails
mkErrorSTTF :: String -> STTF
mkErrorSTTF s
  = \ _ -> do
           env <- ask
           tell [(xpath env, s)]
           return False

-- | Creates a SimpleType test function which does not allow any text
mkNoTextSTTF :: STTF
mkNoTextSTTF
  = \ s -> do
           env <- ask
           if not $ null $ unwords $ words s
             then do
                  tell [(xpath env, "no text allowed here.")]
                  return False
             else return True

-- | Creates a SimpleType test function for basic W3C datatypes
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

-- | Creates the SimpleType test function for a given type reference by name
lookupSTTF :: QName -> ST STTF
lookupSTTF n
  = do
    s <- ask
    case lookup n (sSimpleTypes s) of
      Just t  -> stToSTTF t
      Nothing -> return $ mkW3CCheckSTTF n []

-- | Combines two given lists of restriction params
mergeRestrAttrs :: RestrAttrs -> RestrAttrs -> RestrAttrs
mergeRestrAttrs rlist rlist'
  = rlist ++ rlist'

-- | Converts a list of restriction params into the ParamList datatype
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
                           -- TODO: extend W3CDataTypeCheck
                           -- Enumeration v    -> [(xsd_enumeration,    v)]
                           -- WhiteSpace v     -> [(xsd_whiteSpace,     v)]
                           _                -> []

                 ) rlist

-- | Creates a SimpleType test function which applies two given SimpleType test functions
checkBothSTTF :: STTF -> STTF -> STTF
checkBothSTTF tf1 tf2
  = \ v -> do
           tf1res <- tf1 v
           tf2res <- tf2 v
           return (tf1res && tf2res)

-- | Accumulates SimpleType restrictions to create a combined SimpleType test function
rstrToSTTF :: STRestriction -> ST STTF
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
                    (Lst _)                 -> checkBothSTTF (mkWarnSTTF "no restriction checks implemented for lists.")
                                                         <$> stToSTTF t
                                               -- TODO: length, minLength, maxLength, pattern, enumeration, whiteSpace
                    (Un _)                  -> checkBothSTTF (mkWarnSTTF "no restriction checks implemented for unions.")
                                                         <$> stToSTTF t
                                               -- TODO: pattern, enumeration
      Right tf -> return tf

-- | Creates a SimpleType test function for a given SimpleType
stToSTTF :: SimpleType -> ST STTF
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

-- | Creates an AttrMap entry for a given attribute
createAttrMapEntry :: Attribute -> ST (QName, AttrMapVal)
createAttrMapEntry (AttrRef n)
  = do
    s <- ask
    case lookup n (sAttributes s) of
           Just a  -> createAttrMapEntry a
           Nothing -> do
                      let errorSTTF = mkErrorSTTF $
                                      "attribute validation error: illegal attribute reference in schema file"
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

-- | Transforms an attribute group into an attribute list
attrGrpToAttrList :: AttributeGroup -> ST AttrList
attrGrpToAttrList g
  = do
    s <- ask
    case g of
      AttrGrpRef n -> case lookup n $ sAttributeGroups s of
                        Nothing -> return []
                        Just g' -> attrGrpToAttrList g'
      AttrGrpDef l -> return l

-- | Helper function to wrap values into a one-element list
box :: a -> [a]
box x = [x]

-- | Extracts an attribute map from an attribute list
attrListToAttrMap :: AttrList -> ST [(QName, AttrMapVal)]
attrListToAttrMap l
  = concat <$> mapM (\ x -> case x of
                              Attr    a -> box <$> createAttrMapEntry a
                              AttrGrp g -> attrGrpToAttrList g >>= attrListToAttrMap
                              _         -> return []
                    ) l

-- | Extracts attribute wildcards from an attribute list
attrListToAttrWildcards :: AttrList -> ST AttrWildcards
attrListToAttrWildcards l
  = concat <$> mapM (\ x -> case x of
                              AnyAttr a -> box <$> anyToPredicate a
                              _         -> return []
                    ) l

-- | Transforms an attribute list into an attribute description
attrListToAttrDesc :: AttrList -> ST AttrDesc
attrListToAttrDesc l
  = do
    attrMap <- fromList <$> attrListToAttrMap l
    mkPair attrMap <$> attrListToAttrWildcards l

-- ----------------------------------------

-- | Creates a regex which matches on an element with a given name
mkElemNameRE :: QName -> XmlRegex
mkElemNameRE s = mkPrim $ (== s) . getElemName

-- | Creates a regex which matches on an element with a given namespace predicate
mkElemNamespaceRE :: (QName -> Bool) -> XmlRegex
mkElemNamespaceRE p = mkPrim $ p . getElemName

-- | Creates a regex which matches on text nodes
mkTextRE :: XmlRegex
mkTextRE = mkPrim $ isText

-- | Creates a regex which applies the mixed-behaviour
mkMixedRE :: Bool -> XmlRegex -> XmlRegex
mkMixedRE mixed re
  = if mixed
      then mkAlt re mkTextRE -- TODO: regex for mixed
      else re

-- ----------------------------------------

-- | Creates an element description for elements without subelems
mkSimpleElemDesc :: AttrDesc -> STTF -> ElemDesc
mkSimpleElemDesc ad tf
  = ElemDesc Nothing ad mkTextRE empty tf

-- | Creates an element description for elements without attributes or textual content
mkComposeElemDesc :: XmlRegex -> SubElemDesc -> ElemDesc
mkComposeElemDesc cm se
  = ElemDesc Nothing (empty, []) cm se mkNoTextSTTF

-- | Creates a general element description
mkElemDesc :: AttrDesc -> XmlRegex -> SubElemDesc -> STTF -> ElemDesc
mkElemDesc ad cm se tf
  = ElemDesc Nothing ad cm se tf

-- | Creates an element description which passes an error message
mkErrorElemDesc :: String -> ElemDesc
mkErrorElemDesc s
  = ElemDesc (Just s) (empty, []) mkUnit empty mkNoTextSTTF

-- | 
groupToElemDesc :: Group -> ST ElemDesc
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

combineElemDescs :: ([XmlRegex] -> XmlRegex) -> [ElemDesc] -> ST ElemDesc
combineElemDescs mkRE eds
  = do
    let re = mkRE $ map contentModel eds
    let se = foldr union empty $ map subElemDesc eds
    return $ mkComposeElemDesc re se

allToElemDesc :: All -> ST ElemDesc
allToElemDesc l
  = do
    eds <- mapM (\ (occ, el) -> do 
                                ed <- createElemDesc el
                                let n = elementToName el
                                return $ mkComposeElemDesc (mkMinMaxRE occ $ mkElemNameRE n) (fromList [(n, ed)])
                ) l
    combineElemDescs mkPerms eds

anyToPredicate :: Any -> ST (QName -> Bool)
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

anyToElemDesc :: Any -> ST ElemDesc
anyToElemDesc an
  = do
    re <- mkElemNamespaceRE <$> anyToPredicate an
    return $ mkComposeElemDesc re empty

mkPair :: a -> b -> (a, b)
mkPair x y = (x, y)

chSeqContToElemDesc :: ChSeqContent -> ST ElemDesc
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

choiceToElemDesc :: Choice -> ST ElemDesc
choiceToElemDesc l
  = mapM chSeqContToElemDesc l >>= combineElemDescs mkAlts

sequenceToElemDesc :: Sequence -> ST ElemDesc
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

compToElemDesc :: CTCompositor -> ST ElemDesc
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

ctModelToElemDesc :: CTModel -> ST ElemDesc
ctModelToElemDesc (comp, attrs)
  = do
    ad <- attrListToAttrDesc attrs
    case comp of
      Nothing -> return $ mkElemDesc ad mkUnit empty mkNoTextSTTF
      Just c  -> do
                 ed <- compToElemDesc c
                 return $ mkElemDesc (mergeAttrDescs ad $ attrDesc ed) (contentModel ed) (subElemDesc ed) (sttf ed)

simpleContentToElemDesc :: SimpleContent -> AttrDesc -> RestrAttrs -> ST ElemDesc
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
                                   _        -> return $ mkErrorElemDesc $
                                                        "element validation error: illegal type reference in schema file"
      STRAnonymStDecl _ -> mkSimpleElemDesc ad' <$> rstrToSTTF (tref, mergedRlist)

ctToElemDesc :: ComplexType -> ST ElemDesc
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

createElemDesc :: Element -> ST ElemDesc
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

createRootDesc' :: ST ElemDesc
createRootDesc'
  = do
    s <- ask
    let cm = mkAlts $ map mkElemNameRE $ keys $ sElements s
    se <- fromList <$> zip (keys (sElements s)) <$> (mapM createElemDesc $ elems $ sElements s)
    return $ mkComposeElemDesc cm se

createRootDesc :: XmlSchema -> ElemDesc
createRootDesc schema
  = runST schema createRootDesc'

