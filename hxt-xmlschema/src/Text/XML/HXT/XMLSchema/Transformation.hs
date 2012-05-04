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

-- | Creates a SimpleType test function which does not allow any text
mkNoTextSTTF :: STTF
mkNoTextSTTF
  = \ s -> do
           if not $ null $ unwords $ words s
             then mkErrorSTTF' "no text allowed here."
             else return True

-- | Creates a SimpleType test function which always succeeds
mkPassthroughSTTF :: STTF
mkPassthroughSTTF
  = \ _ -> return True

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
                  Just msg -> mkErrorSTTF' msg

-- | Creates the SimpleType test function for a given type reference by name
lookupSTTF :: QName -> ST STTF
lookupSTTF n
  = do
    s <- ask
    case lookup n $ sSimpleTypes s of
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
           return $ tf1res && tf2res

-- | Accumulates SimpleType restrictions to create a combined SimpleType test function
rstrToSTTF :: STRestriction -> ST STTF
rstrToSTTF (tref, rlist)
  = do
    s <- ask
    let t' = case tref of
               BaseAttr n        -> case lookup n $ sSimpleTypes s of
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
                    if not $ and $ fst $ runSVal env $ mapM baseTF $ words x
                      then mkErrorSTTF' "value does not match list type."
                      else return True

stToSTTF (Un ts)
  = do
    trefTFs <- case memberTypes ts of
                 Nothing    -> return []
                 Just trefs -> mapM lookupSTTF trefs
    tdefTFs <- mapM stToSTTF $ anonymDecls ts
    return $ \ x -> do
                    env <- ask
                    if not $ or $ fst $ runSVal env $ mapM (\ f -> f x) $ trefTFs ++ tdefTFs
                      then mkErrorSTTF' "value does not match union type."
                      else return True

-- ----------------------------------------

-- | Creates an AttrMap entry for a given attribute
createAttrMapEntry :: Attribute -> ST (QName, AttrMapVal)
createAttrMapEntry (AttrRef n)
  = do
    s <- ask
    case lookup n $ sAttributes s of
           Just a  -> createAttrMapEntry a
           Nothing -> do
                      let errorSTTF = mkErrorSTTF $
                                      "attribute validation error: illegal attribute reference in schema file"
                      return (n, (False, errorSTTF))

createAttrMapEntry (AttrDef (AttributeDef n tdef use))
  = do
    let req = maybe False (== "required") use
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
mkTextRE = mkPrim isText

-- ----------------------------------------

-- | Creates an element description for elements without subelems
mkSimpleElemDesc :: AttrDesc -> STTF -> ElemDesc
mkSimpleElemDesc ad
  = ElemDesc Nothing ad False mkTextRE empty

-- | Creates an element description for elements without attributes or textual content
mkComposeElemDesc :: XmlRegex -> SubElemDesc -> ElemDesc
mkComposeElemDesc cm se
  = ElemDesc Nothing (empty, []) False cm se mkPassthroughSTTF

-- | Creates a general element description
mkElemDesc :: AttrDesc -> XmlRegex -> SubElemDesc -> STTF -> ElemDesc
mkElemDesc ad
  = ElemDesc Nothing ad False

-- | Creates a general element description with mixed content flag
mkElemDescMixed :: AttrDesc -> Bool -> XmlRegex -> SubElemDesc -> STTF -> ElemDesc
mkElemDescMixed
  = ElemDesc Nothing

-- | Creates an element description which passes an error message
mkErrorElemDesc :: String -> ElemDesc
mkErrorElemDesc s
  = ElemDesc (Just s) (empty, []) False mkUnit empty mkPassthroughSTTF

-- | Creates the element description for a given group
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

-- | Extracts an element's name
elementToName :: Element -> QName
elementToName e
  = case e of
      ElRef r -> r
      ElDef d -> elemName d

-- | Helper function to combine a list of element descriptions using a regex constructor
combineElemDescs :: ([XmlRegex] -> XmlRegex) -> [ElemDesc] -> ST ElemDesc
combineElemDescs mkRE eds
  = do
    let re = mkRE $ map contentModel eds
    let se = foldr union empty $ map subElemDesc eds
    return $ mkComposeElemDesc re se

-- | Creates the element description for a given all
allToElemDesc :: All -> ST ElemDesc
allToElemDesc l
  = do
    eds <- mapM (\ (occ, el) -> do 
                                ed <- createElemDesc el
                                let n = elementToName el
                                return $ mkComposeElemDesc (mkMinMaxRE occ $ mkElemNameRE n) $ fromList [(n, ed)]
                ) l
    combineElemDescs mkPerms eds

-- | Transforms a wildcard into a namespace checking predicate
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

-- | Creates the element description for a given element wildcard
anyToElemDesc :: Any -> ST ElemDesc
anyToElemDesc an
  = do
    re <- mkElemNamespaceRE <$> anyToPredicate an
    return $ mkComposeElemDesc re empty

-- | Helper function to create a pair from two values
mkPair :: a -> b -> (a, b)
mkPair x y = (x, y)

-- | Create the element description for a choice or sequence content item
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
                         return $ mkComposeElemDesc (mkMinMaxRE occ $ mkElemNameRE n)  $ fromList [(n, ed)]
      _               -> return $ mkComposeElemDesc (mkMinMaxRE occ $ contentModel ed) $ subElemDesc ed

-- | Creates the element description for a given choice
choiceToElemDesc :: Choice -> ST ElemDesc
choiceToElemDesc l
  = mapM chSeqContToElemDesc l >>= combineElemDescs mkAlts

-- | Creates the element description for a given sequence
sequenceToElemDesc :: Sequence -> ST ElemDesc
sequenceToElemDesc l
  = mapM chSeqContToElemDesc l >>= combineElemDescs mkSeqs

-- | Attempts to read an integer value from a string
readMaybeInt :: String -> Maybe Int
readMaybeInt str
  = val $ reads str
    where
    val [(x, "")] = Just x
    val _         = Nothing

-- | Creates a regex wrapper for restrictions on the number of occurrences
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
    minOcc' = maybe "1" id $ minOcc occ
    maxOcc' = maybe "1" id $ maxOcc occ

-- | Creates the element description for a given ComplexType compositor
compToElemDesc :: CTCompositor -> ST ElemDesc
compToElemDesc c
  = do
    (occ, ed) <- case c of
                   CompGr (occ, gr) -> mkPair occ <$> groupToElemDesc gr
                   CompAl (occ, al) -> mkPair occ <$> allToElemDesc al
                   CompCh (occ, ch) -> mkPair occ <$> choiceToElemDesc ch
                   CompSq (occ, sq) -> mkPair occ <$> sequenceToElemDesc sq
    return $ mkElemDesc (attrDesc ed) (mkMinMaxRE occ $ contentModel ed) (subElemDesc ed) $ sttf ed

-- | Combines two given attribute descriptions
mergeAttrDescs :: AttrDesc -> AttrDesc -> AttrDesc
mergeAttrDescs ad ad'
  = (union (fst ad) $ fst ad', snd ad ++ snd ad')

-- | Creates the element description for a given ComplexType model
ctModelToElemDesc :: CTModel -> ST ElemDesc
ctModelToElemDesc (comp, attrs)
  = do
    ad <- attrListToAttrDesc attrs
    case comp of
      Nothing -> return $ mkElemDesc ad mkUnit empty mkNoTextSTTF
      Just c  -> do
                 ed <- compToElemDesc c
                 return $ mkElemDesc (mergeAttrDescs ad $ attrDesc ed) (contentModel ed) (subElemDesc ed) $ sttf ed

-- | Creates the element description for a given simple content
--   Accumulates the restrictions to create the combined element description
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

-- | Creates the element description for a given ComplexType
ctToElemDesc :: ComplexType -> ST ElemDesc
ctToElemDesc ct
  = do
    s <- ask
    case ctDef ct of
      SCont sc      -> simpleContentToElemDesc sc (empty, []) []
      CCont cc      ->
        do
        let mixed = case ccMixed cc of
                      Just "true"  -> True
                      Just "false" -> False
                      _            -> ctMixed ct == Just "true"
        case ccDef cc of
          CCExt   (n, m) -> case lookup n $ sComplexTypes s of
                              Nothing  -> return $ mkErrorElemDesc
                                                   "element validation error: illegal type reference in schema file"
                              Just ct' -> do
                                          base <- ctToElemDesc ct'
                                          ed <- ctModelToElemDesc m
                                          return $ mkElemDescMixed (mergeAttrDescs (attrDesc ed) $ attrDesc base)
                                                                   (mixed)
                                                                   (mkSeq (contentModel base) $ contentModel ed)
                                                                   (union (subElemDesc ed) $ subElemDesc base)
                                                                   (sttf base)
          CCRestr (n, m) -> case lookup n $ sComplexTypes s of
                              Nothing  -> return $ mkErrorElemDesc
                                                   "element validation error: illegal type reference in schema file"
                              Just ct' -> do
                                          base <- ctToElemDesc ct'
                                          ed <- ctModelToElemDesc m
                                          return $ mkElemDescMixed (mergeAttrDescs (attrDesc ed) $ attrDesc base)
                                                                   (mixed)
                                                                   (contentModel ed)
                                                                   (subElemDesc ed)
                                                                   (sttf ed)
      NewCT m       -> do
                       ed <- ctModelToElemDesc m
                       let mixed = ctMixed ct == Just "true"
                       return $ mkElemDescMixed (attrDesc ed)
                                                (mixed)
                                                (contentModel ed)
                                                (subElemDesc ed)
                                                (sttf ed)

-- | Creates the element description for a given element
createElemDesc :: Element -> ST ElemDesc
createElemDesc (ElRef n)
  = do
    s <- ask
    case lookup n $ sElements s of
           Just e  -> createElemDesc e
           Nothing -> return $ mkErrorElemDesc "element validation error: illegal element reference in schema file"
createElemDesc (ElDef (ElementDef _ tdef))
  = do
    s <- ask
    t <- case tdef of
           ETDTypeAttr r      -> case lookup r $ sComplexTypes s of
                                   Nothing  -> Left <$> lookupSTTF r
                                   Just ctr -> return $ Right ctr
           ETDAnonymStDecl st -> Left <$> stToSTTF st
           ETDAnonymCtDecl ct -> return $ Right ct
    case t of
      Left tf  -> return $ mkSimpleElemDesc (empty, []) tf 
      Right ct -> ctToElemDesc ct

-- ----------------------------------------

-- | Creates the element description for the root element
createRootDesc' :: ST ElemDesc
createRootDesc'
  = do
    s <- ask
    let cm = mkAlts $ map mkElemNameRE $ keys $ sElements s
    se <- fromList <$> zip (keys $ sElements s) <$> (mapM createElemDesc $ elems $ sElements s)
    return $ mkComposeElemDesc cm se

-- | Starts the transformation for a given schema representation
createRootDesc :: XmlSchema -> ElemDesc
createRootDesc schema
  = runST schema createRootDesc'

-- ----------------------------------------
