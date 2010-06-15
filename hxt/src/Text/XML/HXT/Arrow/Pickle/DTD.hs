-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.Pickle.DTD
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

Functions for converting a pickler schema
into a DTD

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.Pickle.DTD
where

import           Data.Maybe

import qualified Text.XML.HXT.DOM.XmlNode as XN

import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.Arrow.Pickle.Schema
import           Text.XML.HXT.RelaxNG.XmlSchema.DataTypeLibW3C

-- ------------------------------------------------------------

data DTDdescr                   = DTDdescr Name Schemas [(Name,Schemas)]

instance Show DTDdescr where
    show (DTDdescr n es as)
        = "root element: " ++ n ++ "\n"
          ++
          "elements:\n"
          ++
          concatMap ((++ "\n") .show) es
          ++
          "attributes:\n"
          ++
          concatMap ((++ "\n") . showAttr) as
        where
        showAttr (n1, sc) = n1 ++ ": " ++ show sc

-- ------------------------------------------------------------

-- | convert a DTD descr into XmlTrees

dtdDescrToXml   :: DTDdescr -> XmlTrees
dtdDescrToXml (DTDdescr rt es as)
    = checkErr (null rt) "no unique root element found in pickler DTD, add an \"xpElem\" pickler"
      ++
      concatMap (checkErr True . ("no element decl found in: " ++) . show) (filter (not . isScElem) es)
      ++
      concatMap (uncurry checkContentModell . \ (Element n sc) -> (n,sc)) es1
      ++
      concatMap (uncurry checkAttrModell) as
      ++
      [ XN.mkDTDElem DOCTYPE docAttrs ( concatMap elemDTD es1
                                        ++
                                        concatMap (uncurry attrDTDs) as
                                      ) ]
    where
    es1                 = filter isScElem es

    docAttrs            = [(a_name, if null rt then "no-unique-root-element-found" else rt)]

    elemDTD (Element n sc)
        | lookup1 a_type al == "unknown"
            = cl
        | otherwise
            = [ XN.mkDTDElem ELEMENT ((a_name, n) : al) cl ]
        where
        (al, cl) = scContToXml sc
    elemDTD _
        = error "illegal case in elemDTD"

    attrDTDs en         = concatMap (attrDTD en)
    attrDTD en (Attribute an sc)
                        = [ XN.mkDTDElem ATTLIST ((a_name, en) : (a_value, an) : al) cl ]
                          where
                          (al, cl) = scAttrToXml sc
    attrDTD _ _         = error "illegal case in attrDTD"


checkAttrModell                                 :: Name -> Schemas -> XmlTrees
checkAttrModell n                               = concatMap (checkAM n)

checkAM                                         :: Name -> Schema -> XmlTrees
checkAM en (Attribute an sc)                    = checkAMC en an sc
checkAM _ _                                     = []

checkAMC                                        :: Name -> Name -> Schema -> XmlTrees
checkAMC _en _an (CharData _)                   = []
checkAMC en an sc
    | isScCharData sc   = []
    | isScList sc
      &&
      (sc_1 sc == scNmtoken)
                        = []
    | isScOpt sc        = checkAMC en an (sc_1 sc)
    | otherwise         = foundErr
                          ( "weird attribute type found for attribute "
                            ++ show an
                            ++ " for element "
                            ++ show en
                            ++ "\n\t(internal structure: " ++ show sc ++ ")"
                            ++ "\n\thint: create an element instead of an attribute for "
                            ++ show an
                          )

-- checkContentModell1 n sc = foundErr (n ++ " : " ++ show sc) ++ checkContentModell n sc

checkContentModell                              :: Name -> Schema -> XmlTrees

checkContentModell _ Any
    = []

checkContentModell _ (ElemRef _)
    = []

checkContentModell _ (CharData _)
    = []

checkContentModell _ (Seq [])
    = []

checkContentModell n (Seq scs)
    = checkErr pcDataInCM
      ( "PCDATA found in a sequence spec in the content modell for "
        ++ show n
        ++ "\n\thint: create an element for this data"
      )
      ++
      checkErr somethingElseInCM
      ( "something weired found in a sequence spec in the content modell for "
        ++ show n
      )
      ++
      concatMap (checkContentModell n) scs
    where
    pcDataInCM        = any isScCharData scs
    somethingElseInCM = any (\ sc -> not (isScSARE sc) && not (isScCharData sc)) scs

checkContentModell n (Alt scs)
    = checkErr mixedCM
      ( "PCDATA mixed up with illegal content spec in mixed contents for "
        ++ show n
        ++ "\n\thint: create an element for this data"
      )
      ++
      concatMap (checkContentModell n) scs
    where
    mixedCM
        | any isScCharData scs
            = any (not . isScElemRef) . filter (not . isScCharData) $ scs
        | otherwise
            = False

checkContentModell _ (Rep _ _ (ElemRef _))
    = []

checkContentModell n (Rep _ _ sc@(Seq _))
    = checkContentModell n sc

checkContentModell n (Rep _ _ sc@(Alt _))
    = checkContentModell n sc

checkContentModell n (Rep _ _ _)
    = foundErr
      ( "illegal content spec found for "
        ++ show n
      )

checkContentModell _ _
    = []


scContToXml                     :: Schema -> (Attributes, XmlTrees)

scContToXml Any                 = ( [(a_type, v_any)],    [] )
scContToXml (CharData _)        = ( [(a_type, v_pcdata)], [] )
scContToXml (Seq [])            = ( [(a_type, v_empty)],  [] )
scContToXml sc@(ElemRef _)      = scContToXml (Seq [sc])
scContToXml sc@(Seq _)          = ( [(a_type, v_children)]
                                  , scCont [] sc
                                  )
scContToXml sc@(Alt sc1)
    | isMixed sc1               = ( [(a_type, v_mixed)]
                                  , scCont [ (a_modifier, "*") ] sc
                                  )
    | otherwise                 = ( [(a_type, v_children)]
                                  , scCont [] sc
                                  )
    where
    isMixed                     = not . null . filter isScCharData
scContToXml sc@(Rep _ _ _)      = ( [(a_type, v_children)]
                                  , scCont [] sc
                                  )
scContToXml _sc                 = ( [(a_type, v_any)]           -- default: everything is allowed
                                  , []
                                  )

scWrap                          :: Schema -> Schema
scWrap sc@(Alt _)               = sc
scWrap sc@(Seq _)               = sc
scWrap sc@(Rep _ _  _)          = sc
scWrap sc                       = Seq [sc]

scCont                          :: Attributes -> Schema -> XmlTrees
scCont al (Seq scs)             = scConts ((a_kind, v_seq   ) : al) scs
scCont al (Alt scs)             = scConts ((a_kind, v_choice) : al) scs
scCont al (Rep 0 (-1) sc)       = scCont ((a_modifier, "*")   : al) (scWrap sc)
scCont al (Rep 1 (-1) sc)       = scCont ((a_modifier, "+")   : al) (scWrap sc)
scCont al (Rep 0 1    sc)       = scCont ((a_modifier, "?")   : al) (scWrap sc)
scCont al (ElemRef n)           = [XN.mkDTDElem NAME ((a_name, n) : al) []]
scCont _  (CharData _)          = [XN.mkDTDElem NAME [(a_name, "#PCDATA")] []]
scCont _  _sc                   = [XN.mkDTDElem NAME [(a_name, "bad-content-spec")] []]         -- error case

scConts                         :: Attributes -> Schemas -> XmlTrees
scConts al scs                  = [XN.mkDTDElem CONTENT al (concatMap (scCont []) scs)]

scAttrToXml                     :: Schema -> (Attributes, XmlTrees)

scAttrToXml sc
    | isScFixed sc              = ( [ (a_kind, k_fixed)
                                    , (a_type, k_cdata)
                                    , (a_default, (xsdParam xsd_enumeration sc))
                                    ]
                                  , [])
    | isScEnum sc               = ( [ (a_kind, k_required)
                                    , (a_type, k_enumeration)
                                    ]
                                  , map (\ n -> XN.mkDTDElem NAME [(a_name, n)] []) enums
                                  )
    | isScCharData sc           = ( [ (a_kind, k_required)
                                    , (a_type, d_type)
                                    ]
                                  , [])
    | isScOpt sc                = (addEntry a_kind k_implied al, cl)
    | isScList sc               = (addEntry a_type k_nmtokens al, cl)
    | otherwise                 = ( [ (a_kind, k_fixed)
                                    , (a_default, "bad-attribute-type: " ++ show sc)
                                    ]
                                  , [] )
    where
    (al, cl)                    = scAttrToXml (sc_1 sc)
    d_type
        | sc == scNmtoken       = k_nmtoken
        | otherwise             = k_cdata
    enums                       = words . xsdParam xsd_enumeration $ sc

checkErr                        :: Bool -> String -> XmlTrees
checkErr True s                 = [XN.mkError c_err s]
checkErr _    _                 = []

foundErr                        :: String -> XmlTrees
foundErr                        = checkErr True

-- ------------------------------------------------------------

-- | convert a pickler schema into a DTD descr

dtdDescr        :: Schema -> DTDdescr
dtdDescr sc
    = DTDdescr rt es1 as
    where
    es  = elementDeclarations sc
    es1 = map remAttrDec es
    as  = filter (not. null . snd) . concatMap attrDec $ es
    rt  = fromMaybe "" . elemName $ sc

elementDeclarations     :: Schema -> Schemas
elementDeclarations sc  = elemRefs . elementDecs [] $ [sc]

elementDecs             :: Schemas -> Schemas -> Schemas
elementDecs es []
    = es
elementDecs es (s:ss)
    = elementDecs (elemDecs s) ss
    where
    elemDecs (Seq scs)          = elementDecs es scs
    elemDecs (Alt scs)          = elementDecs es scs
    elemDecs (Rep _ _ sc)       = elemDecs sc
    elemDecs e@(Element n sc)
        | n `elem` elemNames es = es
        | otherwise             = elementDecs (e:es) [sc]
    elemDecs _                  = es

elemNames               :: Schemas -> [Name]
elemNames               = concatMap (maybeToList . elemName)

elemName                :: Schema -> Maybe Name
elemName (Element n _)  = Just n
elemName _              = Nothing

elemRefs        :: Schemas -> Schemas
elemRefs        = map elemRef
    where
    elemRef (Element n sc)   = Element n (pruneElem sc)
    elemRef sc               = sc
    pruneElem (Element n _)  = ElemRef n
    pruneElem (Seq scs)      = Seq (map pruneElem scs)
    pruneElem (Alt scs)      = Alt (map pruneElem scs)
    pruneElem (Rep l u sc)   = Rep l u (pruneElem sc)
    pruneElem sc             = sc

attrDec                 :: Schema -> [(Name, Schemas)]
attrDec (Element n sc)
    = [(n, attrDecs sc)]
      where
      attrDecs a@(Attribute _ _)        = [a]
      attrDecs (Seq scs)                = concatMap attrDecs scs
      attrDecs _                        = []
attrDec _               = []

remAttrDec              :: Schema -> Schema
remAttrDec (Element n sc)
    = Element n (remA sc)
      where
      remA (Attribute _ _) = scEmpty
      remA (Seq scs)       = scSeqs . map remA $ scs
      remA sc1             = sc1
remAttrDec _
    = error "illegal case in remAttrDec"

-- ------------------------------------------------------------

