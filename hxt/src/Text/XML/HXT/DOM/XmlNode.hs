{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DOM.XmlNode
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT


   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Interface for XmlArrow to basic data types NTree and XmlTree

   If this module must be used in code working with arrows,
   it should be imported qualified e.g. @as XN@, to prevent name clashes.

   For code working on the \"node and tree level\" this module
   is the interface for writing code without using the
   constructor functions of 'XNode' and 'NTree' directly

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DOM.XmlNode
    ( module Text.XML.HXT.DOM.XmlNode
    , module Data.Tree.Class
    , module Data.Tree.NTree.TypeDefs
    )
where

import Control.Monad
import Control.FlatSeq

import Data.Function            ( on )
import Data.Maybe               ( fromMaybe
                                , fromJust
                                )
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs


import Text.XML.HXT.DOM.Interface

class XmlNode a where
    -- discriminating predicates

    isText              :: a -> Bool
    isBlob              :: a -> Bool
    isCharRef           :: a -> Bool
    isEntityRef         :: a -> Bool
    isCmt               :: a -> Bool
    isCdata             :: a -> Bool
    isPi                :: a -> Bool
    isElem              :: a -> Bool
    isRoot              :: a -> Bool
    isDTD               :: a -> Bool
    isAttr              :: a -> Bool
    isError             :: a -> Bool

    -- constructor functions for leave nodes

    mkText              :: String -> a
    mkBlob              :: Blob   -> a
    mkCharRef           :: Int    -> a
    mkEntityRef         :: String -> a
    mkCmt               :: String -> a
    mkCdata             :: String -> a
    mkPi                :: QName  -> XmlTrees -> a
    mkError             :: Int    -> String   -> a

    -- selectors

    getText             :: a -> Maybe String
    getBlob             :: a -> Maybe Blob
    getCharRef          :: a -> Maybe Int
    getEntityRef        :: a -> Maybe String
    getCmt              :: a -> Maybe String
    getCdata            :: a -> Maybe String
    getPiName           :: a -> Maybe QName
    getPiContent        :: a -> Maybe XmlTrees
    getElemName         :: a -> Maybe QName
    getAttrl            :: a -> Maybe XmlTrees
    getDTDPart          :: a -> Maybe DTDElem
    getDTDAttrl         :: a -> Maybe Attributes
    getAttrName         :: a -> Maybe QName
    getErrorLevel       :: a -> Maybe Int
    getErrorMsg         :: a -> Maybe String

    -- derived selectors

    getName             :: a -> Maybe QName
    getQualifiedName    :: a -> Maybe String
    getUniversalName    :: a -> Maybe String
    getUniversalUri     :: a -> Maybe String
    getLocalPart        :: a -> Maybe String
    getNamePrefix       :: a -> Maybe String
    getNamespaceUri     :: a -> Maybe String

    -- "modifier" functions

    changeText          :: (String   -> String)   -> a -> a
    changeBlob          :: (Blob     -> Blob)     -> a -> a
    changeCmt           :: (String   -> String)   -> a -> a
    changeName          :: (QName    -> QName)    -> a -> a
    changeElemName      :: (QName    -> QName)    -> a -> a
    changeAttrl         :: (XmlTrees -> XmlTrees) -> a -> a
    changeAttrName      :: (QName    -> QName)    -> a -> a
    changePiName        :: (QName    -> QName)    -> a -> a
    changeDTDAttrl      :: (Attributes -> Attributes) -> a -> a

    setText             :: String   -> a -> a
    setBlob             :: Blob     -> a -> a
    setCmt              :: String   -> a -> a
    setName             :: QName    -> a -> a
    setElemName         :: QName    -> a -> a
    setElemAttrl        :: XmlTrees -> a -> a
    setAttrName         :: QName    -> a -> a
    setPiName           :: QName    -> a -> a
    setDTDAttrl         :: Attributes -> a -> a

    -- default implementations

    getName n           = getElemName n `mplus` getAttrName n `mplus` getPiName n
    getQualifiedName n  = getName n >>= return . qualifiedName
    getUniversalName n  = getName n >>= return . universalName
    getUniversalUri n   = getName n >>= return . universalUri
    getLocalPart n      = getName n >>= return . localPart
    getNamePrefix n     = getName n >>= return . namePrefix
    getNamespaceUri n   = getName n >>= return . namespaceUri

    setText             = changeText     . const
    setBlob             = changeBlob     . const
    setCmt              = changeCmt      . const
    setName             = changeName     . const
    setElemName         = changeElemName . const
    setElemAttrl        = changeAttrl    . const
    setAttrName         = changeAttrName . const
    setPiName           = changePiName   . const
    setDTDAttrl         = changeDTDAttrl . const

-- XNode and XmlTree are instances of XmlNode

instance XmlNode XNode where
    isText (XText _)            = True
    isText (XBlob _)            = True
    isText _                    = False
    {-# INLINE isText #-}

    isBlob (XBlob _)            = True
    isBlob _                    = False
    {-# INLINE isBlob #-}

    isCharRef (XCharRef _)      = True
    isCharRef _                 = False
    {-# INLINE isCharRef #-}

    isEntityRef (XEntityRef _)  = True
    isEntityRef _               = False
    {-# INLINE isEntityRef #-}

    isCmt (XCmt _)              = True
    isCmt _                     = False
    {-# INLINE isCmt #-}

    isCdata (XCdata _)          = True
    isCdata _                   = False
    {-# INLINE isCdata #-}

    isPi (XPi _ _)              = True
    isPi _                      = False
    {-# INLINE isPi #-}

    isElem (XTag _ _)           = True
    isElem _                    = False
    {-# INLINE isElem #-}

    isRoot t                    = isElem t
                                  &&
                                  fromMaybe "" (getQualifiedName t) == t_root

    isDTD (XDTD _ _)            = True
    isDTD _                     = False
    {-# INLINE isDTD #-}

    isAttr (XAttr _)            = True
    isAttr _                    = False
    {-# INLINE isAttr #-}

    isError (XError _ _)        = True
    isError _                   = False
    {-# INLINE isError #-}


    mkText                      = XText
    {-# INLINE mkText #-}
    mkBlob                      = XBlob
    {-# INLINE mkBlob #-}
    mkCharRef                   = XCharRef
    {-# INLINE mkCharRef #-}
    mkEntityRef                 = XEntityRef
    {-# INLINE mkEntityRef #-}
    mkCmt                       = XCmt
    {-# INLINE mkCmt #-}
    mkCdata                     = XCdata
    {-# INLINE mkCdata #-}
    mkPi                        = XPi
    {-# INLINE mkPi #-}

    mkError                     = XError
    {-# INLINE mkError #-}


    getText (XText t)           = Just   t
    getText (XBlob b)           = Just . blobToString $ b
    getText _                   = Nothing
    {-# INLINE getText #-}

    getBlob (XBlob b)           = Just b
    getBlob _                   = Nothing
    {-# INLINE getBlob #-}

    getCharRef (XCharRef c)     = Just c
    getCharRef _                = Nothing
    {-# INLINE getCharRef #-}

    getEntityRef (XEntityRef e) = Just e
    getEntityRef _              = Nothing
    {-# INLINE getEntityRef #-}

    getCmt (XCmt c)             = Just c
    getCmt _                    = Nothing
    {-# INLINE getCmt #-}

    getCdata (XCdata d)         = Just d
    getCdata _                  = Nothing
    {-# INLINE getCdata #-}

    getPiName (XPi n _)         = Just n
    getPiName _                 = Nothing
    {-# INLINE getPiName #-}

    getPiContent (XPi _ c)      = Just c
    getPiContent _              = Nothing
    {-# INLINE getPiContent #-}

    getElemName (XTag n _)      = Just n
    getElemName _               = Nothing
    {-# INLINE getElemName #-}

    getAttrl (XTag _ al)        = Just al
    getAttrl (XPi  _ al)        = Just al
    getAttrl _                  = Nothing
    {-# INLINE getAttrl #-}

    getDTDPart (XDTD p _)       = Just p
    getDTDPart _                = Nothing
    {-# INLINE getDTDPart #-}

    getDTDAttrl (XDTD _ al)     = Just al
    getDTDAttrl _               = Nothing
    {-# INLINE getDTDAttrl #-}

    getAttrName (XAttr n)       = Just n
    getAttrName _               = Nothing
    {-# INLINE getAttrName #-}

    getErrorLevel (XError l _)  = Just l
    getErrorLevel _             = Nothing
    {-# INLINE getErrorLevel #-}

    getErrorMsg (XError _ m)    = Just m
    getErrorMsg _               = Nothing
    {-# INLINE getErrorMsg #-}

    changeText cf (XText t)             = XText . cf $ t
    changeText cf (XBlob b)             = XText . cf . blobToString $ b
    changeText _ _                      = error "changeText undefined"
    {-# INLINE changeText #-}

    changeBlob cf (XBlob b)             = XBlob . cf $ b
    changeBlob _ _                      = error "changeBlob undefined"
    {-# INLINE changeBlob #-}

    changeCmt cf (XCmt c)               = XCmt  . cf $ c
    changeCmt _ _                       = error "changeCmt undefined"
    {-# INLINE changeCmt #-}

    changeName cf (XTag n al)           = XTag   (cf n) al
    changeName cf (XAttr n)             = XAttr . cf $ n
    changeName cf (XPi n al)            = XPi    (cf n) al
    changeName _ _                      = error "changeName undefined"
    {-# INLINE changeName #-}

    changeElemName cf (XTag n al)       = XTag   (cf n) al
    changeElemName _ _                  = error "changeElemName undefined"
    {-# INLINE changeElemName #-}

    changeAttrl cf (XTag n al)          = XTag n (cf al)
    changeAttrl cf (XPi  n al)          = XPi  n (cf al)
    changeAttrl _ _                     = error "changeAttrl undefined"
    {-# INLINE changeAttrl #-}

    changeAttrName cf (XAttr n)         = XAttr . cf $ n
    changeAttrName _ _                  = error "changeAttrName undefined"
    {-# INLINE changeAttrName #-}

    changePiName cf (XPi n al)          = XPi    (cf n) al
    changePiName _ _                    = error "changePiName undefined"
    {-# INLINE changePiName #-}

    changeDTDAttrl cf (XDTD p al)       = XDTD p (cf al)
    changeDTDAttrl _ _                  = error "changeDTDAttrl undefined"
    {-# INLINE changeDTDAttrl #-}

mkElementNode   :: QName -> XmlTrees -> XNode
mkElementNode   = XTag
{-# INLINE mkElementNode #-}

mkAttrNode      :: QName -> XNode
mkAttrNode      = XAttr
{-# INLINE mkAttrNode #-}

mkDTDNode       :: DTDElem -> Attributes -> XNode
mkDTDNode       = XDTD
{-# INLINE mkDTDNode #-}

instance (XmlNode a, Tree t) => XmlNode (t a) where
    isText              = isText      . getNode
    {-# INLINE isText #-}
    isBlob              = isBlob      . getNode
    {-# INLINE isBlob #-}
    isCharRef           = isCharRef   . getNode
    {-# INLINE isCharRef #-}
    isEntityRef         = isEntityRef . getNode
    {-# INLINE isEntityRef #-}
    isCmt               = isCmt       . getNode
    {-# INLINE isCmt #-}
    isCdata             = isCdata     . getNode
    {-# INLINE isCdata #-}
    isPi                = isPi        . getNode
    {-# INLINE isPi #-}
    isElem              = isElem      . getNode
    {-# INLINE isElem #-}
    isRoot              = isRoot      . getNode
    {-# INLINE isRoot #-}
    isDTD               = isDTD       . getNode
    {-# INLINE isDTD #-}
    isAttr              = isAttr      . getNode
    {-# INLINE isAttr #-}
    isError             = isError     . getNode
    {-# INLINE isError #-}

    mkText              = mkLeaf . mkText
    {-# INLINE mkText #-}
    mkBlob              = mkLeaf . mkBlob
    {-# INLINE mkBlob #-}
    mkCharRef           = mkLeaf . mkCharRef
    {-# INLINE mkCharRef #-}
    mkEntityRef         = mkLeaf . mkEntityRef
    {-# INLINE mkEntityRef #-}
    mkCmt               = mkLeaf . mkCmt
    {-# INLINE mkCmt #-}
    mkCdata             = mkLeaf . mkCdata
    {-# INLINE mkCdata #-}
    mkPi n              = mkLeaf . mkPi n
    {-# INLINE mkPi #-}
    mkError l           = mkLeaf . mkError l
    {-# INLINE mkError #-}

    getText             = getText       . getNode
    {-# INLINE getText #-}
    getBlob             = getBlob       . getNode
    {-# INLINE getBlob #-}
    getCharRef          = getCharRef    . getNode
    {-# INLINE getCharRef #-}
    getEntityRef        = getEntityRef  . getNode
    {-# INLINE getEntityRef #-}
    getCmt              = getCmt        . getNode
    {-# INLINE getCmt #-}
    getCdata            = getCdata      . getNode
    {-# INLINE getCdata #-}
    getPiName           = getPiName     . getNode
    {-# INLINE getPiName #-}
    getPiContent        = getPiContent  . getNode
    {-# INLINE getPiContent #-}
    getElemName         = getElemName   . getNode
    {-# INLINE getElemName #-}
    getAttrl            = getAttrl      . getNode
    {-# INLINE getAttrl #-}
    getDTDPart          = getDTDPart    . getNode
    {-# INLINE getDTDPart #-}
    getDTDAttrl         = getDTDAttrl   . getNode
    {-# INLINE getDTDAttrl #-}
    getAttrName         = getAttrName   . getNode
    {-# INLINE getAttrName #-}
    getErrorLevel       = getErrorLevel . getNode
    {-# INLINE getErrorLevel #-}
    getErrorMsg         = getErrorMsg   . getNode
    {-# INLINE getErrorMsg #-}

    changeText          = changeNode . changeText
    {-# INLINE changeText #-}
    changeBlob          = changeNode . changeBlob
    {-# INLINE changeBlob #-}
    changeCmt           = changeNode . changeCmt
    {-# INLINE changeCmt #-}
    changeName          = changeNode . changeName
    {-# INLINE changeName #-}
    changeElemName      = changeNode . changeElemName
    {-# INLINE changeElemName #-}
    changeAttrl         = changeNode . changeAttrl
    {-# INLINE changeAttrl #-}
    changeAttrName      = changeNode . changeAttrName
    {-# INLINE changeAttrName #-}
    changePiName        = changeNode . changePiName
    {-# INLINE changePiName #-}
    changeDTDAttrl      = changeNode . changeDTDAttrl
    {-# INLINE changeDTDAttrl #-}

mkElement       :: QName -> XmlTrees -> XmlTrees -> XmlTree
mkElement n al  = mkTree (mkElementNode n al)
{-# INLINE mkElement #-}

mkRoot          :: XmlTrees -> XmlTrees -> XmlTree
mkRoot al       = mkTree (mkElementNode (mkName t_root) al)

mkAttr          :: QName -> XmlTrees -> XmlTree
mkAttr n        = mkTree (mkAttrNode n)
{-# INLINE mkAttr #-}

mkDTDElem       :: DTDElem -> Attributes -> XmlTrees -> XmlTree
mkDTDElem e al  = mkTree (mkDTDNode e al)

addAttr         :: XmlNode xn => xn -> [xn] -> [xn]
addAttr a al
    | isAttr a  = add al
    | otherwise = al
    where
    an = (qualifiedName . fromJust . getAttrName) a
    add []
        = [a]
    add (a1:al1)
        | isAttr a1
          &&
          (qualifiedName . fromJust . getAttrName) a1 == an
            = a : al1
        | otherwise
            = a1 : add al1

mergeAttrl      :: XmlNode xn => [xn] -> [xn] -> [xn]
mergeAttrl      = foldr addAttr

-- ------------------------------------------------------------

-- | weak normalform versions of constructors

mkElement'              :: QName -> XmlTrees -> XmlTrees -> XmlTree
mkElement' n al cl      = id $!! mkElement n al cl
{-# INLINE mkElement' #-}

mkRoot'                 :: XmlTrees -> XmlTrees -> XmlTree
mkRoot' al cl           = id $!! mkRoot al cl
{-# INLINE mkRoot' #-}

mkAttr'                 :: QName -> XmlTrees -> XmlTree
mkAttr' n av            = id $!! mkAttr n av
{-# INLINE mkAttr' #-}

mkText'                 :: String -> XmlTree
mkText' t               = id $!! mkText t
{-# INLINE mkText' #-}

mkCharRef'              :: Int    -> XmlTree
mkCharRef' i            = id $!! mkCharRef i
{-# INLINE mkCharRef' #-}

mkEntityRef'            :: String -> XmlTree
mkEntityRef' n          = id $!! mkEntityRef n
{-# INLINE mkEntityRef' #-}

mkCmt'                  :: String -> XmlTree
mkCmt' c                = id $!! mkCmt c
{-# INLINE mkCmt' #-}

mkCdata'                :: String -> XmlTree
mkCdata' d              = id $!! mkCdata d
{-# INLINE mkCdata' #-}

mkPi'                   :: QName  -> XmlTrees -> XmlTree
mkPi' n v               = id $!! mkPi n v
{-# INLINE mkPi' #-}

mkError'                :: Int -> String   -> XmlTree
mkError' l m            = id $!! mkError l m
{-# INLINE mkError' #-}

mkDTDElem'              :: DTDElem -> Attributes -> XmlTrees -> XmlTree
mkDTDElem' e al cl      = id $!! mkDTDElem e al cl
{-# INLINE mkDTDElem' #-}

-- ------------------------------------------------------------

toText :: XmlTree -> XmlTree
toText t
    | isCharRef t
        = mkText
          . (:[]) . toEnum
          . fromJust
          . getCharRef
          $ t
    | isCdata t
        = mkText
          . fromJust
          . getCdata
          $ t
    | otherwise
        = t

concText :: XmlTree -> XmlTree -> XmlTrees
concText t1 t2
    | isText t1 && isText t2
        = (:[]) . mkText $ fromJust (getText t1) ++ fromJust (getText t2)
    | otherwise
        = [t1, t2]

mergeText :: XmlTree -> XmlTree -> XmlTrees
mergeText
    = concText `on` toText

-- ------------------------------------------------------------
