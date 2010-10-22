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
    , module Data.Tree.NTree.TypeDefs
    )
where

import Control.Monad

import Data.Maybe
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

    isBlob (XBlob _)            = True
    isBlob _                    = False

    isCharRef (XCharRef _)      = True
    isCharRef _                 = False

    isEntityRef (XEntityRef _)  = True
    isEntityRef _               = False

    isCmt (XCmt _)              = True
    isCmt _                     = False

    isCdata (XCdata _)          = True
    isCdata _                   = False

    isPi (XPi _ _)              = True
    isPi _                      = False

    isElem (XTag _ _)           = True
    isElem _                    = False

    isRoot t                    = isElem t
                                  &&
                                  fromMaybe "" (getQualifiedName t) == t_root

    isDTD (XDTD _ _)            = True
    isDTD _                     = False

    isAttr (XAttr _)            = True
    isAttr _                    = False

    isError (XError _ _)        = True
    isError _                   = False


    mkText                      = XText
    mkBlob                      = XBlob
    mkCharRef                   = XCharRef
    mkEntityRef                 = XEntityRef
    mkCmt                       = XCmt
    mkCdata                     = XCdata
    mkPi n c                    = XPi n (if null c then [] else [mkAttr (mkName a_value) c])
    mkError                     = XError


    getText (XText t)           = Just   t
    getText (XBlob b)           = Just . blobToString $ b
    getText _                   = Nothing

    getBlob (XBlob b)           = Just b
    getBlob _                   = Nothing

    getCharRef (XCharRef c)     = Just c
    getCharRef _                = Nothing

    getEntityRef (XEntityRef e) = Just e
    getEntityRef _              = Nothing

    getCmt (XCmt c)             = Just c
    getCmt _                    = Nothing

    getCdata (XCdata d)         = Just d
    getCdata _                  = Nothing

    getPiName (XPi n _)         = Just n
    getPiName _                 = Nothing

    getPiContent (XPi _ c)      = Just c
    getPiContent _              = Nothing

    getElemName (XTag n _)      = Just n
    getElemName _               = Nothing

    getAttrl (XTag _ al)        = Just al
    getAttrl (XPi  _ al)        = Just al
    getAttrl _                  = Nothing

    getDTDPart (XDTD p _)       = Just p
    getDTDPart _                = Nothing

    getDTDAttrl (XDTD _ al)     = Just al
    getDTDAttrl _               = Nothing

    getAttrName (XAttr n)       = Just n
    getAttrName _               = Nothing

    getErrorLevel (XError l _)  = Just l
    getErrorLevel _             = Nothing

    getErrorMsg (XError _ m)    = Just m
    getErrorMsg _               = Nothing

    changeText cf (XText t)             = XText . cf $ t
    changeText cf (XBlob b)             = XText . cf . blobToString $ b
    changeText _ _                      = error "changeText undefined"

    changeBlob cf (XBlob b)             = XBlob . cf $ b
    changeBlob _ _                      = error "changeBlob undefined"

    changeCmt cf (XCmt c)               = XCmt  . cf $ c
    changeCmt _ _                       = error "changeCmt undefined"

    changeName cf (XTag n al)           = XTag   (cf n) al
    changeName cf (XAttr n)             = XAttr . cf $ n
    changeName cf (XPi n al)            = XPi    (cf n) al
    changeName _ _                      = error "changeName undefined"

    changeElemName cf (XTag n al)       = XTag   (cf n) al
    changeElemName _ _                  = error "changeElemName undefined"

    changeAttrl cf (XTag n al)          = XTag n (cf al)
    changeAttrl cf (XPi  n al)          = XPi  n (cf al)
    changeAttrl _ _                     = error "changeAttrl undefined"

    changeAttrName cf (XAttr n)         = XAttr . cf $ n
    changeAttrName _ _                  = error "changeAttrName undefined"

    changePiName cf (XPi n al)          = XPi    (cf n) al
    changePiName _ _                    = error "changeAttrName undefined"

    changeDTDAttrl cf (XDTD p al)       = XDTD p (cf al)
    changeDTDAttrl _ _                  = error "changeDTDAttrl undefined"

mkElementNode   :: QName -> XmlTrees -> XNode
mkElementNode   = XTag

mkAttrNode      :: QName -> XNode
mkAttrNode      = XAttr

mkDTDNode       :: DTDElem -> Attributes -> XNode
mkDTDNode       = XDTD

instance XmlNode a => XmlNode (NTree a) where
    isText              = isText      . getNode
    isBlob              = isBlob      . getNode
    isCharRef           = isCharRef   . getNode
    isEntityRef         = isEntityRef . getNode
    isCmt               = isCmt       . getNode
    isCdata             = isCdata     . getNode
    isPi                = isPi        . getNode
    isElem              = isElem      . getNode
    isRoot              = isRoot      . getNode
    isDTD               = isDTD       . getNode
    isAttr              = isAttr      . getNode
    isError             = isError     . getNode

    mkText              = mkLeaf . mkText
    mkBlob              = mkLeaf . mkBlob
    mkCharRef           = mkLeaf . mkCharRef
    mkEntityRef         = mkLeaf . mkEntityRef
    mkCmt               = mkLeaf . mkCmt
    mkCdata             = mkLeaf . mkCdata
    mkPi n              = mkLeaf . mkPi n
    mkError l           = mkLeaf . mkError l

    getText             = getText       . getNode
    getBlob             = getBlob       . getNode
    getCharRef          = getCharRef    . getNode
    getEntityRef        = getEntityRef  . getNode
    getCmt              = getCmt        . getNode
    getCdata            = getCdata      . getNode
    getPiName           = getPiName     . getNode
    getPiContent        = getPiContent  . getNode
    getElemName         = getElemName   . getNode
    getAttrl            = getAttrl      . getNode
    getDTDPart          = getDTDPart    . getNode
    getDTDAttrl         = getDTDAttrl   . getNode
    getAttrName         = getAttrName   . getNode
    getErrorLevel       = getErrorLevel . getNode
    getErrorMsg         = getErrorMsg   . getNode

    changeText          = changeNode . changeText
    changeBlob          = changeNode . changeBlob
    changeCmt           = changeNode . changeCmt
    changeName          = changeNode . changeName
    changeElemName      = changeNode . changeElemName
    changeAttrl         = changeNode . changeAttrl
    changeAttrName      = changeNode . changeAttrName
    changePiName        = changeNode . changePiName
    changeDTDAttrl      = changeNode . changeDTDAttrl

mkElement       :: QName -> XmlTrees -> XmlTrees -> XmlTree
mkElement n al  = mkTree (mkElementNode n al)

mkRoot          :: XmlTrees -> XmlTrees -> XmlTree
mkRoot al       = mkTree (mkElementNode (mkName t_root) al)

mkAttr          :: QName -> XmlTrees -> XmlTree
mkAttr n        = mkTree (mkAttrNode n)

mkDTDElem       :: DTDElem -> Attributes -> XmlTrees -> XmlTree
mkDTDElem e al  = mkTree (mkDTDNode e al)

addAttr         :: XmlTree -> XmlTrees -> XmlTrees
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

mergeAttrl      :: XmlTrees -> XmlTrees -> XmlTrees
mergeAttrl      = foldr addAttr

-- ------------------------------------------------------------
