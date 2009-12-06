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

import Data.Tree.NTree.TypeDefs

import Data.Maybe

import Text.XML.HXT.DOM.Interface

class XmlNode a where
    -- discriminating predicates

    isText		:: a -> Bool
    isCharRef		:: a -> Bool
    isEntityRef		:: a -> Bool
    isCmt		:: a -> Bool
    isCdata		:: a -> Bool
    isPi		:: a -> Bool
    isElem		:: a -> Bool
    isRoot		:: a -> Bool
    isDTD		:: a -> Bool
    isAttr		:: a -> Bool
    isError		:: a -> Bool

    -- constructor functions for leave nodes

    mkText		:: String -> a
    mkCharRef		:: Int    -> a
    mkEntityRef		:: String -> a
    mkCmt		:: String -> a
    mkCdata		:: String -> a
    mkPi		:: QName  -> XmlTrees -> a
    mkError		:: Int    -> String   -> a

    -- selectors

    getText		:: a -> Maybe String
    getCharRef		:: a -> Maybe Int
    getEntityRef	:: a -> Maybe String
    getCmt		:: a -> Maybe String
    getCdata		:: a -> Maybe String
    getPiName		:: a -> Maybe QName
    getPiContent	:: a -> Maybe XmlTrees
    getElemName		:: a -> Maybe QName
    getAttrl		:: a -> Maybe XmlTrees
    getDTDPart		:: a -> Maybe DTDElem
    getDTDAttrl		:: a -> Maybe Attributes
    getAttrName		:: a -> Maybe QName
    getErrorLevel	:: a -> Maybe Int
    getErrorMsg		:: a -> Maybe String

    -- derived selectors

    getName		:: a -> Maybe QName
    getQualifiedName	:: a -> Maybe String
    getUniversalName	:: a -> Maybe String
    getUniversalUri	:: a -> Maybe String
    getLocalPart	:: a -> Maybe String
    getNamePrefix	:: a -> Maybe String
    getNamespaceUri	:: a -> Maybe String

    -- "modifier" functions

    changeText		:: (String   -> String)   -> a -> a
    changeCmt		:: (String   -> String)   -> a -> a
    changeName		:: (QName    -> QName)    -> a -> a
    changeElemName	:: (QName    -> QName)    -> a -> a
    changeAttrl		:: (XmlTrees -> XmlTrees) -> a -> a
    changeAttrName	:: (QName    -> QName)    -> a -> a
    changePiName	:: (QName    -> QName)    -> a -> a
    changeDTDAttrl	:: (Attributes -> Attributes) -> a -> a

    setText		:: String   -> a -> a
    setCmt		:: String   -> a -> a
    setName		:: QName    -> a -> a
    setElemName		:: QName    -> a -> a
    setElemAttrl	:: XmlTrees -> a -> a
    setAttrName		:: QName    -> a -> a
    setPiName		:: QName    -> a -> a
    setDTDAttrl		:: Attributes -> a -> a

    -- default implementations

    getName n		= getElemName n `mplus` getAttrName n `mplus` getPiName n
    getQualifiedName n	= getName n >>= \ n' -> return (qualifiedName n')
    getUniversalName n	= getName n >>= \ n' -> return (universalName n')
    getUniversalUri n	= getName n >>= \ n' -> return (universalUri n')
    getLocalPart n	= getName n >>= \ n' -> return (localPart n')
    getNamePrefix n	= getName n >>= \ n' -> return (namePrefix n')
    getNamespaceUri n	= getName n >>= \ n' -> return (namespaceUri n')

    setText t		= changeText     (const t)
    setCmt c		= changeCmt      (const c)
    setName n		= changeName     (const n)
    setElemName n	= changeElemName (const n)
    setElemAttrl al	= changeAttrl    (const al)
    setAttrName	n	= changeAttrName (const n)
    setPiName	n	= changePiName   (const n)
    setDTDAttrl	al	= changeDTDAttrl (const al)

-- XNode and XmlTree are instances of XmlNode

instance XmlNode XNode where
    isText (XText _)		= True
    isText _			= False

    isCharRef (XCharRef _)	= True
    isCharRef _			= False

    isEntityRef (XEntityRef _)	= True
    isEntityRef _		= False

    isCmt (XCmt _)		= True
    isCmt _			= False

    isCdata (XCdata _)		= True
    isCdata _			= False

    isPi (XPi _ _)		= True
    isPi _			= False

    isElem (XTag _ _)		= True
    isElem _			= False

    isRoot t                    = isElem t
				  &&
				  fromMaybe "" (getQualifiedName t) == t_root

    isDTD (XDTD _ _)		= True
    isDTD _			= False

    isAttr (XAttr _)		= True
    isAttr _			= False

    isError (XError _ _)	= True
    isError _			= False


    mkText t			= XText t
    mkCharRef c			= XCharRef c
    mkEntityRef e		= XEntityRef e
    mkCmt c			= XCmt c
    mkCdata d			= XCdata d
    mkPi n c			= XPi n (if null c then [] else [mkAttr (mkName a_value) c])
    mkError l msg		= XError l msg


    getText (XText t)		= Just t
    getText _			= Nothing

    getCharRef (XCharRef c)	= Just c
    getCharRef _		= Nothing

    getEntityRef (XEntityRef e)	= Just e
    getEntityRef _		= Nothing

    getCmt (XCmt c)		= Just c
    getCmt _			= Nothing

    getCdata (XCdata d)		= Just d
    getCdata _			= Nothing

    getPiName (XPi n _)		= Just n
    getPiName _			= Nothing

    getPiContent (XPi _ c)	= Just c
    getPiContent _		= Nothing

    getElemName (XTag n _)	= Just n
    getElemName _		= Nothing

    getAttrl (XTag _ al)	= Just al
    getAttrl (XPi  _ al)	= Just al
    getAttrl _			= Nothing

    getDTDPart (XDTD p _)	= Just p
    getDTDPart _		= Nothing

    getDTDAttrl (XDTD _ al)	= Just al
    getDTDAttrl _		= Nothing

    getAttrName (XAttr n)	= Just n
    getAttrName _		= Nothing

    getErrorLevel (XError l _)	= Just l
    getErrorLevel _		= Nothing

    getErrorMsg (XError _ m)	= Just m
    getErrorMsg _		= Nothing

    changeText cf (XText t)		= XText (cf t)
    changeText _ _			= error "changeText undefined"

    changeCmt cf (XCmt c)		= XCmt (cf c)
    changeCmt _ _			= error "changeCmt undefined"

    changeName cf (XTag n al)		= XTag (cf n) al
    changeName cf (XAttr n)		= XAttr (cf n)
    changeName cf (XPi n al)		= XPi (cf n) al
    changeName _ _			= error "changeName undefined"

    changeElemName cf (XTag n al)	= XTag (cf n) al
    changeElemName _ _			= error "changeElemName undefined"

    changeAttrl cf (XTag n al)		= XTag n (cf al)
    changeAttrl cf (XPi  n al)		= XPi  n (cf al)
    changeAttrl _ _			= error "changeAttrl undefined"

    changeAttrName cf (XAttr n)		= XAttr (cf n)
    changeAttrName _ _			= error "changeAttrName undefined"

    changePiName cf (XPi n al)		= XPi (cf n) al
    changePiName _ _			= error "changeAttrName undefined"

    changeDTDAttrl cf (XDTD p al)	= XDTD p (cf al)
    changeDTDAttrl _ _			= error "changeDTDAttrl undefined"

mkElementNode	:: QName -> XmlTrees -> XNode
mkElementNode	= XTag

mkAttrNode	:: QName -> XNode
mkAttrNode	= XAttr

mkDTDNode	:: DTDElem -> Attributes -> XNode
mkDTDNode	= XDTD

instance XmlNode a => XmlNode (NTree a) where
    isText		= isText      . getNode
    isCharRef		= isCharRef   . getNode
    isEntityRef		= isEntityRef . getNode
    isCmt		= isCmt       . getNode
    isCdata		= isCdata     . getNode
    isPi		= isPi        . getNode
    isElem		= isElem      . getNode
    isRoot		= isRoot      . getNode
    isDTD		= isDTD       . getNode
    isAttr		= isAttr      . getNode
    isError		= isError     . getNode

    mkText		= mkLeaf . mkText
    mkCharRef		= mkLeaf . mkCharRef
    mkEntityRef		= mkLeaf . mkEntityRef
    mkCmt		= mkLeaf . mkCmt
    mkCdata		= mkLeaf . mkCdata
    mkPi n		= mkLeaf . mkPi n
    mkError l		= mkLeaf . mkError l

    getText		= getText       . getNode
    getCharRef		= getCharRef    . getNode
    getEntityRef	= getEntityRef  . getNode
    getCmt		= getCmt        . getNode
    getCdata		= getCdata      . getNode
    getPiName		= getPiName     . getNode
    getPiContent	= getPiContent  . getNode
    getElemName		= getElemName   . getNode
    getAttrl		= getAttrl      . getNode
    getDTDPart		= getDTDPart    . getNode
    getDTDAttrl		= getDTDAttrl   . getNode
    getAttrName		= getAttrName   . getNode
    getErrorLevel	= getErrorLevel . getNode
    getErrorMsg		= getErrorMsg   . getNode

    changeText cf	= changeNode (changeText cf)
    changeCmt cf	= changeNode (changeCmt  cf)
    changeName cf	= changeNode (changeName cf)
    changeElemName cf	= changeNode (changeElemName cf)
    changeAttrl cf	= changeNode (changeAttrl cf)
    changeAttrName cf	= changeNode (changeAttrName cf)
    changePiName cf	= changeNode (changePiName cf)
    changeDTDAttrl cf	= changeNode (changeDTDAttrl cf)

mkElement	:: QName -> XmlTrees -> XmlTrees -> XmlTree
mkElement n al	= mkTree (mkElementNode n al)

mkRoot		:: XmlTrees -> XmlTrees -> XmlTree
mkRoot al	= mkTree (mkElementNode (mkName t_root) al)

mkAttr		:: QName -> XmlTrees -> XmlTree
mkAttr n	= mkTree (mkAttrNode n)

mkDTDElem	:: DTDElem -> Attributes -> XmlTrees -> XmlTree
mkDTDElem e al	= mkTree (mkDTDNode e al)

addAttr		:: XmlTree -> XmlTrees -> XmlTrees
addAttr a al
    | isAttr a	= add al
    | otherwise	= al
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

mergeAttrl	:: XmlTrees -> XmlTrees -> XmlTrees
mergeAttrl 	= foldr addAttr

-- ------------------------------------------------------------
