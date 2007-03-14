-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG.BasicArrows
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Constants and basic arrows for Relax NG

-}

-- ------------------------------------------------------------

module Text.XML.HXT.RelaxNG.BasicArrows
where

import Control.Arrow.ListArrows

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow
    hiding
    ( mkText
    , mkError
    )

hasRngName 	:: ArrowXml a => String -> a XmlTree XmlTree
hasRngName s
    = hasName s 
      `orElse`
      ( hasLocalPart s >>> hasNamespaceUri relaxNamespace )

checkRngName :: ArrowXml a => [String] -> a XmlTree XmlTree
checkRngName l
    = ( isElem
	>>>
	catA (map hasRngName l)
      )
      `guards` this

noOfChildren	:: ArrowXml a => (Int -> Bool) -> a XmlTree XmlTree
noOfChildren p
    = getChildren
      >>.
      (\ l -> if p (length l) then l else [])

-- ------------------------------------------------------------

isAttributeRef	:: ArrowXml a => a XmlTree XmlTree
isAttributeRef
    = checkRngName ["attribute", "ref"]

isAttributeRefTextListGroupInterleaveOneOrMoreEmpty	:: ArrowXml a => a XmlTree XmlTree
isAttributeRefTextListGroupInterleaveOneOrMoreEmpty
    = checkRngName ["attribute", "ref", "text", "list", "group", "interleave", "oneOrMore", "empty"]

isAttributeRefTextListInterleave	:: ArrowXml a => a XmlTree XmlTree
isAttributeRefTextListInterleave
    = checkRngName ["attribute", "ref", "text", "list", "interleave"]

isAttributeListGroupInterleaveOneOrMore	:: ArrowXml a => a XmlTree XmlTree
isAttributeListGroupInterleaveOneOrMore
    = checkRngName ["attribute", "list", "group", "interleave", "oneOrMore"]

isExternalRefInclude	:: ArrowXml a => a XmlTree XmlTree
isExternalRefInclude
    = checkRngName ["externalRef", "include"]

isNameNsNameValue	:: ArrowXml a => a XmlTree XmlTree
isNameNsNameValue
    = checkRngName ["name", "nsName", "value"]

isNameNsName	:: ArrowXml a => a XmlTree XmlTree
isNameNsName
    = checkRngName ["name", "nsName"]

isNameAnyNameNsName	:: ArrowXml a => a XmlTree XmlTree
isNameAnyNameNsName
    = checkRngName ["name", "anyName", "nsName"]

isDefineOneOrMoreZeroOrMoreOptionalListMixed	:: ArrowXml a => a XmlTree XmlTree
isDefineOneOrMoreZeroOrMoreOptionalListMixed
    = checkRngName ["define", "oneOrMore", "zeroOrMore", "optional", "list", "mixed"]

isChoiceGroupInterleave	:: ArrowXml a => a XmlTree XmlTree
isChoiceGroupInterleave
    = checkRngName ["choice", "group", "interleave"]

isChoiceGroupInterleaveOneOrMore	:: ArrowXml a => a XmlTree XmlTree
isChoiceGroupInterleaveOneOrMore
    = checkRngName ["choice", "group", "interleave", "oneOrMore"]

isGroupInterleave	:: ArrowXml a => a XmlTree XmlTree
isGroupInterleave
    = checkRngName ["group", "interleave"]

-- ------------------------------------------------------------

isRngAnyName		:: ArrowXml a => a XmlTree XmlTree
isRngAnyName		= isElem >>> hasRngName "anyName"

isRngAttribute		:: ArrowXml a => a XmlTree XmlTree
isRngAttribute		= isElem >>> hasRngName "attribute"

isRngChoice		:: ArrowXml a => a XmlTree XmlTree
isRngChoice		= isElem >>> hasRngName "choice"

isRngCombine		:: ArrowXml a => a XmlTree XmlTree
isRngCombine		= isElem >>> hasRngName "combine"

isRngData		:: ArrowXml a => a XmlTree XmlTree
isRngData		= isElem >>> hasRngName "data"

isRngDefine		:: ArrowXml a => a XmlTree XmlTree
isRngDefine		= isElem >>> hasRngName "define"

isRngDiv		:: ArrowXml a => a XmlTree XmlTree
isRngDiv		= isElem >>> hasRngName "div"

isRngElement		:: ArrowXml a => a XmlTree XmlTree
isRngElement		= isElem >>> hasRngName "element"

isRngEmpty		:: ArrowXml a => a XmlTree XmlTree
isRngEmpty		= isElem >>> hasRngName "empty"

isRngExcept		:: ArrowXml a => a XmlTree XmlTree
isRngExcept		= isElem >>> hasRngName "except"

isRngExternalRef	:: ArrowXml a => a XmlTree XmlTree
isRngExternalRef	= isElem >>> hasRngName "externalRef"

isRngGrammar		:: ArrowXml a => a XmlTree XmlTree
isRngGrammar		= isElem >>> hasRngName "grammar"

isRngGroup		:: ArrowXml a => a XmlTree XmlTree
isRngGroup		= isElem >>> hasRngName "group"

isRngInclude		:: ArrowXml a => a XmlTree XmlTree
isRngInclude		= isElem >>> hasRngName "include"

isRngInterleave		:: ArrowXml a => a XmlTree XmlTree
isRngInterleave		= isElem >>> hasRngName "interleave"

isRngList		:: ArrowXml a => a XmlTree XmlTree
isRngList		= isElem >>> hasRngName "list"

isRngMixed		:: ArrowXml a => a XmlTree XmlTree
isRngMixed		= isElem >>> hasRngName "mixed"

isRngName		:: ArrowXml a => a XmlTree XmlTree
isRngName		= isElem >>> hasRngName "name"

isRngNotAllowed		:: ArrowXml a => a XmlTree XmlTree
isRngNotAllowed		= isElem >>> hasRngName "notAllowed"

isRngNsName		:: ArrowXml a => a XmlTree XmlTree
isRngNsName		= isElem >>> hasRngName "nsName"

isRngOneOrMore		:: ArrowXml a => a XmlTree XmlTree
isRngOneOrMore		= isElem >>> hasRngName "oneOrMore"

isRngOptional		:: ArrowXml a => a XmlTree XmlTree
isRngOptional		= isElem >>> hasRngName "optional"

isRngParam		:: ArrowXml a => a XmlTree XmlTree
isRngParam		= isElem >>> hasRngName "param"

isRngParentRef		:: ArrowXml a => a XmlTree XmlTree
isRngParentRef		= isElem >>> hasRngName "parentRef"

isRngRef		:: ArrowXml a => a XmlTree XmlTree
isRngRef		= isElem >>> hasRngName "ref"

isRngRelaxError		:: ArrowXml a => a XmlTree XmlTree
isRngRelaxError		= isElem >>> hasRngName "relaxError"

isRngStart		:: ArrowXml a => a XmlTree XmlTree
isRngStart		= isElem >>> hasRngName "start"

isRngText		:: ArrowXml a => a XmlTree XmlTree
isRngText		= isElem >>> hasRngName "text"

isRngType		:: ArrowXml a => a XmlTree XmlTree
isRngType		= isElem >>> hasRngName "type"

isRngValue		:: ArrowXml a => a XmlTree XmlTree
isRngValue		= isElem >>> hasRngName "value"

isRngZeroOrMore		:: ArrowXml a => a XmlTree XmlTree
isRngZeroOrMore		= isElem >>> hasRngName "zeroOrMore"

-- ------------------------------------------------------------

mkRngElement		:: ArrowXml a => String -> a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngElement n		= mkElement (QN "" n relaxNamespace)

mkRngChoice		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngChoice		= mkRngElement "choice"

mkRngDefine		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngDefine		= mkRngElement "define"

mkRngEmpty		:: ArrowXml a => a n XmlTree -> a n XmlTree
mkRngEmpty a		= mkRngElement "empty" a none

mkRngGrammar		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngGrammar		= mkRngElement "grammar"

mkRngGroup		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngGroup		= mkRngElement "group"

mkRngInterleave		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngInterleave		= mkRngElement "interleave"

mkRngName		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngName		= mkRngElement "name"

mkRngNotAllowed		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngNotAllowed		= mkRngElement "notAllowed"

mkRngOneOrMore		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngOneOrMore		= mkRngElement "oneOrMore"

mkRngRef		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngRef		= mkRngElement "ref"

mkRngRelaxError		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngRelaxError		= mkRngElement "relaxError"

mkRngStart		:: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngStart		= mkRngElement "start"

mkRngText		:: ArrowXml a => a n XmlTree -> a n XmlTree
mkRngText a		= mkRngElement "text" a none

-- ------------------------------------------------------------

setRngName		:: ArrowXml a => String -> a XmlTree XmlTree
setRngName n		= setElemName (QN "" n relaxNamespace)

setRngNameDiv		:: ArrowXml a => a XmlTree XmlTree
setRngNameDiv		= setRngName "div"

setRngNameRef		:: ArrowXml a => a XmlTree XmlTree
setRngNameRef		= setRngName "ref"

-- ------------------------------------------------------------

-- Attributes

isRngAttrAttribute		:: ArrowXml a => a XmlTree XmlTree
isRngAttrAttribute		= isAttr >>> hasRngName "attribute"

isRngAttrCombine		:: ArrowXml a => a XmlTree XmlTree
isRngAttrCombine		= isAttr >>> hasRngName "combine"

isRngAttrDatatypeLibrary	:: ArrowXml a => a XmlTree XmlTree
isRngAttrDatatypeLibrary	= isAttr >>> hasRngName "datatypeLibrary"

isRngAttrHref			:: ArrowXml a => a XmlTree XmlTree
isRngAttrHref			= isAttr >>> hasRngName "href"

isRngAttrName			:: ArrowXml a => a XmlTree XmlTree
isRngAttrName			= isAttr >>> hasRngName "name"

isRngAttrNs			:: ArrowXml a => a XmlTree XmlTree
isRngAttrNs			= isAttr >>> hasRngName "ns"

isRngAttrType			:: ArrowXml a => a XmlTree XmlTree
isRngAttrType			= isAttr >>> hasRngName "type"

-- ------------------------------------------------------------

hasRngAttrAttribute		:: ArrowXml a => a XmlTree XmlTree
hasRngAttrAttribute		= hasAttr "attribute"

hasRngAttrCombine		:: ArrowXml a => a XmlTree XmlTree
hasRngAttrCombine		= hasAttr "combine"

hasRngAttrDatatypeLibrary	:: ArrowXml a => a XmlTree XmlTree
hasRngAttrDatatypeLibrary	= hasAttr "datatypeLibrary"

hasRngAttrHref			:: ArrowXml a => a XmlTree XmlTree
hasRngAttrHref			= hasAttr "href"

hasRngAttrName			:: ArrowXml a => a XmlTree XmlTree
hasRngAttrName			= hasAttr "name"

hasRngAttrNs			:: ArrowXml a => a XmlTree XmlTree
hasRngAttrNs			= hasAttr "ns"

hasRngAttrType			:: ArrowXml a => a XmlTree XmlTree
hasRngAttrType			= hasAttr "type"

-- ------------------------------------------------------------

getRngAttrAttribute		:: ArrowXml a => a XmlTree String
getRngAttrAttribute		= getAttrValue "attribute"

getRngAttrCombine		:: ArrowXml a => a XmlTree String
getRngAttrCombine		= getAttrValue "combine"

getRngAttrDatatypeLibrary	:: ArrowXml a => a XmlTree String
getRngAttrDatatypeLibrary	= getAttrValue "datatypeLibrary"

getRngAttrDescr			:: ArrowXml a => a XmlTree String
getRngAttrDescr			= getAttrValue "descr"

getRngAttrHref			:: ArrowXml a => a XmlTree String
getRngAttrHref			= getAttrValue "href"

getRngAttrName			:: ArrowXml a => a XmlTree String
getRngAttrName			= getAttrValue "name"

getRngAttrNs			:: ArrowXml a => a XmlTree String
getRngAttrNs			= getAttrValue "ns"

getRngAttrType			:: ArrowXml a => a XmlTree String
getRngAttrType			= getAttrValue "type"

-- ------------------------------------------------------------

