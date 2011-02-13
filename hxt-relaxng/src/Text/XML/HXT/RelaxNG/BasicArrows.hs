-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG.BasicArrows
   Copyright  : Copyright (C) 2011 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Constants and basic arrows for Relax NG

-}

-- ------------------------------------------------------------

module Text.XML.HXT.RelaxNG.BasicArrows
where

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow

import Text.XML.HXT.RelaxNG.DataTypes
    ( a_relaxSimplificationChanges
    , defineOrigName
    , contextBaseAttr
    )

-- ------------------------------------------------------------

hasRngAttName           :: ArrowXml a => String -> a XmlTree XmlTree
hasRngAttName s
    = isAttr
      >>>
      hasLocalPart s
      >>>
      hasNamespaceUri ""

hasRngElemName          :: ArrowXml a => String -> a XmlTree XmlTree
hasRngElemName s
    = isElem
      >>>
      hasLocalPart s
      >>>
      hasNamespaceUri relaxNamespace

checkRngName :: ArrowXml a => [String] -> a XmlTree XmlTree
checkRngName l
    = catA (map hasRngElemName l)
      `guards`
      this

noOfChildren    :: ArrowXml a => (Int -> Bool) -> a XmlTree XmlTree
noOfChildren p
    = getChildren
      >>.
      (\ l -> if p (length l) then l else [])

-- ------------------------------------------------------------

isAttributeRef  :: ArrowXml a => a XmlTree XmlTree
isAttributeRef
    = checkRngName ["attribute", "ref"]

isAttributeRefTextListGroupInterleaveOneOrMoreEmpty     :: ArrowXml a => a XmlTree XmlTree
isAttributeRefTextListGroupInterleaveOneOrMoreEmpty
    = checkRngName ["attribute", "ref", "text", "list", "group", "interleave", "oneOrMore", "empty"]

isAttributeRefTextListInterleave        :: ArrowXml a => a XmlTree XmlTree
isAttributeRefTextListInterleave
    = checkRngName ["attribute", "ref", "text", "list", "interleave"]

isAttributeListGroupInterleaveOneOrMore :: ArrowXml a => a XmlTree XmlTree
isAttributeListGroupInterleaveOneOrMore
    = checkRngName ["attribute", "list", "group", "interleave", "oneOrMore"]

isExternalRefInclude    :: ArrowXml a => a XmlTree XmlTree
isExternalRefInclude
    = checkRngName ["externalRef", "include"]

isNameNsNameValue       :: ArrowXml a => a XmlTree XmlTree
isNameNsNameValue
    = checkRngName ["name", "nsName", "value"]

isNameNsName    :: ArrowXml a => a XmlTree XmlTree
isNameNsName
    = checkRngName ["name", "nsName"]

isNameAnyNameNsName     :: ArrowXml a => a XmlTree XmlTree
isNameAnyNameNsName
    = checkRngName ["name", "anyName", "nsName"]

isDefineOneOrMoreZeroOrMoreOptionalListMixed    :: ArrowXml a => a XmlTree XmlTree
isDefineOneOrMoreZeroOrMoreOptionalListMixed
    = checkRngName ["define", "oneOrMore", "zeroOrMore", "optional", "list", "mixed"]

isChoiceGroupInterleave :: ArrowXml a => a XmlTree XmlTree
isChoiceGroupInterleave
    = checkRngName ["choice", "group", "interleave"]

isChoiceGroupInterleaveOneOrMore        :: ArrowXml a => a XmlTree XmlTree
isChoiceGroupInterleaveOneOrMore
    = checkRngName ["choice", "group", "interleave", "oneOrMore"]

isGroupInterleave       :: ArrowXml a => a XmlTree XmlTree
isGroupInterleave
    = checkRngName ["group", "interleave"]

-- ------------------------------------------------------------

isRngAnyName            :: ArrowXml a => a XmlTree XmlTree
isRngAnyName            = hasRngElemName "anyName"

isRngAttribute          :: ArrowXml a => a XmlTree XmlTree
isRngAttribute          = hasRngElemName "attribute"

isRngChoice             :: ArrowXml a => a XmlTree XmlTree
isRngChoice             = hasRngElemName "choice"

isRngCombine            :: ArrowXml a => a XmlTree XmlTree
isRngCombine            = hasRngElemName "combine"

isRngData               :: ArrowXml a => a XmlTree XmlTree
isRngData               = hasRngElemName "data"

isRngDefine             :: ArrowXml a => a XmlTree XmlTree
isRngDefine             = hasRngElemName "define"

isRngDiv                :: ArrowXml a => a XmlTree XmlTree
isRngDiv                = hasRngElemName "div"

isRngElement            :: ArrowXml a => a XmlTree XmlTree
isRngElement            = hasRngElemName "element"

isRngEmpty              :: ArrowXml a => a XmlTree XmlTree
isRngEmpty              = hasRngElemName "empty"

isRngExcept             :: ArrowXml a => a XmlTree XmlTree
isRngExcept             = hasRngElemName "except"

isRngExternalRef        :: ArrowXml a => a XmlTree XmlTree
isRngExternalRef        = hasRngElemName "externalRef"

isRngGrammar            :: ArrowXml a => a XmlTree XmlTree
isRngGrammar            = hasRngElemName "grammar"

isRngGroup              :: ArrowXml a => a XmlTree XmlTree
isRngGroup              = hasRngElemName "group"

isRngInclude            :: ArrowXml a => a XmlTree XmlTree
isRngInclude            = hasRngElemName "include"

isRngInterleave         :: ArrowXml a => a XmlTree XmlTree
isRngInterleave         = hasRngElemName "interleave"

isRngList               :: ArrowXml a => a XmlTree XmlTree
isRngList               = hasRngElemName "list"

isRngMixed              :: ArrowXml a => a XmlTree XmlTree
isRngMixed              = hasRngElemName "mixed"

isRngName               :: ArrowXml a => a XmlTree XmlTree
isRngName               = hasRngElemName "name"

isRngNotAllowed         :: ArrowXml a => a XmlTree XmlTree
isRngNotAllowed         = hasRngElemName "notAllowed"

isRngNsName             :: ArrowXml a => a XmlTree XmlTree
isRngNsName             = hasRngElemName "nsName"

isRngOneOrMore          :: ArrowXml a => a XmlTree XmlTree
isRngOneOrMore          = hasRngElemName "oneOrMore"

isRngOptional           :: ArrowXml a => a XmlTree XmlTree
isRngOptional           = hasRngElemName "optional"

isRngParam              :: ArrowXml a => a XmlTree XmlTree
isRngParam              = hasRngElemName "param"

isRngParentRef          :: ArrowXml a => a XmlTree XmlTree
isRngParentRef          = hasRngElemName "parentRef"

isRngRef                :: ArrowXml a => a XmlTree XmlTree
isRngRef                = hasRngElemName "ref"

isRngRelaxError         :: ArrowXml a => a XmlTree XmlTree
isRngRelaxError         = hasRngElemName "relaxError"

isRngStart              :: ArrowXml a => a XmlTree XmlTree
isRngStart              = hasRngElemName "start"

isRngText               :: ArrowXml a => a XmlTree XmlTree
isRngText               = hasRngElemName "text"

isRngType               :: ArrowXml a => a XmlTree XmlTree
isRngType               = hasRngElemName "type"

isRngValue              :: ArrowXml a => a XmlTree XmlTree
isRngValue              = hasRngElemName "value"

isRngZeroOrMore         :: ArrowXml a => a XmlTree XmlTree
isRngZeroOrMore         = hasRngElemName "zeroOrMore"

-- ------------------------------------------------------------

mkRngElement            :: ArrowXml a => String -> a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngElement n          = mkElement (mkQName "" n relaxNamespace)

mkRngChoice             :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngChoice             = mkRngElement "choice"

mkRngDefine             :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngDefine             = mkRngElement "define"

mkRngEmpty              :: ArrowXml a => a n XmlTree -> a n XmlTree
mkRngEmpty a            = mkRngElement "empty" a none

mkRngGrammar            :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngGrammar            = mkRngElement "grammar"

mkRngGroup              :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngGroup              = mkRngElement "group"

mkRngInterleave         :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngInterleave         = mkRngElement "interleave"

mkRngName               :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngName               = mkRngElement "name"

mkRngNotAllowed         :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngNotAllowed         = mkRngElement "notAllowed"

mkRngOneOrMore          :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngOneOrMore          = mkRngElement "oneOrMore"

mkRngRef                :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngRef                = mkRngElement "ref"

mkRngRelaxError         :: ArrowXml a => a n XmlTree
mkRngRelaxError         = mkRngElement "relaxError" none none

mkRngStart              :: ArrowXml a => a n XmlTree -> a n XmlTree -> a n XmlTree
mkRngStart              = mkRngElement "start"

mkRngText               :: ArrowXml a => a n XmlTree -> a n XmlTree
mkRngText a             = mkRngElement "text" a none

-- ------------------------------------------------------------

setRngName              :: ArrowXml a => String -> a XmlTree XmlTree
setRngName n            = setElemName (mkQName "" n relaxNamespace)

setRngNameDiv           :: ArrowXml a => a XmlTree XmlTree
setRngNameDiv           = setRngName "div"

setRngNameRef           :: ArrowXml a => a XmlTree XmlTree
setRngNameRef           = setRngName "ref"

-- ------------------------------------------------------------

-- Attributes

isRngAttrAttribute              :: ArrowXml a => a XmlTree XmlTree
isRngAttrAttribute              = hasRngAttName "attribute"

isRngAttrCombine                :: ArrowXml a => a XmlTree XmlTree
isRngAttrCombine                = hasRngAttName "combine"

isRngAttrDatatypeLibrary        :: ArrowXml a => a XmlTree XmlTree
isRngAttrDatatypeLibrary        = hasRngAttName "datatypeLibrary"

isRngAttrHref                   :: ArrowXml a => a XmlTree XmlTree
isRngAttrHref                   = hasRngAttName "href"

isRngAttrName                   :: ArrowXml a => a XmlTree XmlTree
isRngAttrName                   = hasRngAttName "name"

isRngAttrNs                     :: ArrowXml a => a XmlTree XmlTree
isRngAttrNs                     = hasRngAttName "ns"

isRngAttrType                   :: ArrowXml a => a XmlTree XmlTree
isRngAttrType                   = hasRngAttName "type"

isRngAttrRelaxSimplificationChanges     :: ArrowXml a => a XmlTree XmlTree
isRngAttrRelaxSimplificationChanges     = hasRngAttName a_relaxSimplificationChanges

-- ------------------------------------------------------------

mkRngAttr                       :: ArrowXml a => String -> a b String -> a b XmlTree
mkRngAttr name value            = mkAttr (mkName name) (value >>> mkText)

mkRngAttrName                   :: ArrowXml a => String -> a b XmlTree
mkRngAttrName value             = mkRngAttr "name" (constA value)

mkRngAttrRelaxSimplificationChanges     :: ArrowXml a => String -> a b XmlTree
mkRngAttrRelaxSimplificationChanges value
                                = mkRngAttr a_relaxSimplificationChanges (constA value)

mkRngAttrDefineOrigName         :: ArrowXml a => String -> a b XmlTree
mkRngAttrDefineOrigName value   = mkRngAttr defineOrigName (constA value)

mkRngAttrContextBase            :: ArrowXml a => a b String -> a b XmlTree
mkRngAttrContextBase            = mkRngAttr contextBaseAttr

addRngAttrName                  :: ArrowXml a => String -> a XmlTree XmlTree
addRngAttrName value            = addAttr "name" value

addRngAttrDescr                 :: ArrowXml a => String -> a XmlTree XmlTree
addRngAttrDescr                 = addAttr "descr"

addRngAttrChanges               :: ArrowXml a => String -> a XmlTree XmlTree
addRngAttrChanges               = addAttr "changes"

addRngAttrNs                    :: ArrowXml a => String -> a XmlTree XmlTree
addRngAttrNs                    = addAttr "ns"

rmRngAttrNs                     :: ArrowXml a => a XmlTree XmlTree
rmRngAttrNs                     = removeAttr "ns"

-- ------------------------------------------------------------

hasRngAttrRelaxSimplificationChanges             :: ArrowXml a => a XmlTree XmlTree
hasRngAttrRelaxSimplificationChanges             = hasAttr a_relaxSimplificationChanges

hasRngAttrAttribute             :: ArrowXml a => a XmlTree XmlTree
hasRngAttrAttribute             = hasAttr "attribute"

hasRngAttrCombine               :: ArrowXml a => a XmlTree XmlTree
hasRngAttrCombine               = hasAttr "combine"

hasRngAttrDatatypeLibrary       :: ArrowXml a => a XmlTree XmlTree
hasRngAttrDatatypeLibrary       = hasAttr "datatypeLibrary"

hasRngAttrHref                  :: ArrowXml a => a XmlTree XmlTree
hasRngAttrHref                  = hasAttr "href"

hasRngAttrName                  :: ArrowXml a => a XmlTree XmlTree
hasRngAttrName                  = hasAttr "name"

hasRngAttrNs                    :: ArrowXml a => a XmlTree XmlTree
hasRngAttrNs                    = hasAttr "ns"

hasRngAttrType                  :: ArrowXml a => a XmlTree XmlTree
hasRngAttrType                  = hasAttr "type"

-- ------------------------------------------------------------

getRngAttrAttribute             :: ArrowXml a => a XmlTree String
getRngAttrAttribute             = getAttrValue "attribute"

getRngAttrCombine               :: ArrowXml a => a XmlTree String
getRngAttrCombine               = getAttrValue "combine"

getRngAttrDatatypeLibrary       :: ArrowXml a => a XmlTree String
getRngAttrDatatypeLibrary       = getAttrValue "datatypeLibrary"

getRngAttrDescr                 :: ArrowXml a => a XmlTree String
getRngAttrDescr                 = getAttrValue "descr"

getRngAttrHref                  :: ArrowXml a => a XmlTree String
getRngAttrHref                  = getAttrValue "href"

getRngAttrName                  :: ArrowXml a => a XmlTree String
getRngAttrName                  = getAttrValue "name"

getRngAttrNs                    :: ArrowXml a => a XmlTree String
getRngAttrNs                    = getAttrValue "ns"

getRngAttrType                  :: ArrowXml a => a XmlTree String
getRngAttrType                  = getAttrValue "type"

-- ------------------------------------------------------------

