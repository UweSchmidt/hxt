-- ----------------------------------------
--
-- don't edit this module
-- generated with DTDtoHXml
-- simple access function for Haskell XML Toolbox
-- generated from DTD of document: "../photoalbum/photos.xml"

module Photo
where

import XmlTree (XmlFilter)
import qualified XmlTree as X

-- ----------------------------------------
--
-- namespace declarations

nsArchive :: String

-- ----------------------------------------
--
-- 

nsArchive =  "http://muehle.welt.all/photos.dtd"

-- ----------------------------------------
--
-- element names (tag names)

tagAlbum   :: String
tagArchive :: String
tagAttr    :: String
tagConfig  :: String
tagCopy    :: String
tagError   :: String
tagHtml    :: String
tagLayout  :: String
tagOrig    :: String
tagPage    :: String
tagPicture :: String
tagSize    :: String

-- ----------------------------------------
--
-- 

tagAlbum   =  "album"
tagArchive =  "archive"
tagAttr    =  "attr"
tagConfig  =  "config"
tagCopy    =  "copy"
tagError   =  "error"
tagHtml    =  "html"
tagLayout  =  "layout"
tagOrig    =  "orig"
tagPage    =  "page"
tagPicture =  "picture"
tagSize    =  "size"

-- ----------------------------------------
--
-- isTag test filter

isAlbum   :: XmlFilter
isArchive :: XmlFilter
isAttr    :: XmlFilter
isConfig  :: XmlFilter
isCopy    :: XmlFilter
isError   :: XmlFilter
isHtml    :: XmlFilter
isLayout  :: XmlFilter
isOrig    :: XmlFilter
isPage    :: XmlFilter
isPicture :: XmlFilter
isSize    :: XmlFilter

-- ----------------------------------------
--
-- 

isAlbum   =  X.isTag tagAlbum
isArchive =  X.isTag tagArchive
isAttr    =  X.isTag tagAttr
isConfig  =  X.isTag tagConfig
isCopy    =  X.isTag tagCopy
isError   =  X.isTag tagError
isHtml    =  X.isTag tagHtml
isLayout  =  X.isTag tagLayout
isOrig    =  X.isTag tagOrig
isPage    =  X.isTag tagPage
isPicture =  X.isTag tagPicture
isSize    =  X.isTag tagSize

-- ----------------------------------------
--
-- make tag nodes constructor filter

tAlbum   :: XmlFilter
tArchive :: XmlFilter
tAttr    :: XmlFilter
tConfig  :: XmlFilter
tCopy    :: XmlFilter
tError   :: XmlFilter
tHtml    :: XmlFilter
tLayout  :: XmlFilter
tOrig    :: XmlFilter
tPage    :: XmlFilter
tPicture :: XmlFilter
tSize    :: XmlFilter

-- ----------------------------------------
--
-- 

tAlbum   = X.etag tagAlbum
tArchive = X.etag tagArchive
tAttr    = X.etag tagAttr
tConfig  = X.etag tagConfig
tCopy    = X.etag tagCopy
tError   = X.etag tagError
tHtml    = X.etag tagHtml
tLayout  = X.etag tagLayout
tOrig    = X.etag tagOrig
tPage    = X.etag tagPage
tPicture = X.etag tagPicture
tSize    = X.etag tagSize

-- ----------------------------------------
--
-- attribute names

attrBase     :: String
attrEntity   :: String
attrGamma    :: String
attrHeight   :: String
attrHref     :: String
attrId       :: String
attrModified :: String
attrName     :: String
attrRotate   :: String
attrType     :: String
attrWidth    :: String
attrXmlns    :: String

-- ----------------------------------------
--
-- 

attrBase     =  "base"
attrEntity   =  "entity"
attrGamma    =  "gamma"
attrHeight   =  "height"
attrHref     =  "href"
attrId       =  "id"
attrModified =  "modified"
attrName     =  "name"
attrRotate   =  "rotate"
attrType     =  "type"
attrWidth    =  "width"
attrXmlns    =  "xmlns"

-- ----------------------------------------
--
-- get attribute value access filter

getBase     :: XmlFilter
getEntity   :: XmlFilter
getGamma    :: XmlFilter
getHeight   :: XmlFilter
getHref     :: XmlFilter
getId       :: XmlFilter
getModified :: XmlFilter
getName     :: XmlFilter
getRotate   :: XmlFilter
getType     :: XmlFilter
getWidth    :: XmlFilter
getXmlns    :: XmlFilter

-- ----------------------------------------
--
-- 

getBase     =  X.getValue attrBase
getEntity   =  X.getValue attrEntity
getGamma    =  X.getValue attrGamma
getHeight   =  X.getValue attrHeight
getHref     =  X.getValue attrHref
getId       =  X.getValue attrId
getModified =  X.getValue attrModified
getName     =  X.getValue attrName
getRotate   =  X.getValue attrRotate
getType     =  X.getValue attrType
getWidth    =  X.getValue attrWidth
getXmlns    =  X.getValue attrXmlns

-- ----------------------------------------
--
-- has attribute test filter

hasBase     :: XmlFilter
hasEntity   :: XmlFilter
hasGamma    :: XmlFilter
hasHeight   :: XmlFilter
hasHref     :: XmlFilter
hasId       :: XmlFilter
hasModified :: XmlFilter
hasName     :: XmlFilter
hasRotate   :: XmlFilter
hasType     :: XmlFilter
hasWidth    :: XmlFilter
hasXmlns    :: XmlFilter

-- ----------------------------------------
--
-- 

hasBase     = X.hasAttr attrBase
hasEntity   = X.hasAttr attrEntity
hasGamma    = X.hasAttr attrGamma
hasHeight   = X.hasAttr attrHeight
hasHref     = X.hasAttr attrHref
hasId       = X.hasAttr attrId
hasModified = X.hasAttr attrModified
hasName     = X.hasAttr attrName
hasRotate   = X.hasAttr attrRotate
hasType     = X.hasAttr attrType
hasWidth    = X.hasAttr attrWidth
hasXmlns    = X.hasAttr attrXmlns

-- ----------------------------------------
--
-- make attribute nodes constructor filter for computed attribute values

afBase     :: XmlFilter -> XmlFilter
afEntity   :: XmlFilter -> XmlFilter
afGamma    :: XmlFilter -> XmlFilter
afHeight   :: XmlFilter -> XmlFilter
afHref     :: XmlFilter -> XmlFilter
afId       :: XmlFilter -> XmlFilter
afModified :: XmlFilter -> XmlFilter
afName     :: XmlFilter -> XmlFilter
afRotate   :: XmlFilter -> XmlFilter
afType     :: XmlFilter -> XmlFilter
afWidth    :: XmlFilter -> XmlFilter
afXmlns    :: XmlFilter -> XmlFilter

-- ----------------------------------------
--
-- 

afBase     = X.mkXAttr attrBase
afEntity   = X.mkXAttr attrEntity
afGamma    = X.mkXAttr attrGamma
afHeight   = X.mkXAttr attrHeight
afHref     = X.mkXAttr attrHref
afId       = X.mkXAttr attrId
afModified = X.mkXAttr attrModified
afName     = X.mkXAttr attrName
afRotate   = X.mkXAttr attrRotate
afType     = X.mkXAttr attrType
afWidth    = X.mkXAttr attrWidth
afXmlns    = X.mkXAttr attrXmlns

-- ----------------------------------------
--
-- make attribute nodes constructor filter for string attribute values

aBase     :: String -> XmlFilter
aEntity   :: String -> XmlFilter
aGamma    :: String -> XmlFilter
aHeight   :: String -> XmlFilter
aHref     :: String -> XmlFilter
aId       :: String -> XmlFilter
aModified :: String -> XmlFilter
aName     :: String -> XmlFilter
aRotate   :: String -> XmlFilter
aType     :: String -> XmlFilter
aWidth    :: String -> XmlFilter
aXmlns    :: String -> XmlFilter

-- ----------------------------------------
--
-- 

aBase     = \ v -> X.mkXAttr attrBase (X.txt v)
aEntity   = \ v -> X.mkXAttr attrEntity (X.txt v)
aGamma    = \ v -> X.mkXAttr attrGamma (X.txt v)
aHeight   = \ v -> X.mkXAttr attrHeight (X.txt v)
aHref     = \ v -> X.mkXAttr attrHref (X.txt v)
aId       = \ v -> X.mkXAttr attrId (X.txt v)
aModified = \ v -> X.mkXAttr attrModified (X.txt v)
aName     = \ v -> X.mkXAttr attrName (X.txt v)
aRotate   = \ v -> X.mkXAttr attrRotate (X.txt v)
aType     = \ v -> X.mkXAttr attrType (X.txt v)
aWidth    = \ v -> X.mkXAttr attrWidth (X.txt v)
aXmlns    = \ v -> X.mkXAttr attrXmlns (X.txt v)

-- ----------------------------------------
--
-- end of Photo

