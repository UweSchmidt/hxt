-- |
-- format a xml tree in tree representation
--
-- Version : $Id: FormatXmlTree.hs,v 1.2 2006/05/01 18:56:24 hxml Exp $


module Text.XML.HXT.DOM.FormatXmlTree
    ( formatXmlTree
    , formatXmlContents
    )
where

import Text.XML.HXT.DOM.XmlTree

-- ------------------------------------------------------------


formatXmlContents	:: XmlFilter
formatXmlContents t
    = [mkXTextTree (formatXmlTree t)]

formatXmlTree		:: XmlTree  -> String
formatXmlTree
    = formatTree xnode2String

xnode2String	:: XNode -> String
xnode2String (XTag n al)
    = "XTag " ++ showQn n ++ concatMap showAl al

xnode2String (XPi n al)
    = "XPi "  ++ showQn n ++ concatMap showAl al

xnode2String n
    = show n

showAl		:: XmlTree -> String
showAl (NTree (XAttr an) av)
    = "\n|   " ++ showQn an ++ "=" ++ show (xshow av)
showAl t
    = show t

showQn		:: QName -> String
showQn n
    | null ns
	= show $ qualifiedName n
    | otherwise
	= show $ "{" ++ ns ++ "}" ++ qualifiedName n
    where
    ns = namespaceUri n

-- ------------------------------------------------------------

