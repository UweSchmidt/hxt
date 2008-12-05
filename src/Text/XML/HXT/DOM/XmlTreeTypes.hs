-- |
-- exports the core data types
-- and some additional types and functions
-- for compatibility with none arrow modules
--
-- Version : $Id: XmlTreeTypes.hs,v 1.6 2006/05/09 15:30:43 hxml Exp $

module Text.XML.HXT.DOM.XmlTreeTypes
    ( module Data.Tree.NTree.Filter
    , module Text.XML.HXT.DOM.XmlTreeTypes
    )

where

import Data.Tree.NTree.Filter
import Text.XML.HXT.DOM.TypeDefs

-- aliases for compatiblility reasons

mkNode		:: node -> NTrees node -> NTree node
mkNode		= mkTree

formatNTree	:: (node -> String) -> NTree node -> String
formatNTree	= formatTree

foldNTree	:: (a -> [b] -> b) -> NTree a -> b
foldNTree 	= foldTree

mapNTree	:: (a -> b) -> NTree a -> NTree b
mapNTree 	= fmap

nTreeToList	:: NTree a -> [a]
nTreeToList	= nodesTree

depthNTree	:: NTree a -> Int
depthNTree	= depthTree

cardNTree	:: NTree a -> Int
cardNTree	= cardNTree

-- -----------------------------------------------------------------------------
--
-- Basic types for filters

-- | A functions that takes a node and returns a list of nodes

type XmlFilter	= TFilter  XNode

-- | A function that takes a list of nodes and returns a list of nodes

type XmlSFilter	= TSFilter XNode


-- -----------------------------------------------------------------------------
--

-- | Tag name
type TagName	= QName

-- | Attribute name
type AttrName	= QName

-- -----------------------------------------------------------------------------

-- | shortcut for 'qualifiedName'

tName	:: QName -> String
tName	= qualifiedName

-- | shortcut for 'qualifiedName'

aName	:: QName -> String
aName	= qualifiedName

-- -----------------------------------------------------------------------------

