-- |
-- This helper module exports elements from the basic libraries:
-- XmlTreeTypes, XmlKeywords, XNodeFunctions, XmlTreeFunctions and XmlTreeFilter

module Text.XML.HXT.DOM.XmlTree
    ( module Text.XML.HXT.DOM.XmlTreeTypes
    , module Text.XML.HXT.DOM.XmlKeywords
    , module Text.XML.HXT.DOM.XmlTreeFunctions
    , module Text.XML.HXT.DOM.XmlTreeFilter
    )

where

import Text.XML.HXT.DOM.XmlTreeTypes		-- NTree and XNode types
import Text.XML.HXT.DOM.XmlKeywords		-- keywords for DTD
import Text.XML.HXT.DOM.XmlTreeFunctions	-- tree constructors and selector
import Text.XML.HXT.DOM.XmlTreeFilter		-- basic filter


