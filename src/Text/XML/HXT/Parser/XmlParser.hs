-- |
-- Xml Parser: the main parse filter 
--
-- Version : $Id: XmlParser.hs,v 1.5 2005/06/06 15:57:19 hxml Exp $


module Text.XML.HXT.Parser.XmlParser
    ( module Text.XML.HXT.Parser.XmlParsec
    , module Text.XML.HXT.Parser.XmlTokenParser
    , parseXmlDoc
    )
where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.DOM.XmlState

import Text.XML.HXT.Parser.XmlTokenParser

import Text.XML.HXT.Parser.XmlParsec

import Text.XML.HXT.Parser.XmlOutput
    ( traceTree
    , traceSource
    , traceMsg
    )

-- ------------------------------------------------------------

-- |
-- The monadic parser for a whole document.
-- input must be a root node with a single text child.
-- Error messages are issued and global error state is set.

parseXmlDoc	:: XmlStateFilter a
parseXmlDoc
    = parseDoc
      `whenM` ( isRoot .> getChildren .> isXText )
      where
      parseDoc t
	  = ( traceMsg 2 ("parseXmlDoc: parse XML document " ++ show loc)
	      .>>
	      parser
	      .>>
	      liftMf checkRes
	      .>>
	      traceTree
	      .>>
	      traceSource
	    ) $ t
	  where
	  loc = valueOf a_source t			-- document name
	  checkRes
	      = setStatus c_err ("parsing XML source " ++ show loc)
	        `whenNot`
	        getChildren
	  parser
	      = processChildrenM (liftF (parseXmlText document' loc))

-- ------------------------------------------------------------
