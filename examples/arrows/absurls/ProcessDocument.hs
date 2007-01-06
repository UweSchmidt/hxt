-- ------------------------------------------------------------

{- |
   Module     : ProcessDocument
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental
   Portability: portable
   Version    : $Id: ProcessDocument.hs,v 1.2 2005/05/15 17:01:04 hxml Exp $

AbsURIs - Conversion references into absolute URIs in HTML pages

The REAL processing functions
-}

-- ------------------------------------------------------------

module ProcessDocument
    ( processDocument )
where

import Text.XML.HXT.Arrow		-- import all stuff for parsing, validating, and transforming XML

import Data.Maybe

-- ------------------------------------------------------------


-- simple example of a processing arrow

processDocument	:: IOSArrow XmlTree XmlTree
processDocument
    = processChildren (mkAbs `when` isElem)
      where
      mkAbs = mkAbsURIs $< compBase

compBase	:: IOSArrow XmlTree String
compBase
    = single searchBaseElem	-- search <base href="..."> in <head> element (only for wrong input: make the arrow deterministic)
      `orElse`
      getBaseURI		-- use document base
    where
    searchBaseElem
	= hasName "html"
	  >>> getChildren
          >>> hasName "head"
          >>> getChildren
	  >>> hasName "base"
	  >>> getAttrValue "href"
	  >>> mkAbsURI

mkAbsURIs	:: String -> IOSArrow XmlTree XmlTree
mkAbsURIs base
    = processTopDown editURIs		-- edit all refs in documnt
      where

      -- build the edit filter from the list of element-attribute names

      editURIs
	  = seqA . map (uncurry mkAbs) $ hrefAttrs

      -- HTML elements and attributes, that contain references (possibly not yet complete)

      hrefAttrs
	  = [ ("a",		"href"	)
	    , ("img",		"src"	)
	    , ("frame",		"src"	)
	    , ("iframe",	"src"	)
	    , ("link",		"href"	)
	    , ("script",	"src"	)
	    ]

      -- change the reference in attribute attrName of element elemName

      mkAbs elemName attrName
	  = processAttrl ( changeAttrValue (mkAbsURIString base)
			   `when`
			   hasName attrName
			 )
	    `when`
	    hasName elemName

-- | compute an absolute URI, if not possible leave URI unchanged

mkAbsURIString	:: String -> String -> String
mkAbsURIString base uri
    = fromMaybe uri . expandURIString uri $ base

-- ------------------------------------------------------------
