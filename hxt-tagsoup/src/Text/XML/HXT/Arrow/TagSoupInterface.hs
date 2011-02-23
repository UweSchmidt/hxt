-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.TagSoupInterface
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Interface for TagSoup Parser

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.TagSoupInterface
where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree

import Data.String.Unicode            ( normalizeNL )

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs

import qualified Text.XML.HXT.Parser.TagSoup as TS

-- ------------------------------------------------------------

{- |
   The system config option to enable the tagsoup parser

Here is an example, how to use it:

> ...
> import Text.HXT.XML.Core
> import Text.HXT.XML.TagSoup
> ...
>
> readDocument [ withExpat ] "some-file.xml"
> ...

reads the given document and parses it with the lazy tagsoup parser.
There is no validation enabled.
-}


withTagSoup                     :: SysConfig
withTagSoup                     = setS (theTagSoup       .&&&.
                                        theExpat         .&&&.
                                        theTagSoupParser
                                       ) (True, (False, parseHtmlTagSoup))

-- | Turns off tagsoup parsing. The build in HXT parser will be used.

withoutTagSoup                  :: SysConfig
withoutTagSoup                  = setS theTagSoup False

-- ------------------------------------------------------------

-- | The Tagsoup parser arrow

parseHtmlTagSoup                :: IOSArrow XmlTree XmlTree
parseHtmlTagSoup                = parse
                                  $< getSysVar
                                     (theCheckNamespaces .&&&.
                                      theWarnings        .&&&.
                                      thePreserveComment .&&&.
                                      theRemoveWS        .&&&.
                                      theLowerCaseNames
                                     )
    where
    parse (withNamespaces', (withWarnings', (preserveCmt', (removeWS', lowerCaseNames'))))
                                = traceMsg 1 ("parse document with tagsoup " ++
                                              ( if lowerCaseNames' then "HT" else "X" ) ++ "ML parser"
                                             )
                                  >>>
                                  replaceChildren
                                  ( ( getAttrValue a_source               -- get source name
                                      &&&
                                      (xshow getChildren >>^ normalizeNL) -- get string to be parsed and normalize newline char
                                    )
                                    >>>
                                    arr2L (TS.parseHtmlTagSoup withNamespaces' withWarnings' preserveCmt' removeWS' lowerCaseNames')
                                  )

-- ------------------------------------------------------------

