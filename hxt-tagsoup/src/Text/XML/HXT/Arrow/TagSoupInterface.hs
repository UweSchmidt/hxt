-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.TagSoupInterface
   Copyright  : Copyright (C) 2005 Uwe Schmidt
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

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState

import qualified Text.XML.HXT.Parser.TagSoup as TS

-- ------------------------------------------------------------

-- | enable TagSoup parsing

withTagSoup			:: SysConfig
withTagSoup			= putS (theTagSoup `pairS` theTagSoupParser) (True, parseHtmlTagSoup)

-- ------------------------------------------------------------

-- | The Tagsoup parser arrow

parseHtmlTagSoup                :: IOSArrow XmlTree XmlTree
parseHtmlTagSoup                = parse
                                  $< getSysParam
                                     (theCheckNamespaces `pairS`
                                      (theWarnings `pairS`
                                       (thePreserveComment `pairS`
                                        (theRemoveWS `pairS`
                                         theLowerCaseNames
                                        )
                                       )
                                      )
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
                                      xshow getChildren
                                    )                                     -- get string to be parsed
                                    >>>
                                    arr2L (TS.parseHtmlTagSoup withNamespaces' withWarnings' preserveCmt' removeWS' lowerCaseNames')
                                  )

-- ------------------------------------------------------------

