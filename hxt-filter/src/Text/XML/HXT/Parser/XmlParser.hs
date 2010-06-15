-- |
-- Xml Parser: the main parse filter

module Text.XML.HXT.Parser.XmlParser
    ( module Text.XML.HXT.Parser.XmlParsec
    , module Text.XML.HXT.Parser.XmlTokenParser
    , parseXmlDoc
    , substXmlEntities
    )
where

import Text.XML.HXT.DOM.XmlTree
import Text.XML.HXT.DOM.XmlState

import Text.XML.HXT.Parser.XmlTokenParser

import Text.XML.HXT.Parser.XmlEntities
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

parseXmlDoc     :: XmlStateFilter a
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
          loc = valueOf a_source t                      -- document name
          checkRes
              = setStatus c_err ("parsing XML source " ++ show loc)
                `whenNot`
                getChildren
          parser
              = processChildrenM (liftF (parseXmlText document' loc))

-- ------------------------------------------------------------

-- |
-- Filter for substitution of all occurences of the predefined XML entites quot, amp, lt, gt, apos
-- by equivalent character references

substXmlEntities        :: XmlFilter
substXmlEntities
    = choice
      [ isXEntityRef    :-> substEntity
      , isXTag          :-> processAttr (processChildren substXmlEntities)
      , this            :-> this
      ]
      where
      substEntity t'@(NTree (XEntityRef en) _)
          = case (lookup en xmlEntities) of
            Just i
                -> [mkXCharRefTree i]
            Nothing
                -> this t'

      substEntity _                                     -- just for preventing ghc warning
          = error "substXmlEntities: illegal argument"

-- ------------------------------------------------------------
