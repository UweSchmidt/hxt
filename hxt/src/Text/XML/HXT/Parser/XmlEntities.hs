-- |
-- Predefined XML Entity References
--
-- This module defines a table of all
-- predefined XML entity references

module Text.XML.HXT.Parser.XmlEntities
    ( xmlEntities
    )

where

-- ------------------------------------------------------------

-- |
-- list of predefined XML entity names and their unicode values

xmlEntities     :: [(String, Int)]
xmlEntities     = [ ("quot",    34)
                  , ("amp",     38)
                  , ("lt",      60)
                  , ("gt",      62)
                  , ("apos",    39)
                  ]

-- ------------------------------------------------------------
