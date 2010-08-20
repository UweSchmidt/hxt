-- |
-- This helper module exports elements from the basic Relax NG libraries:
-- Validator, CreatePattern, PatternToString and DataTypes.
-- It is the main entry point to the Relax NG schema validator of the Haskell
-- XML Toolbox.
--
-- Author : Torben Kuseler
--
-- Version : $Id: RelaxNG.hs,v 1.1 2005/09/02 17:09:39 hxml Exp $

module Text.XML.HXT.RelaxNG
  ( module Text.XML.HXT.RelaxNG.PatternToString
  , module Text.XML.HXT.RelaxNG.Validator
  , module Text.XML.HXT.RelaxNG.DataTypes
  , module Text.XML.HXT.RelaxNG.CreatePattern
  )
where

import Text.XML.HXT.RelaxNG.PatternToString
import Text.XML.HXT.RelaxNG.Validator
import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.CreatePattern
