-- |
-- Simple parse functions.
--
-- Version : $Id: Parser.hs,v 1.2 2006/11/09 20:27:42 hxml Exp $
--
--
-- the "main" building blocks for an application.
-- this module exports all important functions for parsing, validating and transforming XML.
-- it also exports all basic datatypes and functions of the toolbox.
--

module Text.XML.HXT.Parser
    ( module Text.XML.HXT.DOM

    , module Text.XML.HXT.Parser.XmlParser
    , module Text.XML.HXT.Parser.HtmlParser
    , module Text.XML.HXT.Parser.XmlInput
    , module Text.XML.HXT.Parser.XmlOutput
    , module Text.XML.HXT.Parser.DTDProcessing
    , module Text.XML.HXT.Parser.MainFunctions

    , module Text.XML.HXT.Validator.Validation
    )
where

import Text.XML.HXT.DOM

import Text.XML.HXT.Parser.XmlParser
import Text.XML.HXT.Parser.HtmlParser
import Text.XML.HXT.Parser.XmlInput
import Text.XML.HXT.Parser.XmlOutput
import Text.XML.HXT.Parser.DTDProcessing
import Text.XML.HXT.Parser.MainFunctions

import Text.XML.HXT.Validator.Validation

-- ------------------------------------------------------------

