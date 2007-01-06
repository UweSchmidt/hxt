-- |
-- interface to the HXT XML and DTD parsers
--
-- version: $Id: ParserInterface.hs,v 1.1 2006/05/01 18:56:24 hxml Exp $

module Text.XML.HXT.Arrow.ParserInterface
    ( module Text.XML.HXT.Arrow.ParserInterface )
where

import Control.Arrow.ArrowList

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow

import qualified Text.XML.HXT.Parser.HtmlParsec          as HP
import qualified Text.XML.HXT.Parser.XmlParsec           as XP
import qualified Text.XML.HXT.Parser.XmlDTDParser	 as DP
import qualified Text.XML.HXT.Validator.ValidationFilter as VA

-- ------------------------------------------------------------

parseXmlDoc			:: ArrowXml a => a (String, String) XmlTree
parseXmlDoc			=  arr2L XP.parseXmlDocument

parseXmlDTDPart			:: ArrowXml a => a (String, XmlTree) XmlTree
parseXmlDTDPart			=  arr2L XP.parseXmlDTDPart

parseXmlContent			:: ArrowXml a => a String XmlTree
parseXmlContent			=  arrL XP.xread

parseXmlEntityEncodingSpec
  , parseXmlDocEncodingSpec
  , removeEncodingSpec
  , substXmlEntityRefs		:: ArrowXml a => a XmlTree XmlTree

parseXmlDocEncodingSpec		=  arrL XP.parseXmlDocEncodingSpec
parseXmlEntityEncodingSpec	=  arrL XP.parseXmlEntityEncodingSpec

removeEncodingSpec		=  arrL XP.removeEncodingSpec

parseXmlDTDdeclPart		:: ArrowXml a => a XmlTree XmlTree
parseXmlDTDdeclPart		=  arrL DP.parseXmlDTDdeclPart

parseXmlDTDdecl			:: ArrowXml a => a XmlTree XmlTree
parseXmlDTDdecl			=  arrL DP.parseXmlDTDdecl

parseXmlDTDEntityValue		:: ArrowXml a => a XmlTree XmlTree
parseXmlDTDEntityValue		=  arrL DP.parseXmlDTDEntityValue

parseXmlAttrValue		:: ArrowXml a => String -> a XmlTree XmlTree
parseXmlAttrValue context	=  arrL (XP.parseXmlAttrValue context)

parseXmlGeneralEntityValue	:: ArrowXml a => String -> a XmlTree XmlTree
parseXmlGeneralEntityValue context
				=  arrL (XP.parseXmlGeneralEntityValue context)

substXmlEntityRefs		=  arrL XP.substXmlEntities

-- ------------------------------------------------------------

parseHtmlDoc			:: ArrowList a => a (String, String) XmlTree
parseHtmlDoc			= arr2L HP.parseHtmlDocument

parseHtmlContent		:: ArrowList a => a String XmlTree
parseHtmlContent		= arrL  HP.parseHtmlContent

substHtmlEntityRefs		:: ArrowList a => a XmlTree XmlTree
substHtmlEntityRefs		= arrL HP.substHtmlEntities

-- ------------------------------------------------------------

validateDoc
  , transformDoc		:: ArrowList a => a XmlTree XmlTree

validateDoc			= arrL VA.validate
transformDoc			= arrL VA.transform

-- ------------------------------------------------------------
