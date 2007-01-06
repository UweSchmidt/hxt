-- |
-- UTF-8 character parser and simple XML token parsers
--
-- Version : $Id: XmlCharParser.hs,v 1.2 2005/05/31 16:01:12 hxml Exp $

module Text.XML.HXT.Parser.XmlCharParser
    ( xmlChar			-- xml char parsers
    , xmlNameChar
    , xmlNameStartChar
    , xmlNCNameChar
    , xmlNCNameStartChar
    , xmlLetter
    , xmlSpaceChar
    )
where

import Text.XML.HXT.DOM.Unicode
import Text.ParserCombinators.Parsec

-- ------------------------------------------------------------
--
-- Char (2.2)
--

-- |
-- parse a single Unicode character

xmlChar			:: GenParser Char state Unicode
xmlChar			= satisfy isXmlChar <?> "legal XML character"

-- |
-- parse a XML name character

xmlNameChar		:: GenParser Char state Unicode
xmlNameChar		= satisfy isXmlNameChar <?> "legal XML name character"
 
-- |
-- parse a XML name start character

xmlNameStartChar	:: GenParser Char state Unicode
xmlNameStartChar	= satisfy isXmlNameStartChar <?> "legal XML name start character"
 
-- |
-- parse a XML NCName character

xmlNCNameChar		:: GenParser Char state Unicode
xmlNCNameChar		= satisfy isXmlNCNameChar <?> "legal XML NCName character"
 
-- |
-- parse a XML NCName start character

xmlNCNameStartChar	:: GenParser Char state Unicode
xmlNCNameStartChar	= satisfy isXmlNCNameStartChar <?> "legal XML NCName start character"
 
-- |
-- parse a XML letter character

xmlLetter		:: GenParser Char state Unicode
xmlLetter		= satisfy isXmlLetter <?> "legal XML letter"

-- |
-- White Space (2.3)
--
-- end of line handling (2.11)
-- \#x0D and \#x0D\#x0A are mapped to \#x0A
-- is done in XmlInput before parsing
-- otherwise \#x0D in internal parsing, e.g. for entities would normalize,
-- would be transformed

xmlSpaceChar		:: GenParser Char state Char
xmlSpaceChar		= satisfy isXmlSpaceChar <?> "white space"

-- ------------------------------------------------------------

