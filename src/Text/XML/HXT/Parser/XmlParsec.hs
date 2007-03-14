-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.XmlParsec
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: XmlParsec.hs,v 1.14 2005/09/02 17:09:39 hxml Exp $

   Xml Parsec parser with pure filter interface

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.XmlParsec
    ( charData
    , charData'
    , comment
    , pI
    , cDSect
    , document
    , document'
    , prolog
    , xMLDecl
    , xMLDecl'
    , versionInfo
    , misc
    , doctypedecl
    , markupdecl
    , sDDecl
    , element
    , content
    , contentWithTextDecl
    , textDecl
    , encodingDecl
    , xread

    , parseXmlAttrValue
    , parseXmlContent
    , parseXmlDocEncodingSpec
    , parseXmlDocument
    , parseXmlDTDPart
    , parseXmlEncodingSpec
    , parseXmlEntityEncodingSpec
    , parseXmlGeneralEntityValue
    , parseXmlPart
    , parseXmlText

    , parseNMToken
    , parseName

    , removeEncodingSpec
    , substXmlEntities
    , xmlEntities
    )
where

import Text.ParserCombinators.Parsec
    ( GenParser
    , Parser
    , parse
    , (<?>), (<|>)
    , char
    , string
    , eof 
    , between
    , many, many1
    , option
    , try
    , unexpected
    , getPosition
    , getInput
    , sourceName
    )

import           Text.XML.HXT.DOM.XmlTree 		hiding     (choice)
import qualified Text.XML.HXT.DOM.XmlTree		as XmlTree (choice)

import Text.XML.HXT.Parser.XmlCharParser               (xmlChar)

import qualified Text.XML.HXT.Parser.XmlTokenParser	as XT
import qualified Text.XML.HXT.Parser.XmlDTDTokenParser	as XD

import Data.Char	(toLower)
import Data.Maybe

-- ------------------------------------------------------------
--
-- Character Data (2.4)

charData		:: GenParser Char state XmlTrees
charData
    = many (charData' <|> XT.referenceT)

charData'		:: GenParser Char state XmlTree
charData'
    = try ( do
	    t <- XT.allBut1 many1 (\ c -> not (c `elem` "<&")) "]]>"
	    return (mkXTextTree $! t)
	  )

-- ------------------------------------------------------------
--
-- Comments (2.5)

comment		:: GenParser Char state XmlTree

comment
    = ( do
	c <- between (try $ string "<!--") (string "-->") (XT.allBut many "--")
	return (mkXCmtTree $! c)
      ) <?> "comment"

-- ------------------------------------------------------------
--
-- Processing Instructions

pI		:: GenParser Char state XmlTree
pI
    = between (try $ string "<?") (string "?>")
      ( do
	n <- pITarget
	p <- option "" (do
			XT.sPace
			XT.allBut many "?>"
		       )
	return (mkXPiTree n $! p)
      ) <?> "processing instruction"
      where
      pITarget	:: GenParser Char state String
      pITarget = ( do
		   n <- XT.name
		   if map toLower n == t_xml
		      then unexpected n
		      else return n
		 )

-- ------------------------------------------------------------
--
-- CDATA Sections (2.7)

cDSect		:: GenParser Char state XmlTree

cDSect
    = do
      t <- between ( try $ string "<![CDATA[") (string "]]>") (XT.allBut many "]]>")
      return (mkXCdataTree $! t)
      <?> "CDATA section"

-- ------------------------------------------------------------
--
-- Document (2.1) and Prolog (2.8)

document	:: GenParser Char state XmlTree
document
    = do
      pos <- getPosition
      dl <- document'
      return ( (head . replaceChildren dl)
	       $ newDocument (sourceName pos)
	     )

document'	:: GenParser Char state XmlTrees
document'
    = do
      pl <- prolog
      el <- element
      ml <- many misc
      eof
      return (pl ++ [el] ++ ml)

prolog		:: GenParser Char state XmlTrees
prolog
    = do
      xml     <- option [] xMLDecl'
      misc1   <- many misc
      dtdPart <- option [] doctypedecl
      misc2   <- many misc
      return (xml ++ misc1 ++ dtdPart ++ misc2)

xMLDecl		:: GenParser Char state XmlTrees
xMLDecl
    = between (try $ string "<?xml") (string "?>")
      ( do
	vi <- versionInfo
	ed <- option [] encodingDecl
	sd <- option [] sDDecl
	XT.skipS0
	return (vi ++ ed ++ sd)
      )
      <?> "xml declaration"

xMLDecl'	:: GenParser Char state XmlTrees
xMLDecl'
    = do
      al <- xMLDecl
      return [mkXmlDeclTree al]

xMLDecl''	:: GenParser Char state XmlTree
xMLDecl''
    = do
      al     <- option [] (try xMLDecl)
      return (newRoot al)

versionInfo	:: GenParser Char state XmlTrees
versionInfo
    = ( do
	try ( do
	      XT.skipS
	      XT.keyword a_version
	    )
	XT.eq
	vi <- XT.quoted XT.versionNum
	return (xattr a_version vi)
      )
      <?> "version info (with quoted version number)"

misc		:: GenParser Char state XmlTree
misc
    = comment
      <|>
      pI
      <|>
      ( ( do
	  ws <- XT.sPace
	  return (mkXTextTree ws)
	) <?> ""
      )

-- ------------------------------------------------------------
--
-- Document Type definition (2.8)

doctypedecl	:: GenParser Char state XmlTrees
doctypedecl
    = between (try $ string "<!DOCTYPE") (char '>')
      ( do
	XT.skipS
	n <- XT.name
	exId <- option [] ( try ( do
				  XT.skipS
				  externalID
				)
			  )
	XT.skipS0
	markup <- option []
	          ( do
		    m <- between (char '[' ) (char ']') markupOrDeclSep
		    XT.skipS0
		    return m
		  )
	return [mkXDTDTree DOCTYPE ((a_name, n) : exId) markup]
      )

markupOrDeclSep	:: GenParser Char state XmlTrees
markupOrDeclSep
    = ( do
	ll <- many ( markupdecl
		     <|>
		     declSep
		     <|>
		     XT.mkList conditionalSect
		   )
	return (concat ll)
      )

declSep		:: GenParser Char state XmlTrees
declSep
    = XT.mkList XT.peReferenceT
      <|>
      ( do
	XT.skipS
	return []
      )

markupdecl	:: GenParser Char state XmlTrees
markupdecl
    = XT.mkList
      ( pI
	<|>
	comment
	<|>
	XD.dtdDeclTokenizer
      )

-- ------------------------------------------------------------
--
-- Standalone Document Declaration (2.9)

sDDecl		:: GenParser Char state XmlTrees
sDDecl
    = do
      try (do
	   XT.skipS
	   XT.keyword a_standalone
	  )
      XT.eq
      sd <- XT.quoted (XT.keywords [v_yes, v_no])
      return (xattr a_standalone sd)

-- ------------------------------------------------------------
--
-- element, tags and content (3, 3.1)

element		:: GenParser Char state XmlTree
element
    = ( do
	e <- elementStart
	elementRest e
      ) <?> "element"

elementStart		:: GenParser Char state (String, [(String, XmlTrees)])
elementStart
    = do
      n <- ( try ( do
		   char '<'
		   n <- XT.name
		   return n
		 )
	     <?> "start tag"
	   )
      ass <- attrList
      XT.skipS0
      return (n, ass)
      where
      attrList
	  = option [] ( do
			XT.skipS
			attrList'
		      )
      attrList'
	  = option [] ( do
			a1 <- attribute
			al <- attrList
			let (n, _v) = a1
			if isJust . lookup n $ al
			  then unexpected ("attribute name " ++ show n ++ " occurs twice in attribute list")
			  else return (a1 : al)
		      )

elementRest	:: (String, [(String, XmlTrees)]) -> GenParser Char state XmlTree
elementRest (n, al)
    = ( do
	try $ string "/>"
	return (mkXTagTree n (map (uncurry mkXAttrTree) al) [])
      )
      <|>
      ( do
	XT.gt
	c <- content
	eTag n
	return (mkXTagTree n (map ((uncurry mkXAttrTree) $!) al) $! c)
      )
      <?> "proper attribute list followed by \"/>\" or \">\""

eTag		:: String -> GenParser Char state ()
eTag n'
    = do
      try ( string "</" ) <?> ""
      n <- XT.name
      XT.skipS0
      XT.gt
      if n == n'
	 then return ()
	 else unexpected ("illegal end tag </" ++ n ++ "> found, </" ++ n' ++ "> expected")

attribute	:: GenParser Char state (String, XmlTrees)
attribute
    = do
      n <- XT.name
      XT.eq
      v <- XT.attrValueT
      return (n, v)

content		:: GenParser Char state XmlTrees
content
    = do
      c1 <- charData
      cl <- many
	    ( do
	      l <- ( element
		     <|>
		     cDSect
		     <|>
		     pI
		     <|>
		     comment
		   )
	      c <- charData
	      return (l : c)
	    )
      return (c1 ++ concat cl)

contentWithTextDecl	:: GenParser Char state XmlTrees
contentWithTextDecl
    = do
      option [] textDecl
      content

-- ------------------------------------------------------------
--
-- Conditional Sections (3.4)
--
-- conditional sections are parsed in two steps,
-- first the whole content is detected,
-- and then, after PE substitution include sections are parsed again

conditionalSect		:: GenParser Char state XmlTree
conditionalSect
    = do
      try $ string "<!["
      cs <- many XD.dtdToken
      char '['
      sect <- condSectCont
      return (mkXDTDTree CONDSECT [(a_value, sect)] cs)
    where

    condSectCont	:: GenParser Char state String
    condSectCont
	= ( do
	    try $ string "]]>"
	    return ""
	  )
          <|>
	  ( do
	    try $ string "<!["
	    cs1 <- condSectCont
	    cs2 <- condSectCont
	    return ("<![" ++ cs1 ++ "]]>" ++ cs2)
	  )
	  <|>
	  ( do
	    c  <- xmlChar
	    cs <- condSectCont
	    return (c : cs)
	  )

-- ------------------------------------------------------------
--
-- External Entities (4.2.2)

externalID	:: GenParser Char state Attributes
externalID
    = ( do
	XT.keyword k_system
	XT.skipS
	lit <- XT.systemLiteral
	return [(k_system, lit)]
      )
      <|>
      ( do
	XT.keyword k_public
	XT.skipS
	pl <- XT.pubidLiteral
	XT.skipS
	sl <- XT.systemLiteral
	return [ (k_system, sl)
	       , (k_public, pl) ]
      )
      <?> "SYSTEM or PUBLIC declaration"

-- ------------------------------------------------------------
--
-- Text Declaration (4.3.1)

textDecl	:: GenParser Char state XmlTrees
textDecl
    = between (try $ string "<?xml") (string "?>")
      ( do
	vi <- option [] versionInfo
	ed <- encodingDecl
	XT.skipS0
	return (vi ++ ed)
      )
      <?> "text declaration"


textDecl''	:: GenParser Char state XmlTree
textDecl''
    = do
      al    <- option [] (try textDecl)
      return (newRoot al)

-- ------------------------------------------------------------
--
-- Encoding Declaration (4.3.3)

encodingDecl	:: GenParser Char state XmlTrees
encodingDecl
    = do
      try (do
	   XT.skipS
	   XT.keyword a_encoding
	  )
      XT.eq
      ed <- XT.quoted XT.encName
      return (xattr a_encoding ed)

-- ------------------------------------------------------------
--
-- the main entry points:
--	parsing the content of a text node
--	or parsing the text children from a tag node

-- |
-- the inverse function to 'xshow', (for XML content).
--
-- the string parameter is parsed with the XML content parser.
-- result is the list of trees or in case of an error a single element list with the
-- error message as node. No entity or character subtitution is done.
-- For substitution of predefined XML entities 'substXmlEntities' can be used.
--
-- see also: 'parseXmlContent', 'substXmlEntities'

xread			:: String -> XmlTrees
xread str
    = parseXmlFromString parser loc str
    where
    loc = "string: " ++ show (if length str > 40 then take 40 str ++ "..." else str)
    parser = do
	     res <- content		-- take the content parser for parsing the string
	     eof			-- and test on everything consumed
	     return res

-- |
-- the filter version of 'xread'

parseXmlContent		:: XmlFilter
parseXmlContent
    = xread . xshow . this

-- |
-- a more general version of 'parseXmlContent'.
-- The parser to be used and the context are extra parameter

parseXmlText		:: Parser XmlTrees -> String -> XmlFilter
parseXmlText p loc	= parseXmlFromString p loc . xshow . this

parseXmlDocument	:: String -> String -> XmlTrees
parseXmlDocument	= parseXmlFromString document'


parseXmlFromString	:: Parser XmlTrees -> String -> String -> XmlTrees
parseXmlFromString parser loc
    = either (xerr . (++ "\n") . show) id . parse parser loc

-- ------------------------------------------------------------
--

removeEncodingSpec	:: XmlFilter
removeEncodingSpec (NTree (XText s) _cs)
    = either (xerr . (++ "\n") . show) xtext . parse parser "remove encoding spec" $ s
    where
    parser :: Parser String
    parser = do
	     option [] textDecl
	     getInput

removeEncodingSpec n
    = [n]

-- ------------------------------------------------------------

-- |
-- general parser for parsing arbitray parts of a XML document

parseXmlPart	:: Parser XmlTrees -> String -> String -> XmlFilter
parseXmlPart parser expected context t
    = parseXmlText ( do
		     res <- parser
		     eof <?> expected
		     return res
		   ) context
      $ t

-- ------------------------------------------------------------

-- |
-- Parser for parts of a DTD

parseXmlDTDPart	:: String -> XmlFilter
parseXmlDTDPart
    = parseXmlPart markupOrDeclSep "markup declaration"

-- ------------------------------------------------------------

-- |
-- Parser for general entites

parseXmlGeneralEntityValue	:: String -> XmlFilter
parseXmlGeneralEntityValue
    = parseXmlPart content "general entity value"

-- ------------------------------------------------------------

-- |
-- Parser for attribute values

parseXmlAttrValue	:: String -> XmlFilter
parseXmlAttrValue
    = parseXmlPart (XT.attrValueT' "<&") "attribute value"

-- ------------------------------------------------------------

-- |
-- Parser for NMTOKENs

parseNMToken		:: String -> XmlFilter
parseNMToken
    = parseXmlPart (many1 XT.nmtokenT) "nmtoken"

-- ------------------------------------------------------------

-- |
-- Parser for XML names

parseName		:: String -> XmlFilter
parseName
    = parseXmlPart (many1 XT.nameT) "name"

-- ------------------------------------------------------------

-- |
-- try to parse a xml encoding spec.
--
--
--    * 1.parameter encParse :  the parser for the encoding decl
--
--    - 2.parameter root :  a document root
--
--    - returns : the same tree, but with an additional
--			  attribute "encoding" in the root node
--			  in case of a valid encoding spec
--			  else the unchanged tree

parseXmlEncodingSpec	:: Parser XmlTree -> XmlFilter
parseXmlEncodingSpec encDecl
    =  parseEncSpec
      `when` isRoot
      where
      parseEncSpec r
	  = case ( parse encDecl source . xshow . getChildren $ r ) of
	    Right t
		-> addAttrl (const (getAttrl t)) r
	    Left _
		-> this r
	  where
	  source = valueOf a_source r

parseXmlEntityEncodingSpec	:: XmlFilter
parseXmlEntityEncodingSpec	= parseXmlEncodingSpec textDecl''

parseXmlDocEncodingSpec		:: XmlFilter
parseXmlDocEncodingSpec		= parseXmlEncodingSpec xMLDecl''

-- ------------------------------------------------------------

-- |
-- Filter for substitution of all occurences the predefined XML entites quot, amp, lt, gt, apos
-- by equivalent character references

substXmlEntities	:: XmlFilter
substXmlEntities
    = XmlTree.choice
      [ isXEntityRef	:-> substEntity
      , isXTag		:-> processAttr (processChildren substXmlEntities)
      , this		:-> this
      ]
      where
      substEntity t'@(NTree (XEntityRef en) _)
	  = case (lookup en xmlEntities) of
	    Just i
		-> [mkXCharRefTree i]
	    Nothing
		-> this t'

      substEntity _					-- just for preventing ghc warning
	  = error "substXmlEntities: illegal argument"

-- |
-- list of predefined XML entity names and their unicode values
--
-- used by 'substXmlEntities'

xmlEntities	:: [(String, Int)]
xmlEntities	= [ ("quot",	34)
		  , ("amp",	38)
		  , ("lt",	60)
		  , ("gt",	62)
		  , ("apos",	39)
		  ]

-- ------------------------------------------------------------
