-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.HtmlParsec
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   This parser tries to interprete everything as HTML
   no errors are emitted during parsing. If something looks
   weired, warning messages are inserted in the document tree.
  
   All filter are pure XmlFilter,
   errror handling and IO is done in 'Text.XML.HXT.Parser.HtmlParser'
   or other modules

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.HtmlParsec
    ( substHtmlEntities
    , parseHtmlText
    , parseHtmlDocument
    , parseHtmlContent
    , isEmptyHtmlTag
    , isInnerHtmlTagOf
    , closesHtmlTag
    , emptyHtmlTags
    )

where

import Text.XML.HXT.DOM.XmlTree

import Text.ParserCombinators.Parsec
    ( Parser
    , SourcePos
    , anyChar
    , between
    , char
    , eof
    , getPosition
    , many
    , noneOf
    , option
    , parse
    , satisfy
    , string
    , try
    , (<|>)
    )

import Text.XML.HXT.Parser.XmlTokenParser
    ( allBut
    , dq
    , eq
    , gt
    , name
    , pubidLiteral
    , skipS
    , skipS0
    , sq
    , systemLiteral

    , singleCharsT
    , referenceT
    )

import Text.XML.HXT.Parser.XmlParsec
    ( cDSect
    , charData'
    , misc
    , parseXmlText
    , pI
    , xMLDecl'
    )

import Text.XML.HXT.Parser.XmlCharParser
    ( xmlChar
    )

import Text.XML.HXT.Parser.XhtmlEntities
    ( xhtmlEntities
    )

import Data.Maybe
    ( fromMaybe
    )

import Data.Char
    ( toLower
    , toUpper
    )

-- ------------------------------------------------------------

parseHtmlText	:: String -> XmlFilter
parseHtmlText loc t
    = parseXmlText htmlDocument loc $ t

-- ------------------------------------------------------------

parseHtmlFromString	:: Parser XmlTrees -> String -> String -> XmlTrees
parseHtmlFromString parser loc
    = either (xerr . (++ "\n") . show) id . parse parser loc

parseHtmlDocument	:: String -> String -> XmlTrees
parseHtmlDocument	= parseHtmlFromString htmlDocument

parseHtmlContent	:: String -> XmlTrees
parseHtmlContent	= parseHtmlFromString htmlContent "text"

-- ------------------------------------------------------------

htmlDocument	:: Parser XmlTrees
htmlDocument
    = do
      pl <- htmlProlog
      el <- htmlContent
      eof
      return (pl ++ el)

htmlProlog	:: Parser XmlTrees
htmlProlog
    = do
      xml <- option []
	     ( try xMLDecl'
	       <|>
	       ( do
		 pos <- getPosition
		 try (string "<?")
		 return $ xwarn (show pos ++ " wrong XML declaration")
	       )
	     )
      misc1   <- many misc
      dtdPart <- option []
		 ( try doctypedecl
		   <|>
		   ( do
		     pos <- getPosition
		     try (upperCaseString "<!DOCTYPE")
		     return $ xwarn (show pos ++ " HTML DOCTYPE declaration ignored")
		   )
		 )
      return (xml ++ misc1 ++ dtdPart)

doctypedecl	:: Parser XmlTrees
doctypedecl
    = between (try $ upperCaseString "<!DOCTYPE") (char '>')
      ( do
	skipS
	n <- name
	exId <- ( do
	          skipS
		  option [] externalID
		)
	skipS0
	return [mkXDTDTree DOCTYPE ((a_name, n) : exId) []]
      )

externalID	:: Parser Attributes
externalID
    = do
      try (upperCaseString k_public)
      skipS
      pl <- pubidLiteral
      sl <- option "" $ try ( do
			      skipS
			      systemLiteral
			    )
      return $ (k_public, pl) : if null sl then [] else [(k_system, sl)]

htmlContent	:: Parser XmlTrees
htmlContent
    = option []
      ( do
	context <- hContent ([], [])
	pos     <- getPosition
	return $ closeTags pos context
      )
      where
      closeTags _ (body, [])
	  = reverse body
      closeTags pos' (body, ((tn, al, body1) : restOpen))
	  = closeTags pos' (addHtmlWarn (show pos' ++ ": no closing tag found for \"<" ++ tn ++ " ...>\"")
			    .
			    addHtmlTag tn al body
			    $
			    (body1, restOpen)
			   )

type OpenTags	= [(String, XmlTrees, XmlTrees)]
type Context	= (XmlTrees, OpenTags)

hElement	:: Context -> Parser Context
hElement context
    = ( do
	t <- hSimpleData
	return (addHtmlElems [t] context)
      )
      <|>
      hOpenTag context
      <|>
      hCloseTag context
      <|>
      ( do			-- wrong tag, take it as text
	pos <- getPosition
	c   <- xmlChar
	return ( addHtmlWarn (show pos ++ " markup char " ++ show c ++ " not allowed in this context")
		 .
		 addHtmlElems (xtext [c])
		 $
		 context
	       )
      )
      <|>
      ( do
	pos <- getPosition
	c <- anyChar
	return ( addHtmlWarn ( show pos
			       ++ " illegal data in input or illegal XML char "
			       ++ show c
			       ++ " found and ignored, possibly wrong encoding scheme used")
		 $
		 context
	       )
      )


hSimpleData	:: Parser XmlTree
hSimpleData
    = charData'
      <|>
      try referenceT
      <|>
      try hComment
      <|>
      try pI
      <|>
      try cDSect

hCloseTag	:: Context -> Parser Context
hCloseTag context
    = do
      n <- try ( do
		 string "</"
		 lowerCaseName
	       )
      skipS0
      pos <- getPosition
      checkSymbol gt ("closing > in tag \"</" ++ n ++ "\" expected") (closeTag pos n context)

hOpenTag	:: Context -> Parser Context
hOpenTag context
    = ( do
	pos <- getPosition
	e   <- hOpenTagStart
	hOpenTagRest pos e context
      )

hOpenTagStart	:: Parser (String, XmlTrees)
hOpenTagStart
    = do
      n <- try ( do
		 char '<'
		 n <- lowerCaseName
		 return n
	       )
      skipS0
      as <- hAttrList
      return (n, as)

hOpenTagRest	:: SourcePos -> (String, XmlTrees) -> Context -> Parser Context
hOpenTagRest pos (tn, al) context
    = ( do
	try $ string "/>"
	return (addHtmlTag tn al [] context)
      )
      <|>
      ( do
	context1 <- checkSymbol gt ("closing > in tag \"<" ++ tn ++ "...\" expected") context
	return ( let context2 = closePrevTag pos tn context1
		 in
		 ( if isEmptyHtmlTag tn
		   then addHtmlTag tn al []
		   else openTag tn al
		 ) context2
	       )
      )

hAttrList	:: Parser XmlTrees
hAttrList
    = many (try hAttribute)
      where
      hAttribute
	  = do
	    n <- lowerCaseName
	    v <- hAttrValue
	    skipS0
	    return $ mkXAttrTree n v

hAttrValue	:: Parser XmlTrees
hAttrValue
    = option []
      ( try ( do
	      eq
	      hAttrValue'
	    )
      )

hAttrValue'	:: Parser XmlTrees
hAttrValue'
    = try ( between dq dq (hAttrValue'' "&\"") )
      <|>
      try ( between sq sq (hAttrValue'' "&\'") )
      <|>
      ( do			-- HTML allows unquoted attribute values
	cs <- many (noneOf " \r\t\n>\"\'")
	return [mkXTextTree cs]
      )

hAttrValue''	:: String -> Parser XmlTrees
hAttrValue'' notAllowed
    = many ( hReference' <|> singleCharsT notAllowed)

hReference'	:: Parser XmlTree
hReference'
    = try referenceT
      <|>
      ( do
	char '&'
	return (mkXTextTree "&")
      )

hContent	:: Context -> Parser Context
hContent context
    = option context
      ( do
	context1 <- hElement context
	hContent context1
      )

-- hComment allows "--" in comments
-- comment from XML spec does not

hComment		:: Parser XmlTree
hComment
    = do
      c <- between (try $ string "<!--") (string "-->") (allBut many "-->")
      return (mkXCmtTree c)

checkSymbol	:: Parser a -> String -> Context -> Parser Context
checkSymbol p msg context
    = do
      pos <- getPosition
      option (addHtmlWarn (show pos ++ " " ++ msg) context)
       ( do 
	 try p
	 return context
       )

lowerCaseName	:: Parser String
lowerCaseName
    = do
      n <- name
      return (map toLower n)

upperCaseString	:: String -> Parser String
upperCaseString
    = sequence . map (\ c -> satisfy (( == c) . toUpper))

-- ------------------------------------------------------------

addHtmlTag	:: String -> XmlTrees -> XmlTrees -> Context -> Context
addHtmlTag tn al body (body1, openTags)
    = (xtag tn al (reverse body) ++ body1, openTags)

addHtmlWarn	:: String -> Context -> Context
addHtmlWarn msg
    = addHtmlElems (xwarn msg)

addHtmlElems	:: XmlTrees -> Context -> Context
addHtmlElems elems (body, openTags)
    = (reverse elems ++ body, openTags)

openTag		:: String -> XmlTrees -> Context -> Context
openTag tn al (body, openTags)
    = ([], (tn, al, body) : openTags)

closeTag	:: SourcePos -> String -> Context -> Context
closeTag pos n context
    | n `elem` (map ( \ (n1, _, _) -> n1) $ snd context)
	= closeTag' n context
    | otherwise
	= addHtmlWarn (show pos ++ " no opening tag found for </" ++ n ++ ">")
	  .
	  addHtmlTag n [] []
          $
	  context
    where
    closeTag' n' (body', (n1, al1, body1) : restOpen)
	= close context1
	  where
	  context1
	      = addHtmlTag n1 al1 body' (body1, restOpen)
	  close
	      | n' == n1
		= id
	      | n1 `isInnerHtmlTagOf` n'
		  = closeTag pos n'
	      | otherwise
		= addHtmlWarn (show pos ++ " no closing tag found for \"<" ++ n1 ++ " ...>\"")
		  .
		  closeTag' n'
    closeTag' _ _
	= error "illegal argument for closeTag'"

closePrevTag	:: SourcePos -> String -> Context -> Context
closePrevTag _pos _n context@(_body, [])
    = context
closePrevTag pos n context@(body, (n1, al1, body1) : restOpen)
    | n `closes` n1
	= closePrevTag pos n
	  ( addHtmlWarn (show pos ++ " tag \"<" ++ n1 ++ " ...>\" implicitly closed by opening tag \"<" ++ n ++ " ...>\"")
	    .
	    addHtmlTag n1 al1 body
	    $
	    (body1, restOpen)
	  )
    | otherwise
	= context

-- ------------------------------------------------------------
--
-- taken from HaXml and extended

isEmptyHtmlTag	:: String -> Bool
isEmptyHtmlTag n
    = n `elem`
      emptyHtmlTags

emptyHtmlTags	:: [String]
emptyHtmlTags
    = [ "area"
      , "base"
      , "br"
      , "col"
      , "frame"
      , "hr"
      , "img"
      , "input"
      , "link"
      , "meta"
      , "param"
      ]

isInnerHtmlTagOf	:: String -> String -> Bool
n `isInnerHtmlTagOf` tn
    = n `elem`
      ( fromMaybe [] . lookup tn
      $ [ ("body",    ["p"])
	, ("caption", ["p"])
	, ("dd",      ["p"])
	, ("div",     ["p"])
	, ("dl",      ["dt","dd"])
	, ("dt",      ["p"])
	, ("li",      ["p"])
	, ("map",     ["p"])
	, ("object",  ["p"])
	, ("ol",      ["li"])
	, ("table",   ["th","tr","td","thead","tfoot","tbody"])
	, ("tbody",   ["th","tr","td"])
	, ("td",      ["p"])
	, ("tfoot",   ["th","tr","td"])
	, ("th",      ["p"])
	, ("thead",   ["th","tr","td"])
	, ("tr",      ["th","td"])
	, ("ul",      ["li"])
	]
      )

closesHtmlTag
  , closes :: String -> String -> Bool

closesHtmlTag	= closes

"a"	`closes` "a"					= True
"li"	`closes` "li"					= True
"th"	`closes`  t    | t `elem` ["th","td"]		= True
"td"	`closes`  t    | t `elem` ["th","td"]		= True
"tr"	`closes`  t    | t `elem` ["th","td","tr"]	= True
"dt"	`closes`  t    | t `elem` ["dt","dd"]		= True
"dd"	`closes`  t    | t `elem` ["dt","dd"]		= True
"hr"	`closes`  "p"                                   = True
"colgroup" 
	`closes` "colgroup"				= True
"form"	`closes` "form"					= True
"label"	`closes` "label"				= True
"map"	`closes` "map"					= True
"object"
	`closes` "object"				= True
_	`closes` t  | t `elem` ["option"
			       ,"script"
			       ,"style"
			       ,"textarea"
			       ,"title"
			       ]			= True
t	`closes` "select" | t /= "option"		= True
"thead"	`closes` t  | t `elem` ["colgroup"]		= True
"tfoot"	`closes` t  | t `elem` ["thead"
			       ,"colgroup"]		= True
"tbody"	`closes` t  | t `elem` ["tbody"
			       ,"tfoot"
			       ,"thead"
			       ,"colgroup"]		= True
t	`closes` t2 | t `elem` ["h1","h2","h3"
			       ,"h4","h5","h6"
			       ,"dl","ol","ul"
			       ,"table"
			       ,"div","p"
			       ]
		      &&
                      t2 `elem` ["h1","h2","h3"
				,"h4","h5","h6"
				,"p"			-- not "div"
				]			= True
_	`closes` _					= False

-- ------------------------------------------------------------
--
-- XHTML entities

substHtmlEntities	:: XmlFilter
substHtmlEntities
    = choice [ isXEntityRef	:-> substEntity
	     , isXTag		:-> processAttr (processChildren substHtmlEntities)
	     , this		:-> this
	     ]
      where
      substEntity t'@(NTree (XEntityRef en) _)
	  = case (lookup en xhtmlEntities) of
	    Just i
		-> [mkXCharRefTree i]
	    Nothing
		-> xwarn ("no XHTML entity found for reference: \"&" ++ en ++ ";\"")
		   ++
		   (xmlTreesToText [t'])

      substEntity _
	  = error "substHtmlEntities: illegal argument"

-- ------------------------------------------------------------
