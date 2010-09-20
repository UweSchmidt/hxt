-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.XmlParsec
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

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

import Text.XML.HXT.DOM.ShowXml
    ( xshow
    )

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.DOM.XmlNode
    ( mkElement
    , mkAttr
    , mkRoot
    , mkDTDElem
    , mkText
    , mkCmt
    , mkCdata
    , mkError
    , mkPi
    , isText
    , isRoot
    , getText
    , getChildren
    , getAttrl
    , getAttrName
    , changeAttrl
    , mergeAttrl
    )

import Text.XML.HXT.Parser.XmlCharParser
    ( xmlChar
    )
import qualified Text.XML.HXT.Parser.XmlTokenParser     as XT
import qualified Text.XML.HXT.Parser.XmlDTDTokenParser  as XD

import Data.Char        (toLower)
import Data.Maybe

-- ------------------------------------------------------------
--
-- Character Data (2.4)

charData                :: GenParser Char state XmlTrees
charData
    = many (charData' <|> XT.referenceT)

charData'               :: GenParser Char state XmlTree
charData'
    = try ( do
            t <- XT.allBut1 many1 (\ c -> not (c `elem` "<&")) "]]>"
            return (mkText $! t)
          )

-- ------------------------------------------------------------
--
-- Comments (2.5)

comment         :: GenParser Char state XmlTree

comment
    = ( do
        c <- between (try $ string "<!--") (string "-->") (XT.allBut many "--")
        return (mkCmt $! c)
      ) <?> "comment"

-- ------------------------------------------------------------
--
-- Processing Instructions

pI              :: GenParser Char state XmlTree
pI
    = between (try $ string "<?") (string "?>")
      ( do
        n <- pITarget
        p <- option "" (do
                        _ <- XT.sPace
                        XT.allBut many "?>"
                       )
        return $ mkPi (mkName n) [mkAttr (mkName a_value) [mkText p]]
      ) <?> "processing instruction"
      where
      pITarget  :: GenParser Char state String
      pITarget = ( do
                   n <- XT.name
                   if map toLower n == t_xml
                      then unexpected n
                      else return n
                 )

-- ------------------------------------------------------------
--
-- CDATA Sections (2.7)

cDSect          :: GenParser Char state XmlTree

cDSect
    = do
      t <- between ( try $ string "<![CDATA[") (string "]]>") (XT.allBut many "]]>")
      return (mkCdata $! t)
      <?> "CDATA section"

-- ------------------------------------------------------------
--
-- Document (2.1) and Prolog (2.8)

document        :: GenParser Char state XmlTree
document
    = do
      pos <- getPosition
      dl <- document'
      return $ mkRoot [ mkAttr (mkName a_source) [mkText (sourceName pos)]
                      , mkAttr (mkName a_status) [mkText (show c_ok)]
                      ] dl

document'       :: GenParser Char state XmlTrees
document'
    = do
      pl <- prolog
      el <- element
      ml <- many misc
      eof
      return (pl ++ [el] ++ ml)

prolog          :: GenParser Char state XmlTrees
prolog
    = do
      xml     <- option [] xMLDecl'
      misc1   <- many misc
      dtdPart <- option [] doctypedecl
      misc2   <- many misc
      return (xml ++ misc1 ++ dtdPart ++ misc2)

xMLDecl         :: GenParser Char state XmlTrees
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

xMLDecl'        :: GenParser Char state XmlTrees
xMLDecl'
    = do
      al <- xMLDecl
      return [mkPi (mkName t_xml) al]

xMLDecl''       :: GenParser Char state XmlTree
xMLDecl''
    = do
      al     <- option [] (try xMLDecl)
      return (mkRoot al [])

versionInfo     :: GenParser Char state XmlTrees
versionInfo
    = ( do
        _ <- try ( do
                   XT.skipS
                   XT.keyword a_version
                 )
        XT.eq
        vi <- XT.quoted XT.versionNum
        return [mkAttr (mkName a_version) [mkText vi]]
      )
      <?> "version info (with quoted version number)"

misc            :: GenParser Char state XmlTree
misc
    = comment
      <|>
      pI
      <|>
      ( ( do
          ws <- XT.sPace
          return (mkText ws)
        ) <?> ""
      )

-- ------------------------------------------------------------
--
-- Document Type definition (2.8)

doctypedecl     :: GenParser Char state XmlTrees
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
        return [mkDTDElem DOCTYPE ((a_name, n) : exId) markup]
      )

markupOrDeclSep :: GenParser Char state XmlTrees
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

declSep         :: GenParser Char state XmlTrees
declSep
    = XT.mkList XT.peReferenceT
      <|>
      ( do
        XT.skipS
        return []
      )

markupdecl      :: GenParser Char state XmlTrees
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

sDDecl          :: GenParser Char state XmlTrees
sDDecl
    = do
      _ <- try (do
                XT.skipS
                XT.keyword a_standalone
               )
      XT.eq
      sd <- XT.quoted (XT.keywords [v_yes, v_no])
      return [mkAttr (mkName a_standalone) [mkText sd]]

-- ------------------------------------------------------------
--
-- element, tags and content (3, 3.1)

element         :: GenParser Char state XmlTree
element
    = ( do
        e <- elementStart
        elementRest e
      ) <?> "element"

elementStart            :: GenParser Char state (String, [(String, XmlTrees)])
elementStart
    = do
      n <- ( try ( do
                   _ <- char '<'
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

elementRest     :: (String, [(String, XmlTrees)]) -> GenParser Char state XmlTree
elementRest (n, al)
    = ( do
        _ <- try $ string "/>"
        return $! (mkElement (mkName n) (map (mkA $!) al) [])
      )
      <|>
      ( do
        _ <- XT.gt
        c <- content
        eTag n
        return $! (mkElement (mkName n) (map (mkA $!) al) $! c)
      )
      <?> "proper attribute list followed by \"/>\" or \">\""
    where
    mkA (n', ts') = mkAttr (mkName n') ts'

eTag            :: String -> GenParser Char state ()
eTag n'
    = do
      _ <- try ( string "</" ) <?> ""
      n <- XT.name
      XT.skipS0
      _ <- XT.gt
      if n == n'
         then return ()
         else unexpected ("illegal end tag </" ++ n ++ "> found, </" ++ n' ++ "> expected")

attribute       :: GenParser Char state (String, XmlTrees)
attribute
    = do
      n <- XT.name
      XT.eq
      v <- XT.attrValueT
      return (n, v)

content         :: GenParser Char state XmlTrees
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

contentWithTextDecl     :: GenParser Char state XmlTrees
contentWithTextDecl
    = do
      _ <- option [] textDecl
      content

-- ------------------------------------------------------------
--
-- Conditional Sections (3.4)
--
-- conditional sections are parsed in two steps,
-- first the whole content is detected,
-- and then, after PE substitution include sections are parsed again

conditionalSect         :: GenParser Char state XmlTree
conditionalSect
    = do
      _ <- try $ string "<!["
      cs <- many XD.dtdToken
      _ <- char '['
      sect <- condSectCont
      return (mkDTDElem CONDSECT [(a_value, sect)] cs)
    where

    condSectCont        :: GenParser Char state String
    condSectCont
        = ( do
            _ <- try $ string "]]>"
            return ""
          )
          <|>
          ( do
            _ <- try $ string "<!["
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

externalID      :: GenParser Char state Attributes
externalID
    = ( do
        _ <- XT.keyword k_system
        XT.skipS
        lit <- XT.systemLiteral
        return [(k_system, lit)]
      )
      <|>
      ( do
        _ <- XT.keyword k_public
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

textDecl        :: GenParser Char state XmlTrees
textDecl
    = between (try $ string "<?xml") (string "?>")
      ( do
        vi <- option [] versionInfo
        ed <- encodingDecl
        XT.skipS0
        return (vi ++ ed)
      )
      <?> "text declaration"


textDecl''      :: GenParser Char state XmlTree
textDecl''
    = do
      al    <- option [] (try textDecl)
      return (mkRoot al [])

-- ------------------------------------------------------------
--
-- Encoding Declaration (4.3.3)

encodingDecl    :: GenParser Char state XmlTrees
encodingDecl
    = do
      _ <- try ( do
                 XT.skipS
                 XT.keyword a_encoding
               )
      XT.eq
      ed <- XT.quoted XT.encName
      return [mkAttr (mkName a_encoding) [mkText ed]]

-- ------------------------------------------------------------
--
-- the main entry points:
--      parsing the content of a text node
--      or parsing the text children from a tag node

-- |
-- the inverse function to 'xshow', (for XML content).
--
-- the string parameter is parsed with the XML content parser.
-- result is the list of trees or in case of an error a single element list with the
-- error message as node. No entity or character subtitution is done.
--
-- see also: 'parseXmlContent'

xread                   :: String -> XmlTrees
xread str
    = parseXmlFromString parser loc str
    where
    loc = "string: " ++ show (if length str > 40 then take 40 str ++ "..." else str)
    parser = do
             res <- content             -- take the content parser for parsing the string
             eof                        -- and test on everything consumed
             return res

-- |
-- the filter version of 'xread'

parseXmlContent         :: XmlTree -> XmlTrees
parseXmlContent
    = xread . xshow . (:[])

-- |
-- a more general version of 'parseXmlContent'.
-- The parser to be used and the context are extra parameter

parseXmlText            :: Parser XmlTrees -> String -> XmlTree -> XmlTrees
parseXmlText p loc      = parseXmlFromString p loc . xshow . (:[])

parseXmlDocument        :: String -> String -> XmlTrees
parseXmlDocument        = parseXmlFromString document'


parseXmlFromString      :: Parser XmlTrees -> String -> String -> XmlTrees
parseXmlFromString parser loc
    = either ((:[]) . mkError c_err . (++ "\n") . show) id . parse parser loc

-- ------------------------------------------------------------
--

removeEncodingSpec      :: XmlTree -> XmlTrees
removeEncodingSpec t
    | isText t
        = ( either ((:[]) . mkError c_err . (++ "\n") . show) ((:[]) . mkText)
            . parse parser "remove encoding spec"
            . fromMaybe ""
            . getText
          ) t
    | otherwise
        = [t]
    where
    parser :: Parser String
    parser = do
             _ <- option [] textDecl
             getInput

-- ------------------------------------------------------------

-- |
-- general parser for parsing arbitray parts of a XML document

parseXmlPart    :: Parser XmlTrees -> String -> String -> XmlTree -> XmlTrees
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

parseXmlDTDPart :: String -> XmlTree -> XmlTrees
parseXmlDTDPart
    = parseXmlPart markupOrDeclSep "markup declaration"

-- ------------------------------------------------------------

-- |
-- Parser for general entites

parseXmlGeneralEntityValue      :: String -> XmlTree -> XmlTrees
parseXmlGeneralEntityValue
    = parseXmlPart content "general entity value"

-- ------------------------------------------------------------

-- |
-- Parser for attribute values

parseXmlAttrValue       :: String -> XmlTree -> XmlTrees
parseXmlAttrValue
    = parseXmlPart (XT.attrValueT' "<&") "attribute value"

-- ------------------------------------------------------------

-- |
-- Parser for NMTOKENs

parseNMToken            :: String -> XmlTree -> XmlTrees
parseNMToken
    = parseXmlPart (many1 XT.nmtokenT) "nmtoken"

-- ------------------------------------------------------------

-- |
-- Parser for XML names

parseName               :: String -> XmlTree -> XmlTrees
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
--                        attribute \"encoding\" in the root node
--                        in case of a valid encoding spec
--                        else the unchanged tree

parseXmlEncodingSpec    :: Parser XmlTree -> XmlTree -> XmlTrees
parseXmlEncodingSpec encDecl x
    = (:[]) .
      ( if isRoot x
        then parseEncSpec
        else id
      ) $ x
    where
    parseEncSpec r
        = case ( parse encDecl source . xshow . getChildren $ r ) of
          Right t
              -> changeAttrl (mergeAttrl . fromMaybe [] . getAttrl $ t) r
          Left _
              -> r
        where
        -- arrow \"getAttrValue a_source\" programmed on the tree level (oops!)
        source = xshow . concat . map getChildren . filter ((== a_source) . maybe "" qualifiedName . getAttrName) . fromMaybe [] . getAttrl $ r

parseXmlEntityEncodingSpec      :: XmlTree -> XmlTrees
parseXmlEntityEncodingSpec      = parseXmlEncodingSpec textDecl''

parseXmlDocEncodingSpec         :: XmlTree -> XmlTrees
parseXmlDocEncodingSpec         = parseXmlEncodingSpec xMLDecl''

-- ------------------------------------------------------------
