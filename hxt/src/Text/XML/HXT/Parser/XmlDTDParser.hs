-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.XmlDTDParser
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Parsec parser for DTD declarations for ELEMENT, ATTLIST, ENTITY and NOTATION declarations

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.XmlDTDParser
    ( parseXmlDTDdecl
    , parseXmlDTDdeclPart
    , parseXmlDTDEntityValue
    , elementDecl
    , attlistDecl
    , entityDecl
    , notationDecl
    )
where

import Data.Maybe

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

import Text.XML.HXT.DOM.Interface



import Text.XML.HXT.DOM.ShowXml
    ( xshow
    )
import Text.XML.HXT.DOM.XmlNode
    ( mkDTDElem
    , mkText
    , mkError
    , isText
    , isDTD
    , getText
    , getDTDPart
    , getDTDAttrl
    , getChildren
    , setChildren
    )
import qualified Text.XML.HXT.Parser.XmlTokenParser    as XT
import qualified Text.XML.HXT.Parser.XmlCharParser     as XC ( xmlSpaceChar )
import qualified Text.XML.HXT.Parser.XmlDTDTokenParser as XD ( dtdToken )

-----------------------------------------------------------
--
-- all parsers dealing with whitespace will be redefined
-- to handle parameter entity substitution

type LocalState = (Int, [(Int, String, SourcePos)])

type SParser a  = GenParser Char LocalState a

initialLocalState       :: SourcePos -> LocalState
initialLocalState p     = (0, [(0, sourceName p, p)])

pushPar         :: String -> SParser ()
pushPar n       = do
                  p <- getPosition
                  updateState (\ (i, s) -> (i+1, (i+1, n, p) : s))
                  setPosition ( newPos (sourceName p ++ " (line " ++ show (sourceLine p) ++ ", column " ++ show (sourceColumn p) ++ ") in content of parameter entity ref %" ++ n ++ ";") 1 1)

popPar          :: SParser ()
popPar          = do
                  oldPos <- getPos
                  updateState pop
                  setPosition oldPos
                where
                pop (i, [(_, s, p)]) = (i+1, [(i+1, s, p)])     -- if param entity substitution is correctly implemented, this case does not occur
                pop (i, _t:s)        = (i, s)
                pop (_i, [])         = undefined                -- stack is never empty

getParNo        :: SParser Int
getParNo        = do
                  (_i, (top, _n, _p) : _s) <- getState
                  return top

getPos          :: SParser SourcePos
getPos          = do
                  (_i, (_top, _n, p) : _s) <- getState
                  return p

delPE   :: SParser ()
delPE   = do
          _ <- char '\0'
          return ()

startPE :: SParser ()
startPE
    = do
      try ( do
            delPE
            n <- many1 (satisfy (/= '\0'))
            delPE
            pushPar n
          )

endPE   :: SParser ()
endPE
    = do
      try (do
           delPE
           delPE
           popPar
          )

inSamePE        :: SParser a -> SParser a
inSamePE p
    = do
      i <- getParNo
      r <- p
      j <- getParNo
      if (i == j)
         then return r
         else fail $ "parameter entity contents does not fit into the structure of a DTD declarations"

-- ------------------------------------------------------------

xmlSpaceChar    :: SParser ()
xmlSpaceChar    = ( do
                    _ <- XC.xmlSpaceChar
                    return ()
                  )
                  <|>
                  startPE
                  <|>
                  endPE
                  <?> "white space"

skipS           :: SParser ()
skipS
    = do
      skipMany1 xmlSpaceChar
      return ()

skipS0          :: SParser ()
skipS0
    = do
      skipMany xmlSpaceChar
      return ()

name            :: SParser XmlTree
name
    = do
      n <- XT.name
      return (mkDTDElem NAME [(a_name, n)] [])

nmtoken         :: SParser XmlTree
nmtoken
    = do
      n <- XT.nmtoken
      return (mkDTDElem NAME [(a_name, n)] [])

-- ------------------------------------------------------------
--
-- Element Type Declarations (3.2)

elementDecl     :: SParser XmlTrees
elementDecl
    = between (try $ string "<!ELEMENT") (char '>') elementDeclBody

elementDeclBody :: SParser XmlTrees
elementDeclBody
    = do
      skipS
      n <- XT.name
      skipS
      (al, cl) <- contentspec
      skipS0
      return [mkDTDElem ELEMENT ((a_name, n) : al) cl]

contentspec     :: SParser (Attributes, XmlTrees)
contentspec
    = simplespec k_empty v_empty
      <|>
      simplespec k_any v_any
      <|>
      inSamePE mixed
      <|>
      inSamePE children
      <?> "content specification"
    where
    simplespec kw v
        = do
          _ <- XT.keyword kw
          return ([(a_type, v)], [])

-- ------------------------------------------------------------
--
-- Element Content (3.2.1)

children        :: SParser (Attributes, XmlTrees)
children
    = ( do
        (al, cl) <- choiceOrSeq
        modifier <- optOrRep
        return ([(a_type, v_children)], [mkDTDElem CONTENT (modifier ++ al) cl])
      )
      <?> "element content"

optOrRep        :: SParser Attributes
optOrRep
    = do
      m <- option "" (XT.mkList (oneOf "?*+"))
      return [(a_modifier, m)]

choiceOrSeq     :: SParser (Attributes, XmlTrees)
choiceOrSeq
    = inSamePE $
      do
      cl <- try ( do
                  lpar
                  choiceOrSeqBody
                )
      rpar
      return cl

choiceOrSeqBody :: SParser (Attributes, XmlTrees)
choiceOrSeqBody
    = do
      cp1 <- cp
      choiceOrSeq1 cp1
    where
    choiceOrSeq1        :: XmlTree -> SParser (Attributes, XmlTrees)
    choiceOrSeq1 c1
        = ( do
            bar
            c2 <- cp
            cl <- many ( do
                         bar
                         cp
                       )
            return ([(a_kind, v_choice)], (c1 : c2 : cl))
          )
          <|>
          ( do
            cl <- many ( do
                         comma
                         cp
                       )
            return ([(a_kind, v_seq)], (c1 : cl))
          )
          <?> "sequence or choice"

cp              :: SParser XmlTree
cp
    = ( do
        n <- name
        m <- optOrRep
        return ( case m of
                 [(_, "")] -> n
                 _         -> mkDTDElem CONTENT (m ++ [(a_kind, v_seq)]) [n]
               )
      )
      <|>
      ( do
        (al, cl) <- choiceOrSeq
        m <- optOrRep
        return (mkDTDElem CONTENT (m ++ al) cl)
      )

-- ------------------------------------------------------------
--
-- Mixed Content (3.2.2)

mixed           :: SParser (Attributes, XmlTrees)
mixed
    = ( do
        _ <- try ( do
                   lpar
                   string k_pcdata
                 )
        nl <- many ( do
                     bar
                     name
                   )
        rpar
        if null nl
          then do
               _ <- option ' ' (char '*')               -- (#PCDATA) or (#PCDATA)* , both are legal
               return ( [ (a_type, v_pcdata) ]
                      , []
                      )
          else do
               _ <- char '*' <?> "closing parent for mixed content (\")*\")"
               return ( [ (a_type, v_mixed) ]
                      , [ mkDTDElem CONTENT [ (a_modifier, "*")
                                             , (a_kind, v_choice)
                                             ] nl
                        ]
                      )
      )
      <?> "mixed content"

-- ------------------------------------------------------------
--
-- Attribute-List Declarations (3.3)

attlistDecl             :: SParser XmlTrees
attlistDecl
    = between (try $ string "<!ATTLIST") (char '>') attlistDeclBody

attlistDeclBody         :: SParser XmlTrees
attlistDeclBody
    = do
      skipS
      n <- XT.name
      al <- many attDef
      skipS0
      return (map (mkDTree n) al)
    where
    mkDTree n' (al, cl)
        = mkDTDElem ATTLIST ((a_name, n') : al) cl

attDef          :: SParser (Attributes, XmlTrees)
attDef
    = do
      n <- try ( do
                 skipS
                 XT.name
               ) <?> "attribute name"
      skipS
      (t, cl) <- attType
      skipS
      d <- defaultDecl
      return (((a_value, n) : d) ++ t, cl)

attType :: SParser (Attributes, XmlTrees)
attType
    = tokenizedOrStringType
      <|>
      enumeration
      <|>
      notationType
      <?> "attribute type"

tokenizedOrStringType   :: SParser (Attributes, XmlTrees)
tokenizedOrStringType
    = do
      n <- choice $ map XT.keyword typl
      return ([(a_type, n)], [])
      where
      typl      = [ k_cdata
                  , k_idrefs
                  , k_idref
                  , k_id
                  , k_entity
                  , k_entities
                  , k_nmtokens
                  , k_nmtoken
                  ]

enumeration     :: SParser (Attributes, XmlTrees)
enumeration
    = do
      nl <- inSamePE (between lpar rpar (sepBy1 nmtoken bar))
      return ([(a_type, k_enumeration)], nl)

notationType    :: SParser (Attributes, XmlTrees)
notationType
    = do
      _ <- XT.keyword k_notation
      skipS
      nl <- inSamePE (between lpar rpar ( sepBy1 name bar ))
      return ([(a_type, k_notation)], nl)

defaultDecl     :: SParser Attributes
defaultDecl
    = ( do
        str <- try $ string k_required
        return [(a_kind, str)]
      )
      <|>
      ( do
        str <- try $ string k_implied
        return [(a_kind, str)]
      )
      <|>
      ( do
        l <- fixed
        v <- XT.attrValueT
        return ((a_default, xshow v) : l)
      )
      <?> "default declaration"
    where
    fixed = option [(a_kind, k_default)]
            ( do
              _ <- try $ string k_fixed
              skipS
              return [(a_kind, k_fixed)]
            )

-- ------------------------------------------------------------
--
-- Entity Declarations (4.2)

entityDecl              :: SParser XmlTrees
entityDecl
    = between ( try $ string "<!ENTITY" ) (char '>') entityDeclBody

entityDeclBody          :: SParser XmlTrees
entityDeclBody
    = do
      skipS
      ( peDecl
        <|>
        geDecl
        <?> "entity declaration" )      -- don't move the ) to the next line

geDecl                  :: SParser XmlTrees
geDecl
    = do
      n <- XT.name
      skipS
      (al, cl) <- entityDef
      skipS0
      return [mkDTDElem ENTITY ((a_name, n) : al) cl]

entityDef               :: SParser (Attributes, XmlTrees)
entityDef
    = entityValue
      <|>
      externalEntitySpec

externalEntitySpec      :: SParser (Attributes, XmlTrees)
externalEntitySpec
    = do
      al <- externalID
      nd <- option [] nDataDecl
      return ((al ++ nd), [])

peDecl                  :: SParser XmlTrees
peDecl
    = do
      _ <- char '%'
      skipS
      n <- XT.name
      skipS
      (al, cs) <- peDef
      skipS0
      return [mkDTDElem PENTITY ((a_name, n) : al) cs]

peDef                   :: SParser (Attributes, XmlTrees)
peDef
    = entityValue
      <|>
      do
      al <- externalID
      return (al, [])

entityValue     :: GenParser Char state (Attributes, XmlTrees)
entityValue
    = do
      v <- XT.entityValueT
      return ([], v)

-- ------------------------------------------------------------
--
-- External Entities (4.2.2)

externalID      :: SParser Attributes
externalID
    = ( do
        _ <- XT.keyword k_system
        skipS
        lit <- XT.systemLiteral
        return [(k_system, lit)]
      )
      <|>
      ( do
        _ <- XT.keyword k_public
        skipS
        pl <- XT.pubidLiteral
        skipS
        sl <- XT.systemLiteral
        return [ (k_system, sl)
               , (k_public, pl) ]
      )
      <?> "SYSTEM or PUBLIC declaration"

nDataDecl       :: SParser Attributes
nDataDecl
    = do
      _ <- try ( do
                 skipS
                 XT.keyword k_ndata
               )
      skipS
      n <- XT.name
      return [(k_ndata, n)]

-- ------------------------------------------------------------
--
-- Notation Declarations (4.7)

notationDecl            :: SParser XmlTrees
notationDecl
    = between (try $ string "<!NOTATION") (char '>' <?> "notation declaration") notationDeclBody

notationDeclBody        :: SParser XmlTrees
notationDeclBody
    = do
      skipS
      n <- XT.name
      skipS
      eid <- ( try externalID
               <|>
               publicID
             )
      skipS0
      return [mkDTDElem NOTATION ((a_name, n) : eid) []]

publicID                :: SParser Attributes
publicID
    = do
      _ <- XT.keyword k_public
      skipS
      l <- XT.pubidLiteral
      return [(k_public, l)]

-- ------------------------------------------------------------

condSectCondBody        :: SParser XmlTrees
condSectCondBody
    = do
      skipS0
      n <- XT.name
      skipS0
      let n' = stringToUpper n
      if n' `elem` [k_include, k_ignore]
         then return [mkText  n']
         else fail $ "INCLUDE or IGNORE expected in conditional section"

-- ------------------------------------------------------------

separator       :: Char -> SParser ()
separator c
    = do
      _ <- try ( do
                 skipS0
                 char c
               )
      skipS0
      <?> [c]


bar, comma, lpar, rpar  :: SParser ()

bar     = separator '|'
comma   = separator ','

lpar
    = do
      _ <- char '('
      skipS0

rpar
    = do
      skipS0
      _ <- char ')'
      return ()


-- ------------------------------------------------------------

parseXmlDTDEntityValue  :: XmlTree -> XmlTrees
parseXmlDTDEntityValue t        -- (NTree (XDTD PEREF al) cl)
    | isDTDPEref t
        = ( either
            ( (:[]) . mkError c_err . (++ "\n") . show )
            ( \cl' -> if null cl'
                         then [mkText ""]
                         else cl'
            )
            .
            parse parser source
          ) input
    | otherwise
        = []
    where
    al     = fromMaybe [] . getDTDAttrl $ t
    cl     = getChildren t
    parser = XT.entityTokensT "%&"
    source = "value of parameter entity " ++ lookupDef "" a_peref al
    input  = xshow cl

{-
parseXmlDTDEntityValue n
    = error ("parseXmlDTDEntityValue: illegal argument: " ++ show n)
-}
-- ------------------------------------------------------------

parseXmlDTDdeclPart     :: XmlTree -> XmlTrees
parseXmlDTDdeclPart t           -- @(NTree (XDTD PEREF al) cl)
    | isDTDPEref t
        = ( (:[])
            .
            either
               ( mkError c_err . (++ "\n") . show )
               ( flip setChildren $ t ) -- \ cl' -> setChildren cl' t)
            .
            parse parser source
          ) input
    | otherwise
        = []
    where
    al     = fromMaybe [] . getDTDAttrl $ t
    cl     = getChildren t
    parser = many XD.dtdToken
    source = "value of parameter entity " ++ lookupDef "" a_peref al
    input  = xshow cl

{-
parseXmlDTDdeclPart n
    = error ("parseXmlDTDdeclPart: illegal argument: " ++ show n)
-}
-- ------------------------------------------------------------
--
-- the main entry point

-- | parse a tokenized DTD declaration represented by a DTD tree.
-- The content is represented by the children containing text and parameter entity reference nodes.
-- The parameter entity reference nodes contain their value in the children list, consisting of text
-- and possibly again parameter entity reference nodes. This structure is build by the parameter entity
-- substitution.
-- Output is again a DTD declaration node, but this time completely parsed and ready for further DTD processing

parseXmlDTDdecl :: XmlTree -> XmlTrees
parseXmlDTDdecl t       -- (NTree (XDTD dtdElem al) cl)
    | isDTD t
        = ( either ((:[]) . mkError c_err . (++ "\n") . show) id
            .
            runParser parser (initialLocalState pos) source
          ) input
    | otherwise
        = []
    where
    dtdElem = fromJust     . getDTDPart  $ t
    al      = fromMaybe [] . getDTDAttrl $ t
    cl      = getChildren t
    dtdParsers
        = [ (ELEMENT,  elementDeclBody)
          , (ATTLIST,  attlistDeclBody)
          , (ENTITY,   entityDeclBody)
          , (NOTATION, notationDeclBody)
          , (CONDSECT, condSectCondBody)
          ]
    source = lookupDef "DTD declaration" a_source al
    line   = lookupDef "1" a_line al
    column = lookupDef "1" a_column al
    pos    = newPos source (read line) (read column)
    parser = do
             setPosition pos
             res <- fromJust . lookup dtdElem $ dtdParsers
             eof
             return res
    input  = concatMap collectText cl
{-
parseXmlDTDdecl _
    = []
-}

-- | collect the tokens of a DTD declaration body and build
-- a string ready for parsing. The structure of the parameter entity values
-- is stll stored in this string for checking the scope of the parameter values

collectText     :: XmlTree -> String

collectText t
    | isText t
        = fromMaybe "" . getText $ t
    | isDTDPEref t
        = prefixPe ++ concatMap collectText (getChildren t) ++ suffixPe
    | otherwise
        = ""
    where
    al       = fromMaybe [] . getDTDAttrl $ t
    delPe    = "\0"
    prefixPe = delPe ++ lookupDef "???" a_peref al ++ delPe
    suffixPe = delPe ++ delPe

{-

collectText (NTree n _)
    | isXTextNode n
        = textOfXNode n

collectText (NTree (XDTD PEREF al) cl)
    = prefixPe ++ concatMap collectText cl ++ suffixPe
      where
      delPe    = "\0"
      prefixPe = delPe ++ lookupDef "???" a_peref al ++ delPe
      suffixPe = delPe ++ delPe

collectText _
    = ""
-}

isDTDPEref      :: XmlTree -> Bool
isDTDPEref
    = maybe False (== PEREF) . getDTDPart

-- ------------------------------------------------------------
