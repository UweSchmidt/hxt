-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.XmlDTDTokenParser
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: XmlDTDTokenParser.hs,v 1.4 2005/09/02 17:09:39 hxml Exp $

   Parsec parser for tokenizing DTD declarations for ELEMENT, ATTLIST, ENTITY and NOTATION

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.XmlDTDTokenParser where

import           Text.ParserCombinators.Parsec

import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.DOM.XmlNode               ( mkDTDElem
                                                        , mkText
                                                        )
import qualified Text.XML.HXT.Parser.XmlTokenParser     as XT

-- ------------------------------------------------------------
--
-- DTD declaration tokenizer

dtdDeclTokenizer        :: GenParser Char state XmlTree
dtdDeclTokenizer
    = do
      (dcl, al) <- dtdDeclStart
      content <- many1 dtdToken
      dtdDeclEnd
      return $! mkDTDElem dcl al content

dtdDeclStart :: GenParser Char state (DTDElem, Attributes)
dtdDeclStart
    = foldr1 (<|>) $
      map (uncurry dtdStart) $
              [ ("ELEMENT",  ELEMENT )
              , ("ATTLIST",  ATTLIST )
              , ("ENTITY",   ENTITY  )
              , ("NOTATION", NOTATION)
              ]
    where
    dtdStart    :: String -> DTDElem -> GenParser Char state (DTDElem, Attributes)
    dtdStart dcl element
        = try ( do
                _ <- string "<!"
                _ <- string dcl
                pos <- getPosition
                return (element, [ (a_source, sourceName pos)
                                 , (a_line,   show (sourceLine pos))
                                 , (a_column, show (sourceColumn pos))
                                 ]
                       )
              )

dtdDeclEnd      :: GenParser Char state ()
dtdDeclEnd
    = do
      _ <- XT.gt
      return ()

dtdToken        :: GenParser Char state XmlTree
dtdToken
    = dtdChars
      <|>
      attrValue
      <|>
      try peReference           -- first try parameter entity ref %xxx;
      <|>
      percent                   -- else % may be indicator for parameter entity declaration
      <?> "DTD token"

peReference     :: GenParser Char state XmlTree
peReference
    = do
      r <- XT.peReference
      return $! (mkDTDElem PEREF [(a_peref, r)] [])

attrValue       :: GenParser Char state XmlTree
attrValue
    = do
      v <- XT.attrValue
      return $! mkText v

dtdChars        :: GenParser Char state XmlTree
dtdChars
    = do
      v <- many1 (XT.singleChar "%\"'<>[]")             -- everything except string constants, < and >, [ and ] (for cond sections)
      return $! mkText v                                -- all illegal chars will be detected later during declaration parsing

percent         :: GenParser Char state XmlTree
percent
    = do
      c <- char '%'
      return $! mkText [c]

-- ------------------------------------------------------------
