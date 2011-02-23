-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.XmlDTDTokenParser
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Parsec parser for tokenizing DTD declarations for ELEMENT, ATTLIST, ENTITY and NOTATION

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.XmlDTDTokenParser where

import           Text.ParserCombinators.Parsec

import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.DOM.XmlNode               ( mkDTDElem'
                                                        , mkText'
                                                        )
import qualified Text.XML.HXT.Parser.XmlTokenParser     as XT
import           Text.XML.HXT.Parser.XmlCharParser      ( XParser )

-- ------------------------------------------------------------
--
-- DTD declaration tokenizer

dtdDeclTokenizer        :: XParser s XmlTree
dtdDeclTokenizer
    = do
      (dcl, al) <- dtdDeclStart
      content <- many1 dtdToken
      dtdDeclEnd
      return $ mkDTDElem' dcl al content

dtdDeclStart :: XParser s (DTDElem, Attributes)
dtdDeclStart
    = foldr1 (<|>) $
      map (uncurry dtdStart) $
              [ ("ELEMENT",  ELEMENT )
              , ("ATTLIST",  ATTLIST )
              , ("ENTITY",   ENTITY  )
              , ("NOTATION", NOTATION)
              ]
    where
    dtdStart    :: String -> DTDElem -> XParser s (DTDElem, Attributes)
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

dtdDeclEnd      :: XParser s ()
dtdDeclEnd
    = do
      _ <- XT.gt
      return ()

dtdToken        :: XParser s XmlTree
dtdToken
    = dtdChars
      <|>
      entityValue
      <|>
      try peReference           -- first try parameter entity ref %xxx;
      <|>
      percent                   -- else % may be indicator for parameter entity declaration
      <?> "DTD token"

peReference     :: XParser s XmlTree
peReference
    = do
      r <- XT.peReference
      return $! (mkDTDElem' PEREF [(a_peref, r)] [])

entityValue       :: XParser s XmlTree
entityValue
    = do
      v <- XT.entityValue
      return $ mkText' v

dtdChars        :: XParser s XmlTree
dtdChars
    = do
      v <- many1 (XT.singleChar "%\"'<>[]")             -- everything except string constants, < and >, [ and ] (for cond sections)
      return $ mkText' v                                -- all illegal chars will be detected later during declaration parsing

percent         :: XParser s XmlTree
percent
    = do
      c <- char '%'
      return $ mkText' [c]

-- ------------------------------------------------------------
