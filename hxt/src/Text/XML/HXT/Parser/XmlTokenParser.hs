-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.XmlTokenParser
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Parsec parser for XML tokens

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.XmlTokenParser
    ( allBut
    , allBut1
    , amp
    , asciiLetter
    , attrChar
    , attrValue
    , bar
    , charRef
    , checkString
    , comma
    , dq
    , encName
    , entityRef
    , entityValue
    , eq
    , gt
    , keyword
    , keywords
    , lpar
    , lt
    , name
    , names
    , ncName
    , nmtoken
    , nmtokens
    , peReference
    , pubidLiteral
    , qName
    , quoted
    , reference
    , rpar
    , semi
    , separator
    , singleChar
    , singleChars
    , skipS
    , skipS0
    , sPace
    , sPace0
    , sq
    , systemLiteral
    , versionNum

    , concRes
    , mkList

    , nameT
    , nmtokenT

    , entityValueT
    , entityTokensT
    , entityCharT

    , attrValueT
    , attrValueT'

    , referenceT
    , charRefT
    , entityRefT
    , peReferenceT

    , singleCharsT
    )
where

import Data.Char.Properties.XMLCharProps        ( isXmlChar
                                                , isXmlCharCR
                                                )
import Data.String.Unicode                      ( intToCharRef
                                                , intToCharRefHex
                                                )
import Text.ParserCombinators.Parsec

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.DOM.XmlNode                 ( mkDTDElem'
                                                , mkText'
                                                , mkCharRef'
                                                , mkEntityRef'
                                                )
import Text.XML.HXT.Parser.XmlCharParser        ( xmlNameChar
                                                , xmlNameStartChar
                                                , xmlNCNameChar
                                                , xmlNCNameStartChar
                                                , xmlSpaceChar
                                                , xmlCRLFChar
                                                , XParser
                                                )

-- ------------------------------------------------------------
--
-- Character (2.2) and White Space (2.3)
--
-- Unicode parsers in module XmlCharParser

-- ------------------------------------------------------------

sPace           :: XParser s String
sPace
    = many1 xmlSpaceChar

sPace0          :: XParser s String
sPace0
    = many xmlSpaceChar

skipS           :: XParser s ()
skipS
    = skipMany1 xmlSpaceChar

skipS0          :: XParser s ()
skipS0
    = skipMany xmlSpaceChar

-- ------------------------------------------------------------
--
-- Names and Tokens (2.3)

asciiLetter             :: XParser s Char
asciiLetter
    = satisfy (\ c -> ( c >= 'A' && c <= 'Z' ||
                        c >= 'a' && c <= 'z' )
              )
      <?> "ASCII letter"

name            :: XParser s String
name
    = do
      s1 <- xmlNameStartChar
      sl <- many xmlNameChar
      return (s1 : sl)
      <?> "Name"

-- Namespaces in XML: Rules [4-5] NCName:

ncName          :: XParser s String
ncName
    = do
      s1 <- xmlNCNameStartChar
      sl <- many xmlNCNameChar
      return (s1 : sl)
      <?> "NCName"

-- Namespaces in XML: Rules [6-8] QName:

qName           :: XParser s (String, String)
qName
    = do
      s1 <- ncName
      s2 <- option "" (char ':' >> ncName)
      return ( if null s2
               then (s2, s1)
               else (s1, s2)
             )

nmtoken         :: XParser s String
nmtoken
    = try (many1 xmlNameChar)
      <?> "Nmtoken"

names           :: XParser s [String]
names
    = sepBy1 name sPace

nmtokens        :: XParser s [String]
nmtokens
    = sepBy1 nmtoken sPace

-- ------------------------------------------------------------
--
-- Literals (2.3)

singleChar              :: String -> XParser s Char
singleChar notAllowed
    = satisfy (\ c -> isXmlCharCR c && c `notElem` notAllowed)
      <|>
      xmlCRLFChar

singleChars             :: String -> XParser s String
singleChars notAllowed
    = many1 (singleChar notAllowed)

entityValue	:: XParser s String
entityValue
    = ( do
	v <- entityValueDQ
	return ("\"" ++ v ++ "\"")
      )
      <|>
      ( do
	v <- entityValueSQ
	return ("'" ++ v ++ "'")
      )
      <?> "entity value (in quotes)"

entityValueDQ	:: XParser s String
entityValueDQ
    = between dq dq (concRes $ many $ attrChar "&\"")

entityValueSQ	:: XParser s String
entityValueSQ
    = between sq sq (concRes $ many $ attrChar "&\'")

attrValue       :: XParser s String
attrValue
    = ( do
        v <- attrValueDQ
        return ("\"" ++ v ++ "\"")
      )
      <|>
      ( do
        v <- attrValueSQ
        return ("'" ++ v ++ "'")
      )
      <?> "attribute value (in quotes)"

attrValueDQ     :: XParser s String
attrValueDQ
    = between dq dq (concRes $ many $ attrChar "<&\"")

attrValueSQ     :: XParser s String
attrValueSQ
    = between sq sq (concRes $ many $ attrChar "<&\'")

attrChar        :: String -> XParser s String
attrChar notAllowed
    = reference
      <|>
      mkList (singleChar notAllowed)
      <?> ("legal attribute or entity character or reference (not allowed: " ++ show notAllowed ++ " )")

systemLiteral           :: XParser s String
systemLiteral
    = between dq dq (many $ noneOf "\"")
      <|>
      between sq sq (many $ noneOf "\'")
      <?> "system literal (in quotes)"

pubidLiteral            :: XParser s String
pubidLiteral
    = between dq dq (many $ pubidChar "\'")
      <|>
      between sq sq (many $ pubidChar "")
      <?> "pubid literal (in quotes)"
      where
      pubidChar         :: String -> XParser s Char
      pubidChar quoteChars
          = asciiLetter
            <|>
            digit
            <|>
            oneOf " \r\n"               -- no "\t" allowed, so xmlSpaceChar parser not used
            <|>
            oneOf "-()+,./:=?;!*#@$_%"
            <|>
            oneOf quoteChars

-- ------------------------------------------------------------
--
-- Character and Entity References (4.1)

reference       :: XParser s String
reference
    = ( do
        i <- charRef
        return ("&#" ++ show i ++ ";")
      )
      <|>
      ( do
        n <- entityRef
        return ("&" ++ n ++ ";")
      )

checkCharRef    :: Int -> XParser s Int
checkCharRef i
    = if ( i <= fromEnum (maxBound::Char)
           && isXmlChar (toEnum i)
         )
        then return i
        else unexpected ("illegal value in character reference: " ++ intToCharRef i ++ " , in hex: " ++ intToCharRefHex i)

charRef         :: XParser s Int
charRef
    = do
      checkString "&#x"
      d <- many1 hexDigit
      semi
      checkCharRef (hexStringToInt d)
      <|>
      do
      checkString "&#"
      d <- many1 digit
      semi
      checkCharRef (decimalStringToInt d)
      <?> "character reference"

entityRef       :: XParser s String
entityRef
    = do
      amp
      n <- name
      semi
      return n
      <?> "entity reference"

peReference     :: XParser s String
peReference
    = try ( do
            _ <- char '%'
            n <- name
            semi
            return n
          )
      <?> "parameter-entity reference"

-- ------------------------------------------------------------
--
-- 4.3

encName         :: XParser s String
encName
    = do
      c <- asciiLetter
      r <- many (asciiLetter <|> digit <|> oneOf "._-")
      return (c:r)

versionNum      :: XParser s String
versionNum
    = many1 xmlNameChar


-- ------------------------------------------------------------
--
-- keywords

keyword         :: String -> XParser s String
keyword kw
    = try ( do
            n <- name
            if n == kw
              then return n
              else unexpected n
          )
      <?> kw

keywords        :: [String] -> XParser s String
keywords
    = foldr1 (<|>) . map keyword

-- ------------------------------------------------------------
--
-- parser for quoted attribute values

quoted          :: XParser s a -> XParser s a
quoted p
    = between dq dq p
      <|>
      between sq sq p

-- ------------------------------------------------------------
--
-- simple char parsers

dq, sq, lt, gt, semi, amp    :: XParser s ()

dq      = char '\"' >> return ()
sq      = char '\'' >> return ()
lt      = char '<'  >> return ()
gt      = char '>'  >> return ()
semi    = char ';'  >> return ()
amp     = char '&'  >> return ()

{-# INLINE  dq #-}
{-# INLINE  sq #-}
{-# INLINE  lt #-}
{-# INLINE  gt #-}
{-# INLINE  semi #-}
{-# INLINE  amp #-}

separator       :: Char -> XParser s ()
separator c
    = do
      _ <- try ( do
                 skipS0
                 char c
               )
      skipS0
      <?> [c]

bar, comma, eq, lpar, rpar      :: XParser s ()

bar     = separator '|'
comma   = separator ','
eq      = separator '='

{-# INLINE bar #-}
{-# INLINE comma #-}
{-# INLINE eq #-}

lpar	= char '(' >> skipS0
{-# INLINE lpar #-}

rpar	= skipS0 >> char ')' >> return ()
{-# INLINE rpar #-}

checkString	:: String -> XParser s ()
checkString s
    = try $ string s >> return ()
{-# INLINE checkString #-}

-- ------------------------------------------------------------
--
-- all chars but not a special substring

allBut          :: (XParser s Char -> XParser s String) -> String -> XParser s String
allBut p str
    = allBut1 p (const True) str

allBut1         :: (XParser s Char -> XParser s String) -> (Char -> Bool) -> String -> XParser s String
allBut1 p prd (c:rest)
    = p ( satisfy (\ x -> isXmlCharCR x && prd x && not (x == c) )
          <|>
          xmlCRLFChar
          <|>
          try ( char c
                >>
                notFollowedBy (try (string rest) >> return c)
                >>
                return c
              )
        )

allBut1 _p _prd str
    = error ("allBut1 _ _ " ++ show str ++ " is undefined")

-- ------------------------------------------------------------
--
-- concatenate parse results

concRes         :: XParser s [[a]] -> XParser s [a]
concRes p
    = do
      sl <- p
      return (concat sl)

mkList          :: XParser s a -> XParser s [a]
mkList p
    = do
      r <- p
      return [r]

-- ------------------------------------------------------------
--
-- token parsers returning XmlTrees
--
-- ------------------------------------------------------------
--
-- Literals (2.3)

nameT           :: XParser s XmlTree
nameT
    = do
      n <- name
      return (mkDTDElem' NAME [(a_name, n)] [])

nmtokenT        :: XParser s XmlTree
nmtokenT
    = do
      n <- nmtoken
      return (mkDTDElem' NAME [(a_name, n)] [])


entityValueT    :: XParser s XmlTrees
entityValueT
    =  do
       sl <- between dq dq (entityTokensT "%&\"")
       return sl
       <|>
       do
       sl <- between sq sq (entityTokensT "%&\'")
       return sl
       <?> "entity value (in quotes)"

entityTokensT   :: String -> XParser s XmlTrees
entityTokensT notAllowed
    = many (entityCharT notAllowed)

entityCharT     :: String -> XParser s XmlTree
entityCharT notAllowed
    = peReferenceT
      <|>
      charRefT
      <|>
      bypassedEntityRefT
      <|>
      ( do
        cs <- many1 (singleChar notAllowed)
        return (mkText' cs)
      )

attrValueT      :: XParser s XmlTrees
attrValueT
    = between dq dq (attrValueT' "<&\"")
      <|>
      between sq sq (attrValueT' "<&\'")
      <?> "attribute value (in quotes)"

attrValueT'     :: String -> XParser s XmlTrees
attrValueT' notAllowed
    = many ( referenceT <|> singleCharsT notAllowed)

singleCharsT    :: String -> XParser s XmlTree
singleCharsT notAllowed
    = do
      cs <- singleChars notAllowed
      return (mkText' cs)

-- ------------------------------------------------------------
--
-- Character and Entity References (4.1)

referenceT      :: XParser s XmlTree
referenceT
    = charRefT
      <|>
      entityRefT

charRefT        :: XParser s XmlTree
charRefT
    = do
      i <- charRef
      return (mkCharRef' i)

entityRefT      :: XParser s XmlTree
entityRefT
    = do
      n <- entityRef
      return $! (maybe (mkEntityRef' n) mkCharRef' . lookup n $ predefinedXmlEntities)

-- optimization: predefined XML entity refs are converted into equivalent char refs
-- so there is no need for an entitiy substitution phase, if there is no DTD
-- Attention: entityRefT must only be called from within XML/HTML content
-- in DTD parsing this optimization is not allowed because of different semantics
-- of charRefs and entityRefs during substitution of entites in ENTITY definitions

predefinedXmlEntities   :: [(String, Int)]
predefinedXmlEntities
    = [ ("lt",   60)
      , ("gt",   62)
      , ("amp",  38)
      , ("apos", 39)
      , ("quot", 34)
      ]

bypassedEntityRefT      :: XParser s XmlTree
bypassedEntityRefT
    = do
      n <- entityRef
      return $! (mkText' ("&" ++ n ++ ";"))

peReferenceT    :: XParser s XmlTree
peReferenceT
    = do
      r <- peReference
      return $! (mkDTDElem' PEREF [(a_peref, r)] [])

-- ------------------------------------------------------------


