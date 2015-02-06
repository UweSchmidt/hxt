-- ------------------------------------------------------------

{- |
   Module     : Data.String.Unicode
   Copyright  : Copyright (C) 2010- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Unicode and UTF-8 Conversion Functions

-}

-- ------------------------------------------------------------

module Data.String.Unicode
    (
     -- * Unicode Type declarations
     Unicode,
     UString,
     UTF8Char,
     UTF8String,
     UStringWithErrors,
     DecodingFct,
     DecodingFctEmbedErrors,

      utf8ToUnicode
    , utf8ToUnicodeEmbedErrors
    , latin1ToUnicode
    , ucs2ToUnicode
    , ucs2BigEndianToUnicode
    , ucs2LittleEndianToUnicode
    , utf16beToUnicode
    , utf16leToUnicode

    , unicodeCharToUtf8

    , unicodeToUtf8
    , unicodeToXmlEntity
    , unicodeToLatin1
    , unicodeRemoveNoneAscii
    , unicodeRemoveNoneLatin1

    , intToCharRef
    , intToCharRefHex
    , intToHexString

    , getDecodingFct
    , getDecodingFctEmbedErrors
    , getOutputEncodingFct

    , normalizeNL
    , guessEncoding

    , getOutputEncodingFct'
    , unicodeCharToUtf8'
    , unicodeCharToXmlEntity'
    , unicodeCharToLatin1'
    )
where

import           Data.Char                         (toUpper)

import           Data.Char.IsoLatinTables
import           Data.Char.Properties.XMLCharProps (isXml1ByteChar,
                                                    isXmlLatin1Char)

import           Data.String.EncodingNames
import           Data.String.UTF8Decoding          (decodeUtf8,
                                                    decodeUtf8EmbedErrors)

-- ------------------------------------------------------------

-- | Unicode is represented as the Char type
--   Precondition for this is the support of Unicode character range
--   in the compiler (e.g. ghc but not hugs)

type Unicode    = Char

-- | the type for Unicode strings

type UString    = [Unicode]

-- | UTF-8 charachters are represented by the Char type

type UTF8Char   = Char

-- | UTF-8 strings are implemented as Haskell strings

type UTF8String = String

-- | Decoding function with a pair containing the result string and a list of decoding errors as result

type DecodingFct = String -> (UString, [String])

type UStringWithErrors = [Either String Char]

-- | Decoding function where decoding errors are interleaved with decoded characters

type DecodingFctEmbedErrors = String -> UStringWithErrors

-- ------------------------------------------------------------

-- |
-- conversion from Unicode strings (UString) to UTF8 encoded strings.

unicodeToUtf8           :: UString -> UTF8String
unicodeToUtf8           = concatMap unicodeCharToUtf8

-- |
-- conversion from Unicode (Char) to a UTF8 encoded string.

unicodeCharToUtf8       :: Unicode -> UTF8String
unicodeCharToUtf8 c
    | i >= 0          && i <= 0x0000007F        -- 1 byte UTF8 (7 bits)
        = [ toEnum i ]
    | i >= 0x00000080 && i <= 0x000007FF        -- 2 byte UTF8 (5 + 6 bits)
        = [ toEnum (0xC0 + i `div` 0x40)
          , toEnum (0x80 + i                  `mod` 0x40)
          ]
    | i >= 0x00000800 && i <= 0x0000FFFF        -- 3 byte UTF8 (4 + 6 + 6 bits)
        = [ toEnum (0xE0 +  i `div`   0x1000)
          , toEnum (0x80 + (i `div`     0x40) `mod` 0x40)
          , toEnum (0x80 +  i                 `mod` 0x40)
          ]
    | i >= 0x00010000 && i <= 0x001FFFFF        -- 4 byte UTF8 (3 + 6 + 6 + 6 bits)
        = [ toEnum (0xF0 +  i `div`    0x40000)
          , toEnum (0x80 + (i `div`     0x1000) `mod` 0x40)
          , toEnum (0x80 + (i `div`       0x40) `mod` 0x40)
          , toEnum (0x80 +  i                   `mod` 0x40)
          ]
    | i >= 0x00200000 && i <= 0x03FFFFFF        -- 5 byte UTF8 (2 + 6 + 6 + 6 + 6 bits)
        = [ toEnum (0xF8 +  i `div`  0x1000000)
          , toEnum (0x80 + (i `div`    0x40000) `mod` 0x40)
          , toEnum (0x80 + (i `div`     0x1000) `mod` 0x40)
          , toEnum (0x80 + (i `div`       0x40) `mod` 0x40)
          , toEnum (0x80 +  i                   `mod` 0x40)
          ]
    | i >= 0x04000000 && i <= 0x7FFFFFFF        -- 6 byte UTF8 (1 + 6 + 6 + 6 + 6 + 6 bits)
        = [ toEnum (0xFC +  i `div` 0x40000000)
          , toEnum (0x80 + (i `div`  0x1000000) `mod` 0x40)
          , toEnum (0x80 + (i `div`    0x40000) `mod` 0x40)
          , toEnum (0x80 + (i `div`     0x1000) `mod` 0x40)
          , toEnum (0x80 + (i `div`       0x40) `mod` 0x40)
          , toEnum (0x80 +  i                   `mod` 0x40)
          ]
    | otherwise                                 -- other values not supported
        = error ("unicodeCharToUtf8: illegal integer argument " ++ show i)
    where
    i = fromEnum c

-- ------------------------------------------------------------

-- |
-- code conversion from latin1 to Unicode

latin1ToUnicode :: String -> UString
latin1ToUnicode = id

latinToUnicode  :: [(Char, Char)] -> String -> UString
latinToUnicode tt
    = map charToUni
    where
    charToUni c =
       foldr (\(src,dst) r ->
          case compare c src of
             EQ -> dst
             LT -> c {- not found in table -}
             GT -> r) c tt

-- | conversion from ASCII to unicode with check for legal ASCII char set
--
-- Structure of decoding function copied from 'Data.Char.UTF8.decode'.

decodeAscii     :: DecodingFct
decodeAscii
    = swap . partitionEither . decodeAsciiEmbedErrors

decodeAsciiEmbedErrors  :: String -> UStringWithErrors
decodeAsciiEmbedErrors str
    = map (\(c,pos) -> if isValid c
                         then Right c
                         else Left (toErrStr c pos)) posStr
    where
    posStr = zip str [(0::Int)..]
    toErrStr errChr pos
        = " at input position " ++ show pos ++ ": none ASCII char " ++ show errChr
    isValid x = x < '\x80'

-- |
-- UCS-2 big endian to Unicode conversion

ucs2BigEndianToUnicode  :: String -> UString

ucs2BigEndianToUnicode (b : l : r)
    = toEnum (fromEnum b * 256 + fromEnum l) : ucs2BigEndianToUnicode r

ucs2BigEndianToUnicode []
    = []

ucs2BigEndianToUnicode _
    = []                                -- error "illegal UCS-2 byte input sequence with odd length"
                                        -- is ignored (garbage in, garbage out)

-- ------------------------------------------------------------

-- |
-- UCS-2 little endian to Unicode conversion

ucs2LittleEndianToUnicode       :: String -> UString

ucs2LittleEndianToUnicode (l : b : r)
    = toEnum (fromEnum b * 256 + fromEnum l) : ucs2LittleEndianToUnicode r

ucs2LittleEndianToUnicode []
    = []

ucs2LittleEndianToUnicode [_]
    = []                                -- error "illegal UCS-2 byte input sequence with odd length"
                                        -- is ignored

-- ------------------------------------------------------------

-- |
-- UCS-2 to UTF-8 conversion with byte order mark analysis

ucs2ToUnicode           :: String -> UString

ucs2ToUnicode ('\xFE':'\xFF':s)         -- 2 byte mark for big endian encoding
    = ucs2BigEndianToUnicode s

ucs2ToUnicode ('\xFF':'\xFE':s)         -- 2 byte mark for little endian encoding
    = ucs2LittleEndianToUnicode s

ucs2ToUnicode s
    = ucs2BigEndianToUnicode s          -- default: big endian

-- ------------------------------------------------------------

-- |
-- UTF-8 to Unicode conversion with deletion of leading byte order mark, as described in XML standard F.1

utf8ToUnicode           :: DecodingFct

utf8ToUnicode ('\xEF':'\xBB':'\xBF':s)  -- remove byte order mark ( XML standard F.1 )
    = decodeUtf8 s

utf8ToUnicode s
    = decodeUtf8 s

utf8ToUnicodeEmbedErrors        :: DecodingFctEmbedErrors

utf8ToUnicodeEmbedErrors ('\xEF':'\xBB':'\xBF':s)       -- remove byte order mark ( XML standard F.1 )
    = decodeUtf8EmbedErrors s

utf8ToUnicodeEmbedErrors s
    = decodeUtf8EmbedErrors s

-- ------------------------------------------------------------

-- |
-- UTF-16 big endian to UTF-8 conversion with removal of byte order mark

utf16beToUnicode                :: String -> UString

utf16beToUnicode ('\xFE':'\xFF':s)              -- remove byte order mark
    = ucs2BigEndianToUnicode s

utf16beToUnicode s
    = ucs2BigEndianToUnicode s

-- ------------------------------------------------------------

-- |
-- UTF-16 little endian to UTF-8 conversion with removal of byte order mark

utf16leToUnicode                :: String -> UString

utf16leToUnicode ('\xFF':'\xFE':s)              -- remove byte order mark
    = ucs2LittleEndianToUnicode s

utf16leToUnicode s
    = ucs2LittleEndianToUnicode s


-- ------------------------------------------------------------

-- |
-- substitute all Unicode characters, that are not legal 1-byte
-- UTF-8 XML characters by a character reference.
--
-- This function can be used to translate all text nodes and
-- attribute values into pure ascii.
--
-- see also : 'unicodeToLatin1'

unicodeToXmlEntity      :: UString -> String
unicodeToXmlEntity
    = escape isXml1ByteChar (intToCharRef . fromEnum)

-- |
-- substitute all Unicode characters, that are not legal latin1
-- UTF-8 XML characters by a character reference.
--
-- This function can be used to translate all text nodes and
-- attribute values into ISO latin1.
--
-- see also : 'unicodeToXmlEntity'

unicodeToLatin1 :: UString -> String
unicodeToLatin1
    = escape isXmlLatin1Char (intToCharRef . fromEnum)


-- |
-- substitute selected characters
-- The @check@ function returns 'True' whenever a character needs to substitution
-- The function @esc@ computes a substitute.

escape :: (Unicode -> Bool) -> (Unicode -> String) -> UString -> String
escape check esc =
    concatMap (\uc -> if check uc then [uc] else esc uc)

-- |
-- removes all non ascii chars, may be used to transform
-- a document into a pure ascii representation by removing
-- all non ascii chars from tag and attibute names
--
-- see also : 'unicodeRemoveNoneLatin1', 'unicodeToXmlEntity'

unicodeRemoveNoneAscii  :: UString -> String
unicodeRemoveNoneAscii
    = filter isXml1ByteChar

-- |
-- removes all non latin1 chars, may be used to transform
-- a document into a pure ascii representation by removing
-- all non ascii chars from tag and attibute names
--
-- see also : 'unicodeRemoveNoneAscii', 'unicodeToLatin1'

unicodeRemoveNoneLatin1 :: UString -> String
unicodeRemoveNoneLatin1
    = filter isXmlLatin1Char

-- ------------------------------------------------------------

-- |
-- convert an Unicode into a XML character reference.
--
-- see also : 'intToCharRefHex'

intToCharRef            :: Int -> String
intToCharRef i
    = "&#" ++ show i ++ ";"

-- |
-- convert an Unicode into a XML hexadecimal character reference.
--
-- see also: 'intToCharRef'

intToCharRefHex         :: Int -> String
intToCharRefHex i
    = "&#x" ++ h2 ++ ";"
      where
      h1 = intToHexString i
      h2 = if length h1 `mod` 2 == 1
           then '0': h1
           else h1

-- ------------------------------------------------------------

intToHexString          :: Int -> String
intToHexString i
    | i == 0
        = "0"
    | i > 0
        = intToStr i
    | otherwise
        = error ("intToHexString: negative argument " ++ show i)
    where
    intToStr 0  = ""
    intToStr i' = intToStr (i' `div` 16) ++ [fourBitsToChar (i' `mod` 16)]

fourBitsToChar          :: Int -> Char
fourBitsToChar i        = "0123456789ABCDEF" !! i
{-# INLINE fourBitsToChar #-}

-- ------------------------------------------------------------
--
-- | White Space (XML Standard 2.3) and
-- end of line handling (2.11)
--
-- \#x0D and \#x0D\#x0A are mapped to \#x0A

normalizeNL     :: String -> String
normalizeNL ('\r' : '\n' : rest)        = '\n' : normalizeNL rest
normalizeNL ('\r' : rest)               = '\n' : normalizeNL rest
normalizeNL (c : rest)                  = c    : normalizeNL rest
normalizeNL []                          = []


-- ------------------------------------------------------------

-- |
-- the table of supported character encoding schemes and the associated
-- conversion functions into Unicode:q

{-
This table could be derived from decodingTableEither,
but this way it is certainly more efficient.
-}

decodingTable   :: [(String, DecodingFct)]
decodingTable
    = [ (utf8,          utf8ToUnicode                           )
      , (isoLatin1,     liftDecFct latin1ToUnicode              )
      , (usAscii,       decodeAscii                             )
      , (ucs2,          liftDecFct ucs2ToUnicode                )
      , (utf16,         liftDecFct ucs2ToUnicode                )
      , (utf16be,       liftDecFct utf16beToUnicode             )
      , (utf16le,       liftDecFct utf16leToUnicode             )
      , (iso8859_2,     liftDecFct (latinToUnicode iso_8859_2)  )
      , (iso8859_3,     liftDecFct (latinToUnicode iso_8859_3)  )
      , (iso8859_4,     liftDecFct (latinToUnicode iso_8859_4)  )
      , (iso8859_5,     liftDecFct (latinToUnicode iso_8859_5)  )
      , (iso8859_6,     liftDecFct (latinToUnicode iso_8859_6)  )
      , (iso8859_7,     liftDecFct (latinToUnicode iso_8859_7)  )
      , (iso8859_8,     liftDecFct (latinToUnicode iso_8859_8)  )
      , (iso8859_9,     liftDecFct (latinToUnicode iso_8859_9)  )
      , (iso8859_10,    liftDecFct (latinToUnicode iso_8859_10) )
      , (iso8859_11,    liftDecFct (latinToUnicode iso_8859_11) )
      , (iso8859_13,    liftDecFct (latinToUnicode iso_8859_13) )
      , (iso8859_14,    liftDecFct (latinToUnicode iso_8859_14) )
      , (iso8859_15,    liftDecFct (latinToUnicode iso_8859_15) )
      , (iso8859_16,    liftDecFct (latinToUnicode iso_8859_16) )
      , (unicodeString, liftDecFct id                           )
      , ("",            liftDecFct id                           )       -- default
      ]
    where
    liftDecFct df = \ s -> (df s, [])

-- |
-- the lookup function for selecting the decoding function

getDecodingFct          :: String -> Maybe DecodingFct
getDecodingFct enc
    = lookup (map toUpper enc) decodingTable


-- |
-- Similar to 'decodingTable' but it embeds errors
-- in the string of decoded characters.

decodingTableEmbedErrors        :: [(String, DecodingFctEmbedErrors)]
decodingTableEmbedErrors
    = [ (utf8,          utf8ToUnicodeEmbedErrors                )
      , (isoLatin1,     liftDecFct latin1ToUnicode              )
      , (usAscii,       decodeAsciiEmbedErrors                  )
      , (ucs2,          liftDecFct ucs2ToUnicode                )
      , (utf16,         liftDecFct ucs2ToUnicode                )
      , (utf16be,       liftDecFct utf16beToUnicode             )
      , (utf16le,       liftDecFct utf16leToUnicode             )
      , (iso8859_2,     liftDecFct (latinToUnicode iso_8859_2)  )
      , (iso8859_3,     liftDecFct (latinToUnicode iso_8859_3)  )
      , (iso8859_4,     liftDecFct (latinToUnicode iso_8859_4)  )
      , (iso8859_5,     liftDecFct (latinToUnicode iso_8859_5)  )
      , (iso8859_6,     liftDecFct (latinToUnicode iso_8859_6)  )
      , (iso8859_7,     liftDecFct (latinToUnicode iso_8859_7)  )
      , (iso8859_8,     liftDecFct (latinToUnicode iso_8859_8)  )
      , (iso8859_9,     liftDecFct (latinToUnicode iso_8859_9)  )
      , (iso8859_10,    liftDecFct (latinToUnicode iso_8859_10) )
      , (iso8859_11,    liftDecFct (latinToUnicode iso_8859_11) )
      , (iso8859_13,    liftDecFct (latinToUnicode iso_8859_13) )
      , (iso8859_14,    liftDecFct (latinToUnicode iso_8859_14) )
      , (iso8859_15,    liftDecFct (latinToUnicode iso_8859_15) )
      , (iso8859_16,    liftDecFct (latinToUnicode iso_8859_16) )
      , (unicodeString, liftDecFct id                           )
      , ("",            liftDecFct id                           )       -- default
      ]
    where
    liftDecFct df = map Right . df

-- |
-- the lookup function for selecting the decoding function

getDecodingFctEmbedErrors       :: String -> Maybe DecodingFctEmbedErrors
getDecodingFctEmbedErrors enc
    = lookup (map toUpper enc) decodingTableEmbedErrors



-- |
-- the table of supported output encoding schemes and the associated
-- conversion functions from Unicode

outputEncodingTable     :: [(String, (UString -> String))]
outputEncodingTable
    = [ (utf8,          unicodeToUtf8           )
      , (isoLatin1,     unicodeToLatin1         )
      , (usAscii,       unicodeToXmlEntity      )
      , (unicodeString, id                      )
      , ("",            unicodeToUtf8           )       -- default
      ]

-- |
-- the lookup function for selecting the encoding function

getOutputEncodingFct            :: String -> Maybe (String -> UString)
getOutputEncodingFct enc
    = lookup (map toUpper enc) outputEncodingTable

-- ------------------------------------------------------------
--

guessEncoding           :: String -> String

guessEncoding ('\xFF':'\xFE':'\x00':'\x00':_)   = "UCS-4LE"             -- with byte order mark
guessEncoding ('\xFF':'\xFE':_)                 = "UTF-16LE"            -- with byte order mark

guessEncoding ('\xFE':'\xFF':'\x00':'\x00':_)   = "UCS-4-3421"          -- with byte order mark
guessEncoding ('\xFE':'\xFF':_)                 = "UTF-16BE"            -- with byte order mark

guessEncoding ('\xEF':'\xBB':'\xBF':_)          = utf8                  -- with byte order mark

guessEncoding ('\x00':'\x00':'\xFE':'\xFF':_)   = "UCS-4BE"             -- with byte order mark
guessEncoding ('\x00':'\x00':'\xFF':'\xFE':_)   = "UCS-4-2143"          -- with byte order mark

guessEncoding ('\x00':'\x00':'\x00':'\x3C':_)   = "UCS-4BE"             -- "<" of "<?xml"
guessEncoding ('\x3C':'\x00':'\x00':'\x00':_)   = "UCS-4LE"             -- "<" of "<?xml"
guessEncoding ('\x00':'\x00':'\x3C':'\x00':_)   = "UCS-4-2143"          -- "<" of "<?xml"
guessEncoding ('\x00':'\x3C':'\x00':'\x00':_)   = "UCS-4-3412"          -- "<" of "<?xml"

guessEncoding ('\x00':'\x3C':'\x00':'\x3F':_)   = "UTF-16BE"            -- "<?" of "<?xml"
guessEncoding ('\x3C':'\x00':'\x3F':'\x00':_)   = "UTF-16LE"            -- "<?" of "<?xml"

guessEncoding ('\x4C':'\x6F':'\xA7':'\x94':_)   = "EBCDIC"              -- "<?xm" of "<?xml"

guessEncoding _                                 = ""                    -- no guess

-- ------------------------------------------------------------

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
{-# INLINE swap #-}

partitionEither :: [Either a b] -> ([a], [b])
partitionEither =
   foldr (\x ~(ls,rs) -> either (\l -> (l:ls,rs)) (\r -> (ls,r:rs)) x) ([],[])
{-# INLINE partitionEither #-}

-- ------------------------------------------------------------

-- output encoding for bytestrings

-- |
-- the table of supported output encoding schemes and the associated
-- conversion functions from Unicode

type StringFct          = String -> String

outputEncodingTable'     :: [(String, (Char -> StringFct))]
outputEncodingTable'
    = [ (utf8,          unicodeCharToUtf8'           )
      , (isoLatin1,     unicodeCharToLatin1'         )
      , (usAscii,       unicodeCharToXmlEntity'      )
      , ("",            unicodeCharToUtf8'           )       -- default
      ]

-- |
-- the lookup function for selecting the encoding function

getOutputEncodingFct'            :: String -> Maybe (Char -> StringFct)
getOutputEncodingFct' enc
    = lookup (map toUpper enc) outputEncodingTable'


-- ------------------------------------------------------------

-- |
-- conversion from Unicode (Char) to a UTF8 encoded string.

unicodeCharToUtf8'      :: Char -> StringFct
unicodeCharToUtf8' c
    | i >= 0          && i <= 0x0000007F        -- 1 byte UTF8 (7 bits)
        = (c :)

    | i >= 0x00000080 && i <= 0x000007FF        -- 2 byte UTF8 (5 + 6 bits)
        = ((toEnum (0xC0 + i `div` 0x40)                   ) :) .
          ((toEnum (0x80 + i                    `mod` 0x40)) :)

    | i >= 0x00000800 && i <= 0x0000FFFF        -- 3 byte UTF8 (4 + 6 + 6 bits)
        = ((toEnum (0xE0 +  i `div`     0x1000)            ) :) .
          ((toEnum (0x80 + (i `div`       0x40) `mod` 0x40)) :) .
          ((toEnum (0x80 +  i                   `mod` 0x40)) :)

    | i >= 0x00010000 && i <= 0x001FFFFF        -- 4 byte UTF8 (3 + 6 + 6 + 6 bits) -- extension to encode 21 bit values
        = ((toEnum (0xF0 +  i `div`    0x40000)            ) :) .
          ((toEnum (0x80 + (i `div`     0x1000) `mod` 0x40)) :) .
          ((toEnum (0x80 + (i `div`       0x40) `mod` 0x40)) :) .
          ((toEnum (0x80 +  i                   `mod` 0x40)) :)

    | i >= 0x00200000 && i <= 0x03FFFFFF        -- 5 byte UTF8 (2 + 6 + 6 + 6 + 6 bits) -- extension to encode 26 bit values
        = ((toEnum (0xF8 +  i `div`  0x1000000)            ) :) .
          ((toEnum (0x80 + (i `div`    0x40000) `mod` 0x40)) :) .
          ((toEnum (0x80 + (i `div`     0x1000) `mod` 0x40)) :) .
          ((toEnum (0x80 + (i `div`       0x40) `mod` 0x40)) :) .
          ((toEnum (0x80 +  i                   `mod` 0x40)) :)

    | i >= 0x04000000 && i <= 0x7FFFFFFF        -- 6 byte UTF8 (1 + 6 + 6 + 6 + 6 + 6 bits) -- extension to encode 31 bit values
        = ((toEnum (0xFC +  i `div` 0x40000000)            ) :) .
          ((toEnum (0x80 + (i `div`  0x1000000) `mod` 0x40)) :) .
          ((toEnum (0x80 + (i `div`    0x40000) `mod` 0x40)) :) .
          ((toEnum (0x80 + (i `div`     0x1000) `mod` 0x40)) :) .
          ((toEnum (0x80 + (i `div`       0x40) `mod` 0x40)) :) .
          ((toEnum (0x80 +  i                   `mod` 0x40)) :)

    | otherwise                                 -- other values not supported
        = error ("unicodeCharToUtf8: illegal integer argument " ++ show i)
    where
    i = fromEnum c

-- ------------------------------------------------------------

-- |
-- substitute all Unicode characters, that are not legal 1-byte
-- UTF-8 XML characters by a character reference.

unicodeCharToXmlEntity' :: Char -> StringFct
unicodeCharToXmlEntity' c
    | isXml1ByteChar c  = (c :)
    | otherwise         = ((intToCharRef . fromEnum $ c) ++)

-- ------------------------------------------------------------

-- |
-- substitute all Unicode characters, that are not legal latin1
-- UTF-8 XML characters by a character reference.

unicodeCharToLatin1'    :: Char -> StringFct
unicodeCharToLatin1' c
    | isXmlLatin1Char c = (c :)
    | otherwise         = ((intToCharRef . fromEnum $ c) ++)

-- ------------------------------------------------------------


