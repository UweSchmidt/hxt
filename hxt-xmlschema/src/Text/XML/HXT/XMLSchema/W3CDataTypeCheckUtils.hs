-- Composed of Text.XML.HXT.RelaxNG.DataTypeLibUtils, Text.XML.HXT.RelaxNG.DataTypes and Text.XML.HXT.RelaxNG.Utils

module Text.XML.HXT.XMLSchema.W3CDataTypeCheckUtils
  (

    DatatypeName
  , ParamList

  , errorMsgDataTypeNotAllowed
  , errorMsgDataLibQName
  , errorMsgParam

  , module Control.Arrow

  , stringValid         -- checkString
  , listValid

  , isNumber
  , isNmtoken
  , isName

  , CheckA              -- Check datatype

  , performCheck        -- run a CheckA
  , ok                  -- always true
  , failure             -- create an error meesage
  , assert              -- create a primitive check from a predicate
  , checkWith           -- convert value before checking
  )

where

import Data.Maybe                         ( fromMaybe )

import Text.ParserCombinators.Parsec      ( option
                                          , string
                                          , many1
                                          , digit
                                          , runParser
                                          , eof)

import Text.XML.HXT.Parser.XmlCharParser  ( SimpleXParser
                                          , withNormNewline
                                          )

import Text.XML.HXT.Parser.XmlTokenParser ( skipS0
                                          , nmtoken
                                          , name
                                          )

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow

import Text.XML.HXT.XMLSchema.DataTypeLibW3CNames

type DatatypeName      = String

-- | List of parameters; each parameter is a pair consisting of a local name and a value.

type ParamList = [(LocalName, String)]
type LocalName = String

-- =================== UTILS ====================

-- | Function table type

type FunctionTable      = [(String, String -> String -> Bool)]

-- ====== Check strings ======

-- | Function table for string tests,
-- XML document value is first operand, schema value second
fctTableString :: FunctionTable
fctTableString
    = [ (xsd_length,    (numParamValid (==)))
      , (xsd_maxLength, (numParamValid (<=)))
      , (xsd_minLength, (numParamValid (>=)))
      ]

-- | tests whether a string value matches a numeric param

numParamValid :: (Integer -> Integer -> Bool) -> String -> String -> Bool
numParamValid fct a b
  = isNumber b
    &&
    ( toInteger (length a) `fct` (read b) )

-- | Tests whether a string matches a number [-](0-9)*
isNumber :: String -> Bool
isNumber
    = checkByParsing parseNumber'
    where
    parseNumber' :: SimpleXParser String
    parseNumber'
        = do
          skipS0
          m <- option "" (string "-")
          n <- many1 digit
          skipS0
          return $ m ++ n

checkByParsing  :: SimpleXParser String -> String -> Bool
checkByParsing p s
    = either (const False)
             (const True)
             (runParser p' (withNormNewline ()) "" s)
      where
      p' = do
           r <- p
           eof
           return r

isNmtoken       :: String -> Bool
isNmtoken    = checkByParsing nmtoken

isName  :: String -> Bool
isName  = checkByParsing name

type CheckString        = CheckA String String
newtype CheckA a b      = C { runCheck :: a -> Either String b }

instance Category CheckA where
    id          = C $ Right

    f2 . f1     = C $                           -- logical and: f1 and f2 must hold
                  \ x -> case runCheck f1 x of
                         Right y        -> runCheck f2 y
                         Left  e        -> Left e

instance Arrow CheckA where
    arr f       = C ( Right . f )               -- unit: no check, always o.k., just a conversion

    first f1    = C $                           -- check 1. component of a pair
                  \ ~(x1, x2) -> case runCheck f1 x1 of
                                 Right y1       -> Right (y1, x2)
                                 Left  e        -> Left  e

    second f2   = C $                           -- check 2. component of a pair
                  \ ~(x1, x2) -> case runCheck f2 x2 of
                                 Right y2       -> Right (x1, y2)
                                 Left  e        -> Left  e

-- | everything is fine

ok              :: CheckA a a
ok              = arr id

-- | perform a simple check with a predicate p,
--   when the predicate holds, assert acts as identity,
--   else an error message is generated

assert  :: (a -> Bool) -> (a -> String) -> CheckA a a
assert p msg    = C $ \ x -> if p x then Right x else Left (msg x)

errorMsgParam   :: LocalName -> String -> String -> String
errorMsgParam pn pv v
    = ( "Parameter restriction: \""
        ++ pn ++ " = " ++ pv
        ++ "\" does not hold for value = \"" ++ v ++ "\""
      )

-- ======== Check lists ========

-- | Function table for list tests,
-- XML document value is first operand, schema value second

fctTableList :: FunctionTable
fctTableList
    = [ (xsd_length,    (listParamValid (==)))
      , (xsd_maxLength, (listParamValid (<=)))
      , (xsd_minLength, (listParamValid (>=)))
      ]

listValid       :: DatatypeName -> ParamList -> CheckString
listValid d     = stringValidFT fctTableList d 0 (-1)

-- | tests whether a list value matches a length constraint

listParamValid :: (Integer -> Integer -> Bool) -> String -> String -> Bool
listParamValid fct a b
  = isNumber b
    &&
    ( toInteger (length . words $ a) `fct` (read b) )

stringValid     :: DatatypeName -> Integer -> Integer -> ParamList -> CheckString
stringValid     = stringValidFT fctTableString

stringValidFT :: FunctionTable -> DatatypeName -> Integer -> Integer -> ParamList -> CheckString
stringValidFT ft datatype lowerBound upperBound params
    = assert boundsOK boundsErr
      >>>
      paramsStringValid params
    where
    boundsOK v
        = ( (lowerBound == 0)
            ||
            (toInteger (length v) >= lowerBound)
          )
          &&
          ( (upperBound == (-1))
            ||
            (toInteger (length v) <= upperBound)
          )
    boundsErr v
        = "Length of " ++ v
          ++ " (" ++ (show $ length v) ++ " chars) out of range: "
          ++ show lowerBound ++ " .. " ++ show upperBound
          ++ " for datatype " ++ datatype
    paramStringValid :: (LocalName, String) -> CheckString
    paramStringValid (pn, pv)
        = assert paramOK (errorMsgParam pn pv)
          where
          paramOK v  = paramFct pn v pv
          paramFct n = fromMaybe (const . const $ True) $ lookup n ft
    paramsStringValid :: ParamList -> CheckString
    paramsStringValid
        = foldr (>>>) ok . map paramStringValid

-- | run a check and deliver Just an error message or Nothing

performCheck    :: CheckA a b -> a -> Maybe String
performCheck c  = either Just (const Nothing) . runCheck c

-- | perform a check, but convert the value before checking

checkWith       :: (a -> b) -> CheckA b c -> CheckA a a
checkWith f c   = C $
                  \ x -> case runCheck c (f x) of
                         Right _        -> Right x
                         Left  e        -> Left  e

-- | always failure

failure         :: (a -> String) -> CheckA a b
failure msg     = C (Left . msg)

errorMsgDataTypeNotAllowed :: String -> [(String, String)] -> String -> String
errorMsgDataTypeNotAllowed t p v
    = ( "Datatype " ++ show t ++ " with parameter(s) " ++
        formatStringListPairs p ++ " and value = " ++ show v ++
        " is no basic W3C type."
      )

errorMsgDataLibQName :: String -> String -> String
errorMsgDataLibQName n v
    = show v ++ " is no valid " ++ n

formatStringListPairs :: [(String,String)] -> String
formatStringListPairs
    = formatStringList id ", "
      . map (\ (a, b) -> a ++ " = " ++ show b)

formatStringList :: (String -> String) -> String -> [String] -> String
formatStringList _sf _sp []
    = ""
formatStringList sf spacer l
    = reverse $ drop (length spacer) $ reverse $
      foldr (\e -> ((if e /= "" then sf e ++ spacer else "") ++)) "" l
