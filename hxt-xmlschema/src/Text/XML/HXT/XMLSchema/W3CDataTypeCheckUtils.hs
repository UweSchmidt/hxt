{- |
   Module     : Text.XML.HXT.XMLSchema.W3CDataTypeCheckUtils
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

-}

module Text.XML.HXT.XMLSchema.W3CDataTypeCheckUtils

  ( module Control.Arrow

  , DatatypeName
  , ParamList

  , CheckA
  , performCheck
  , ok
  , failure
  , assert
  , checkWith

  , isNumber
  , isNmtoken
  , isName

  , stringValid
  , listValid

  , errorMsgDataTypeNotAllowed
  , errorMsgDataLibQName
  , errorMsgParam
  )

where

import Text.XML.HXT.XMLSchema.DataTypeLibW3CNames

import Text.XML.HXT.Parser.XmlCharParser  ( SimpleXParser
                                          , withNormNewline
                                          )
import Text.XML.HXT.Parser.XmlTokenParser ( skipS0
                                          , nmtoken
                                          , name
                                          )

import Text.ParserCombinators.Parsec      ( option
                                          , string
                                          , many1
                                          , digit
                                          , runParser
                                          , eof)

import Data.Maybe                         ( fromMaybe )

import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow

-- ----------------------------------------

-- | Name of a datatype
type DatatypeName = String

-- | List of parameters: each parameter is a pair consisting of a local name and a value.
type ParamList = [(LocalName, String)]
type LocalName = String

-- | Function table type
type FunctionTable = [(String, String -> String -> Bool)]

-- | Check arrow type
newtype CheckA a b = C { runCheck :: a -> Either String b }

instance Category CheckA where
  id      = C $ Right

  f2 . f1 = C $ \ x -> case runCheck f1 x of -- logical and: f1 and f2 must hold
                         Right y -> runCheck f2 y
                         Left  e -> Left e

instance Arrow CheckA where
  arr f     = C ( Right . f ) -- unit: no check, always o.k., just a conversion

  first f1  = C $ \ ~(x1, x2) -> case runCheck f1 x1 of -- check 1. component of a pair
                                   Right y1 -> Right (y1, x2)
                                   Left  e  -> Left  e

  second f2 = C $ \ ~(x1, x2) -> case runCheck f2 x2 of -- check 2. component of a pair
                                   Right y2 -> Right (x1, y2)
                                   Left  e  -> Left  e

-- | String check arrow type
type CheckString = CheckA String String

-- ----------------------------------------

-- | Run a check and deliver Just an error message or Nothing
performCheck :: CheckA a b -> a -> Maybe String
performCheck c = either Just (const Nothing) . runCheck c

-- | Everything is fine
ok :: CheckA a a
ok = arr id

-- | Always fail
failure :: (a -> String) -> CheckA a b
failure msg = C (Left . msg)

-- | Perform a simple check with a predicate p,
--   when the predicate holds, assert acts like identity,
--   else an error message is generated
assert :: (a -> Bool) -> (a -> String) -> CheckA a a
assert p msg
  = C $ \ x -> if p x then Right x else Left (msg x)

-- | Perform a check, but convert the value before checking
checkWith :: (a -> b) -> CheckA b c -> CheckA a a
checkWith f c
  = C $ \ x -> case runCheck c (f x) of
                 Right _ -> Right x
                 Left  e -> Left  e

-- ----------------------------------------

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

-- | Tests whether a string matches a Nmtoken
isNmtoken :: String -> Bool
isNmtoken = checkByParsing nmtoken

-- | Tests whether a string matches a name
isName :: String -> Bool
isName = checkByParsing name

-- | Helper function which parses a string
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

-- ----------------------------------------

-- | Tests whether a string is valid
stringValid :: DatatypeName -> Integer -> Integer -> ParamList -> CheckString
stringValid = stringValidFT fctTableString

-- | Function table for string tests
fctTableString :: FunctionTable
fctTableString
  = [ (xsd_length,    (numParamValid (==)))
    , (xsd_maxLength, (numParamValid (<=)))
    , (xsd_minLength, (numParamValid (>=)))
    ]

-- | Tests whether a string value matches a numeric param
numParamValid :: (Integer -> Integer -> Bool) -> String -> String -> Bool
numParamValid fct a b
  = isNumber b
    &&
    ( toInteger (length a) `fct` (read b) )

-- | Generalized stringValid
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

-- ----------------------------------------

-- | Tests whether a list is valid
listValid :: DatatypeName -> ParamList -> CheckString
listValid d = stringValidFT fctTableList d 0 (-1)

-- | Function table for list tests
fctTableList :: FunctionTable
fctTableList
  = [ (xsd_length,    (listParamValid (==)))
    , (xsd_maxLength, (listParamValid (<=)))
    , (xsd_minLength, (listParamValid (>=)))
    ]

-- | Tests whether a list value matches a length constraint
listParamValid :: (Integer -> Integer -> Bool) -> String -> String -> Bool
listParamValid fct a b
  = isNumber b
    &&
    ( toInteger (length . words $ a) `fct` (read b) )

-- ----------------------------------------

-- | Error message for unknown types
errorMsgDataTypeNotAllowed :: String -> [(String, String)] -> String -> String
errorMsgDataTypeNotAllowed t p v
  = ( "Datatype " ++ show t ++ " with parameter(s) " ++
      formatStringListPairs p ++ " and value = " ++ show v ++
      " is no basic W3C type."
    )

-- | Helper function to format strings
formatStringListPairs :: [(String,String)] -> String
formatStringListPairs
  = formatStringList id ", "
    . map (\ (a, b) -> a ++ " = " ++ show b)

-- | Helper function to format strings
formatStringList :: (String -> String) -> String -> [String] -> String
formatStringList _sf _sp []
  = ""
formatStringList sf spacer l
  = reverse $ drop (length spacer) $ reverse $
    foldr (\e -> ((if e /= "" then sf e ++ spacer else "") ++)) "" l

-- | Error message for not matching datatype
errorMsgDataLibQName :: String -> String -> String
errorMsgDataLibQName n v
  = show v ++ " is no valid " ++ n ++ "."

-- | Error message for not matching params
errorMsgParam :: LocalName -> String -> String -> String
errorMsgParam pn pv v
  = ( "Parameter restriction: \""
      ++ pn ++ " = " ++ pv
      ++ "\" does not hold for value = \"" ++ v ++ "\"."
    )

