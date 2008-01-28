-- |
-- exports helper functions for the integration of new datatype-libraries

module Text.XML.HXT.RelaxNG.DataTypeLibUtils
  ( errorMsgEqual
  , errorMsgDataTypeNotAllowed
  , errorMsgDataTypeNotAllowed0
  , errorMsgDataTypeNotAllowed2
  , errorMsgDataLibQName
  , errorMsgParam

  , rng_length
  , rng_maxLength
  , rng_minLength
   ,rng_maxExclusive
  , rng_minExclusive
  , rng_maxInclusive
  , rng_minInclusive

  , module Control.Arrow
  , module Text.XML.HXT.DOM.Util
  , module Text.XML.HXT.RelaxNG.Utils
  , module Text.XML.HXT.RelaxNG.DataTypes  

  , FunctionTable

  , stringValidFT	-- generalized checkString
  , fctTableString	-- minLength, maxLenght, length
  , fctTableList	-- minLength, maxLenght, length

  , stringValid		-- checkString
  , numberValid		-- checkNumeric

  , numParamValid

  , CheckA		-- Check datatype
  , CheckString		-- CheckA String String
  , CheckInteger	-- CheckA Integer Integer

  , performCheck	-- run a CheckA
  , ok			-- always true
  , failure		-- create an error meesage
  , assert		-- create a primitive check from a predicate
  , assertMaybe		-- create a primitive check from a maybe
  , checkWith		-- convert value before checking
  )

where

import Text.XML.HXT.DOM.Util

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.Utils

import Control.Arrow

import Data.Either
import Data.Maybe

-- ------------------------------------------------------------

newtype CheckA a b	= C { runCheck :: a -> Either String b }

instance Arrow CheckA where
    arr f	= C ( Right . f )		-- unit: no check, always o.k., just a conversion

    f1 >>> f2	= C $				-- logical and: f1 and f2 must hold
		  \ x -> case runCheck f1 x of
			 Right y	-> runCheck f2 y
			 Left  e	-> Left e

    first f1	= C $				-- check 1. component of a pair
		  \ ~(x1, x2) -> case runCheck f1 x1 of
				 Right y1	-> Right (y1, x2)
				 Left  e	-> Left  e

    second f2	= C $				-- check 2. component of a pair
		  \ ~(x1, x2) -> case runCheck f2 x2 of
				 Right y2	-> Right (x1, y2)
				 Left  e	-> Left  e



instance ArrowZero CheckA where
    zeroArrow	= C $ const (Left "")		-- always false: zero

instance ArrowPlus CheckA where
    f1 <+> f2	= C $				-- logical or
		  \ x -> case runCheck f1 x of
			 Right y1	-> Right y1
			 Left  e1	-> case runCheck f2 x of
					   Right y2	-> Right y2
					   Left  e2	-> Left ( if null e1
								  then e2
								  else
								  if null e2
								  then e1
								  else e1 ++ " or " ++ e2
								)

type CheckString	= CheckA String String
type CheckInteger	= CheckA Integer Integer

-- | run a check and deliver Just an error message or Nothing

performCheck	:: CheckA a b -> a -> Maybe String
performCheck c	= either Just (const Nothing) . runCheck c

-- | always failure

failure		:: (a -> String) -> CheckA a b
failure	msg	= C (Left . msg)

-- | every thing is fine

ok		:: CheckA a a
ok		= arr id

-- | perform a simple check with a predicate p,
--   when the predicate holds, assert acts as identity,
--   else an error message is generated

assert	:: (a -> Bool) -> (a -> String) -> CheckA a a
assert p msg	= C $ \ x -> if p x then Right x else Left (msg x)

-- | perform a simple check with a Maybe function, Nothing indicates error

assertMaybe	:: (a -> Maybe b) -> (a -> String) -> CheckA a b
assertMaybe f msg
    = C $ \ x -> case f x of
                 Nothing	-> Left (msg x)
		 Just y		-> Right y

-- | perform a check, but convert the value before checking

checkWith	:: (a -> b) -> CheckA b c -> CheckA a a
checkWith f c	= C $
		  \ x -> case runCheck c (f x) of
			 Right _	-> Right x
			 Left  e	-> Left  e

-- ------------------------------------------------------------

-- RelaxNG attribute names

rng_length, rng_maxLength, rng_minLength
 ,rng_maxExclusive, rng_minExclusive, rng_maxInclusive, rng_minInclusive :: String

rng_length		= "length"
rng_maxLength		= "maxLength"
rng_minLength		= "minLength"

rng_maxExclusive	= "maxExclusive"
rng_minExclusive	= "minExclusive"
rng_maxInclusive	= "maxInclusive"
rng_minInclusive	= "minInclusive"

-- ------------------------------------------------------------

-- | Function table type

type FunctionTable	= [(String, String -> String -> Bool)]

-- | Function table for numeric tests,
-- XML document value is first operand, schema value second

fctTableNum :: (Ord a, Num a) => [(String, a -> a -> Bool)]
fctTableNum
    = [ (rng_maxExclusive, (<))
      , (rng_minExclusive, (>))
      , (rng_maxInclusive, (<=))
      , (rng_minInclusive, (>=))
      ]

-- | Function table for string tests,
-- XML document value is first operand, schema value second
fctTableString :: FunctionTable
fctTableString
    = [ (rng_length,    (numParamValid (==)))
      , (rng_maxLength, (numParamValid (<=)))
      , (rng_minLength, (numParamValid (>=)))
      ]

-- | Function table for list tests,
-- XML document value is first operand, schema value second

fctTableList :: FunctionTable
fctTableList
    = [ (rng_length,    (listParamValid (==)))
      , (rng_maxLength, (listParamValid (<=)))
      , (rng_minLength, (listParamValid (>=)))
      ]

{- | 
tests whether a string value matches a numeric param

valid example:

> <data type="CHAR"> <param name="maxLength">5</param> </data>

invalid example:

> <data type="CHAR"> <param name="minLength">foo</param> </data>

-}

numParamValid :: (Integer -> Integer -> Bool) -> String -> String -> Bool
numParamValid fct a b
  = isNumber b
    &&
    ( toInteger (length a) `fct` (read b) )

{- | 
tests whether a list value matches a length constraint

valid example:

> <data type="IDREFS"> <param name="maxLength">5</param> </data>

invalid example:

> <data type="IDREFS"> <param name="minLength">foo</param> </data>

-}

listParamValid :: (Integer -> Integer -> Bool) -> String -> String -> Bool
listParamValid fct a b
  = isNumber b
    &&
    ( toInteger (length . words $ a) `fct` (read b) )

-- ------------------------------------------------------------
-- new check functions

{- | 
Tests whether a \"string\" datatype value is between the lower and 
upper bound of the datatype and matches all parameters.

All tests are performed on the string value.
   
   * 1.parameter  :  datatype
   
   - 2.parameter  :  lower bound of the datatype range
   
   - 3.parameter  :  upper bound of the datatype range (-1 = no upper bound) 
   
   - 4.parameter  :  list of parameters
   
   - 5.parameter  :  datatype value to be checked

   - return : Just \"Errormessage\" in case of an error, else Nothing
   
-}

stringValid 	:: DatatypeName -> Integer -> Integer -> ParamList -> CheckString
stringValid	= stringValidFT fctTableString

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

-- ------------------------------------------------------------

{- | 
Tests whether a \"numeric\" datatype value is between the lower and upper 
bound of the datatype and matches all parameters.

First, the string value is parsed into a numeric representation.
If no error occur, all following tests are performed on the numeric value.

   * 1.parameter  :  datatype
   
   - 2.parameter  :  lower bound of the datatype range
   
   - 3.parameter  :  upper bound of the datatype range (-1 = no upper bound) 
   
   - 4.parameter  :  list of parameters
   
   - 5.parameter  :  datatype value to be checked

   - return : Just \"Errormessage\" in case of an error, else Nothing
   
-}

numberValid :: DatatypeName -> Integer -> Integer -> ParamList -> CheckString
numberValid datatype lowerBound upperBound params
    = assert isNumber numErr
      >>>
      checkWith read ( assert inRange rangeErr
		       >>>
		       paramsNumValid params
		     )
    where
    inRange	:: Integer -> Bool
    inRange x	= x >= lowerBound
		  &&
		  x <= upperBound

    rangeErr v	= ( "Value = " ++ show v ++ " out of range: "
		    ++ show lowerBound ++ " .. " ++ show upperBound
		    ++ " for datatype " ++ datatype
		  )
    numErr v
	= "Value = " ++ v ++ " is not a number"

paramsNumValid	:: ParamList -> CheckInteger
paramsNumValid
    = foldr (>>>) ok . map paramNumValid

paramNumValid	:: (LocalName, String) -> CheckInteger
paramNumValid (pn, pv)
    = assert paramOK (errorMsgParam pn pv . show)
    where
    paramOK  v = isNumber pv
		 &&
		 paramFct pn v (read pv)
    paramFct n = fromJust $ lookup n fctTableNum

-- ------------------------------------------------------------
    
{- | 
Error Message for the equality test of two datatype values
   
   * 1.parameter  :  datatype
   
   - 2.parameter  :  datatype value

   - 3.parameter  :  datatype value
   
example:

> errorMsgEqual "Int" "21" "42" -> "Datatype Int with value = 21 expected, but value = 42 found"

-}

errorMsgParam	:: LocalName -> String -> String -> String
errorMsgParam pn pv v
    = ( "Parameter restriction: \""
	++ pn ++ " = " ++ pv
	++ "\" does not hold for value = \"" ++ v ++ "\""
      )

errorMsgEqual :: DatatypeName -> String -> String -> String
errorMsgEqual d s1 s2
    = ( "Datatype" ++ show d ++
	" with value = " ++ show s1 ++
	" expected, but value = " ++ show s2 ++ " found" 
      )
errorMsgDataTypeNotAllowed :: String -> String -> [(String, String)] -> String -> String
errorMsgDataTypeNotAllowed l t p v
    = ( "Datatype " ++ show t ++ " with parameter(s) " ++
        formatStringListPairs p ++ " and value = " ++ show v ++
        " not allowed for DatatypeLibrary " ++ show l
      )

errorMsgDataTypeNotAllowed0 :: String -> String -> String
errorMsgDataTypeNotAllowed0 l t
    = ( "Datatype " ++ show t ++
        " not allowed for DatatypeLibrary " ++ show l
      )
errorMsgDataTypeNotAllowed2 :: String -> String -> String -> String -> String
errorMsgDataTypeNotAllowed2 l t v1 v2
    = ( "Datatype " ++ show t ++
	" with values = " ++ show v1 ++
	" and " ++ show v2 ++ 
        " not allowed for DatatypeLibrary " ++ show l
      )

errorMsgDataLibQName :: String -> String -> String -> String
errorMsgDataLibQName l n v
    = show v ++ " is not a valid " ++ n ++ " for DatatypeLibrary " ++ l

-- ------------------------------------------------------------
