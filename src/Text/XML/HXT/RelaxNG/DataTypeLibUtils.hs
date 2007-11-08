-- |
-- exports helper functions for the integration of new datatype-libraries

module Text.XML.HXT.RelaxNG.DataTypeLibUtils
  ( errorMsgEqual
  , errorMsgDataTypeNotAllowed
  , errorMsgDataTypeNotAllowed0
  , errorMsgDataTypeNotAllowed2
  , errorMsgDataLibQName
  , rng_length
  , rng_maxLength
  , rng_minLength
   ,rng_maxExclusive
  , rng_minExclusive
  , rng_maxInclusive
  , rng_minInclusive

  , module Text.XML.HXT.DOM.Util
  , module Text.XML.HXT.RelaxNG.Utils
  , module Text.XML.HXT.RelaxNG.DataTypes  

  , alwaysOK
  , alwaysErr
  , orErr
  , andCheck
  , withVal

  , stringValid		-- checkString
  , numberValid		-- checkNumeric
  )

where

import Text.XML.HXT.DOM.Util

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.Utils

import Data.Maybe
  ( fromJust )

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
type Check a		= a -> Maybe String

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

-- ------------------------------------------------------------

-- | Unit for checks
alwaysOK	:: Check a
alwaysOK _	= Nothing

-- | Zero for checks: Create an error message for an illegal value

alwaysErr	:: (a -> String) -> Check a
alwaysErr msg	= Just . msg

-- | Perform check and generate error message on failure

orErr	:: (a -> Bool) -> (a -> String) -> Check a
orErr p msg s
    | p s	= Nothing
    | otherwise	= Just $ msg s

-- | Combine two checks

andCheck	:: Check a -> Check a -> Check a
andCheck c1 c2 s
    = res (c1 s)
    where
    res Nothing	= c2 s
    res r1	= r1

withVal	:: Check a -> (b -> a) -> Check b
withVal c1 f v
    = c1 (f v)

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

stringValid :: DatatypeName -> Integer -> Integer -> ParamList -> Check String
stringValid datatype lowerBound upperBound params
    = boundsOK `orErr` boundsErr
      `andCheck`
      paramsStringValid params
    where
    boundsOK v
	= (toInteger (length v) >= lowerBound)
	  &&
	  ((upperBound == (-1)) || (toInteger (length v) <= upperBound))
    boundsErr v
	= "Length of " ++ v
          ++ " (" ++ (show $ length v) ++ " chars) out of range: "
          ++ show lowerBound ++ " .. " ++ show upperBound
          ++ " for datatype " ++ datatype

paramStringValid :: (LocalName, String) -> (Check String)
paramStringValid (pn, pv)
    = paramOK `orErr` errorMsgParam pn pv
    where
    paramOK v  = paramFct pn v pv
    paramFct n = fromJust $ lookup n fctTableString

paramsStringValid :: ParamList -> (Check String)
paramsStringValid
    = foldr andCheck alwaysOK . map paramStringValid

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

numberValid :: DatatypeName -> Integer -> Integer -> ParamList -> Check String
numberValid datatype lowerBound upperBound params
    = (isNumber `orErr` numErr)
      `andCheck`
      ( ( (inRange `orErr` rangeErr)
	  `andCheck`
	  paramsNumValid params
	)
	`withVal` read
      )
    where
    inRange	:: Integer -> Bool
    inRange x
	= x >= lowerBound
	  &&
	  x <= upperBound
    rangeErr v
	= "Value = " ++ show v ++ " out of range: "
          ++ show lowerBound ++ " .. " ++ show upperBound
          ++ " for datatype " ++ datatype
    numErr v
	= "Value = " ++ v ++ " is not a number"

paramsNumValid	:: ParamList -> Check Integer
paramsNumValid
    = foldr andCheck alwaysOK . map paramNumValid

paramNumValid	:: (LocalName, String) -> Check Integer
paramNumValid (pn, pv)
    = paramOK `orErr` (errorMsgParam pn pv . show)
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
errorMsgDataTypeNotAllowed :: String -> [(String, String)] -> String -> String -> String
errorMsgDataTypeNotAllowed t p v l
    = ( "Datatype " ++ show t ++ " with parameter(s) " ++
        formatStringListPairs p ++ " and value = " ++ show v ++
        " not allowed for DatatypeLibrary " ++ show l
      )

errorMsgDataTypeNotAllowed0 :: String -> String -> String
errorMsgDataTypeNotAllowed0 t l
    = ( "Datatype " ++ show t ++
        " not allowed for DatatypeLibrary " ++ show l
      )
errorMsgDataTypeNotAllowed2 :: String -> String -> String -> String -> String
errorMsgDataTypeNotAllowed2 t v1 v2 l
    = ( "Datatype " ++ show t ++
	" with values = " ++ show v1 ++
	" and " ++ show v2 ++ 
        " not allowed for DatatypeLibrary " ++ show l
      )

errorMsgDataLibQName :: String -> String -> String -> String
errorMsgDataLibQName v n l
    = show v ++ " is not a valid " ++ n ++ " for DatatypeLibrary " ++ l
