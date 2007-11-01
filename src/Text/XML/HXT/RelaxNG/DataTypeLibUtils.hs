-- |
-- exports helper functions for the integration of new datatype-libraries

module Text.XML.HXT.RelaxNG.DataTypeLibUtils
  ( checkString
  , checkNumeric
  , errorMsgEqual
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
    = [ (rng_length,    (checkStrWithNumParam (==)))
      , (rng_maxLength, (checkStrWithNumParam (<=)))
      , (rng_minLength, (checkStrWithNumParam (>=)))
      ]

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
          ++ " (" ++ (show $ length v) ++ " Chars) out of Range: "
          ++ show lowerBound ++ " .. " ++ show upperBound
          ++ " for datatype " ++ datatype

paramStringValid :: (LocalName, String) -> (Check String)
paramStringValid (pn, pv)
    = paramOK `orErr` paramErr
    where
    paramOK v  = paramFct pn v pv
    paramErr v = "Can't check Param-Restriction: " ++ pn ++ " = " ++ pv
		  ++ " against value = " ++ v
    paramFct n = fromJust $ lookup n fctTableString

paramsStringValid :: ParamList -> (Check String)
paramsStringValid
    = foldr andCheck alwaysOK . map paramStringValid

-- ------------------------------------------------------------

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
	= "Value = " ++ show v ++ " out of Range: "
          ++ show lowerBound ++ " .. " ++ show upperBound
          ++ " for datatype " ++ datatype
    numErr v
	= "Value = " ++ v ++ " is not a number"

paramsNumValid	:: ParamList -> Check Integer
paramsNumValid
    = foldr andCheck alwaysOK . map paramNumValid

paramNumValid	:: (LocalName, String) -> Check Integer
paramNumValid (pn, pv)
    = paramOK `orErr` paramErr
    where
    paramOK  v = isNumber pv
		 &&
		 paramFct pn v (read pv)
    paramErr v = "Can't check Param-Restriction: "
		 ++ pn ++ " = " ++ pv
		 ++ " against value = " ++ show v
    paramFct n = fromJust $ lookup n fctTableNum

-- ------------------------------------------------------------

{- | 
tests whether a string value matches a numeric param

valid example:

> <data type="CHAR"> <param name="maxLength">5</param> </data>

invalid example:

> <data type="CHAR"> <param name="minLength">foo</param> </data>

-}
checkStrWithNumParam :: (Integer -> Integer -> Bool) -> String -> String -> Bool
checkStrWithNumParam fct a b
  = isNumber b
    &&
    ( toInteger (length a) `fct` (read b) )
    
{- | 
Tests whether a \"string\" datatype value is between the lower and 
upper bound of the datatype and matches all parameters.

All tests are performed on the string value.
   
   * 1.parameter  :  datatype
   
   - 2.parameter  :  datatype value

   - 3.parameter  :  lower bound of the datatype range
   
   - 4.parameter  :  upper bound of the datatype range (-1 = no upper bound) 
   
   - 5.parameter  :  list of parameters
   
   - return : Just \"Errormessage\" in case of an error, else Nothing
   
-}

checkString :: DatatypeName -> String -> Integer -> Integer -> ParamList -> Maybe String
checkString datatype value lowerBound upperBound params
  = if (toInteger (length value) >= lowerBound) && 
       ((upperBound == (-1)) || (toInteger (length value) <= upperBound))
    then checkParamsString value params
    else Just $ "Length of " ++ value ++ " (" ++ (show $ length value) ++ 
                " Chars) out of Range: " ++ show lowerBound ++ 
                " .. " ++ show upperBound ++ " for datatype " ++ datatype


-- | tests whether a string value matches a list of parameters
checkParamsString :: String -> ParamList -> Maybe String
checkParamsString _ [] = Nothing
checkParamsString value ((pName, pValue):xs)
  = if (getFct pName) value pValue
    then checkParamsString value xs
    else Just $ "Can't check Param-Restriction: " ++ pName ++ " = " ++ pValue ++
                " against value = " ++ value
    where
    getFct :: String -> (String -> String -> Bool)
    getFct paramName = fromJust $ lookup paramName fctTableString                


{- | 
Tests whether a \"numeric\" datatype value is between the lower and upper 
bound of the datatype and matches all parameters.

First, the string value is parsed into a numeric representation.
If no error occur, all following tests are performed on the numeric value.

   * 1.parameter  :  datatype
   
   - 2.parameter  :  datatype value

   - 3.parameter  :  lower bound of the datatype range
   
   - 4.parameter  :  upper bound of the datatype range (-1 = no upper bound) 
   
   - 5.parameter  :  list of parameters
   
   - return : Just \"Errormessage\" in case of an error, else Nothing
   
-}
checkNumeric :: DatatypeName -> String -> Int -> Int -> ParamList -> Maybe String
checkNumeric datatype value lowerBound upperBound params
  = if isNumber value 
    then ( if (x >= lowerBound) && (x <= upperBound)
           then checkParamsNumeric x params
           else Just $ "Value = " ++ value ++ " out of Range: " ++ show lowerBound ++ 
                       " .. " ++ show upperBound ++ " for datatype " ++ datatype
         )
    else Just ("Value = " ++ value ++ " is not a number")
    where 
    x = read value


-- | tests whether a numeric value matches a list of parameters
checkParamsNumeric :: (Read a, Ord a, Num a) => a -> ParamList -> Maybe String
checkParamsNumeric _ [] = Nothing
checkParamsNumeric value (x@(pName, pValue):xs)
  = if checkParam x  
    then checkParamsNumeric value xs
    else Just $ "Can't check Param-Restriction: " ++ pName ++ " = " ++ pValue ++
                " against value = " ++ show value
    where
    getFct :: (Ord a, Num a) => String -> (a -> a -> Bool)
    getFct paramName = fromJust $ lookup paramName fctTableNum
    checkParam :: (String, String) -> Bool
    checkParam (pName', pValue')
      = if isNumber pValue'
        then (getFct pName') value (read pValue')
        else False


{- | 
Error Message for the equality test of two datatype values
   
   * 1.parameter  :  datatype
   
   - 2.parameter  :  datatype value

   - 3.parameter  :  datatype value
   
example:

> errorMsgEqual "Int" "21" "42" -> "Datatype Int with value = 21 expected, but value = 42 found"

-}

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
