-- |
-- exports helper functions for the integration of new datatype-libraries

module Text.XML.HXT.RelaxNG.DataTypeLibUtils
  ( checkString
  , checkNumeric
  , errorMsgEqual
  , module Text.XML.HXT.DOM.Util
  , module Text.XML.HXT.RelaxNG.Utils
  , module Text.XML.HXT.RelaxNG.DataTypes  
  )

where

import Text.XML.HXT.DOM.Util

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.Utils

import Maybe
  ( fromJust )


-- ------------------------------------------------------------

-- | Function table for numeric tests,
-- XML document value is first operand, schema value second
fctTableNum :: (Ord a, Num a) => [(String, a -> a -> Bool)]
fctTableNum = [ ("maxExclusive", (<))
              , ("minExclusive", (>))
              , ("maxInclusive", (<=))
              , ("minInclusive", (>=))
              ]


-- | Function table for string tests,
-- XML document value is first operand, schema value second
fctTableString :: [(String, String -> String -> Bool)]
fctTableString = [ ("length", (checkStrWithNumParam (==)))
                 , ("maxLength", (checkStrWithNumParam (<=)))
                 , ("minLength", (checkStrWithNumParam (>=)))
                 ]


{- | 
tests whether a string value matches a numeric param

valid example:

> <data type="CHAR"> <param name="maxLength">5</param> </data>

invalid example:

> <data type="CHAR"> <param name="minLength">foo</param> </data>

-}
checkStrWithNumParam :: (Int -> Int -> Bool) -> String -> String -> Bool
checkStrWithNumParam fct a b
  = if parseNumber b
    then (length a) `fct` (read b)
    else False

    
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
checkString :: DatatypeName -> String -> Int -> Int -> ParamList -> Maybe String
checkString datatype value lowerBound upperBound params
  = if (length value >= lowerBound) && 
       ((upperBound == (-1)) || (length value <= upperBound))
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
  = if parseNumber value 
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
      = if parseNumber pValue'
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
errorMsgEqual d s1 s2 = "Datatype" ++ d ++ " with value = " ++ s1 ++ " expected, but value = " ++ s2 ++ " found"        
