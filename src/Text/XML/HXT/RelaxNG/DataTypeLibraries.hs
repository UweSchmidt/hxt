-- | This modul exports the list of supported datatype libraries.
-- It also exports the main functions to validate in XML instance value
-- with respect to a datatype.

module Text.XML.HXT.RelaxNG.DataTypeLibraries
  ( datatypeLibraries
  , datatypeEqual
  , datatypeAllows
  )
where

import Text.XML.HXT.Arrow.DOMInterface
    ( relaxNamespace
    )

import Text.XML.HXT.RelaxNG.DataTypeLibUtils  

import Text.XML.HXT.RelaxNG.DataTypeLibMysql
    ( mysqlDatatypeLib )

import Text.XML.HXT.RelaxNG.DataTypeLibW3C
    ( w3cDatatypeLib )

import Maybe
    ( fromJust )

    
-- ------------------------------------------------------------
    

-- | List of all supported datatype libraries which can be 
-- used within the Relax NG validator modul.
datatypeLibraries :: DatatypeLibraries 
datatypeLibraries = [ relaxDatatypeLib
                    , relaxDatatypeLib'
                    , mysqlDatatypeLib
                    , w3cDatatypeLib
                    ]


{- |
Tests whether a XML instance value matches a value-pattern.

The following tests are performed:
   
   * 1. :  does the uri exist in the list of supported datatype libraries

   - 2. :  does the library support the datatype
   
   - 3. :  does the XML instance value match the value-pattern
   
The hard work is done by the specialized 'DatatypeEqual' function 
(see also: 'DatatypeCheck') of the datatype library.
-}
datatypeEqual :: Uri -> DatatypeEqual  
datatypeEqual uri d s1 c1 s2 c2 
  = if elem uri (map fst datatypeLibraries)  
    then dtEqFct d s1 c1 s2 c2 
    else Just $ "Unknown DatatypeLibrary " ++ uri
  where
  DTC _ dtEqFct _ = fromJust $ lookup uri datatypeLibraries


{- |
Tests whether a XML instance value matches a data-pattern.

The following tests are performed:
   
   * 1. :  does the uri exist in the list of supported datatype libraries

   - 2. :  does the library support the datatype
   
   - 3. :  does the XML instance value match the data-pattern
   
   - 4. :  does the XML instance value match all params
   
The hard work is done by the specialized 'DatatypeAllows' function 
(see also: 'DatatypeCheck') of the datatype library.
   
-}
datatypeAllows :: Uri -> DatatypeAllows
datatypeAllows uri d params s1 c1 
  = if elem uri (map fst datatypeLibraries)
    then dtAllowFct d params s1 c1 
    else Just $ "Unknown DatatypeLibrary " ++ uri
  where
  DTC dtAllowFct _ _ = fromJust $ lookup uri datatypeLibraries


-- --------------------------------------------------------------------------------------                    
-- Relax NG build in datatype library

relaxDatatypeLib :: DatatypeLibrary
relaxDatatypeLib 
  = (relaxNamespace, DTC datatypeAllowsRelax datatypeEqualRelax relaxDatatypes)


-- | if there is no datatype uri, the build in datatype library is used
relaxDatatypeLib' :: DatatypeLibrary
relaxDatatypeLib'
  = ("", DTC datatypeAllowsRelax datatypeEqualRelax relaxDatatypes)


-- | The build in Relax NG datatype lib supportes only the token and string datatype,
-- without any params.
relaxDatatypes :: AllowedDatatypes
relaxDatatypes = [ ("token", [])
                 , ("string", [])
                 ]

datatypeAllowsRelax :: DatatypeAllows
datatypeAllowsRelax "string" [] _ _ = Nothing
datatypeAllowsRelax "token" [] _ _ = Nothing
datatypeAllowsRelax d p v _ 
  = Just $ "Datatype " ++ d ++ " with parameter(s) " ++
           formatStringList ", " (map (\(a, b) -> a ++ " = " ++ b) p) ++ 
           " and value = " ++ v ++
           " not allowed for DatatypeLibrary " ++ relaxNamespace


-- | If the token datatype is used, the values have to be normalized
-- (trailing and leading whitespaces are removed).
-- token does not perform any changes to the values.
datatypeEqualRelax :: DatatypeEqual
datatypeEqualRelax d@"string" s1 _ s2 _
  = if s1 == s2
    then Nothing 
    else Just $ errorMsgEqual d s1 s2
datatypeEqualRelax d@"token" s1 _ s2 _
  = if (normalizeWhitespace s1) == (normalizeWhitespace s2) 
    then Nothing 
    else Just $ errorMsgEqual d s1 s2
datatypeEqualRelax d s1 _ s2 _
  = Just $ "Datatype " ++ d ++ " with values = " ++ s1 ++ 
           " and " ++ s2 ++ 
           " not allowed for DatatypeLibrary " ++ relaxNamespace
