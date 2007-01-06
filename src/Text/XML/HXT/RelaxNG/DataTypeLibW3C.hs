-- |
-- Datatype library for the W3C XML schema datatypes

module Text.XML.HXT.RelaxNG.DataTypeLibW3C
  ( w3cNS
  , w3cDatatypeLib
  )
where

import Text.XML.HXT.RelaxNG.DataTypeLibUtils  

import Network.URI
  ( isURIReference )

import Text.XML.HXT.DOM.NamespaceFilter
  ( isWellformedQualifiedName
  , isNCName
  )

  
-- ------------------------------------------------------------

-- | Namespace of the W3C XML schema datatype library
w3cNS :: String
w3cNS = "http://www.w3.org/2001/XMLSchema-datatypes"


-- | The main entry point to the W3C XML schema datatype library.
--
-- The 'DTC' constructor exports the list of supported datatypes and params.
-- It also exports the specialized functions to validate a XML instance value with
-- respect to a datatype.
w3cDatatypeLib :: DatatypeLibrary
w3cDatatypeLib = (w3cNS, DTC datatypeAllowsW3C datatypeEqualW3C w3cDatatypes)


-- | All supported datatypes of the library
w3cDatatypes :: AllowedDatatypes
w3cDatatypes = [ ("NCName", stringParams)
               , ("anyURI", stringParams)
               , ("QName", stringParams)
               , ("string", stringParams)               
               ]


-- | List of allowed params for the string datatypes
stringParams :: AllowedParams
stringParams = ["length", "maxLength", "minLength"]


-- | Tests whether a XML instance value matches a data-pattern.
-- (see also: 'checkString')
datatypeAllowsW3C :: DatatypeAllows
datatypeAllowsW3C d@"NCName" params value _
  = let v = normalizeWhitespace value
    in ( if isNCName v && v /= ""
         then checkString d value 0 (-1) params 
         else Just $ value ++ " is not a valid NCName for DatatypeLibrary " ++ w3cNS
       )
datatypeAllowsW3C d@"anyURI" params value _
  = let v = escapeURI $ normalizeWhitespace value
    in ( if isURIReference v 
         then checkString d value 0 (-1) params
         else Just $ value ++ " is not a valid anyURI for DatatypeLibrary " ++ w3cNS
       )
datatypeAllowsW3C d@"QName" params value _
  = let v = normalizeWhitespace value
    in ( if isWellformedQualifiedName v && v /= "" 
         then checkString d value 0 (-1) params
         else Just $ value ++ " is not a valid QName for DatatypeLibrary " ++ w3cNS
       )

datatypeAllowsW3C d@"string" params value _
  = checkString d value 0 (-1) params

datatypeAllowsW3C d p v _ 
  = Just $ "Datatype " ++ d ++ " with parameter(s) " ++
           formatStringList ", "(map (\(a, b) -> a ++ " = " ++ b) p) ++ " and value = " ++ v ++
           " not allowed for DatatypeLibrary " ++ w3cNS

-- | Tests whether a XML instance value matches a value-pattern.
datatypeEqualW3C :: DatatypeEqual
datatypeEqualW3C d@"NCName" s1 _ s2 _
  = if s1 == s2 then Nothing else Just $ errorMsgEqual d s1 s2 
datatypeEqualW3C d@"anyURI" s1 _ s2 _ 
  = if s1 == s2 then Nothing else Just $ errorMsgEqual d s1 s2 
datatypeEqualW3C d@"QName" s1 _ s2 _ 
  = if s1 == s2 then Nothing else Just $ errorMsgEqual d s1 s2 
datatypeEqualW3C d@"string" s1 _ s2 _ 
  = if s1 == s2 then Nothing else Just $ errorMsgEqual d s1 s2 
datatypeEqualW3C d _ _ _ _
  = Just $ "Datatype " ++ d ++ " not allowed for DatatypeLibrary " ++ w3cNS
