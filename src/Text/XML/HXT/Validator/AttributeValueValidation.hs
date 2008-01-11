-- |
-- This module provides functions for validating attributes.

-- The main functions are:
--
--  - Check if the attribute value meets the lexical constraints of its type
--
--  - Normalization of an attribute value
--

-- Special namings in source code:
--
--  - nd - XDTD node
--
--  - n  - XTag node
--
-- Version : $Id: AttributeValueValidation.hs,v 1.1 2004/09/02 19:12:02 hxml Exp $
--
module Text.XML.HXT.Validator.AttributeValueValidation
    ( checkAttributeValue
    , normalizeAttributeValue
    )
where

import Text.XML.HXT.Parser.XmlParser
    ( parseNMToken
    , parseName
    )

import Text.XML.HXT.DOM.XmlTree
import Text.XML.HXT.DOM.Util

-- |
-- Checks if the attribute value meets the lexical constraints of its type.
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 2.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - returns : a function which takes an element (XTag or XDTD ATTLIST),
--                    checks if the attribute value meets the lexical constraints
--                    of its type and returns a list of errors

checkAttributeValue :: XmlTrees -> XmlTree -> XmlFilter
checkAttributeValue dtdPart attrDecl@(NTree (XDTD ATTLIST al) _) n@(NTree (XTag _ _al') _)
    = checkValue attrType dtdPart normalizedVal attrDecl n
      where
      attrType      = lookup1 a_type al
      attrValue     = valueOf (getAttrName al) n
      normalizedVal = normalizeAttributeValue (Just attrDecl) attrValue

checkAttributeValue dtdPart attrDecl@(NTree (XDTD ATTLIST al) _) n@(NTree (XDTD ATTLIST al') _)
    = checkValue attrType dtdPart normalizedVal attrDecl n
      where
      attrType      = lookup1 a_type al
      attrValue     = lookup1 a_default al'
      normalizedVal = normalizeAttributeValue (Just attrDecl) attrValue

checkAttributeValue _ nd n
    = error ("checkAttributeValue: illegeal parameter:\n" ++ show nd ++ show n)


-- |
-- Dispatches the attibute check by the attribute type.
--
--    * 1.parameter typ :  the attribute type
--
--    - 2.parameter dtdPart :  the children of the @DOCTYPE@ node
--
--    - 3.parameter attrValue :  the normalized attribute value to be checked
--
--    - 4.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - returns : a functions which takes an element (XTag or XDTD ATTLIST),
--                        checks if the attribute value meets the lexical constraints
--                        of its type and returns a list of errors

checkValue :: String -> XmlTrees -> String -> XmlTree -> XmlFilter
checkValue typ dtdPart attrValue attrDecl
	| typ == k_cdata	= none
	| typ == k_enumeration	= checkValueEnumeration attrDecl attrValue
	| typ == k_entity	= checkValueEntity dtdPart attrDecl attrValue
	| typ == k_entities	= checkValueEntities dtdPart attrDecl attrValue
	| typ == k_id		= checkValueId attrDecl attrValue
	| typ == k_idref	= checkValueIdref attrDecl attrValue
	| typ == k_idrefs	= checkValueIdrefs attrDecl attrValue
	| typ == k_nmtoken	= checkValueNmtoken attrDecl attrValue
	| typ == k_nmtokens	= checkValueNmtokens attrDecl attrValue
	| typ == k_notation	= checkValueEnumeration attrDecl attrValue
	| otherwise		= error ("Attribute type " ++ show typ ++ " unknown.")

-- |
-- Checks the value of Enumeration attribute types. (3.3.1 \/ p.27 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueEnumeration :: XmlTree -> String -> XmlFilter
checkValueEnumeration (NTree (XDTD ATTLIST al) cs) attrValue
    = if attrValue `elem` enumVals
      then none
      else err ("Attribute " ++ show (getAttrName al) ++ " for element " ++ show (getElemName al) ++
                " must have a value from list "++ show enumVals ++ ".")
      where
      enumVals :: [String]
      enumVals = map (getEnumVal) cs

      getEnumVal :: XmlTree -> String
      getEnumVal (NTree (XDTD NAME al') _) = lookup1 a_name al'
      getEnumVal _                         = ""

checkValueEnumeration n _
    = error ("checkValueEnumeration: illegeal parameter:\n" ++ show n)


-- |
-- Checks the value of ENTITY attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node, to get the
--                    unparsed entity declarations
--
--    - 2.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 3.parameter attrValue :  the normalized attribute value to be checked

checkValueEntity :: XmlTrees -> XmlTree -> String -> XmlFilter
checkValueEntity dtdPart (NTree (XDTD ATTLIST al) _) attrValue
    = if attrValue `elem` upEntities
      then none
      else err ("Entity " ++ show attrValue ++ " of attribute " ++ show (getAttrName al) ++
                " for element " ++ show (getElemName al) ++ " is not unparsed. " ++
                "The following unparsed entities exist: " ++ show upEntities ++ ".")
      where
      upEntities :: [String]
      upEntities = map (getEnumVal) (isUnparsedEntity $$ dtdPart)

      getEnumVal :: XmlTree -> String
      getEnumVal (NTree (XDTD ENTITY al') _) = lookup1 a_name al'
      getEnumVal _                           = ""


checkValueEntity _ n _
    = error ("checkValueEntity: illegeal parameter:\n" ++ show n)


-- |
-- Checks the value of ENTITIES attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node, to get the
--                    unparsed entity declarations
--
--    - 2.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 3.parameter attrValue :  the normalized attribute value to be checked

checkValueEntities ::XmlTrees -> XmlTree -> String -> XmlFilter
checkValueEntities dtdPart attrDecl@(NTree (XDTD ATTLIST al) _) attrValue
    = if null valueList
      then err ("Attribute " ++ show (getAttrName al) ++ " of element " ++
                 show (getElemName al) ++ " must be one or more names.")
      else cat (map (checkValueEntity dtdPart attrDecl) valueList)
      where
      valueList = words attrValue

checkValueEntities _ n _
    = error ("checkValueEntities: illegeal parameter:\n" ++ show n)


-- |
-- Checks the value of NMTOKEN attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueNmtoken :: XmlTree -> String -> XmlFilter
checkValueNmtoken (NTree (XDTD ATTLIST al) _) attrValue
    = checkNmtoken parseRes
      where
      parseRes :: XmlTrees
      parseRes = parseNMToken "" (mkXTextTree attrValue)

      checkNmtoken :: XmlTrees -> XmlFilter
      checkNmtoken ((NTree (XError _ s) _):_)
          = err ("Attribute value " ++ show attrValue ++ " of attribute " ++ show (getAttrName al) ++
	         " for element " ++ show (getElemName al) ++ " must be a name token, "++ (lines s) !! 1 ++".")

      checkNmtoken _ = none


checkValueNmtoken n _
    = error ("checkValueNmtoken: illegeal parameter:\n" ++ show n)


-- |
-- Checks the value of NMTOKENS attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueNmtokens :: XmlTree -> String -> XmlFilter
checkValueNmtokens attrDecl@(NTree (XDTD ATTLIST al) _) attrValue
    = if null valueList
      then err ("Attribute "++ show (getAttrName al) ++" of element " ++
                 show (getElemName al) ++ " must be one or more name tokens.")
      else cat (map (checkValueNmtoken attrDecl) valueList)
      where
      valueList = words attrValue

checkValueNmtokens n _
    = error ("checkValueNmtokens: illegeal parameter:\n" ++ show n)


-- |
-- Checks the value of ID attribute types. (3.3.1 \/ p.25 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueId :: XmlTree -> String -> XmlFilter
checkValueId attrDecl attrValue
    = checkForName "Attribute value" attrDecl attrValue


-- |
-- Checks the value of IDREF attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueIdref :: XmlTree -> String -> XmlFilter
checkValueIdref attrDecl attrValue
    = checkForName "Attribute value" attrDecl attrValue


-- |
-- Checks the value of IDREFS attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueIdrefs :: XmlTree -> String -> XmlFilter
checkValueIdrefs attrDecl attrValue
    = cat (map (checkValueIdref attrDecl) (words attrValue))



-- -----------------------------------------------------------------------------
-- General helper functions for checking attribute values
--

-- |
-- Checks if the value of an attribute is a name.
--
--    * 1.parameter msg :  error message, should be "Entity" or "Attribute value"
--
--    - 2.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 3.parameter attrValue :  the normalized attribute value to be checked

checkForName ::  String -> XmlTree -> String -> XmlFilter
checkForName msg (NTree (XDTD ATTLIST al) _) attrValue
    = checkName parseRes
      where
      parseRes :: XmlTrees
      parseRes = parseName "" (mkXTextTree attrValue)

      checkName :: XmlTrees -> XmlFilter
      checkName ((NTree (XError _ s) _):_)
          = err (msg ++ " "++ show attrValue ++" of attribute " ++ show (getAttrName al) ++
	    " for element "++ show (getElemName al) ++" must be a name, " ++ (lines s) !! 1 ++ ".")

      checkName _ = none


checkForName _ n _
    = error ("checkForName: illegeal parameter:\n" ++ show n)


-- |
-- Gets the element name from an attribute list of an XDTD ATTLIST node.
--
--    * 1.parameter tag :  the attibute list of an XDTD ATTLIST
--
--    - returns : the element name

getElemName :: Attributes -> String
getElemName = lookup1 a_name


-- |
-- Gets the attribute name from an attribute list of an XDTD ATTLIST node.
--
--    * 1.parameter tag :  the attibute list of an XDTD ATTLIST
--
--    - returns : the attribute name

getAttrName :: Attributes -> String
getAttrName = lookup1 a_value



-- -----------------------------------------------------------------------------


-- |
-- Normalizes an attribute value with respect to its type. (3.3.3 \/ p.29 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD. Expected
--                   is a list. If the list is empty, no declaration exists.
--
--    - 2.parameter value :  the attribute value to be normalized
--
--    - returns : the normalized value
--
normalizeAttributeValue :: Maybe XmlTree -> String -> String
normalizeAttributeValue (Just (NTree (XDTD ATTLIST al) _)) value
    = normalizeAttribute attrType
      where
      attrType = lookup1 a_type al

      normalizeAttribute :: String -> String
      normalizeAttribute typ
          | typ == k_cdata	= cdataNormalization value
          | otherwise		= otherNormalization value


-- Attribute not declared in DTD, normalization as CDATA
normalizeAttributeValue _ value
    = cdataNormalization value


-- ------------------------------------------------------------
-- Helper functions for normalization


-- |
-- Normalization of CDATA attribute values.
-- is already done when parsing
-- during entity substituion for attribute values
cdataNormalization :: String -> String
cdataNormalization = id


-- | Normalization of attribute values other than CDATA.
otherNormalization :: String -> String
otherNormalization = reduceWSSequences . stringTrim . cdataNormalization

-- | Reduce whitespace sequences to a single whitespace.
reduceWSSequences :: String -> String
reduceWSSequences str = unwords (words str)


