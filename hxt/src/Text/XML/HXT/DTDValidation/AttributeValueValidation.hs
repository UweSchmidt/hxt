-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DTDValidation.TypeDefs
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   This module provides functions for validating attributes.

   The main functions are:

    - Check if the attribute value meets the lexical constraints of its type

    - Normalization of an attribute value
-}

-- ------------------------------------------------------------

-- Special namings in source code:
--
--  - nd - XDTD node
--
--  - n  - XTag node
--

module Text.XML.HXT.DTDValidation.AttributeValueValidation
    ( checkAttributeValue
    , normalizeAttributeValue
    )
where

import Text.XML.HXT.Parser.XmlParsec
    ( parseNMToken
    , parseName
    )

import Text.XML.HXT.DTDValidation.TypeDefs

-- ------------------------------------------------------------

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

checkAttributeValue :: XmlTrees -> XmlTree -> XmlArrow
checkAttributeValue dtdPart attrDecl
    | isDTDAttlistNode attrDecl
	= choiceA
	  [ isElem       :-> ( checkAttrVal $< getAttrValue attrName )
	  , isDTDAttlist :-> ( checkAttrVal $< (getDTDAttrl >>^ dtd_default) )
	  , this	     :-> none
	  ]
    | otherwise
	= none
      where
      al	= getDTDAttributes attrDecl
      attrName	= dtd_value al
      attrType  = dtd_type  al
      checkAttrVal attrValue
	  = checkValue attrType dtdPart normalizedVal attrDecl
	    where
	    normalizedVal = normalizeAttributeValue (Just attrDecl) attrValue

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

checkValue :: String -> XmlTrees -> String -> XmlTree -> XmlArrow
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

checkValueEnumeration :: XmlTree -> String -> XmlArrow
checkValueEnumeration attrDecl attrValue
    | isDTDAttlistNode attrDecl
      &&
      attrValue `notElem` enumVals
	= err ( "Attribute " ++ show (dtd_value al) ++ " for element " ++ show (dtd_name al) ++
                " must have a value from list "++ show enumVals {- ++ " but has value " ++ show attrValue-} ++ ".")
    | otherwise
	= none
      where
      al	= getDTDAttributes attrDecl

      enumVals :: [String]
      enumVals = map (dtd_name . getDTDAttributes) $ (runLA getChildren attrDecl)

-- |
-- Checks the value of ENTITY attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node, to get the
--                    unparsed entity declarations
--
--    - 2.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 3.parameter attrValue :  the normalized attribute value to be checked

checkValueEntity :: XmlTrees -> XmlTree -> String -> XmlArrow
checkValueEntity dtdPart attrDecl attrValue
    | isDTDAttlistNode attrDecl
      &&
      attrValue `notElem` upEntities
	= err ( "Entity " ++ show attrValue ++ " of attribute " ++ show (dtd_value al) ++
                " for element " ++ show (dtd_name al) ++ " is not unparsed. " ++
                "The following unparsed entities exist: " ++ show upEntities ++ ".")
    | otherwise
	= none
      where
      al	= getDTDAttributes attrDecl

      upEntities :: [String]
      upEntities = map (dtd_name . getDTDAttributes) (isUnparsedEntity $$ dtdPart)

-- |
-- Checks the value of ENTITIES attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter dtdPart :  the children of the @DOCTYPE@ node, to get the
--                    unparsed entity declarations
--
--    - 2.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 3.parameter attrValue :  the normalized attribute value to be checked

checkValueEntities ::XmlTrees -> XmlTree -> String -> XmlArrow
checkValueEntities dtdPart attrDecl attrValue
    | isDTDAttlistNode attrDecl
	= if null valueList
	  then err ("Attribute " ++ show (dtd_value al) ++ " of element " ++
                    show (dtd_name al) ++ " must be one or more names.")
          else catA . map (checkValueEntity dtdPart attrDecl) $ valueList
    | otherwise
	= none
      where
      al	= getDTDAttributes attrDecl
      valueList = words attrValue

-- |
-- Checks the value of NMTOKEN attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueNmtoken :: XmlTree -> String -> XmlArrow
checkValueNmtoken attrDecl attrValue
    | isDTDAttlistNode attrDecl
	= constA attrValue >>> checkNmtoken
    | otherwise
	= none
      where
      al	= getDTDAttributes attrDecl
      checkNmtoken
	  = mkText >>> arrL (parseNMToken "")
	    >>>
	    isError
	    >>>
	    getErrorMsg
	    >>>
	    arr (\ s -> ( "Attribute value " ++ show attrValue ++ " of attribute " ++ show (dtd_value al) ++
			  " for element " ++ show (dtd_name al) ++ " must be a name token, "++ (lines s) !! 1 ++".") )
            >>>
	    mkError c_err

-- |
-- Checks the value of NMTOKENS attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueNmtokens :: XmlTree -> String -> XmlArrow
checkValueNmtokens attrDecl attrValue
    | isDTDAttlistNode attrDecl
	= if null valueList
	  then err ( "Attribute "++ show (dtd_value al) ++" of element " ++
                     show (dtd_name al) ++ " must be one or more name tokens.")
          else catA . map (checkValueNmtoken attrDecl) $ valueList
    | otherwise
	= none
      where
      al	= getDTDAttributes attrDecl
      valueList = words attrValue

-- |
-- Checks the value of ID attribute types. (3.3.1 \/ p.25 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueId :: XmlTree -> String -> XmlArrow
checkValueId attrDecl attrValue
    = checkForName "Attribute value" attrDecl attrValue


-- |
-- Checks the value of IDREF attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueIdref :: XmlTree -> String -> XmlArrow
checkValueIdref attrDecl attrValue
    = checkForName "Attribute value" attrDecl attrValue


-- |
-- Checks the value of IDREFS attribute types. (3.3.1 \/ p.26 in Spec)
--
--    * 1.parameter attrDecl :  the declaration of the attribute from the DTD
--
--    - 2.parameter attrValue :  the normalized attribute value to be checked

checkValueIdrefs :: XmlTree -> String -> XmlArrow
checkValueIdrefs attrDecl attrValue
    = catA . map (checkValueIdref attrDecl) . words $ attrValue



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

checkForName ::  String -> XmlTree -> String -> XmlArrow
checkForName msg attrDecl attrValue
    | isDTDAttlistNode attrDecl
	= constA attrValue >>> checkName
    | otherwise
	= none
    where
    al	= getDTDAttributes attrDecl
    checkName
	= mkText >>> arrL (parseName "")
	  >>>
	  isError
	  >>>
	  getErrorMsg
	  >>>
	  arr (\s -> ( msg ++ " " ++ show attrValue ++" of attribute " ++ show (dtd_value al) ++
		       " for element "++ show (dtd_name al) ++" must be a name, " ++ (lines s) !! 1 ++ ".") )
          >>>
	  mkError c_err

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
normalizeAttributeValue (Just attrDecl) value
    = normalizeAttribute attrType
      where
      al	     = getDTDAttributes attrDecl
      attrType = dtd_type al

      normalizeAttribute :: String -> String
      normalizeAttribute typ
          | typ == k_cdata	= cdataNormalization value
          | otherwise		= otherNormalization value

-- Attribute not declared in DTD, normalization as CDATA
normalizeAttributeValue Nothing value
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

-- ------------------------------------------------------------

