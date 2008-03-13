-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DTDValidation.Validation
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   This module provides functions for validating XML documents represented as
   XmlTree.

   Unlike other popular XML validation tools the validation functions return
   a list of errors instead of aborting after the first error was found.

   Note: The validation process has been split into validation and transformation!
   If @validate@ did not report any errors, @transform@
   should be called, to change the document the way a validating parser
   is expected to do.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DTDValidation.Validation
    ( getDTDSubset
    , validate
    , validateDTD
    , validateDoc
    , removeDoublicateDefs
    , transform
    )

where

import Text.XML.HXT.DTDValidation.TypeDefs

import qualified Text.XML.HXT.DTDValidation.DTDValidation     as DTDValidation
import qualified Text.XML.HXT.DTDValidation.DocValidation     as DocValidation
import qualified Text.XML.HXT.DTDValidation.IdValidation      as IdValidation
import qualified Text.XML.HXT.DTDValidation.DocTransformation as DocTransformation

-- |
-- Main validation filter. Check if the DTD and the document are valid.
--
--
--    - returns : a function which expects a complete document as XmlTree input
--                     and returns a list of all errors found.

validate 	:: XmlArrow
validate	= validateDTD <+> validateDoc

-- |
-- Check if the DTD is valid.
--
--
--    - returns : a function which expects an XmlTree from the parser as input
--                     and returns a list of all errors found in the DTD.

validateDTD	:: XmlArrow
validateDTD	= choiceA
		  [ getDTDSubset	:-> DTDValidation.validateDTD
		  , this		:-> err "Can't validate DTD: There is no DOCTYPE declaration in the document."
		  ]
-- |
-- Check if the document corresponds to the given DTD.
--
--
--    - returns : a function which expects a complete document as XmlTree input
--                     and returns a list of all errors found in the content part.

validateDoc	:: XmlArrow
validateDoc
    = validateDoc' $< getDTD
    where
    validateDoc' []		= err "Can't validate document: There is no DOCTYPE declaration in the document."
    validateDoc' (dtdPart:_)	= DocValidation.validateDoc dtdPart
				  <+>
				  IdValidation.validateIds  dtdPart

getDTD		:: XmlArrowS
getDTD		= listA ( getDTDSubset
			  >>>
			  removeDoublicateDefs
			)

-- |
-- filter for transforming a document with respect to the given DTD.
--
-- Validating parsers
-- are expected to  normalize attribute values and add default values.
-- This function should be called after a successful validation.
--
--
--    - returns : a function which expects a complete XML document tree
--                and returns the transformed XmlTree

transform	:: XmlArrow
transform	= choiceA
		  [ isRoot	:-> (transformDoc $< getDTD)
		  , this	:-> fatal "Can't transform document: No document root given"
		  ]
                  where
		  transformDoc []	= this
		  transformDoc dtd	= DocTransformation.transform (head dtd)

-- |
-- Removes doublicate declarations from the DTD which first declaration is
-- binding. This is the case for ATTLIST and ENTITY declarations.
--
--
--    - returns : A function that replaces the children of DOCTYPE nodes by a list
--               where all multiple declarations are removed.

removeDoublicateDefs	:: XmlArrow
removeDoublicateDefs	= DTDValidation.removeDoublicateDefs

--
-- selects the DTD part of a document
-- but only, if there is more than the internal part for the 4 predefined XML entities

getDTDSubset		:: XmlArrow
getDTDSubset		= getChildren
			  >>>
			  ( filterA $ isDTDDoctype >>> getDTDAttrl >>> isA (hasEntry a_name) )


-- ------------------------------------------------------------