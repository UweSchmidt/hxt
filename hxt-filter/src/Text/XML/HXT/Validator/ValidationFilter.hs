-- |
-- This module provides functions for validating XML documents represented as
-- XmlTree.

-- Unlike other popular XML validation tools the validation functions return
-- a list of errors instead of aborting after the first error was found.
--
-- Note: The validation process has been split into validation and transformation!
--       If @validate@ did not report any errors, @transform@
--       should be called, to change the document the way a validating parser
--       is expected to do.

--
-- Author : .\\artin Schmidt

module Text.XML.HXT.Validator.ValidationFilter
    ( getDTDSubset
    , validate
    , validateDTD
    , validateDoc
    , removeDoublicateDefs
    , transform
    )

where

import Text.XML.HXT.DOM.XmlTree

import qualified Text.XML.HXT.Validator.DTDValidation as DTDValidation
import qualified Text.XML.HXT.Validator.DocValidation as DocValidation
import qualified Text.XML.HXT.Validator.IdValidation as IdValidation
import qualified Text.XML.HXT.Validator.DocTransformation as DocTransformation

-- |
-- Main validation filter. Check if the DTD and the document are valid.
--
--
--    - returns : a function which expects a complete document as XmlTree input
--                     and returns a list of all errors found.

validate        :: XmlFilter
validate        = validateDTD +++ validateDoc

-- |
-- Check if the DTD is valid.
--
--
--    - returns : a function which expects an XmlTree from the parser as input
--                     and returns a list of all errors found in the DTD.

validateDTD     :: XmlFilter
validateDTD
    = choice [ getDTDSubset
               :-> DTDValidation.validateDTD
             , this
               :-> err "Can't validate DTD: There is no DOCTYPE declaration in the document."
             ]
-- |
-- Check if the document corresponds to the given DTD.
--
--
--    - returns : a function which expects a complete document as XmlTree input
--                     and returns a list of all errors found in the content part.

validateDoc     :: XmlFilter
validateDoc t
    = ( if null dtds
        then err "Can't validate document: There is no DOCTYPE declaration in the document."
        else ( DocValidation.validateDoc dtdPart
               +++
               IdValidation.validateIds dtdPart
             )
      ) $ t
      where
      dtds = getDTDSubset .> processBottomUp removeDoublicateDefs $ t
      dtdPart  = head dtds

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

transform       :: XmlFilter
transform
    = choice [ isRoot
               :-> transformDoc
             , this
               :-> fatal "Can't transform document: No document root given"
             ]
      where
      transformDoc t
          | null dtds
              = this t
          | otherwise
              = DocTransformation.transform (head dtds) t
          where
          dtds = getDTDSubset .> processBottomUp removeDoublicateDefs $ t

-- |
-- Removes doublicate declarations from the DTD which first declaration is
-- binding. This is the case for ATTLIST and ENTITY declarations.
--
--
--    - returns : A function that replaces the children of DOCTYPE nodes by a list
--               where all multiple declarations are removed.

removeDoublicateDefs :: XmlFilter
removeDoublicateDefs
    = DTDValidation.removeDoublicateDefs

--
-- selects the DTD part of a document
-- but only, if there is more than the internal part for the 4 predefined XML entities

getDTDSubset    :: XmlFilter
getDTDSubset
    = getChildren .> isDoctype .> hasRootName
      where
      hasRootName = isOf (hasEntry a_name . attrlOfDTD)
