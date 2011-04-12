-- |
-- This module exports the core functions from the basic validation und simplification libraries.
-- It also exports some helper functions for easier access to the validation functionality.

module Text.XML.HXT.RelaxNG.Validator
  ( validateDocumentWithRelaxSchema
  , validateDocumentWithRelax
  , validateSchemaWithRelax
  , validateWithSpezification
  , validateSchemaWithSpezification

  , module Text.XML.HXT.RelaxNG.Validation
  , module Text.XML.HXT.RelaxNG.Simplification
  )
where

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs

import Text.XML.HXT.RelaxNG.BasicArrows
import Text.XML.HXT.RelaxNG.Validation
import Text.XML.HXT.RelaxNG.Simplification
import Text.XML.HXT.RelaxNG.Schema        as S

-- ------------------------------------------------------------

{- |
   validate a document with a Relax NG schema

   * 1.parameter  : the system configuration option list for validation

   - 2.parameter  : the URI of the Relax NG Schema

   - arrow-input  : the document to be validated, namespaces must have been processed

   - arrow-output : the input document, or in case of validation errors, an empty document with status information in the root

configuration options evaluated by validateDocumentWithRelaxSchema:

    * 'withRelaxCheckRestr'      : check Relax NG schema restrictions when simplifying the schema (default: on)

    - 'withRelaxValidateExtRef'  : validate a Relax NG schema referenced by a externalRef-Pattern (default: on)

    - 'withRelaxValidateInclude' : validate a Relax NG schema referenced by a include-Pattern (default: on)

example:

> validateDocumentWithRelaxSchema [withRelaxCheckRestr yes, withRelaxValidateExtRef no] "testSchema.rng"


-}

validateDocumentWithRelaxSchema :: SysConfigList -> String -> IOStateArrow s XmlTree XmlTree
validateDocumentWithRelaxSchema config relaxSchema
    = ( withoutUserState
        $
        localSysEnv
        $
        configSysVars config
        >>>
        traceMsg 1 ( "start validating document with Relax NG schema: " ++ show relaxSchema )
        >>>
        ( ( ( validate' $< validateSchemaWithRelax relaxSchema)    -- try to validate, only possible if schema is o.k.
            >>>
            traceMsg 1 ( "validating document with Relax NG schema done" )
          )
          `orElse`
          ( setDocumentStatusFromSystemState "validating Relax NG schema"
            >>>
            traceMsg 1 ( "no validation done, Relax NG schema is not correct" )
          )
        )
      )
      `when`
      documentStatusOk                                 -- only do something when document status is ok
    where
    validate' schema
        = setDocumentStatusFromSystemState "read and build Relax NG schema"
          >>>
          validateDocumentWithRelax schema

validateSchemaWithRelax         :: String -> IOSArrow XmlTree XmlTree
validateSchemaWithRelax relaxSchema
    = traceMsg 2 ( "read and check Relax NG schema document: " ++ show relaxSchema )
      >>>
      readForRelax relaxSchema
      >>>
      ( let checkSchema = True in           -- test option in al
            if checkSchema
            then validateWithRelax S.relaxSchemaArrow `guards` this
            else this
      )
      >>>
      traceMsg 2 "create simplified schema"
      >>>
      ( (\ (b1, (b2, b3)) -> createSimpleForm b1 b2 b3)
        $<
        getSysVar (theRelaxCheckRestr     .&&&.
                   theRelaxValidateExtRef .&&&.
                   theRelaxValidateInclude
                  )
      )
      >>>
      traceDoc "simplified schema"
      >>>
      traceMsg 2 "collect and issue schema errors"
      >>>
      perform handleSimplificationErrors
      >>>
      resetStates
      >>>
      setDocumentStatusFromSystemState "validating Relax NG schema"
      >>>
      documentStatusOk
      >>>
      traceMsg 2 "Relax NG schema is o.k."

handleSimplificationErrors      :: IOSArrow XmlTree XmlTree
handleSimplificationErrors
    = traceDoc "simplification errors"
      >>>
      getErrors
      >>>
      getRngAttrDescr
      >>>
      arr ("Relax NG validation: " ++)
      >>>
      mkError c_err
      >>>
      filterErrorMsg


{- | validate an XML document with respect to a Relax NG schema

   * 1.parameter  : the valid and simplified schema as XML tree

   - arrow-input  : the document to be validated

   - arrow-output : the validated and unchanged document or the empty document with status information set in the root node
-}

validateDocumentWithRelax       :: XmlTree -> IOSArrow XmlTree XmlTree
validateDocumentWithRelax schema
    = ( traceMsg 1 "validate document with Relax NG schema"
        >>>
        perform ( validateWithRelax (constA schema) )
        >>>
        setDocumentStatusFromSystemState "validate document with Relax NG schema"
      )
      `when` documentStatusOk                           -- only do something when document status is ok

-- ------------------------------------------------------------

{- | Relax NG schema validation

    see 'validateSchemaWithRelax' and 'validateWithSpezification'

   * 1.parameter :  Relax NG schema file

   - arrow-input  :  Relax NG Specification in simple form
-}

validateSchemaWithSpezification :: String -> IOSArrow XmlTree XmlTree
validateSchemaWithSpezification relaxSchema
    = validateWithSpezification "" relaxSchema


{- | Document validation

Validates a xml document with respect to a Relax NG schema

   * 1.parameter :  XML document

   - 2.parameter :  Relax NG schema file

-}


validateWithSpezification :: String -> String -> IOSArrow XmlTree XmlTree
validateWithSpezification xmlDocument relaxSchema
    = validDoc $< listA (validateSchemaWithRelax relaxSchema)
    where
    validDoc [theSchema]
        | null xmlDocument
            = none
        | otherwise
            = ifA ( readForRelax xmlDocument
                    >>>
                    normalizeForRelaxValidation
                    >>>
                    validateRelax theSchema
                  )
              none
              ( err "Document is not valid with respect to Relax NG Schema" )
    validDoc _
        = err "Relax NG Schema not correct"

-- ------------------------------------------------------------

