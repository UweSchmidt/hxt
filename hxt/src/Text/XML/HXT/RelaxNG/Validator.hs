-- |
-- This module exports the core functions from the basic validation und simplification libraries.
-- It also exports some helper functions for easier access to the validation functionality.

module Text.XML.HXT.RelaxNG.Validator
  ( validateDocumentWithRelaxSchema
  , validateDocumentWithRelax

  , validate
  , validateSchema
  , validateWithSpezification
  , validateSchemaWithSpezification
  , validateWithoutSpezification
  , module Text.XML.HXT.RelaxNG.Validation
  , module Text.XML.HXT.RelaxNG.Simplification
  )
where

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState

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
        localSysParam (theInputConfig `pairS` theParseConfig `pairS` theRelaxConfig)
        $
        configSysParams config
        >>>
        ( ( validate' $< validSchema )    -- try to validate, only possible if schema is o.k.
          `orElse`
          this
        )
      )
      `when`
      documentStatusOk                  -- only do something when document status is ok
    where
    validate' schema
        = setDocumentStatusFromSystemState "read and build Relax NG schema"
          >>>
          validateDocumentWithRelax schema

    validSchema
        = traceMsg 2 ( "read Relax NG schema document: " ++ show relaxSchema )
          >>>
          readForRelax relaxSchema
          >>>
          perform ( let checkSchema = True in           -- test option in al
                    if checkSchema
                    then validateWithRelaxAndHandleErrors S.relaxSchemaArrow
                    else none
                  )
          >>>
          traceMsg 2 "create simplified schema"
          >>>
          ( (\ (b1, (b2, b3)) -> createSimpleForm b1 b2 b3)
            $<
            getSysParam (theRelaxCheckRestr `pairS`
                         (theRelaxValidateExtRef `pairS`
                          theRelaxValidateInclude
                         )
                        )
          )
          >>>
          traceDoc "simplified schema"
          >>>
          perform ( getErrors
                    >>>
                    handleSimplificationErrors
                  )

handleSimplificationErrors      :: IOSArrow XmlTree XmlTree
handleSimplificationErrors
    = traceDoc "simplification errors"
      >>>
      getChildren >>> getAttrValue "desc"
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
        perform ( validateWithRelaxAndHandleErrors (constA schema) )
        >>>
        setDocumentStatusFromSystemState "validate document with Relax NG schema"
      )
      `when` documentStatusOk                           -- only do something when document status is ok


{- |
   normalize a document for Relax NG validation,
   call the 'Text.XML.HXT.RelaxNG.Validation.validateRelax' function for doing the hard work,
   and issue errors

   * 1.parameter  : the arrow for computing the schema

   - arrow-input  : the document to be validated

   - arrow-output : nothing
-}

-- ------------------------------------------------------------

{- | Document validation

Validates a xml document with respect to a Relax NG schema.

First, the schema is validated with respect to the Relax NG Spezification. If no error is found, the xml document is validated with respect to the schema.

   * 1.parameter :  list of options; namespace progagation is always done

   - 2.parameter :  XML document

   - 3.parameter :  Relax NG schema file

available options:

    * 'a_do_not_check_restrictions' : do not check Relax NG schema restrictions (includes do-not-validate-externalRef, do-not-validate-include)

    - 'a_do_not_validate_externalRef' : do not validate a Relax NG schema referenced by a externalRef-Pattern

    - 'a_validate_externalRef' : validate a Relax NG schema referenced by a externalRef-Pattern (default)

    - 'a_do_not_validate_include' : do not validate a Relax NG schema referenced by a include-Pattern

    - 'a_validate_include' : validate a Relax NG schema referenced by a include-Pattern (default)

    - 'a_output_changes' : output Pattern transformations in case of an error

    - 'a_do_not_collect_errors' : stop Relax NG simplification after the first error has occurred

    - all 'Text.XML.HXT.Arrow.ReadDocument.readDocument' options

example:

> validate [(a_do_not_check_restrictions, "1")] "test.xml" "testSchema.rng"

-}
validate :: String -> String -> IOSArrow n XmlTree
validate xmlDocument relaxSchema
  = S.relaxSchemaArrow
    >>>
    validateWithSpezification xmlDocument relaxSchema




{- | Relax NG schema validation

Validates a Relax NG schema with respect to the Relax NG Spezification.

   * 1.parameter :  Relax NG schema file

-}
validateSchema :: String -> IOSArrow n XmlTree
validateSchema relaxSchema
  = validate "" relaxSchema



{- | Document validation

Validates a xml document with respect to a Relax NG schema. Similar to 'validate', but the Relax NG Specification is not created. Can be used, to check a list of documents more efficiently.

   * 1.parameter :  XML document

   - 2.parameter :  Relax NG schema file

   - arrow-input  :  Relax NG Specification in simple form

example:

> Text.XML.HXT.RelaxNG.Schema.relaxSchemaArrow
> >>>
> ( validateWithSpezification "foo.xml" "foo.rng"
>   &&&
>   validateWithSpezification "bar.xml" "bar.rng"
> )


-}
validateWithSpezification :: String -> String -> IOSArrow XmlTree XmlTree
validateWithSpezification xmlDocument relaxSchema
  = -- validation of the schema with respect to the specification
    -- returns a list of errors or none if the schema is correct

    ( validateRelax $< ( readForRelax relaxSchema
                         >>>
                         getChildren
                       )
    )
    `orElse`

    -- validation of the xml document with respect to the schema
    -- returns a list of errors or none if the xml document is correct

    validateWithoutSpezification xmlDocument relaxSchema

{- | Relax NG schema validation

    see 'validateSchema' and 'validateWithSpezification'

   * 1.parameter :  Relax NG schema file

   - arrow-input  :  Relax NG Specification in simple form
-}

validateSchemaWithSpezification :: String -> IOSArrow XmlTree XmlTree
validateSchemaWithSpezification relaxSchema
  = validateWithSpezification "" relaxSchema



{- | Document validation

Validates a xml document with respect to a Relax NG schema, but the schema is @not@ validated with respect to a specification first. Should be used only for valid Relax NG schemes.

   * 1.parameter :  XML document

   - 2.parameter :  Relax NG schema file

-}


validateWithoutSpezification :: String -> String -> IOSArrow n XmlTree
validateWithoutSpezification xmlDocument relaxSchema
  = readForRelax relaxSchema
    >>>
    createSimpleForm True True True
    >>>
    ( ( getErrors >>> perform handleSimplificationErrors )              -- issue errors in schema simplification
      `orElse`
      ( if null xmlDocument                                             -- no errors: validate document
        then none
        else validateRelax $< ( readForRelax xmlDocument
                                >>>
                                normalizeForRelaxValidation
                                >>>
                                getChildren
                              )
      )
    )
