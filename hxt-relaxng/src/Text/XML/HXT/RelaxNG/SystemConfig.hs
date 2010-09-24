-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG
   Copyright  : Copyright (C) 2010 Uwe Schmidt, Torben Kuseler
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   This helper module exports elements from the basic Relax NG libraries:
   Validator, CreatePattern, PatternToString and DataTypes.
   It is the main entry point to the Relax NG schema validator of the Haskell
   XML Toolbox.

-}

-- ------------------------------------------------------------

module   Text.XML.HXT.RelaxNG.SystemConfig
where

import Data.Function.Selector                   ( setS
                                                , (.&&&.)
                                                )
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.XmlOptions

import Text.XML.HXT.RelaxNG.Validator           ( validateDocumentWithRelaxSchema )


import System.Console.GetOpt

-- ------------------------------------------------------------

withRelaxNG                     :: String -> SysConfig
withRelaxNG s                   = setS (theRelaxValidate
					.&&&. theRelaxSchema
					.&&&. theRelaxValidator
				       ) (True, (s, validateDocumentWithRelaxSchema [] s))

withRelaxCheckRestr             ::  Bool -> SysConfig
withRelaxCheckRestr             = setS theRelaxCheckRestr

withRelaxValidateExtRef         :: Bool -> SysConfig
withRelaxValidateExtRef         = setS theRelaxValidateExtRef

withRelaxValidateInclude        :: Bool -> SysConfig
withRelaxValidateInclude        = setS theRelaxValidateInclude

withRelaxCollectErrors          :: Bool -> SysConfig
withRelaxCollectErrors          = setS theRelaxCollectErrors

-- ------------------------------------------------------------

-- | available Relax NG validation options
--
-- defines options
-- 'a_check_restrictions', 'a_validate_externalRef', 'a_validate_include', 'a_do_not_check_restrictions',
-- 'a_do_not_validate_externalRef', 'a_do_not_validate_include'

relaxOptions :: [OptDescr SysConfig]
relaxOptions
    = [ Option "X" [a_relax_schema]                     (ReqArg withRelaxNG             "SCHEMA")  "validation with Relax NG, SCHEMA is the URI for the Relax NG schema"
      , Option ""  [a_check_restrictions]               (NoArg (withRelaxCheckRestr        True))  "check Relax NG schema restrictions during schema simplification (default)"
      , Option ""  [a_do_not_check_restrictions]        (NoArg (withRelaxCheckRestr       False))  "do not check Relax NG schema restrictions"
      , Option ""  [a_validate_externalRef]             (NoArg (withRelaxValidateExtRef    True))  "validate a Relax NG schema referenced by a externalRef-Pattern (default)"
      , Option ""  [a_do_not_validate_externalRef]      (NoArg (withRelaxValidateExtRef   False))  "do not validate a Relax NG schema referenced by an externalRef-Pattern"
      , Option ""  [a_validate_include]                 (NoArg (withRelaxValidateInclude   True))  "validate a Relax NG schema referenced by an include-Pattern (default)"
      , Option ""  [a_do_not_validate_include]          (NoArg (withRelaxValidateInclude  False))   "do not validate a Relax NG schema referenced by an include-Pattern"
      , Option ""  [a_collect_errors]                   (NoArg (withRelaxCollectErrors     True))   "collect errors, default"
      , Option ""  [a_do_not_collect_errors]            (NoArg (withRelaxCollectErrors    False))   "do not collect errors"
      ]

-- ------------------------------------------------------------
-- option for Relax NG

a_relax_schema,
 a_do_not_check_restrictions,
 a_check_restrictions,
 a_do_not_validate_externalRef,
 a_validate_externalRef,
 a_do_not_validate_include,
 a_validate_include,
 a_do_not_collect_errors :: String

a_relax_schema                = "relax-schema"
a_do_not_check_restrictions   = "do-not-check-restrictions"
a_check_restrictions          = "check-restrictions"
a_do_not_validate_externalRef = "do-not-validate-externalRef"
a_validate_externalRef        = "validate-externalRef"
a_do_not_validate_include     = "do-not-validate-include"
a_validate_include            = "validate-include"
a_do_not_collect_errors       = "do-not-collect-errors"

-- ------------------------------------------------------------
