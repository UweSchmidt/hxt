{- |
   Module     : Text.XML.HXT.XMLSchema.ValidationTypes
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   Contains the basic datatypes which are used during validation.
-}

module Text.XML.HXT.XMLSchema.ValidationTypes

where

import Text.XML.HXT.Core           ( QName )

import Text.XML.HXT.Arrow.XmlRegex ( XmlRegex )

import Control.Monad.Identity      ( Identity
                                   , runIdentity
                                   )

import Control.Monad.Reader        ( ReaderT
                                   , runReaderT
                                   )

import Control.Monad.Writer        ( WriterT
                                   , runWriterT
                                   )

import Data.Map                    ( Map )

-- ----------------------------------------

-- | The environment used during validation
data SValEnv       = SValEnv
                   { xpath    :: XPath
                   , elemDesc :: ElemDesc
                   }

-- | Simple XPath representation
type XPath         = String

-- | Description for an element under test
data ElemDesc      = ElemDesc
                   { errmsg       :: Maybe String
                   , attrDesc     :: AttrDesc
                   , mixedContent :: Bool
                   , contentModel :: XmlRegex
                   , subElemDesc  :: SubElemDesc
                   , sttf         :: STTF
                   }

-- | Description for allowed attributes of an element
type AttrDesc      = (AttrMap, AttrWildcards)

-- | Table for regular attributes
type AttrMap       = Map QName AttrMapVal

-- | Entry for AttrMap: required-flag and test function
type AttrMapVal    = (Bool, STTF)

-- | List of test functions for attribute wildcards
type AttrWildcards = [(QName -> Bool)]

-- | Table of possible subelems and their descriptions
type SubElemDesc   = Map QName ElemDesc

-- | SimpleType test function to validate basic values
type STTF          = String -> SVal Bool

-- | Validation result contains the validation status and log
type SValResult    = (Bool, SValLog)

-- | Validation log is a list of XPaths and messages
type SValLog       = [(XPath, String)]

-- | Schema validation monad
type SVal a        = ReaderT SValEnv (WriterT SValLog Identity) a

-- | Runs a computation in the schema validation monad
runSVal :: SValEnv -> SVal a -> (a, SValLog)
runSVal env val = runIdentity $ runWriterT $ runReaderT val env

