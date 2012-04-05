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

-- | ...
data SValEnv       = SValEnv
                   { xpath    :: XPath
                   , elemDesc :: ElemDesc
                   }

type XPath         = String

data ElemDesc      = ElemDesc
                   { errmsg       :: Maybe String
                   , attrDesc     :: AttrDesc
                   , contentModel :: XmlRegex
                   , subElemDesc  :: SubElemDesc
                   , sttf         :: STTF
                   }
type AttrDesc      = (AttrMap, AttrWildcards)
type AttrMap       = Map QName AttrMapVal
type AttrMapVal    = (Bool, STTF)
type AttrWildcards = [(QName -> Bool)]
type SubElemDesc   = Map QName ElemDesc
type STTF          = String -> SVal Bool

type SValResult    = (Bool, SValLog)
type SValLog       = [(XPath, String)]

type SVal a        = ReaderT SValEnv (WriterT SValLog Identity) a

runSVal :: SValEnv -> SVal a -> (a, SValLog)
runSVal env val = runIdentity $ runWriterT $ runReaderT val env

