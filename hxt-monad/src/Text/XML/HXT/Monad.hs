-- ------------------------------------------------------------

{- |
   The HXT interface based on monads

   The application programming interface
   to the monad base modules of the Haskell XML Toolbox.
   This module exports all important arrows for input,
   output, parsing, validating and transforming XML.
   It also exports all basic datatypes and functions of the toolbox.
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Monad
    ( module Control.Monad
    , module Control.Monad.Arrow
    , module Data.Sequence.ArrowTypes
    , module Text.XML.HXT.DOM.Interface
    , module Text.XML.HXT.Monad.ArrowXml
    , module Text.XML.HXT.Monad.XmlState
    , module Text.XML.HXT.Monad.DocumentInput
    , module Text.XML.HXT.Monad.DocumentOutput
    , module Text.XML.HXT.Monad.Edit
    , module Text.XML.HXT.Monad.GeneralEntitySubstitution
    , module Text.XML.HXT.Monad.Namespace
    , module Text.XML.HXT.Monad.Pickle
    , module Text.XML.HXT.Monad.ProcessDocument
    , module Text.XML.HXT.Monad.ReadDocument
    , module Text.XML.HXT.Monad.WriteDocument
    , module Text.XML.HXT.Monad.Binary
    , module Text.XML.HXT.Monad.XmlOptions
    )
where

import           Control.Monad
import           Control.Monad.Arrow

import           Data.Atom                                    ()
import           Data.Sequence.ArrowTypes

import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.Monad.ArrowXml
import           Text.XML.HXT.Monad.Binary
import           Text.XML.HXT.Monad.DocumentInput
import           Text.XML.HXT.Monad.DocumentOutput
import           Text.XML.HXT.Monad.Edit
import           Text.XML.HXT.Monad.GeneralEntitySubstitution
import           Text.XML.HXT.Monad.Namespace
import           Text.XML.HXT.Monad.Pickle
import           Text.XML.HXT.Monad.ProcessDocument
import           Text.XML.HXT.Monad.ReadDocument
import           Text.XML.HXT.Monad.WriteDocument
import           Text.XML.HXT.Monad.XmlOptions
import           Text.XML.HXT.Monad.XmlRegex                  ()
import           Text.XML.HXT.Monad.XmlState

-- ------------------------------------------------------------
