-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow
   Copyright  : Copyright (C) 2006-2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   The HXT arrow interface

   The application programming interface to the arrow modules of the Haskell XML Toolbox.
   This module exports all important arrows for input, output, parsing, validating and transforming XML.
   It also exports all basic datatypes and functions of the toolbox.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow
    ( module Control.Arrow.ListArrows

    , module Text.XML.HXT.DOM.Interface

    , module Text.XML.HXT.Arrow.XmlArrow
    , module Text.XML.HXT.Arrow.XmlIOStateArrow

    , module Text.XML.HXT.Arrow.DocumentInput
    , module Text.XML.HXT.Arrow.DocumentOutput
    , module Text.XML.HXT.Arrow.Edit
    , module Text.XML.HXT.Arrow.GeneralEntitySubstitution
    , module Text.XML.HXT.Arrow.Namespace
    , module Text.XML.HXT.Arrow.ProcessDocument
    , module Text.XML.HXT.Arrow.ReadDocument
    , module Text.XML.HXT.Arrow.WriteDocument
    , module Text.XML.HXT.Arrow.Pickle

    , module Text.XML.HXT.Version
    )
where

import Control.Arrow.ListArrows                 -- arrow classes

import Data.Atom ()                             -- import this explicitly

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.DocumentInput
import Text.XML.HXT.Arrow.DocumentOutput
import Text.XML.HXT.Arrow.Edit
import Text.XML.HXT.Arrow.GeneralEntitySubstitution
import Text.XML.HXT.Arrow.Namespace
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.ProcessDocument
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.WriteDocument
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow
import Text.XML.HXT.Arrow.XmlRegex ()           -- import this explicitly

import Text.XML.HXT.Version

-- ------------------------------------------------------------
