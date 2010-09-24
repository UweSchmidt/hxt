-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlState.MimeTypeTable
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   the mime type configuration functions

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlState.MimeTypeTable
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIO

import Data.Function.Selector           ( setS
                                        , (.&&&.)
                                        )

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlState.TypeDefs

-- ------------------------------------------------------------

-- | set the table mapping of file extensions to mime types in the system state
--
-- Default table is defined in 'Text.XML.HXT.DOM.MimeTypeDefaults'.
-- This table is used when reading loacl files, (file: protocol) to determine the mime type

setMimeTypeTable                :: MimeTypeTable -> IOStateArrow s b b
setMimeTypeTable mtt            = configSysVar $ setS (theMimeTypes .&&&. theMimeTypeFile) (mtt, "")

-- | set the table mapping of file extensions to mime types by an external config file
--
-- The config file must follow the conventions of /etc/mime.types on a debian linux system,
-- that means all empty lines and all lines starting with a # are ignored. The other lines
-- must consist of a mime type followed by a possible empty list of extensions.
-- The list of extenstions and mime types overwrites the default list in the system state
-- of the IOStateArrow

setMimeTypeTableFromFile        :: FilePath -> IOStateArrow s b b
setMimeTypeTableFromFile file   = configSysVar $ setS theMimeTypeFile file

-- | read the system mimetype table

getMimeTypeTable                :: IOStateArrow s b MimeTypeTable
getMimeTypeTable                = getMime $< getSysVar (theMimeTypes .&&&. theMimeTypeFile)
    where
    getMime (mtt, "")           = constA mtt
    getMime (_,  mtf)           = perform (setMimeTypeTable $< arrIO0 ( readMimeTypeTable mtf))
                                  >>>
                                  getMimeTypeTable

-- ------------------------------------------------------------
