-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlState.SystemConfig
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   system configuration and common options options

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlState.SystemConfig
where

import Control.Arrow

import Data.Map                         ( insert )

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlState.ErrorHandling
import Text.XML.HXT.Arrow.XmlState.TypeDefs

-- ------------------------------

-- config options

-- | @withTrace level@ : system option, set the trace level, (0..4)

withTrace                       :: Int -> SysConfig
withTrace                       = setS theTraceLevel

-- | @withSysAttr key value@ : store an arbitrary key value pair in system state

withSysAttr                     :: String -> String -> SysConfig
withSysAttr n v                 = chgS theAttrList (addEntry n v)

-- | Specify the set of accepted mime types.
--
-- All contents of documents for which the mime type is not found in this list
-- are discarded.

withAcceptedMimeTypes           :: [String] -> SysConfig
withAcceptedMimeTypes           = setS theAcceptedMimeTypes

-- | Specify a content handler for documents of a given mime type

withMimeTypeHandler             :: String -> IOSArrow XmlTree XmlTree -> SysConfig
withMimeTypeHandler mt pa       = chgS theMimeTypeHandlers $ insert mt pa

-- | @withMimeTypeFile filename@ : input option,
-- set the mime type table for @file:@ documents by given file.
-- The format of this config file must be in the syntax of a debian linux \"mime.types\" config file

withMimeTypeFile                :: String -> SysConfig
withMimeTypeFile                = setS theMimeTypeFile

-- | Force a given mime type for all file contents.
--
-- The mime type for file access will then not be computed by looking into a mime.types file

withFileMimeType                :: String -> SysConfig
withFileMimeType                = setS theFileMimeType

-- | @withWarnings yes/no@ : system option, issue warnings during reading, HTML parsing and processing,
-- default is 'yes'

withWarnings                    :: Bool -> SysConfig
withWarnings                    = setS theWarnings

-- | @withErrors yes/no@ : system option for suppressing error messages, default is 'no'

withErrors                      :: Bool -> SysConfig
withErrors b                    = setS theErrorMsgHandler h
    where
    h | b                       = errorOutputToStderr
      | otherwise               = const $ return ()

-- | @withRemoveWS yes/no@ : read and write option, remove all whitespace, used for document indentation, default is 'no'

withRemoveWS                    :: Bool -> SysConfig
withRemoveWS                    = setS theRemoveWS

-- | @withPreserveComment yes/no@ : read option, preserve comments during canonicalization, default is 'no'

withPreserveComment             :: Bool -> SysConfig
withPreserveComment             = setS thePreserveComment

-- | @withParseByMimeType yes/no@  : read option, select the parser by the mime type of the document
-- (pulled out of the HTTP header).
--
-- When the mime type is set to \"text\/html\"
-- the configured HTML parser is taken, when it\'s set to
-- \"text\/xml\" or \"text\/xhtml\" the configured XML parser is taken.
-- If the mime type is something else, no further processing is performed,
-- the contents is given back to the application in form of a single text node.
-- If the default document encoding is set to isoLatin1, this even enables processing
-- of arbitray binary data.

withParseByMimeType             :: Bool -> SysConfig
withParseByMimeType             = setS theParseByMimeType

-- | @withParseHTML yes/no@: read option, use HTML parser, default is 'no' (use XML parser)

withParseHTML                   :: Bool -> SysConfig
withParseHTML                   = setS theParseHTML

-- | @withValidate yes/no@: read option, validate document against DTD, default is 'yes'

withValidate                    :: Bool -> SysConfig
withValidate                    = setS theValidate

-- | @withSubstDTDEntities yes/no@: read option, substitute general entities defined in DTD, default is 'yes'.
-- switching this option and the validate option off can lead to faster parsing, because then
-- there is no need to access the DTD

withSubstDTDEntities            :: Bool -> SysConfig
withSubstDTDEntities            = setS theSubstDTDEntities

-- | @withSubstHTMLEntities yes/no@: read option, substitute general entities defined in HTML DTD, default is 'no'.
-- switching this option on and the substDTDEntities and validate options off can lead to faster parsing
-- because there is no need to access a DTD, but still the HTML general entities are substituted

withSubstHTMLEntities            :: Bool -> SysConfig
withSubstHTMLEntities            = setS theSubstHTMLEntities

-- | @withCheckNamespaces yes/no@: read option, check namespaces, default is 'no'

withCheckNamespaces             :: Bool -> SysConfig
withCheckNamespaces             = setS theCheckNamespaces

-- | @withCanonicalize yes/no@ : read option, canonicalize document, default is 'yes'

withCanonicalize                :: Bool -> SysConfig
withCanonicalize                = setS theCanonicalize

-- | @withIgnoreNoneXmlContents yes\/no@ : input option, ignore document contents of none XML\/HTML documents.
--
-- This option can be useful for implementing crawler like applications, e.g. an URL checker.
-- In those cases net traffic can be reduced.

withIgnoreNoneXmlContents       :: Bool -> SysConfig
withIgnoreNoneXmlContents       = setS theIgnoreNoneXmlContents

-- ------------------------------------------------------------

-- | @withStrictInput yes/no@ : input option, input of file and HTTP contents is read eagerly, default is 'no'

withStrictInput                 :: Bool -> SysConfig
withStrictInput                 = setS theStrictInput

-- | @withEncodingErrors yes/no@ : input option, ignore all encoding errors, default is 'no'

withEncodingErrors              :: Bool -> SysConfig
withEncodingErrors              = setS theEncodingErrors

-- | @withInputEncoding encodingName@ : input option
--
-- Set default document encoding ('utf8', 'isoLatin1', 'usAscii', 'iso8859_2', ... , 'iso8859_16', ...).
-- Only XML, HTML and text documents are decoded,
-- default decoding for XML\/HTML is utf8, for text iso latin1 (no decoding).

withInputEncoding               :: String -> SysConfig
withInputEncoding               = setS theInputEncoding

-- | @withDefaultBaseURI URI@ , input option, set the default base URI
--
-- This option can be useful when parsing documents from stdin or contained in a string, and interpreting
-- relative URIs within the document

withDefaultBaseURI              :: String -> SysConfig
withDefaultBaseURI              = setS theDefaultBaseURI

withInputOption                 :: String -> String -> SysConfig
withInputOption n v             = chgS theInputOptions (addEntry n v)

withInputOptions                :: Attributes -> SysConfig
withInputOptions                = foldr (>>>) id . map (uncurry withInputOption)

-- | @withRedirect yes/no@ : input option, automatically follow redirected URIs, default is 'yes'

withRedirect                    :: Bool -> SysConfig
withRedirect                    = setS theRedirect

-- | @withProxy \"host:port\"@ : input option, configure a proxy for HTTP access, e.g. www-cache:3128

withProxy                       :: String -> SysConfig
withProxy                       = setS theProxy

-- ------------------------------------------------------------

-- | @withIndent yes/no@ : output option, indent document before output, default is 'no'

withIndent                      :: Bool -> SysConfig
withIndent                      = setS theIndent

-- | @withOutputEncoding encoding@ , output option,
-- default is the default input encoding or utf8, if input encoding is not set

withOutputEncoding              :: String -> SysConfig
withOutputEncoding              = setS theOutputEncoding

-- | @withOutputXML@ : output option, default writing
--
-- Default is writing XML: quote special XML chars \>,\<,\",\',& where neccessary,
-- add XML processing instruction
-- and encode document with respect to 'withOutputEncoding'

withOutputXML                   :: SysConfig
withOutputXML                   = setS theOutputFmt XMLoutput

-- | Write XHTML: quote all special XML chars, use HTML entity refs or char refs for none ASCII chars

withOutputHTML                  :: SysConfig
withOutputHTML                  = setS theOutputFmt HTMLoutput

-- | Write XML: quote only special XML chars, don't substitute chars by HTML entities,
-- and don\'t generate empty elements for HTML elements,
-- which may contain any contents, e.g. @<script src=...></script>@ instead of @<script src=... />@

withOutputXHTML                 :: SysConfig
withOutputXHTML                 = setS theOutputFmt XHTMLoutput

-- | suppreses all char and entitiy substitution

withOutputPLAIN                 :: SysConfig
withOutputPLAIN                 = setS theOutputFmt PLAINoutput

withXmlPi                       :: Bool -> SysConfig
withXmlPi                       = setS theXmlPi

withNoEmptyElemFor              :: [String] -> SysConfig
withNoEmptyElemFor              = setS theNoEmptyElemFor

withAddDefaultDTD               :: Bool -> SysConfig
withAddDefaultDTD               = setS theAddDefaultDTD

withTextMode                    :: Bool -> SysConfig
withTextMode                    = setS theTextMode

withShowTree                    :: Bool -> SysConfig
withShowTree                    = setS theShowTree

withShowHaskell                 :: Bool -> SysConfig
withShowHaskell                 = setS theShowHaskell

-- | Configure compression and decompression for binary serialization/deserialization.
-- First component is the compression function applied after serialization,
-- second the decompression applied before deserialization.

withCompression                 :: (CompressionFct, DeCompressionFct) -> SysConfig
withCompression                 = setS (theBinaryCompression .&&&. theBinaryDeCompression)

-- | Strict input for deserialization of binary data

withStrictDeserialize           :: Bool -> SysConfig
withStrictDeserialize           = setS theStrictDeserialize

-- ------------------------------------------------------------

yes                             :: Bool
yes                             = True

no                              :: Bool
no                              = False

-- ------------------------------------------------------------
