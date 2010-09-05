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

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlState.ErrorHandling
import Text.XML.HXT.Arrow.XmlState.TypeDefs

import Data.Maybe

import System.Console.GetOpt

-- ------------------------------

-- config options

-- | @withTace level@ : system option, set the trace level, (0..4)

withTrace                       :: Int -> SysConfig
withTrace                       = putS theTraceLevel

-- | @withAttr key value@ : store an arbitarty key value pair in system state

withAttr                        :: String -> String -> SysConfig
withAttr n v                    = chgS theAttrList (addEntry n v)

withAcceptedMimeTypes           :: [String] -> SysConfig
withAcceptedMimeTypes           = putS theAcceptedMimeTypes

-- | @withMimeTypeFile filename@ : input option,
-- set the mime type table for @file:@ documents by given file.
-- The format of this config file must be in the syntax of a debian linux \"mime.types\" config file

withMimeTypeFile                :: String -> SysConfig
withMimeTypeFile                = putS theMimeTypeFile

-- | @withWarnings yes/no@ : system option, issue warnings during reading, HTML parsing and processing,
-- default is 'yes'

withWarnings                    :: Bool -> SysConfig
withWarnings                    = putS theWarnings

-- | @withErrors yes/no@ : system option for suppressing error messages, default is 'no'

withErrors                      :: Bool -> SysConfig
withErrors b                    = putS theErrorMsgHandler h
    where
    h | b                       = errorOutputToStderr
      | otherwise               = const $ return ()

-- | @withRemoveWS yes/no@ : read and write option, remove all whitespace, used for document indentation, default is 'no'

withRemoveWS                    :: Bool -> SysConfig
withRemoveWS                    = putS theRemoveWS

-- | @withPreserveComment yes/no@ : read option, preserve comments during canonicalization, default is 'no'

withPreserveComment             :: Bool -> SysConfig
withPreserveComment             = putS thePreserveComment

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
withParseByMimeType             = putS theParseByMimeType

-- | @withParseHTML yes/no@: read option, use HTML parser, default is 'no' (use XML parser)

withParseHTML                   :: Bool -> SysConfig
withParseHTML                   = putS theParseHTML

-- | @withValidate yes/no@: read option, validate document againsd DTD, default is 'yes'

withValidate                    :: Bool -> SysConfig
withValidate                    = putS theValidate

-- | @withCheckNamespaces yes/no@: read option, check namespaces, default is 'no'

withCheckNamespaces             :: Bool -> SysConfig
withCheckNamespaces             = putS theCheckNamespaces

-- | @withCanonicalize yes/no@ : read option, canonicalize document, default is 'yes'

withCanonicalize                :: Bool -> SysConfig
withCanonicalize                = putS theCanonicalize

-- | @withIgnoreNoneXmlContents yes/no@ : input option, ignore document contents of none XML\/HTML documents.
--
-- This option can be useful for implementing crawler like applications, e.g. an URL checker.
-- In those cases net traffic can be reduced.

withIgnoreNoneXmlContents       :: Bool -> SysConfig
withIgnoreNoneXmlContents       = putS theIgnoreNoneXmlContents

-- ------------------------------------------------------------

-- | @withStrictInput yes/no@ : input option, input of file and HTTP contents is read eagerly, default is 'no'

withStrictInput                 :: Bool -> SysConfig
withStrictInput                 = putS theStrictInput

-- | @withEncodingErrors yes/no@ : input option, ignore all encoding errors, default is 'no'

withEncodingErrors              :: Bool -> SysConfig
withEncodingErrors              = putS theEncodingErrors

-- | @withInputEncoding encodingName@ : input option
--
-- Set default document encoding ('utf8', 'isoLatin1', 'usAscii', 'iso8859_2', ... , 'iso8859_16', ...).
-- Only XML, HTML and text documents are decoded,
-- default decoding for XML\/HTML is utf8, for text iso latin1 (no decoding).

withInputEncoding               :: String -> SysConfig
withInputEncoding               = putS theInputEncoding

-- | @withDefaultBaseURI URI@ , input option, set the default base URI
--
-- This option can be useful when parsing documents from stdin or contained in a string, and interpreting
-- relative URIs within the document

withDefaultBaseURI              :: String -> SysConfig
withDefaultBaseURI              = putS theDefaultBaseURI

withInputOption                 :: String -> String -> SysConfig
withInputOption n v             = chgS theInputOptions (addEntry n v)

withInputOptions                :: Attributes -> SysConfig
withInputOptions                = foldr (>>>) id . map (uncurry withInputOption)

-- | @withRedirect yes/no@ : input option, automatically follow redirected URIs, default is 'yes'

withRedirect                    :: Bool -> SysConfig
withRedirect                    = putS theRedirect

-- | @withProxy \"host:port\"@ : input option, configure a proxy for HTTP access, e.g. www-cache:3128

withProxy                       :: String -> SysConfig
withProxy                       = putS theProxy

-- ------------------------------------------------------------

-- | @withIndent yes/no@ : output option, indent document before output, default is 'no'

withIndent                      :: Bool -> SysConfig
withIndent                      = putS theIndent

-- | @withOutputEncoding encoding@ , output option,
-- default is the default input encoding or utf8, if input encoding is not set

withOutputEncoding              :: String -> SysConfig
withOutputEncoding              = putS theOutputEncoding

withOutputFile                  :: String -> SysConfig
withOutputFile                  = putS theOutputFile

-- | @withOutputXML@ : output option, default writing
--
-- Default is writing XML: quote special XML chars \>,\<,\",\',& where neccessary,
-- add XML processing instruction
-- and encode document with respect to 'withOutputEncoding'

withOutputXML                   :: SysConfig
withOutputXML                   = putS theOutputFmt XMLoutput

-- | Write XHTML: quote all special XML chars, use HTML entity refs or char refs for none ASCII chars

withOutputHTML                  :: SysConfig
withOutputHTML                  = putS theOutputFmt HTMLoutput

-- | Write XML: quote only special XML chars, don't substitute chars by HTML entities, and don\'t generate empty elements for HTML elements,
-- which may contain any contents, e.g. @<script src=...></script>@ instead of @<script src=... />@

withOutputXHTML                 :: SysConfig
withOutputXHTML                 = putS theOutputFmt XHTMLoutput

-- | suppreses all char and entitiy substitution

withOutputPLAIN                 :: SysConfig
withOutputPLAIN                 = putS theOutputFmt PLAINoutput

withNoXmlPi                     :: Bool -> SysConfig
withNoXmlPi                     = putS theNoXmlPi

withNoEmptyElemFor              :: [String] -> SysConfig
withNoEmptyElemFor              = putS theNoEmptyElemFor

withNoEmptyElements             :: Bool -> SysConfig
withNoEmptyElements             = putS theNoEmptyElements

withAddDefaultDTD               :: Bool -> SysConfig
withAddDefaultDTD               = putS theAddDefaultDTD

withTextMode                    :: Bool -> SysConfig
withTextMode                    = putS theTextMode

withShowTree                    :: Bool -> SysConfig
withShowTree                    = putS theShowTree

withShowHaskell                 :: Bool -> SysConfig
withShowHaskell                 = putS theShowHaskell

-- ------------------------------------------------------------

withRelaxNG                     :: String -> SysConfig
withRelaxNG s                   = putS (theRelaxValidate `pairS` theRelaxSchema) (True, s)

withRelaxCheckRestr             ::  Bool -> SysConfig
withRelaxCheckRestr             = putS theRelaxCheckRestr

withRelaxValidateExtRef         :: Bool -> SysConfig
withRelaxValidateExtRef         = putS theRelaxValidateExtRef

withRelaxValidateInclude        :: Bool -> SysConfig
withRelaxValidateInclude        = putS theRelaxValidateInclude

-- ------------------------------------------------------------
--

yes                             :: Bool
yes                             = True

no                              :: Bool
no                              = False


-- |
-- commonly useful options for XML input
--
-- can be used for option definition with haskell getopt
--
-- defines options: 'a_trace', 'a_proxy', 'a_use_curl', 'a_do_not_use_curl', 'a_options_curl', 'a_encoding',
-- 'a_issue_errors', 'a_do_not_issue_errors', 'a_parse_html', 'a_parse_by_mimetype', 'a_issue_warnings', 'a_do_not_issue_warnings',
-- 'a_parse_xml', 'a_validate', 'a_do_not_validate', 'a_canonicalize', 'a_do_not_canonicalize',
--- 'a_preserve_comment', 'a_do_not_preserve_comment', 'a_check_namespaces', 'a_do_not_check_namespaces',
-- 'a_remove_whitespace', 'a_do_not_remove_whitespace'

inputSysConfigOptions    :: [OptDescr SysConfig]
inputSysConfigOptions
    = [ Option "t"      [a_trace]                       (OptArg trc "LEVEL")                    "trace level (0-4), default 1"
      , Option "p"      [a_proxy]                       (ReqArg  withProxy            "PROXY")  "proxy for http access (e.g. \"www-cache:3128\")"
      , Option ""       [a_redirect]                    (NoArg  (withRedirect           True))  "automatically follow redirected URIs"
      , Option ""       [a_no_redirect]                 (NoArg  (withRedirect          False))  "switch off following redirected URIs"
      , Option ""       [a_default_baseuri]             (ReqArg  withDefaultBaseURI     "URI")  "default base URI, default: \"file:///<cwd>/\""
      , Option "e"      [a_encoding]                    (ReqArg  withInputEncoding  "CHARSET")  ( "default document encoding (" ++ utf8 ++ ", " ++ isoLatin1 ++ ", " ++ usAscii ++ ", ...)" )
      , Option ""       [a_mime_types]                  (ReqArg  withMimeTypeFile      "FILE")  "set mime type configuration file, e.g. \"/etc/mime.types\""
      , Option ""       [a_issue_errors]                (NoArg  (withErrors             True))  "issue all error messages on stderr (default)"
      , Option ""       [a_do_not_issue_errors]         (NoArg  (withErrors            False))  "ignore all error messages"
      , Option ""       [a_ignore_encoding_errors]      (NoArg  (withEncodingErrors    False))   "ignore encoding errors"
      , Option ""       [a_ignore_none_xml_contents]    (NoArg  (withIgnoreNoneXmlContents True)) "discards all contents of none XML/HTML documents, only the meta info remains in the doc tree"
      , Option ""       [a_accept_mimetypes]            (ReqArg  withMT           "MIMETYPES") "only accept documents matching the given list of mimetype specs"
      , Option "H"      [a_parse_html]                  (NoArg  (withParseHTML          True))  "parse input as HTML, try to interprete everything as HTML, no validation"
      , Option "M"      [a_parse_by_mimetype]           (NoArg  (withParseByMimeType    True))  "parse dependent on mime type: text/html as HTML, text/xml and text/xhtml and others as XML, else no parse"
      , Option ""       [a_parse_xml]                   (NoArg  (withParseHTML         False))  "parse input as XML, (default)"
      , Option ""       [a_strict_input]                (NoArg  (withStrictInput        True))  "read input files strictly, this ensures closing the files correctly even if not read completely"
      , Option ""       [a_issue_warnings]              (NoArg  (withWarnings           True))  "issue warnings, when parsing HTML (default)"
      , Option "Q"      [a_do_not_issue_warnings]       (NoArg  (withWarnings          False))  "ignore warnings, when parsing HTML"
      , Option ""       [a_validate]                    (NoArg  (withValidate           True))  "document validation when parsing XML (default)"
      , Option "w"      [a_do_not_validate]             (NoArg  (withValidate          False))  "only wellformed check, no validation"
      , Option ""       [a_canonicalize]                (NoArg  (withCanonicalize       True))  "canonicalize document, remove DTD, comment, transform CDATA, CharRef's, ... (default)"
      , Option "c"      [a_do_not_canonicalize]         (NoArg  (withCanonicalize      False))  "do not canonicalize document, don't remove DTD, comment, don't transform CDATA, CharRef's, ..."
      , Option "C"      [a_preserve_comment]            (NoArg  (withPreserveComment    True))  "don't remove comments during canonicalisation"
      , Option ""       [a_do_not_preserve_comment]     (NoArg  (withPreserveComment   False))  "remove comments during canonicalisation (default)"
      , Option "n"      [a_check_namespaces]            (NoArg  (withCheckNamespaces    True))  "tag tree with namespace information and check namespaces"
      , Option ""       [a_do_not_check_namespaces]     (NoArg  (withCheckNamespaces   False))  "ignore namespaces (default)"
      , Option "r"      [a_remove_whitespace]           (NoArg  (withRemoveWS           True))  "remove redundant whitespace, simplifies tree and processing"
      , Option ""       [a_do_not_remove_whitespace]    (NoArg  (withRemoveWS          False))  "don't remove redundant whitespace (default)"
      ]
    where
    withMT = withAcceptedMimeTypes . words
    trc = withTrace . max 0 . min 9 . (read :: String -> Int) . ('0':) . filter (`elem` "0123456789") . fromMaybe v_1

-- | available Relax NG validation options
--
-- defines options
-- 'a_check_restrictions', 'a_validate_externalRef', 'a_validate_include', 'a_do_not_check_restrictions',
-- 'a_do_not_validate_externalRef', 'a_do_not_validate_include'

relaxSysConfigOptions :: [OptDescr SysConfig]
relaxSysConfigOptions
    = [ Option "X" [a_relax_schema]                     (ReqArg withRelaxNG             "SCHEMA")  "validation with Relax NG, SCHEMA is the URI for the Relax NG schema"
      , Option ""  [a_check_restrictions]               (NoArg (withRelaxCheckRestr        True))  "check Relax NG schema restrictions during schema simplification (default)"
      , Option ""  [a_do_not_check_restrictions]        (NoArg (withRelaxCheckRestr       False))  "do not check Relax NG schema restrictions"
      , Option ""  [a_validate_externalRef]             (NoArg (withRelaxValidateExtRef    True))  "validate a Relax NG schema referenced by a externalRef-Pattern (default)"
      , Option ""  [a_do_not_validate_externalRef]      (NoArg (withRelaxValidateExtRef   False))  "do not validate a Relax NG schema referenced by an externalRef-Pattern"
      , Option ""  [a_validate_include]                 (NoArg (withRelaxValidateInclude   True))  "validate a Relax NG schema referenced by an include-Pattern (default)"
      , Option ""  [a_do_not_validate_include]          (NoArg (withRelaxValidateInclude  False))   "do not validate a Relax NG schema referenced by an include-Pattern"
      ]

-- |
-- commonly useful options for XML output
--
-- defines options: 'a_indent', 'a_output_encoding', 'a_output_file', 'a_output_html'

outputSysConfigOptions   :: [OptDescr SysConfig]
outputSysConfigOptions
    = [ Option "i"      [a_indent]              (NoArg  (withIndent                 True))      "indent XML output for readability"
      , Option "o"      [a_output_encoding]     (ReqArg  withOutputEncoding     "CHARSET")      ( "encoding of output (" ++ utf8 ++ ", " ++ isoLatin1 ++ ", " ++ usAscii ++ ")" )
      , Option "f"      [a_output_file]         (ReqArg  withOutputFile            "FILE")      "output file for resulting document (default: stdout)"
      , Option ""       [a_output_xml]          (NoArg   withOutputXML                   )      "output of none ASCII chars as HTMl entity references"
      , Option ""       [a_output_html]         (NoArg   withOutputHTML                  )      "output of none ASCII chars as HTMl entity references"
      , Option ""       [a_output_xhtml]        (NoArg   withOutputXHTML                 )      "output of HTML elements with empty content (script, ...) done in format <elem...></elem> instead of <elem/>"
      , Option ""       [a_output_plain]        (NoArg   withOutputPLAIN                 )      "output of HTML elements with empty content (script, ...) done in format <elem...></elem> instead of <elem/>"
      , Option ""       [a_no_xml_pi]           (NoArg  (withNoXmlPi                True))      ("output without <?xml ...?> processing instruction, useful in combination with --" ++ show a_output_html)
      , Option ""       [a_no_empty_elem_for]   (ReqArg (withNoEmptyElemFor . words') "NAMES")  "output of empty elements done in format <elem...></elem> only for given list of element names"
      , Option ""       [a_no_empty_elements]   (NoArg  (withNoEmptyElements        True))      "output of empty elements done in format <elem...></elem> instead of <elem/>"
      , Option ""       [a_add_default_dtd]     (NoArg  (withAddDefaultDTD          True))      "add the document type declaration given in the input document"
      , Option ""       [a_text_mode]           (NoArg  (withTextMode               True))      "output in text mode"
      ]
    where
    words'
        = words
          . map (\ c -> if c == ',' then ' ' else c)

-- |
-- commonly useful options
--
-- defines options: 'a_verbose', 'a_help'

generalSysConfigOptions  :: [OptDescr SysConfig]
generalSysConfigOptions
    = [ Option "v"      [a_verbose]             (NoArg  (withAttr a_verbose v_1))               "verbose output"
      , Option "h?"     [a_help]                (NoArg  (withAttr a_help    v_1))               "this message"
      ]

-- |
-- defines 'a_version' option

versionSysConfigOptions  :: [OptDescr SysConfig]
versionSysConfigOptions
    = [ Option "V"      [a_version]             (NoArg  (withAttr a_version v_1))               "show program version"
      ]

-- |
-- debug output options

showSysConfigOptions     :: [OptDescr SysConfig]
showSysConfigOptions
    = [ Option ""       [a_show_tree]           (NoArg  (withShowTree      True))          "output tree representation instead of document source"
      , Option ""       [a_show_haskell]        (NoArg  (withShowHaskell   True))          "output internal Haskell representation instead of document source"
      ]

-- ------------------------------------------------------------
