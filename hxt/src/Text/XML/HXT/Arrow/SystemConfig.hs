-- |
-- Common useful options
--
-- Version : $Id: XmlOptions.hs,v 1.1 2006/11/09 20:27:42 hxml Exp $
--
--

module Text.XML.HXT.Arrow.SystemConfig
    ( inputSysConfigOptions
    , relaxSysConfigOptions
    , outputSysConfigOptions
    , generalSysConfigOptions
    , versionSysConfigOptions
    , showSysConfigOptions
    )
where

import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Data.Maybe
import Data.String.EncodingNames

import System.Console.GetOpt

-- ------------------------------------------------------------
--

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
      , Option "p"      [a_proxy]                       (ReqArg (att a_proxy)         "PROXY")  "proxy for http access (e.g. \"www-cache:3128\")"
      , Option ""       [a_redirect]                    (NoArg  (att a_redirect          v_1))  "automatically follow redirected URIs"
      , Option ""       [a_no_redirect]                 (NoArg  (att a_redirect          v_0))  "switch off following redirected URIs"
      , Option ""       [a_use_curl]                    (NoArg  (att a_use_curl          v_1))  "obsolete, since hxt-8.1 HTTP access is always done with curl bindings"
      , Option ""       [a_do_not_use_curl]             (NoArg  (att a_use_curl          v_0))  "obsolete, since hxt-8.1 HTTP access is always done with curl bindings"
      , Option ""       [a_options_curl]                (ReqArg (att a_options_curl)    "STR")  "additional curl options, e.g. for timeout, ..."
      , Option ""       [a_default_baseuri]             (ReqArg (att transferURI)       "URI")  "default base URI, default: \"file:///<cwd>/\""
      , Option "e"      [a_encoding]                    (ReqArg (att a_encoding)    "CHARSET")  ( "default document encoding (" ++ utf8 ++ ", " ++ isoLatin1 ++ ", " ++ usAscii ++ ", ...)" )
      , Option ""       [a_mime_types]                  (ReqArg  withMimeTypeFile      "FILE")  "set mime type configuration file, e.g. \"/etc/mime.types\""
      , Option ""       [a_issue_errors]                (NoArg  (withErrors             True))  "issue all error messages on stderr (default)"
      , Option ""       [a_do_not_issue_errors]         (NoArg  (withErrors            False))  "ignore all error messages"
      , Option ""       [a_ignore_encoding_errors]      (NoArg  (att a_ignore_encoding_errors v_1))   "ignore encoding errors"
      , Option ""       [a_ignore_none_xml_contents]    (NoArg  (att a_ignore_none_xml_contents v_1)) "discards all contents of none XML/HTML documents, only the meta info remains in the doc tree"
      , Option ""       [a_accept_mimetypes]            (ReqArg  withMT           "MIMETYPES") "only accept documents matching the given list of mimetype specs"
      , Option "H"      [a_parse_html]                  (NoArg  (withParseHTML          True))  "parse input as HTML, try to interprete everything as HTML, no validation"
      , Option "M"      [a_parse_by_mimetype]           (NoArg  (withParseByMimeType    True))  "parse dependent on mime type: text/html as HTML, text/xml and text/xhtml and others as XML, else no parse"
      , Option ""       [a_parse_xml]                   (NoArg  (withParseHTML         False))  "parse input as XML, (default)"
      , Option ""       [a_strict_input]                (NoArg  (att a_strict_input      v_1))  "read input files strictly, this ensures closing the files correctly even if not read completely"
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
    trc = att a_trace . show . max 0 . min 9 . (read :: String -> Int) . ('0':) . filter (`elem` "0123456789") . fromMaybe v_1

att	:: String -> String -> SysConfig
att 	= curry optionToSysConfig

-- | available Relax NG validation options
--
-- defines options
-- 'a_check_restrictions', 'a_validate_externalRef', 'a_validate_include', 'a_do_not_check_restrictions',
-- 'a_do_not_validate_externalRef', 'a_do_not_validate_include'

relaxSysConfigOptions :: [OptDescr SysConfig]
relaxSysConfigOptions
    = [ Option "X" [a_relax_schema]                     (ReqArg (att a_relax_schema) "SCHEMA")  "validation with Relax NG, SCHEMA is the URI for the Relax NG schema"
      , Option ""  [a_check_restrictions]               (NoArg (att a_check_restrictions    v_1))  "check Relax NG schema restrictions during schema simplification (default)"
      , Option ""  [a_do_not_check_restrictions]        (NoArg (att a_check_restrictions    v_0))  "do not check Relax NG schema restrictions"
      , Option ""  [a_validate_externalRef]             (NoArg (att a_validate_externalRef  v_1))  "validate a Relax NG schema referenced by a externalRef-Pattern (default)"
      , Option ""  [a_do_not_validate_externalRef]      (NoArg (att a_validate_externalRef  v_0))  "do not validate a Relax NG schema referenced by an externalRef-Pattern"
      , Option ""  [a_validate_include]                 (NoArg (att a_validate_include      v_1))  "validate a Relax NG schema referenced by an include-Pattern (default)"
      , Option ""  [a_do_not_validate_include]          (NoArg (att a_validate_include      v_0))   "do not validate a Relax NG schema referenced by an include-Pattern"
        {-
      , Option ""  [a_output_changes]                   (NoArg (a_output_changes,        v_1))  "output Pattern transformations in case of an error"
      , Option ""  [a_do_not_collect_errors]            (NoArg (a_do_not_collect_errors, v_1))  "stop Relax NG simplification after the first error has occurred"
        -}
      ]

-- |
-- commonly useful options for XML output
--
-- defines options: 'a_indent', 'a_output_encoding', 'a_output_file', 'a_output_html'

outputSysConfigOptions   :: [OptDescr SysConfig]
outputSysConfigOptions
    = [ Option "i"      [a_indent]              (NoArg  (att a_indent                v_1))      "indent XML output for readability"
      , Option "o"      [a_output_encoding]     (ReqArg (att a_output_encoding) "CHARSET")      ( "encoding of output (" ++ utf8 ++ ", " ++ isoLatin1 ++ ", " ++ usAscii ++ ")" )
      , Option "f"      [a_output_file]         (ReqArg (att a_output_file)        "FILE")      "output file for resulting document (default: stdout)"
      , Option ""       [a_output_html]         (NoArg  (att a_output_html           v_1))      "output of none ASCII chars as HTMl entity references"
      , Option ""       [a_no_xml_pi]           (NoArg  (att a_no_xml_pi             v_1))      ("output without <?xml ...?> processing instruction, useful in combination with --" ++ show a_output_html)
      , Option ""       [a_output_xhtml]        (NoArg  (att a_output_xhtml          v_1))      "output of HTML elements with empty content (script, ...) done in format <elem...></elem> instead of <elem/>"
      , Option ""       [a_no_empty_elem_for]   (ReqArg (att a_no_empty_elem_for) "NAMES")      "output of empty elements done in format <elem...></elem> only for given list of element names"
      , Option ""       [a_no_empty_elements]   (NoArg  (att a_no_empty_elements     v_1))      "output of empty elements done in format <elem...></elem> instead of <elem/>"
      , Option ""       [a_add_default_dtd]     (NoArg  (att a_add_default_dtd       v_1))      "add the document type declaration given in the input document"
      , Option ""       [a_text_mode]           (NoArg  (att a_text_mode             v_1))      "output in text mode"
      ]

-- |
-- commonly useful options
--
-- defines options: 'a_verbose', 'a_help'

generalSysConfigOptions  :: [OptDescr SysConfig]
generalSysConfigOptions
    = [ Option "v"      [a_verbose]             (NoArg  (att a_verbose v_1))               "verbose output"
      , Option "h?"     [a_help]                (NoArg  (att a_help    v_1))               "this message"
      ]

-- |
-- defines 'a_version' option

versionSysConfigOptions  :: [OptDescr SysConfig]
versionSysConfigOptions
    = [ Option "V"      [a_version]             (NoArg  (att a_version v_1))               "show program version"
      ]

-- |
-- debug output options

showSysConfigOptions     :: [OptDescr SysConfig]
showSysConfigOptions
    = [ Option ""       [a_show_tree]           (NoArg  (att a_show_tree    v_1))          "output tree representation instead of document source"
      , Option ""       [a_show_haskell]        (NoArg  (att a_show_haskell v_1))          "output internal Haskell representation instead of document source"
      ]

-- ------------------------------------------------------------
