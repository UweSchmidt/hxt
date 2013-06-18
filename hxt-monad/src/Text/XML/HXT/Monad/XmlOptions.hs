-- ------------------------------------------------------------

{- |
   system configuration and common options options
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Monad.XmlOptions
where

import           Text.XML.HXT.DOM.Interface

import           Text.XML.HXT.Monad.XmlState.SystemConfig
import           Text.XML.HXT.Monad.XmlState.TypeDefs

import           Data.Maybe

import           System.Console.GetOpt

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

inputOptions    :: [OptDescr SysConfig]
inputOptions
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
      , Option ""       [a_accept_mimetypes]            (ReqArg  withMT           "MIMETYPES")  "only accept documents matching the given comma separated list of mimetype specs"
      , Option "H"      [a_parse_html]                  (NoArg  (withParseHTML          True))  "parse input as HTML, try to interprete everything as HTML, no validation"
      , Option "M"      [a_parse_by_mimetype]           (NoArg  (withParseByMimeType    True))  "parse dependent on mime type: text/html as HTML, text/xml and text/xhtml and others as XML, else no parse"
      , Option ""       [a_parse_xml]                   (NoArg  (withParseHTML         False))  "parse input as XML, (default)"
      , Option ""       [a_strict_input]                (NoArg  (withStrictInput        True))  "read input files strictly, this ensures closing the files correctly even if not read completely"
      , Option ""       [a_issue_warnings]              (NoArg  (withWarnings           True))  "issue warnings, when parsing HTML (default)"
      , Option "Q"      [a_do_not_issue_warnings]       (NoArg  (withWarnings          False))  "ignore warnings, when parsing HTML"
      , Option ""       [a_validate]                    (NoArg  (withValidate           True))  "document validation when parsing XML (default)"
      , Option "w"      [a_do_not_validate]             (NoArg  (withValidate          False))  "only wellformed check, no validation"
      , Option ""       [a_subst_dtd_entities]          (NoArg  (withSubstDTDEntities   True))  "entities defined in DTD are substituted when parsing XML (default)"
      , Option ""       [a_do_not_subst_dtd_entities]   (NoArg  (withSubstDTDEntities  False))  "entities defined in DTD are NOT substituted when parsing XML"
      , Option ""       [a_subst_html_entities]         (NoArg  (withSubstHTMLEntities   True)) "entities defined in XHTML are substituted when parsing XML, only in effect when prev. option is switched off"
      , Option ""       [a_do_not_subst_html_entities]  (NoArg  (withSubstHTMLEntities  False)) "only entities predefined in XML are substituted when parsing XML (default)"
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
    withMT = withAcceptedMimeTypes . words . map (\ x -> if x == ',' then ' ' else x)
    trc = withTrace . max 0 . min 9 . (read :: String -> Int) . ('0':) . filter (`elem` "0123456789") . fromMaybe v_1

-- |
-- commonly useful options for XML output
--
-- defines options: 'a_indent', 'a_output_encoding', 'a_output_html' and others

outputOptions   :: [OptDescr SysConfig]
outputOptions
    = [ Option "f"      [a_output_file]         (ReqArg (withSysAttr a_output_file) "FILE")   "output file for resulting document (default: stdout)"
      , Option "i"      [a_indent]              (NoArg  (withIndent               True))      "indent XML output for readability"
      , Option "o"      [a_output_encoding]     (ReqArg  withOutputEncoding    "CHARSET")      ( "encoding of output (" ++ utf8 ++ ", " ++ isoLatin1 ++ ", " ++ usAscii ++ ")" )
      , Option ""       [a_output_xml]          (NoArg   withOutputXML                  )      "output of none ASCII chars as HTMl entity references"
      , Option ""       [a_output_html]         (NoArg   withOutputHTML                 )      "output of none ASCII chars as HTMl entity references"
      , Option ""       [a_output_xhtml]        (NoArg   withOutputXHTML                )      "output of HTML elements with empty content (script, ...) done in format <elem...></elem> instead of <elem/>"
      , Option ""       [a_output_plain]        (NoArg   withOutputPLAIN                )      "output of HTML elements with empty content (script, ...) done in format <elem...></elem> instead of <elem/>"
      , Option ""       [a_no_xml_pi]           (NoArg  (withXmlPi                False))      ("output without <?xml ...?> processing instruction, useful in combination with --" ++ show a_output_html)
      , Option ""       [a_no_empty_elem_for]   (ReqArg (withNoEmptyElemFor . words') "NAMES")   "output of empty elements done in format <elem...></elem> only for given list of element names"
      , Option ""       [a_add_default_dtd]     (NoArg  (withAddDefaultDTD         True))      "add the document type declaration given in the input document"
      , Option ""       [a_text_mode]           (NoArg  (withTextMode              True))      "output in text mode"
      ]
    where
    words'
        = words
          . map (\ c -> if c == ',' then ' ' else c)

-- |
-- commonly useful options
--
-- defines options: 'a_verbose', 'a_help'

generalOptions  :: [OptDescr SysConfig]
generalOptions
    = [ Option "v"      [a_verbose]             (NoArg  (withSysAttr a_verbose v_1))               "verbose output"
      , Option "h?"     [a_help]                (NoArg  (withSysAttr a_help    v_1))               "this message"
      ]

-- |
-- defines 'a_version' option

versionOptions  :: [OptDescr SysConfig]
versionOptions
    = [ Option "V"      [a_version]             (NoArg  (withSysAttr a_version v_1))               "show program version"
      ]

-- |
-- debug output options

showOptions     :: [OptDescr SysConfig]
showOptions
    = [ Option ""       [a_show_tree]           (NoArg  (withShowTree      True))          "output tree representation instead of document source"
      , Option ""       [a_show_haskell]        (NoArg  (withShowHaskell   True))          "output internal Haskell representation instead of document source"
      ]

-- ------------------------------------------------------------

a_accept_mimetypes,
 a_add_default_dtd,
 a_canonicalize,
 a_check_namespaces,
 a_collect_errors,
 a_default_baseuri,
 a_do_not_canonicalize,
 a_do_not_check_namespaces,
 a_do_not_issue_errors,
 a_do_not_issue_warnings,
 a_do_not_preserve_comment,
 a_do_not_remove_whitespace,
 a_do_not_subst_dtd_entities,
 a_do_not_subst_html_entities,
 a_do_not_validate,
 a_error,
 a_error_log,
 a_help,
 a_if_modified_since,
 a_if_unmodified_since,
 a_ignore_encoding_errors,
 a_ignore_none_xml_contents,
 a_indent,
 a_issue_errors,
 a_issue_warnings,
 a_mime_types,
 a_no_empty_elements,
 a_no_empty_elem_for,
 a_no_redirect,
 a_no_xml_pi,
 a_output_file,
 a_output_xml,
 a_output_html,
 a_output_xhtml,
 a_output_plain,
 a_parse_by_mimetype,
 a_parse_html,
 a_parse_xml,
 a_preserve_comment,
 a_proxy,
 a_redirect,
 a_remove_whitespace,
 a_show_haskell,
 a_show_tree,
 a_strict_input,
 a_subst_dtd_entities,
 a_subst_html_entities,
 a_text_mode,
 a_trace,
 a_validate,
 a_verbose      :: String

a_accept_mimetypes              = "accept-mimetypes"
a_add_default_dtd               = "add-default-dtd"
a_canonicalize                  = "canonicalize"
a_check_namespaces              = "check-namespaces"
a_collect_errors                = "collect-errors"
a_default_baseuri               = "default-base-URI"
a_do_not_canonicalize           = "do-not-canonicalize"
a_do_not_check_namespaces       = "do-not-check-namespaces"
a_do_not_issue_errors           = "do-not-issue-errors"
a_do_not_issue_warnings         = "do-not-issue-warnings"
a_do_not_preserve_comment       = "do-not-preserve-comment"
a_do_not_remove_whitespace      = "do-not-remove-whitespace"
a_do_not_subst_dtd_entities     = "do-not-subst-dtd-entities"
a_do_not_subst_html_entities    = "do-not-subst-html-entities"
a_do_not_validate               = "do-not-validate"
a_error                         = "error"
a_error_log                     = "errorLog"
a_help                          = "help"
a_if_modified_since             = "if-modified-since"
a_if_unmodified_since           = "if-unmodified-since"
a_ignore_encoding_errors        = "ignore-encoding-errors"
a_ignore_none_xml_contents      = "ignore-none-xml-contents"
a_indent                        = "indent"
a_issue_warnings                = "issue-warnings"
a_issue_errors                  = "issue-errors"
a_mime_types                    = "mimetypes"
a_no_empty_elements             = "no-empty-elements"
a_no_empty_elem_for             = "no-empty-elem-for"
a_no_redirect                   = "no-redirect"
a_no_xml_pi                     = "no-xml-pi"
a_output_file                   = "output-file"
a_output_html                   = "output-html"
a_output_xhtml                  = "output-xhtml"
a_output_xml                    = "output-xml"
a_output_plain                  = "output-plain"
a_parse_by_mimetype             = "parse-by-mimetype"
a_parse_html                    = "parse-html"
a_parse_xml                     = "parse-xml"
a_preserve_comment              = "preserve-comment"
a_proxy                         = "proxy"
a_redirect                      = "redirect"
a_remove_whitespace             = "remove-whitespace"
a_show_haskell                  = "show-haskell"
a_show_tree                     = "show-tree"
a_strict_input                  = "strict-input"
a_subst_dtd_entities            = "subst-dtd-entities"
a_subst_html_entities           = "subst-html-entities"
a_text_mode                     = "text-mode"
a_trace                         = "trace"
a_validate                      = "validate"
a_verbose                       = "verbose"

-- ------------------------------------------------------------

-- |
-- select options from a predefined list of option desciptions

selectOptions   :: [String] -> [OptDescr a] -> [OptDescr a]
selectOptions ol os
    = concat . map (\ on -> filter (\ (Option _ ons _ _) -> on `elem` ons) os) $ ol

removeOptions   :: [String] -> [OptDescr a] -> [OptDescr a]
removeOptions ol os
    = filter (\ (Option _ ons _ _) -> not . any (`elem` ol) $ ons ) os

-- ------------------------------------------------------------
