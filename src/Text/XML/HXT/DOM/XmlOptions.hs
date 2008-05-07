-- |
-- Common useful options
--
-- Version : $Id: XmlOptions.hs,v 1.1 2006/11/09 20:27:42 hxml Exp $
--
--

module Text.XML.HXT.DOM.XmlOptions
    ( inputOptions
    , relaxOptions
    , outputOptions
    , generalOptions
    , versionOptions
    , showOptions

    , selectOptions
    , removeOptions
    , optionIsSet
    , isTrueValue
    )
where

import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.TypeDefs

import Data.Maybe

import System.Console.GetOpt

-- ------------------------------------------------------------
--

-- |
-- commonly useful options for XML input
--
-- can be used for option definition with haskell getopt
--
-- defines options: 'a_trace', 'a_proxy', 'a_use_curl', 'a_do_not_use_curl', 'a_options_curl', 'a_encoding',
-- 'a_issue_errors', 'a_do_not_issue_errors', 'a_parse_html', 'a_parse_by_mimetype', 'a_tagsoup' 'a_issue_warnings', 'a_do_not_issue_warnings',
-- 'a_parse_xml', 'a_validate', 'a_do_not_validate', 'a_canonicalize', 'a_do_not_canonicalize',
--- 'a_preserve_comment', 'a_do_not_preserve_comment', 'a_check_namespaces', 'a_do_not_check_namespaces',
-- 'a_remove_whitespace', 'a_do_not_remove_whitespace'

inputOptions 	:: [OptDescr (String, String)]
inputOptions
    = [ Option "t"	[a_trace]			(OptArg trc "LEVEL")			"trace level (0-4), default 1"
      , Option "p"	[a_proxy]			(ReqArg (att a_proxy)         "PROXY")	"proxy for http access (e.g. \"www-cache:3128\")"
      , Option ""	[a_use_curl]			(NoArg  (att a_use_curl          v_1))	"HTTP access via external program \"curl\", more functionality, supports HTTP/1.0, less efficient"
      , Option ""	[a_do_not_use_curl]		(NoArg  (att a_use_curl          v_0))	"HTTP access via built in HTTP/1.1 module (default)"
      , Option ""	[a_options_curl]		(ReqArg (att a_options_curl)    "STR")	"additional curl options, e.g. for timeout, ..."
      , Option ""	[a_default_baseuri]		(ReqArg (att transferURI) 	"URI")	"default base URI, default: \"file:///<cwd>/\""
      , Option "e"	[a_encoding]			(ReqArg (att a_encoding)    "CHARSET")	( "default document encoding (" ++ utf8 ++ ", " ++ isoLatin1 ++ ", " ++ usAscii ++ ", ...)" )
      , Option ""	[a_issue_errors]		(NoArg	(att a_issue_errors      v_1))	"issue all errorr messages on stderr (default)"
      , Option ""	[a_do_not_issue_errors]		(NoArg	(att a_issue_errors      v_0))	"ignore all error messages"
      , Option ""	[a_ignore_encoding_errors]	(NoArg  (att a_ignore_encoding_errors v_1))  "ignore encoding errors"
      , Option "H"	[a_parse_html]			(NoArg	(att a_parse_html        v_1))	"parse input as HTML, try to interprete everything as HTML, no validation"
      , Option "M"	[a_parse_by_mimetype]		(NoArg  (att a_parse_by_mimetype v_1))	"parse dependent on mime type: text/html as HTML, text/xml and text/xhtml as XML, else no parse"
      , Option "T"	[a_tagsoup]			(NoArg	(att a_tagsoup           v_1))	"lasy tagsoup parser, for HTML and XML, no DTD, no validation, no PIs, only XHTML entityrefs"
      , Option ""	[a_issue_warnings]		(NoArg	(att a_issue_warnings    v_1))	"issue warnings, when parsing HTML (default)"
      , Option "Q"	[a_do_not_issue_warnings]	(NoArg	(att a_issue_warnings    v_0))	"ignore warnings, when parsing HTML"
      , Option ""	[a_parse_xml]			(NoArg	(att a_parse_html        v_0))	"parse input as XML (default)"
      , Option ""	[a_validate]			(NoArg  (att a_validate          v_1))	"document validation when parsing XML (default)"
      , Option "w"	[a_do_not_validate]		(NoArg  (att a_validate          v_0))	"only wellformed check, no validation"
      , Option ""	[a_canonicalize]		(NoArg  (att a_canonicalize      v_1))	"canonicalize document, remove DTD, comment, transform CDATA, CharRef's, ... (default)"
      , Option "c"	[a_do_not_canonicalize]		(NoArg  (att a_canonicalize      v_0))	"do not canonicalize document, don't remove DTD, comment, don't transform CDATA, CharRef's, ..."
      , Option "C"	[a_preserve_comment]		(NoArg	(att a_preserve_comment  v_1))	"don't remove comments during canonicalisation"
      , Option ""	[a_do_not_preserve_comment]	(NoArg	(att a_preserve_comment  v_0))	"remove comments during canonicalisation (default)"
      , Option "n"	[a_check_namespaces]		(NoArg  (att a_check_namespaces  v_1))	"tag tree with namespace information and check namespaces"
      , Option ""	[a_do_not_check_namespaces]	(NoArg  (att a_check_namespaces  v_0))	"ignore namespaces (default)"
      , Option "r"	[a_remove_whitespace]		(NoArg  (att a_remove_whitespace v_1))	"remove redundant whitespace, simplifies tree and processing"
      , Option ""	[a_do_not_remove_whitespace]	(NoArg  (att a_remove_whitespace v_0))	"don't remove redundant whitespace (default)"
      ]
    where
    att n v	= (n, v)
    trc = att a_trace . show . max 0 . min 9 . (read :: String -> Int) . ('0':) . filter (`elem` "0123456789") . fromMaybe v_1


-- | available Relax NG validation options
--
-- defines options
-- 'a_check_restrictions', 'a_validate_externalRef', 'a_validate_include', 'a_do_not_check_restrictions',  
-- 'a_do_not_validate_externalRef', 'a_do_not_validate_include'

relaxOptions :: [OptDescr (String, String)]
relaxOptions    
    = [ Option "X" [a_relax_schema]	        	(ReqArg (att a_relax_schema) "SCHEMA")	"validation with Relax NG, SCHEMA is the URI for the Relax NG schema"
      , Option ""  [a_check_restrictions]	        (NoArg (a_check_restrictions,    v_1))	"check Relax NG schema restrictions during schema simplification (default)"
      , Option ""  [a_do_not_check_restrictions]	(NoArg (a_check_restrictions,    v_0))	"do not check Relax NG schema restrictions"
      , Option ""  [a_validate_externalRef]		(NoArg (a_validate_externalRef,  v_1))	"validate a Relax NG schema referenced by a externalRef-Pattern (default)"
      , Option ""  [a_do_not_validate_externalRef]	(NoArg (a_validate_externalRef,  v_0))	"do not validate a Relax NG schema referenced by an externalRef-Pattern"
      , Option ""  [a_validate_include]			(NoArg (a_validate_include,      v_1))	"validate a Relax NG schema referenced by an include-Pattern (default)"
      , Option ""  [a_do_not_validate_include]		(NoArg (a_validate_include,      v_0))   "do not validate a Relax NG schema referenced by an include-Pattern"
	{-
      , Option ""  [a_output_changes]			(NoArg (a_output_changes,        v_1))	"output Pattern transformations in case of an error"    
      , Option ""  [a_do_not_collect_errors]     	(NoArg (a_do_not_collect_errors, v_1))	"stop Relax NG simplification after the first error has occurred"
	-}
      ]
    where
    att n v	= (n, v)

-- |
-- commonly useful options for XML output
--
-- defines options: 'a_indent', 'a_output_encoding', 'a_output_file', 'a_output_html'

outputOptions 	:: [OptDescr (String, String)]
outputOptions
    = [ Option "i"	[a_indent]		(NoArg  (att a_indent                v_1))	"indent XML output for readability"
      , Option "o"	[a_output_encoding]	(ReqArg (att a_output_encoding) "CHARSET")	( "encoding of output (" ++ utf8 ++ ", " ++ isoLatin1 ++ ", " ++ usAscii ++ ")" )
      , Option "f"	[a_output_file]		(ReqArg (att a_output_file)        "FILE")	"output file for resulting document (default: stdout)"
      , Option ""	[a_output_html]	        (NoArg  (att a_output_html           v_1))	"output of none ASCII chars as HTMl entity references"
      , Option ""       [a_no_xml_pi]	        (NoArg  (att a_no_xml_pi             v_1))      ("output without <?xml ...?> processing instruction, useful in combination with --" ++ show a_output_html)
      ]
    where
    att n v	= (n, v)

-- |
-- commonly useful options
--
-- defines options: 'a_verbose', 'a_help'

generalOptions 	:: [OptDescr (String, String)]
generalOptions
    = [ Option "v"	[a_verbose]		(NoArg  (a_verbose, v_1))		"verbose output"
      , Option "h?"	[a_help]		(NoArg  (a_help,    v_1))		"this message"
      ]

-- |
-- defines 'a_version' option

versionOptions 	:: [OptDescr (String, String)]
versionOptions
    = [ Option "V"	[a_version]		(NoArg  (a_version, v_1))		"show program version"
      ]

-- |
-- debug output options

showOptions	:: [OptDescr (String, String)]
showOptions
    = [ Option ""	[a_show_tree]		(NoArg  (a_show_tree,    v_1))		"output tree representation instead of document source"
      , Option ""	[a_show_haskell]	(NoArg  (a_show_haskell, v_1))		"output internal Haskell representation instead of document source"
      ]

-- ------------------------------------------------------------

-- |
-- select options from a predefined list of option desciptions

selectOptions	:: [String] -> [OptDescr (String, String)] -> [OptDescr (String, String)]
selectOptions ol os
    = concat . map (\ on -> filter (\ (Option _ ons _ _) -> on `elem` ons) os) $ ol

removeOptions	:: [String] -> [OptDescr (String, String)] -> [OptDescr (String, String)]
removeOptions ol os
    = filter (\ (Option _ ons _ _) -> not . any (`elem` ol) $ ons ) os

-- |
-- check whether an option is set
--
-- reads the value of an attribute, usually applied to a document root node,
-- and checks if the value represents True. The following strings are interpreted
-- as true: \"1\", \"True\", \"true\", \"yes\", \"Yes\".

optionIsSet	:: String -> Attributes -> Bool
optionIsSet n	= isTrueValue . lookupDef "" n

-- | check whether a string represents True
--
-- definition:
--
-- > isTrueValue	= (`elem` ["1", "True", "true", "Yes", "yes"])

isTrueValue	:: String -> Bool
isTrueValue	= (`elem` ["1", "True", "true", "Yes", "yes"])

-- ------------------------------------------------------------
