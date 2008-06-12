-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DOM.XmlKeywords
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Constants for XML keywords, for special attribute names
   and special attribute values

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DOM.XmlKeywords
where

-- ------------------------------------------------------------
--
-- string constants for representing DTD keywords and attributes

t_xml,				-- tag names
 t_root		:: String

a_canonicalize,
 a_default,			-- attribute names
 a_check_namespaces,
 a_contentLength,
 a_collect_errors,
 a_column,
 a_default_baseuri,
 a_do_not_canonicalize,
 a_do_not_check_namespaces,
 a_do_not_issue_errors,
 a_do_not_issue_warnings,
 a_do_not_preserve_comment,
 a_do_not_remove_whitespace,
 a_do_not_use_curl,
 a_do_not_validate,
 a_encoding,
 a_error,
 a_error_log,
 a_help,
 a_ignore_encoding_errors,
 a_indent,
 a_issue_errors,
 a_issue_warnings,
 a_kind,
 a_line,
 a_mime_types,
 a_module,
 a_modifier,
 a_name,
 a_no_xml_pi,
 a_options_curl,
 a_output_encoding,
 a_output_file,
 a_output_xml,
 a_output_html,
 a_parse_by_mimetype,
 a_parse_html,
 a_parse_xml,
 a_peref,
 a_preserve_comment,
 a_propagate_errors,
 a_proxy,
 a_remove_whitespace,
 a_show_haskell,
 a_show_tree,
 a_source,
 a_status,
 a_standalone,
 a_tagsoup,
 a_trace,
 a_type,
 a_use_curl,
 a_url,
 a_validate,
 a_value,
 a_verbose,
 a_version,
 a_xml,
 a_xmlns	:: String

v_0,				-- attribute values
 v_1,
 v_yes,
 v_no,
 v_any,
 v_children,
 v_choice,
 v_empty,
 v_mixed,
 v_seq,
 v_null,
 v_option,
 v_pcdata,
 v_star,
 v_plus		:: String

k_any,				-- DTD keywords
 k_cdata,
 k_empty,
 k_entity,
 k_entities,
 k_id,
 k_idref,
 k_idrefs,
 k_include,
 k_ignore,
 k_nmtoken,
 k_nmtokens,
 k_peref,
 k_public,
 k_system,
 k_enumeration,
 k_fixed,
 k_implied,
 k_ndata,
 k_notation,
 k_pcdata,
 k_required,
 k_default	:: String

-- ------------------------------------------------------------

t_xml		= "xml"
t_root		= "/"		-- name of root node tag

a_canonicalize			= "canonicalize"
a_check_namespaces		= "check-namespaces"
a_collect_errors		= "collect-errors"
a_column			= "column"
a_contentLength			= "Content-Length"
a_default			= "default"
a_default_baseuri		= "default-base-URI"
a_do_not_canonicalize		= "do-not-canonicalize"
a_do_not_check_namespaces	= "do-not-check-namespaces"
a_do_not_issue_errors		= "do-not-issue-errors"
a_do_not_issue_warnings		= "do-not-issue-warnings"
a_do_not_preserve_comment	= "do-not-preserve-comment"
a_do_not_remove_whitespace	= "do-not-remove-whitespace"
a_do_not_use_curl		= "do-not-use-curl"
a_do_not_validate		= "do-not-validate"
a_encoding			= "encoding"
a_error				= "error"
a_error_log			= "errorLog"
a_help				= "help"
a_ignore_encoding_errors	= "ignore-encoding-errors"
a_indent			= "indent"
a_issue_warnings		= "issue-warnings"
a_issue_errors			= "issue-errors"
a_kind				= "kind"
a_line				= "line"
a_mime_types			= "mimetypes"
a_module			= "module"
a_modifier			= "modifier"
a_name				= "name"
a_no_xml_pi                     = "no-xml-pi"
a_options_curl			= "options-curl"
a_output_file			= "output-file"
a_output_encoding		= "output-encoding"
a_output_html			= "output-html"
a_output_xml			= "output-xml"
a_parse_by_mimetype		= "parse-by-mimetype"
a_parse_html			= "parse-html"
a_parse_xml			= "parse-xml"
a_peref				= k_peref
a_preserve_comment 		= "preserve-comment"
a_propagate_errors		= "propagate-errors"
a_proxy				= "proxy"
a_remove_whitespace 		= "remove-whitespace"
a_show_haskell			= "show-haskell"
a_show_tree			= "show-tree"
a_source			= "source"
a_standalone			= "standalone"
a_status			= "status"
a_tagsoup			= "tagsoup"
a_trace				= "trace"
a_type				= "type"
a_url				= "url"
a_use_curl			= "use-curl"
a_validate			= "validate"
a_value				= "value"
a_verbose			= "verbose"
a_version			= "version"
a_xml				= "xml"
a_xmlns				= "xmlns"

v_yes		= "yes"
v_no		= "no"
v_1		= "1"
v_0		= "0"

v_any		= k_any
v_children	= "children"
v_choice	= "choice"
v_empty		= k_empty
v_pcdata	= k_pcdata
v_mixed		= "mixed"
v_seq		= "seq"

v_null		= ""
v_option	= "?"
v_star		= "*"
v_plus		= "+"

k_any		= "ANY"
k_cdata		= "CDATA"
k_empty		= "EMPTY"
k_entity	= "ENTITY"
k_entities	= "ENTITIES"
k_id		= "ID"
k_idref		= "IDREF"
k_idrefs	= "IDREFS"
k_include	= "INCLUDE"
k_ignore	= "IGNORE"
k_nmtoken	= "NMTOKEN"
k_nmtokens	= "NMTOKENS"
k_peref		= "PERef"
k_public	= "PUBLIC"
k_system	= "SYSTEM"

k_enumeration	= "#ENUMERATION"
k_fixed		= "#FIXED"
k_implied	= "#IMPLIED"
k_ndata		= "NDATA"
k_notation	= "NOTATION"
k_pcdata	= "#PCDATA"
k_required	= "#REQUIRED"
k_default	= "#DEFAULT"


-- ------------------------------------------------------------
--

-- attribute names for transfer protocol attributes
-- used in XmlInput for describing header information
-- of http and other requests

transferPrefix
 , transferProtocol
 , transferMimeType
 , transferEncoding
 , transferURI
 , transferDefaultURI
 , transferStatus
 , transferMessage
 , transferVersion :: String

transferPrefix		= "transfer-"

transferProtocol	= transferPrefix ++ "Protocol"
transferVersion		= transferPrefix ++ "Version"
transferMimeType	= transferPrefix ++ "MimeType"
transferEncoding	= transferPrefix ++ "Encoding"
transferDefaultURI	= transferPrefix ++ "DefaultURI"
transferStatus		= transferPrefix ++ "Status"
transferMessage		= transferPrefix ++ "Message"
transferURI		= transferPrefix ++ "URI"

-- ------------------------------------------------------------
--

httpPrefix	:: String
httpPrefix	= "http-"

stringProtocol	:: String
stringProtocol	= "string:"

-- ------------------------------------------------------------
--
-- encoding names

isoLatin1
  , iso8859_1, iso8859_2, iso8859_3, iso8859_4, iso8859_5
  , iso8859_6, iso8859_7, iso8859_8, iso8859_9, iso8859_10
  , iso8859_11, iso8859_13, iso8859_14, iso8859_15, iso8859_16
  , usAscii, ucs2, utf8, utf16, utf16be, utf16le, unicodeString	:: String

isoLatin1	= iso8859_1
iso8859_1	= "ISO-8859-1"
iso8859_2	= "ISO-8859-2"
iso8859_3	= "ISO-8859-3"
iso8859_4	= "ISO-8859-4"
iso8859_5	= "ISO-8859-5"
iso8859_6	= "ISO-8859-6"
iso8859_7	= "ISO-8859-7"
iso8859_8	= "ISO-8859-8"
iso8859_9	= "ISO-8859-9"
iso8859_10	= "ISO-8859-10"
iso8859_11	= "ISO-8859-11"
iso8859_13	= "ISO-8859-13"
iso8859_14	= "ISO-8859-14"
iso8859_15	= "ISO-8859-15"
iso8859_16	= "ISO-8859-16"
usAscii		= "US-ASCII"
ucs2		= "ISO-10646-UCS-2"
utf8		= "UTF-8"
utf16		= "UTF-16"
utf16be		= "UTF-16BE"
utf16le		= "UTF-16LE"
unicodeString	= "UNICODE"

-- ------------------------------------------------------------
--
-- known namespaces

-- |
-- the predefined namespace uri for xml: \"http:\/\/www.w3.org\/XML\/1998\/namespace\"

xmlNamespace	:: String
xmlNamespace	= "http://www.w3.org/XML/1998/namespace"

-- |
-- the predefined namespace uri for xmlns: \"http:\/\/www.w3.org\/2000\/xmlns\/\"

xmlnsNamespace	:: String
xmlnsNamespace	= "http://www.w3.org/2000/xmlns/"

-- | Relax NG namespace
relaxNamespace	:: String
relaxNamespace	= "http://relaxng.org/ns/structure/1.0"

-- ------------------------------------------------------------
-- option for Relax NG

a_relax_schema,
 a_do_not_check_restrictions,  
 a_check_restrictions,
 a_do_not_validate_externalRef,
 a_validate_externalRef,
 a_do_not_validate_include,
 a_validate_include,
 a_output_changes, 
 a_do_not_collect_errors :: String 

a_relax_schema		      = "relax-schema"
a_do_not_check_restrictions   = "do-not-check-restrictions"
a_check_restrictions          = "check-restrictions"
a_do_not_validate_externalRef = "do-not-validate-externalRef"
a_validate_externalRef        = "validate-externalRef" 
a_do_not_validate_include     = "do-not-validate-include"
a_validate_include            = "validate-include"
a_output_changes              = "output-pattern-transformations"
a_do_not_collect_errors       = "do-not-collect-errors"

-- ------------------------------------------------------------
