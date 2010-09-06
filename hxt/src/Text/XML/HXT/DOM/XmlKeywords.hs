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

t_xml,                          -- tag names
 t_root         :: String

a_default,                     -- attribute names
 a_contentLength,
 a_column,
 a_encoding,
 a_kind,
 a_line,
 a_module,
 a_modifier,
 a_name,
 a_output_encoding,
 a_peref,
 a_source,
 a_status,
 a_standalone,
 a_type,
 a_url,
 a_value,
 a_version,
 a_xml,
 a_xmlns        :: String

v_0,                            -- attribute values
 v_1,
 v_2,
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
 v_plus         :: String

k_any,                          -- DTD keywords
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
 k_default      :: String

-- ------------------------------------------------------------

t_xml           = "xml"
t_root          = "/"           -- name of root node tag

a_column                        = "column"
a_contentLength                 = "Content-Length"
a_default                       = "default"
a_encoding                      = "encoding"
a_kind                          = "kind"
a_line                          = "line"
a_module                        = "module"
a_modifier                      = "modifier"
a_name                          = "name"
a_output_encoding               = "output-encoding"
a_peref                         = k_peref
a_source                        = "source"
a_standalone                    = "standalone"
a_status                        = "status"
a_type                          = "type"
a_url                           = "url"
a_value                         = "value"
a_version                       = "version"
a_xml                           = "xml"
a_xmlns                         = "xmlns"

v_yes           = "yes"
v_no            = "no"
v_0             = "0"
v_1             = "1"
v_2             = "2"

v_any           = k_any
v_children      = "children"
v_choice        = "choice"
v_empty         = k_empty
v_pcdata        = k_pcdata
v_mixed         = "mixed"
v_seq           = "seq"

v_null          = ""
v_option        = "?"
v_star          = "*"
v_plus          = "+"

k_any           = "ANY"
k_cdata         = "CDATA"
k_empty         = "EMPTY"
k_entity        = "ENTITY"
k_entities      = "ENTITIES"
k_id            = "ID"
k_idref         = "IDREF"
k_idrefs        = "IDREFS"
k_include       = "INCLUDE"
k_ignore        = "IGNORE"
k_nmtoken       = "NMTOKEN"
k_nmtokens      = "NMTOKENS"
k_peref         = "PERef"
k_public        = "PUBLIC"
k_system        = "SYSTEM"

k_enumeration   = "#ENUMERATION"
k_fixed         = "#FIXED"
k_implied       = "#IMPLIED"
k_ndata         = "NDATA"
k_notation      = "NOTATION"
k_pcdata        = "#PCDATA"
k_required      = "#REQUIRED"
k_default       = "#DEFAULT"


dtdPrefix       :: String
dtdPrefix       = "doctype-"

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

transferPrefix          = "transfer-"

transferProtocol        = transferPrefix ++ "Protocol"
transferVersion         = transferPrefix ++ "Version"
transferMimeType        = transferPrefix ++ "MimeType"
transferEncoding        = transferPrefix ++ "Encoding"
transferDefaultURI      = transferPrefix ++ "DefaultURI"
transferStatus          = transferPrefix ++ "Status"
transferMessage         = transferPrefix ++ "Message"
transferURI             = transferPrefix ++ "URI"

-- ------------------------------------------------------------
--

httpPrefix      :: String
httpPrefix      = "http-"

stringProtocol  :: String
stringProtocol  = "string:"

-- ------------------------------------------------------------
--
-- known namespaces

-- |
-- the predefined namespace uri for xml: \"http:\/\/www.w3.org\/XML\/1998\/namespace\"

xmlNamespace    :: String
xmlNamespace    = "http://www.w3.org/XML/1998/namespace"

-- |
-- the predefined namespace uri for xmlns: \"http:\/\/www.w3.org\/2000\/xmlns\/\"

xmlnsNamespace  :: String
xmlnsNamespace  = "http://www.w3.org/2000/xmlns/"

-- | Relax NG namespace
relaxNamespace  :: String
relaxNamespace  = "http://relaxng.org/ns/structure/1.0"

-- ------------------------------------------------------------
