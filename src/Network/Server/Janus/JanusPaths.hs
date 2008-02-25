module Network.Server.Janus.JanusPaths
where

import Text.ParserCombinators.Parsec

import Text.XML.HXT.Arrow
import Text.XML.HXT.XPath
import Text.XML.HXT.XPath.XPathDataTypes
    (
    Expr (..),
    LocationPath (..),
    Path (..),
    XStep (..),
    NodeTest (..),
    AxisSpec (..)
    )

-- simple variant of an XPath expression to address the Janus XML store
data JanusPath          = NoPath
			| ChildPath String String
			| AttrPath  String String

instance Show JanusPath where
    show  NoPath              = ""
    show (ChildPath loc path) = path ++ "/"  ++ loc
    show (AttrPath  loc path) = path ++ "/@" ++ loc

-- ------------------------------------------------------------
--
-- smart constructor for Janus paths

jp	:: String -> JanusPath
jp xpath
    = let
      path  = base xpath
      parts = case runParser parseXPath [] "" xpath
	      of
	      Right xpExpr -> local_xpath xpExpr
	      Left _       -> Nothing
      in
      case parts of
      Just (axis, loc) -> case axis of
			  Attribute -> AttrPath  loc path
			  Child     -> ChildPath loc path
			  _         -> NoPath
      Nothing          -> NoPath
    where
    local_xpath (PathExpr _ (Just (LocPath Abs path)))  = select_parts (reverse $ path)
    local_xpath _                                       = Nothing

    select_parts ((Step axis (NameTest name) []):_)     = Just (axis, qualifiedName name)
    select_parts _                                      = Just (Child, ".")

    base xpath'
        = let xpath'' = removeLastSlash xpath' in
          case xpath'' of
	  []   -> "/"
          ('/':[]) -> "/"
          (_:xs)   -> reverse xs

    removeLastSlash xpath'  = dropWhile (\c -> c /= '/') (reverse xpath')


jpAttr		:: String -> String -> JanusPath
jpAttr xpath an	= jp $ xpath ++ "/@" ++ an

-- ------------------------------------------------------------

_config_haskell			:: JanusPath
_config_haskell			= jp "/config/@haskell"

_config_id			:: JanusPath
_config_id			= jp "/config/@id"

_config_port			:: JanusPath
_config_port			= jp "/config/port"

_config_state			:: JanusPath
_config_state			= jp "/config/@state"

_config_type			:: JanusPath
_config_type			= jp "/config/@type"

_janus				:: JanusPath
_janus				= jp "/janus"

_janus_shader			:: JanusPath
_janus_shader			= jp "/janus/shader"

_janus__shader			:: JanusPath
_janus__shader			= jp "/janus//shader"

_janus__shader_config_id	:: JanusPath
_janus__shader_config_id	= jp "/janus//shader/config/@id"

_message_code			:: JanusPath
_message_code			= jp "/message/@code"

_message_level			:: JanusPath
_message_level			= jp "/message/@level"

_message_source			:: JanusPath
_message_source			= jp "/message/@source"

_message_timestamp		:: JanusPath
_message_timestamp		= jp "/message/@timestamp"

_message_state			:: JanusPath
_message_state			= jp $ "/message/state"

_message_state_			:: String -> JanusPath
_message_state_ key		= jp $ "/message/state/@" ++ key

_message_type			:: JanusPath
_message_type			= jp "/message/@type"

_message_value			:: JanusPath
_message_value			= jp "/message/@value"

_shader_config			:: JanusPath
_shader_config			= jp "/shader/config"

_shader_config_			:: String -> JanusPath
_shader_config_ key		= jp $ "/shader/config/" ++ key

_shader_config_accepts		:: JanusPath
_shader_config_accepts		= jp "/shader/config/@accepts"

_shader_config_alias		:: JanusPath
_shader_config_alias		= jp "/shader/config/@alias"

_shader_config_baseUrl		:: JanusPath
_shader_config_baseUrl		= jp "/shader/config/@base_url"

_shader_config_channel		:: JanusPath
_shader_config_channel		= jp "/shader/config/@channel"

_shader_config_default		:: JanusPath
_shader_config_default		= jp "/shader/config/@default"

_shader_config_deffile		:: JanusPath
_shader_config_deffile		= jp "/shader/config/@default_file"

_shader_config_delim		:: JanusPath
_shader_config_delim		= jp "/shader/config/@delim"

_shader_config_file		:: JanusPath
_shader_config_file		= jp "/shader/config/@file"

_shader_config_fromState	:: JanusPath
_shader_config_fromState	= jp "/shader/config/@from_state"

_shader_config_handler_config	:: JanusPath
_shader_config_handler_config	= jp "/shader/config/handler/config"

_shader_config_haskell		:: JanusPath
_shader_config_haskell		= jp "/shader/config/haskell"

_shader_config_id		:: JanusPath
_shader_config_id		= jp "/shader/config/@id"

_shader_config_loadsTo		:: JanusPath
_shader_config_loadsTo		= jp "/shader/config/@loads_to"

_shader_config_mapsTo		:: JanusPath
_shader_config_mapsTo		= jp "/shader/config/@maps_to"

_shader_config_match		:: JanusPath
_shader_config_match		= jp "/shader/config/@match"

_shader_config_module		:: JanusPath
_shader_config_module		= jp "/shader/config/@module"

_shader_config_move		:: JanusPath
_shader_config_move		= jp "/shader/config/@move"

_shader_config_node		:: JanusPath
_shader_config_node		= jp "/shader/config/@node"

_shader_config_object		:: JanusPath
_shader_config_object		= jp "/shader/config/@object"

_shader_config_op		:: JanusPath
_shader_config_op		= jp "/shader/config/@op"

_shader_config_path		:: JanusPath
_shader_config_path		= jp "/shader/config/@path"

_shader_config_pricenode	:: JanusPath
_shader_config_pricenode	= jp "/shader/config/@pricenode"

_shader_config_productnode	:: JanusPath
_shader_config_productnode	= jp "/shader/config/@productnode"

_shader_config_reference	:: JanusPath
_shader_config_reference	= jp "/shader/config/@reference"

_shader_config_rootState	:: JanusPath
_shader_config_rootState	= jp "/shader/config/@root_state"

_shader_config_scope		:: JanusPath
_shader_config_scope		= jp "/shader/config/@scope"

_shader_config_state		:: JanusPath
_shader_config_state		= jp "/shader/config/@state"

_shader_config_statemap		:: JanusPath
_shader_config_statemap		= jp "/shader/config/statemap"

_shader_config_tamap		:: JanusPath
_shader_config_tamap		= jp "/shader/config/tamap"

_shader_config_to		:: JanusPath
_shader_config_to		= jp "/shader/config/@to"

_shader_config_toState		:: JanusPath
_shader_config_toState		= jp "/shader/config/@to_state"

_shader_config_type		:: JanusPath
_shader_config_type		= jp "/shader/config/@type"

_shader_config_types_		:: String -> JanusPath
_shader_config_types_ n		= jp $ "/shader/config/types/@" ++ n

_shader_config_typefile		:: JanusPath
_shader_config_typefile		= jp "/shader/config/@typefile"

_shader_config_userdb		:: JanusPath
_shader_config_userdb		= jp "/shader/config/@userdb"

_shader_config_value		:: JanusPath
_shader_config_value		= jp "/shader/config/@value"

_shader_init_shader		:: JanusPath
_shader_init_shader		= jp "/shader/init/shader"

_shader_shader			:: JanusPath
_shader_shader			= jp "/shader/shader"

_statemap_from			:: JanusPath
_statemap_from			= jp "/statemap/@from"

_statemap_to			:: JanusPath
_statemap_to			= jp "/statemap/@to"

_tamap_from			:: JanusPath
_tamap_from			= jp "/tamap/@from"

_tamap_to			:: JanusPath
_tamap_to			= jp "/tamap/@to"

_transaction			:: JanusPath
_transaction			= jp "/transaction"

_transaction_console		:: JanusPath
_transaction_console		= jp "/transaction/console"

_transaction_console_argcount	:: JanusPath
_transaction_console_argcount	= jp "/transaction/console/@argcount"

_transaction_console_args_1	:: JanusPath
_transaction_console_args_1	= jp "/transaction/console/args/_1"

_transaction_console_args_	:: String -> JanusPath
_transaction_console_args_ n	= jp $ "/transaction/console/args/" ++ n

_transaction_console_args_2	:: JanusPath
_transaction_console_args_2	= jp "/transaction/console/args/_2"

_transaction_console_args_3	:: JanusPath
_transaction_console_args_3	= jp "/transaction/console/args/_3"

_transaction_console_command	:: JanusPath
_transaction_console_command	= jp "/transaction/console/@command"

_transaction_end		:: JanusPath
_transaction_end		= jp "/transaction/@end"

_transaction_handler			:: JanusPath
_transaction_handler			= jp "/transaction/handler"

_transaction_http_request_body		:: JanusPath
_transaction_http_request_body		= jp "/transaction/http/request/body"

_transaction_http_request_cgi		:: JanusPath
_transaction_http_request_cgi		= jp "/transaction/http/request/cgi"

_transaction_http_request_cgi_		:: String -> JanusPath
_transaction_http_request_cgi_	n	= jp $ "/transaction/http/request/cgi/" ++ n

_transaction_http_request_cgi_node	:: JanusPath
_transaction_http_request_cgi_node	= jp "/transaction/http/request/cgi/@node"

_transaction_http_request_cgi_sessionid	:: JanusPath
_transaction_http_request_cgi_sessionid	= jp "/transaction/http/request/cgi/@sessionid"

_transaction_http_request_header_	:: String -> JanusPath
_transaction_http_request_header_ n	= jp $ "/transaction/http/request/header/" ++ n

_transaction_http_request_method	:: JanusPath
_transaction_http_request_method	= jp "/transaction/http/request/@method"

_transaction_http_request_uriFrag	:: JanusPath
_transaction_http_request_uriFrag	= jp "/transaction/http/request/@uri_frag"

_transaction_http_request_uriPath	:: JanusPath
_transaction_http_request_uriPath	= jp "/transaction/http/request/@uri_path"

_transaction_http_request_uriQuery	:: JanusPath
_transaction_http_request_uriQuery	= jp "/transaction/http/request/@uri_query"

_transaction_http_request_uriScheme	:: JanusPath
_transaction_http_request_uriScheme	= jp "/transaction/http/request/@uri_scheme"

_transaction_http_request_url		:: JanusPath
_transaction_http_request_url		= jp "/transaction/http/request/@url"

_transaction_http_response_body		:: JanusPath
_transaction_http_response_body		= jp "/transaction/http/response/body"

_transaction_http_response_body_filesize	:: JanusPath
_transaction_http_response_body_filesize	= jp "/transaction/http/response/body/@filesize"

_transaction_http_response_body_hdlop	:: JanusPath
_transaction_http_response_body_hdlop	= jp "/transaction/http/response/body/@hdlop"

_transaction_http_response_header_	:: String -> JanusPath
_transaction_http_response_header_ n	= jp $ "/transaction/http/response/header/" ++ n

_transaction_http_response_mime		:: JanusPath
_transaction_http_response_mime		= jp "/transaction/http/response/@mime"

_transaction_http_response_status	:: JanusPath
_transaction_http_response_status	= jp "/transaction/http/response/@status"

_transaction_messages			:: JanusPath
_transaction_messages                   = jp "/transaction/messages"

_transaction_messages_			:: String -> JanusPath
_transaction_messages_ key              = jp $ "/transaction/messages/" ++ key

_transaction_requestFragment		:: JanusPath
_transaction_requestFragment		= jp "/transaction/request_fragment"

_transaction_responseFragment		:: JanusPath
_transaction_responseFragment		= jp "/transaction/response_fragment"

_transaction_session			:: JanusPath
_transaction_session			= jp "/transaction/session"

_transaction_session_sessionid		:: JanusPath
_transaction_session_sessionid		= jp "/transaction/session/@sessionid"

_transaction_session_state		:: JanusPath
_transaction_session_state		= jp "/transaction/session/state"

_transaction_session_state_authfailed	:: JanusPath
_transaction_session_state_authfailed	= jp "/transaction/session/state/@authfailed"

_transaction_session_state_authuser	:: JanusPath
_transaction_session_state_authuser	= jp "/transaction/session/state/@authuser"

_transaction_session_state_count	:: JanusPath
_transaction_session_state_count	= jp "/transaction/session/state/@count"

_transaction_session_username		:: JanusPath
_transaction_session_username		= jp "/transaction/session/@username"

_transaction_session_userpass		:: JanusPath
_transaction_session_userpass		= jp "/transaction/session/@userpass"

_transaction_start			:: JanusPath
_transaction_start			= jp "/transaction/@start"

_transaction_tcp_remoteHost		:: JanusPath
_transaction_tcp_remoteHost		= jp "/transaction/tcp/@remote_host"

_transaction_tcp_remoteIp		:: JanusPath
_transaction_tcp_remoteIp		= jp "/transaction/tcp/@remote_ip"

_transaction_tcp_remotePort		:: JanusPath
_transaction_tcp_remotePort		= jp "/transaction/tcp/@remote_port"

_transaction_transactionId		:: JanusPath
_transaction_transactionId		= jp "/transaction/@transaction_id"

_transaction_transactionState		:: JanusPath
_transaction_transactionState		= jp "/transaction/@transaction_state"

_value					:: JanusPath
_value					= jp "/value"
