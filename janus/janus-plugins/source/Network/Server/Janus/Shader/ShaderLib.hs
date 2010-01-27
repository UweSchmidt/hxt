-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.ShaderLib
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ShaderLib.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus Basic Shader Library

   Provides a set of universal, mostly transport-independent Shaders.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.ShaderLib
    (
    -- shaders
      localEchoShader
    , remoteEchoShader
    , sessionReadShader
    , sessionWriteShader
    , authShader
    , dataLoadShader
    , dataStoreShader
    , loadXmlShader
    , storeXmlShader
    , contextShader
    , traceShader
    , traceTAShader
    , registerChannel
    , registerScope
    , mappingShader
    , mappingWrapper
    , aliasShader
    , setValShader
    , setStateShader
    )
where

import Data.List
import Data.Map as Map
import Network.URI (unEscapeString)
import Text.Regex
import Text.XML.HXT.Arrow

import Network.Server.Janus.Core as Shader
import Network.Server.Janus.Messaging
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths


{- |
Shows \/transaction\/request_fragment as plain text at command line (no effect on transaction).
Shows the empty string if the value cannot be found.
-}
localEchoShader :: ShaderCreator
localEchoShader =
    mkStaticCreator $
    proc in_ta -> do
        request     <- getValDef _transaction_requestFragment ""                    -<  in_ta
        let request_list    = (removeEmpty . splitRegex (mkRegex "\r")) request
        let output          = foldr (\str current -> str ++ "\n" ++ current) "" request_list
        globalMsg output                                                            -<< ()
        returnA                                                                     -<  in_ta

{- |
Copies \/transaction\/request_fragment into \/transaction\/response_fragment and shows \/transaction\/request_fragment as plain text at
command line. Shows the empty string if the value cannot be found.
-}
remoteEchoShader :: ShaderCreator
remoteEchoShader =
    mkStaticCreator $
    proc in_ta -> do
        request <- getValDef _transaction_requestFragment ""                        -<  in_ta
        let request_list = (removeEmpty . splitRegex (mkRegex "\r")) request
        out_ta  <- setVal _transaction_http_response_body (concat request_list)     -<< in_ta
        returnA                                                                     -<  out_ta

{- |
Retrieves session information from Context and stores it in the transaction (\/transaction\/session subtree).
A previously existing session subtree gets deleted.
If no session information can be acquired from the Context, a new session id and session is generated.
A session is identified by \/transaction\/http\/request\/cgi\/\@sessionid (sessionid-parameter of a URL-query part).
A session state is stored in the context at global:\/session\/[sessionid].
The session state is represented by the \/transaction\/session\/state subtree (attributes of \/session and subtrees
other than \/state are not stored in the Context).
-}
sessionReadShader :: ShaderCreator
sessionReadShader =
    mkStaticCreator $
    proc in_ta -> do
        ta      <- insEmptyTree _transaction_session                                -<  in_ta
        sid     <- (getVal _transaction_http_request_cgi_sessionid >>> parseA)
                   `orElse`
                   (getQualifiedUID "Sessions" 1)                                   -<  in_ta
        ta'     <- setVal _transaction_session_sessionid (show sid)                 -<< ta
        (XmlVal s_state)
                <- (getSV ("/global/session/_" ++ show sid))
                   `orElse`
                   (eelem "state" >>> arr XmlVal)                                   -<< ()

        ta''    <- insTree _transaction_session   (constA s_state)                  -<< ta'

        chGlobal <-@ mkSimpleLog "Lib_Shader:sessionReadShader" ("sessionReadShader read session: " ++ show sid) l_info -<< ()
        returnA                                                                     -<  ta''

{- |
Stores session information (\/transaction\/session\/state subtree) in the context (global:\/session\/[sessionid]).
The session id is read from \/transaction\/session\/\@sessionid. If no session id or no session state can be found, the shader fails.
-}
sessionWriteShader :: ShaderCreator
sessionWriteShader =
    mkStaticCreator $
    proc in_ta -> do
        sid     <- getVal _transaction_session_sessionid
                   <!> ( chGlobal
		       , "sessionWriteShader"
		       , TAValueNotFound
		       , ""
		       , [("value", show _transaction_session_sessionid)]
		       )                                                            -<  in_ta
        state   <- getTree _transaction_session_state
                   <!> ( chGlobal
		       , "sessionWriteShader"
		       , TAValueNotFound
		       , ""
		       , [("value", show _transaction_session_state)]
		       )                                                            -<  in_ta
        let stateVal    = XmlVal state
        ("/global/session/_" ++ sid) <$! stateVal                                   -<< ()

        chGlobal <-@ mkSimpleLog "Lib_Shader:sessionWriteShader" ("sessionWriteShader updated session: " ++ sid) l_info -<< ()
        returnA                                                                     -<  in_ta

{- |
Provides a simple mean of authentication by clear user and password transmission. The user data is checked against a configurable
credential repository in the \"global\" scope.
The database is configured via \/shader\/config\/\@userdb. If no database is defined, the default is \/userdb.
Users are stored in subnodes of the userdb (f.e. \/userdb\/user10). The passwords are stored in a \/password subnode of the user (f.e.
\/userdb\/user10\/password).
The transmitted username is read from \/transaction\/session\/\@username. The transmitted password is read from \/transaction\/session\/\@userpass.
If authentication is successful, the username is stored in \/transaction\/session\/state\/\@authuser. If authentication fails, the
username is stored in \/transaction\/session\/state\/\@authfailed.
Subshaders are invoked only if authentication is successful.
If \/transaction\/session\/state\/\@authuser is already present when the shader is invoked, subshaders are executed without further
credential check - this is to allow forwarding of already authenticated sessions. After all the idea of this shader is to encapsulate
subshaders into a zone guaranteed to be reachable for authenticated users only.
-}
authShader :: ShaderCreator
authShader =
    mkDynamicCreator $ proc (conf, associations) -> do
        let shader = proc in_ta -> do
            userdb      <- getValDef _shader_config_userdb "/userdb"                -<  conf
            authuser    <- listA $ getVal _transaction_session_state_authuser       -<  in_ta
            username    <- getVal _transaction_session_username
                           <!> ( chGlobal
			       , "authShader"
			       , TAValueNotFound
			       , ""
			       , [("value", show _transaction_session_username)]
			       )                                                    -<  in_ta
            userpass    <- getVal _transaction_session_userpass
                           <!> ( chGlobal
			       , "authShader"
			       , TAValueNotFound
			       , ""
			       , [("value", show _transaction_session_userpass)]
			       )                                                    -<  in_ta
            dbpass      <- (getSVS (userdb ++ "/" ++ username ++ "/password"))
                           <!> ( chGlobal
			       , "authShader"
			       , ValueNotFound
			       , ""
			       , [("value",userdb ++ "/" ++ username ++ "/password")]
			       )                                                    -<< in_ta

            (if Prelude.null authuser
                then (if dbpass == userpass
                        then setVal _transaction_session_state_authuser username >>> seqShader associations
                        else setVal _transaction_session_state_authfailed username
                        )
                else (seqShader associations)
                )                                                                   -<< in_ta
        returnA                                                                             -<  shader

{- |
Provides a mean to load data from a file into the context. The file has to contain data linewise, where each line represents a single
entity. Each line is divided into columns by means of a delimiter (e.g. \';\' in case of CSV-format). The Shader can be configured which
column contains data for which field of the entity. Lines with less columns than expected are ignored. Columns exceeding the expected
ones are ignored.
The file is configured by \/shader\/config\/\@file with no default.
The target node for all entities is defined by \/shader\/config\/\@to_state. If no target node is specified, the Shader fails.
The delimiter is configured by \/shader\/config\/\@delim, default is \';\'. If a string is provided here, only the first character gets used.
The columns are configured by config-attributes starting with an underline character (f.e. \/shader\/config\/\@_1). The value of each attribute
represents the name of the target field in the entities. The strings following the underline character do not need to represent a number -
only their lexicographic order is utilized for field identification in the data file. The first column in the data is considered to be
the key and it's values are used to name the entities.
Example: to_state=\"\/global\/mynode\" _a=\"name\" _b=\"price\", the file contains rows like: P1;Banana;50. This will translate into an
entity \/global\/mynode\/P1 with subnodes \/global\/mynode\/P1\/name (Banana) and \/global\/mynode\/P1\/price (50).
Please note that the key field must comply to XPath location path step syntax - e.g. its values are not allowed to start with a number.
-}
dataLoadShader :: ShaderCreator
dataLoadShader =
    mkDynamicCreator $ proc (conf, _) -> do
        let shader = proc in_ta -> do
            filename    <- getVal _shader_config_file
                           <+!> ( "dataLoadShader"
				, TAValueNotFound
				, "No data file specified."
				, [("value", show _shader_config_file)]
				)                                                   -<  conf
            delimiter   <- getValDef _shader_config_delim ";"
                           >>>
                           arr (\xs -> if Prelude.null xs then ';' else head xs)    -<  conf
            columns     <- listA $ listValPairs (_shader_config_ "@*")              -<  conf
            let columns'    = Prelude.map snd . Prelude.filter (\(key:_,_) ->
                                case key of
                                    '_'     -> True
                                    _       -> False) . sort $ columns
            to          <- getVal _shader_config_toState
                           <+!> ( "dataLoadShader"
                                , TAValueNotFound
                                , "No node specified."
                                , [("value", show _shader_config_toState)]
				)                                                   -<  conf
            file        <- exceptZeroA readFile
                           <+!> ( "dataLoadShader"
			        , FileNotFound
				, ("File '" ++ filename ++ "' not found.")
				, []
				)                                                   -<< filename
            let file_lines  = lines file
            let line_words  = Prelude.map (splitAtElem delimiter) file_lines

            load to columns' line_words                                             -<< in_ta
        returnA                                                                             -<  shader
    where
        load state cols ((key:ls):xs)
            | length ls >= length cols  =
                    foldr   (\(col,val) arrow ->
                              arrow
                              >>>
                              ((state ++ "/" ++ key ++ "/" ++ col) <-! val)
                              )
                            (this)
                            (Prelude.zip cols ls)
                    >>>
                    load state cols xs
            | otherwise                 = load state cols xs
        load state cols (_:xs)    = load state cols xs
        load _     _    []        = this
        splitAtElem delimiter str       =
            Prelude.map unEscapeString . lines $
                Prelude.map (\ch -> if ch == delimiter then '\n' else ch) str

{- |
Provides a mean to store data from the Context into a file. The data is stores linewise, where each line represents a single
entity. Each line is divided into columns by means of a delimiter (e.g. \';\' in case of CSV-format). The shader can be configured which
column contains data for which field of the entity.
The file is configured by \/shader\/config\/\@file with no default.
The source node for all entities is defined by \/shader\/config\/\@from_state. If no source node is specified, the Shader fails.
The delimiter is configured by \/shader\/config\/\@delim, default is \';\'. If a string is provided here, only the first character gets used.
The columns are configured by config-attributes starting with an underline character (f.e. \/shader\/config\/\@_1). The value of each attribute
represents the name of the source field in the entities. The strings following the underline character do not need to represent a number -
only their lexicographic order is utilized for field identification in the data file. The first column in the data is considered to be
the key and it's values are equal to the names of the entities.
Example: from_state=\"\/global\/mynode\" _a=\"name\" _b=\"price\", their exists a node \/global\/mynode\/P1 with subnodes
\/global\/mynode\/P1\/name (Banana) and \/global\/mynode\/P1\/price (50). This entity will be translated into a row \"P1;Banana;50\" in the
file.
-}
dataStoreShader :: ShaderCreator
dataStoreShader =
    mkDynamicCreator $ proc (conf, _) -> do
        let shader = proc in_ta -> do
            filename    <- getVal _shader_config_file
                           <+!> ( "dataStoreShader"
				, TAValueNotFound
				, "No data file specified."
				, [("value", show _shader_config_file)]
				)                                                   -<  conf
            delimiter   <- getValDef _shader_config_delim ";"
                           >>>
                           arr (\xs -> if Prelude.null xs then ';' else head xs)    -<  conf
            columns     <- listA $ listValPairs (_shader_config_ "@*")              -<  conf
            let columns'    = Prelude.map snd . Prelude.filter (\(key:_,_) ->
                                case key of
                                    '_'     -> True
                                    _       -> False) . sort $ columns
            from        <- getVal _shader_config_fromState
                           <+!> ( "dataStoreShader"
                                , TAValueNotFound
                                , "No node specified."
                                , [("value", show _shader_config_fromState)])
                                                                                    -<  conf
            rows        <- listA $
                (proc _ -> do
                    name    <- listStateTrees from                              -<< ()
                    pairs   <- listA $
                        listStatePairsStr (from ++ "/" ++ name)                 -<< ()
                    let state   = fromList pairs
                    let row     = Prelude.map (\col -> if member col state then state ! col else "") columns'
                    let row''   = foldl (\row' col -> row' ++ [delimiter] ++ col) name row
                    returnA                                                     -<  row''
                    )                                                               -<< ()

            arrIO $ writeFile filename                                              -<< unlines rows
            returnA                                                                 -<  in_ta
        returnA                                                                             -<  shader

{- |
TODO
-}
loadXmlShader :: ShaderCreator
loadXmlShader =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc in_ta -> do
        file    <- getVal _shader_config_file
                    <+!> ( "loadXmlShader"
                         , TAValueNotFound
                         , "No file specified."
                         , [("value", show _shader_config_file)]
			 )                                                          -<  conf
        node    <- getVal _shader_config_node
                    <+!> ( "loadXmlShader"
                         , TAValueNotFound
                         , "No node specified."
                         , [("value", show _shader_config_node)]
			 )                                                          -<  conf
        tree    <- fileSource file                                                  -<< ()
        node <$! (XmlVal tree)                                                      -<< ()
        returnA                                                                     -<  in_ta

{- |
TODO
-}
storeXmlShader :: ShaderCreator
storeXmlShader =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc in_ta -> do
        file        <- getVal _shader_config_file
                        <+!> ( "loadXmlShader"
			     , TAValueNotFound
			     , "No file specified."
			     , [("value", show _shader_config_file)]
			     )                                                      -<  conf
        node        <- getVal _shader_config_node
                        <+!> ( "loadXmlShader"
			     , TAValueNotFound
			     , "No node specified."
			     , [("value", show _shader_config_node)]
			     )                                                      -<  conf
        XmlVal tree <- getSV node                                                   -<< ()
        writeDocument [(a_indent, "1")] file                                        -<< tree
        returnA                                                                     -<  in_ta

{- |
Retrieves data from the context and stores it in the transaction as an attribute to a given node.
The source node is defined by \/shader\/config\/\@node. If no node is specified, the Shader fails.
The target node in the transaction is defined by \/shader\/config\/\@to. If no node is specified, the Shader fails.
-}
contextShader :: ShaderCreator
contextShader =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc in_ta -> do
        node        <- getVal _shader_config_node
                       <+!> ( "contextShader"
			    , TAValueNotFound
			    , "No source node specified."
			    , [("value", show _shader_config_node)]
			    )                                                       -<  conf
        target      <- getVal _shader_config_to
                       <+!> ( "contextShader"
			    , TAValueNotFound
			    , "No target node specified."
			    , [("value", show _shader_config_to)]
			    )                                                       -<  conf
        val         <- getSVS node                                                  -<< ()
        ta'         <- setVal (jp target) val                                       -<< in_ta
        returnA                                                                     -<  ta'

{- |
The value of \/shader\/config is send to the \"global\" message channel as a plain message (mandatory log level). Typically, this
will immediately result in a stdout output on the server console when the Shader is executed. This allows for simple
tracing of shader execution.
-}
traceShader :: ShaderCreator
traceShader =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc in_ta -> do
        message     <- getVal _shader_config            -<  conf
        globalMsg message                               -<< ()
        returnA                                         -<  in_ta

{- |
Similar to traceShader, but no message can be defined. Instead, the current Transaction is send to the \"global\"
channel as a plain message.
-}
traceTAShader :: ShaderCreator
traceTAShader =
    mkStaticCreator $
    proc in_ta -> do
        message     <- xshow (constA in_ta)             -<< ()
        globalMsg message                               -<< ()
        returnA                                         -<  in_ta

{- |
Adds a new message channel when the Shader is executed (typically used in init-elements of the server or handlers).
The channel name is defined by \/shader\/config\/\@channel. If no name is provided, the Shader fails.
Please note that this operation is local to the current system entity - when executed in the server's init-sequence,
the new channel will be visible throughout the whole system. When executed in a handler's init-sequence, the
new channel will be local to that handler.
-}
registerChannel :: ShaderCreator
registerChannel =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc through -> do
        reg_name    <- getVal _shader_config_channel
                       <+!> ( "registerChannel"
			    , TAValueNotFound
			    , "No channel specified."
			    , [("value", show _shader_config_channel)]
			    )                                   -<  conf
        addChannel (mkChannelId reg_name)                       -<< ()
        returnA                                                 -<  through

{- |
Adds a new state scope when the Shader is executed (typically used in init-elements of the server or handlers).
The scope name is defined by \/shader\/config\/\@scope. If no name is provided, the Shader fails.
Please note that this operation is local to the current system entity - when executed in the server's init-sequence,
the new scope will be visible throughout the whole system. When executed in a handler's init-sequence, the
new channel will be local to that handler.
-}
registerScope :: ShaderCreator
registerScope =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc through -> do
        reg_name    <- getVal _shader_config_scope
                       <+!> ( "registerScope"
			    , TAValueNotFound
			    , "No scope specified."
			    , [("value", show _shader_config_scope)]
			    )                                   -<  conf
        addScope reg_name                                       -<< ()
        returnA                                                 -<  through

{- |
Maps a set of state values and Transaction values to new locations. Transaction values can only be mapped to
Transaction locations, state values can only be mapped to a location in the same scope.
If \/shader\/config\/\@move is set to \"yes\", the original value gets deleted. Defaults to \"no\".
Each Transaction mapping is defined by a \"tamap\"-element in the config, where the \"from\"-attribute defines the source
and the \"to\"-attribute defines the target.
Each state mapping is defined by a \"statemap\"-element in the config, where the \"scope\"-attribute defines the scope,
the \"from\"-attribute defines the source and the \"to\"-attribute defines the target. Remember, state mappings are
limited to a single scope, the \"scope\"-attribute belongs to both the \"from\"- and the \"to\"-attribute.
-}
mappingShader :: ShaderCreator
mappingShader =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc in_ta -> do
        move        <- getValDef _shader_config_move "no"                           -< conf
        ta_maps     <- listA $ (proc xml -> do
                            xml'    <- getTree _shader_config_tamap         -<  xml
                            from    <- getVal _tamap_from                   -<  xml'
                            to      <- getVal _tamap_to                     -<  xml'
                            returnA                                         -<  (from, to)
                            )                                                       -<  conf
        state_maps  <- listA $ (proc xml -> do
                            xml'    <- getTree _shader_config_statemap      -<  xml
                            from    <- getVal _statemap_from                -<  xml'
                            to  <- getVal _statemap_to                      -<  xml'
                            returnA                                         -<  (from, to)
                            )                                                       -<  conf
        ta' <- foldl
                (\_ (from,to) ->
                    proc in_ta' -> do
                        val     <- getVal (jp from)                         -<  in_ta'
                        ta'     <- (if move == "yes"
                                        then delVal (jp from)
                                        else this
                                        )                                   -<  in_ta'
                        setVal (jp to) val                                  -<< ta'
                    )
                this
                ta_maps                                                             -<< in_ta
        foldl
            (\_ (from,to) ->
                proc in_ta' -> do
                    val <- getSVS from                                      -<< ()
                    (if move == "yes"
                        then delStateTree from
                        else this
                        )                                                   -<< ()
                    to <-! val                                              -<< in_ta'
                )
            this
            state_maps                                                              -<< ta'

{- |
Executes a Transaction\/state-mapping before the sub-shaders are executed. Afterwards the mappings are reverted,
the new values of the mappings are transfered back to the original locations.
If \/shader\/config\/\@move is set to \"yes\", the mapped values will be deleted at the end (defaults to \"yes\"). This shall regenerate
the original structure before the whole operation, implying that this operation transparently executes a shader with an arbitrary
interface in the presence of a different, arbitrary state\/Transaction structure. However, if intermediate nodes are created due to
the mapping, these are not deleted after the operation. E.g. if a mapping maps to \/transaction\/my\/node and \"my\" didn't exist before the
operation, it is created during the mapping. Afterwards, the \/transaction\/my\/node will get deleted, but \/transaction\/my remains.
Each Transaction mapping is defined by a \"tamap\"-element in the config, where the \"from\"-attribute defines the source
and the \"to\"-attribute defines the target.
Each state mapping is defined by a \"statemap\"-element in the config, where the \"scope\"-attribute defines the scope,
the \"from\"-attribute defines the source and the \"to\"-attribute defines the target. Remember, state mappings are
limited to a single scope, the \"scope\"-attribute belongs to both the \"from\"- and the \"to\"-attribute.
-}
mappingWrapper :: ShaderCreator
mappingWrapper =
    mkDynamicCreator $ arr $ \(conf, associations) ->
    proc in_ta -> do
        conf'   <-  setVal _shader_config_move "yes"                                        -<  conf
        conf''  <-  invert conf'                                                            -<< ()
        let wrapped = seqShader associations
        wrap    <-  mappingShader                                                           -< conf'
        unwrap  <-  mappingShader                                                           -< conf''
        wrap >>> wrapped >>> unwrap                                                         -<< in_ta
    where
        invert conf' = proc _ -> do
            new_ta_maps     <- listA $
                                (proc xml -> do
                                    xml'    <- getTree _shader_config_tamap         -<  xml
                                    from    <- getVal _tamap_from                   -<  xml'
                                    to      <- getVal _tamap_to                     -<  xml'
                                    let result = eelem "tamap"					-- names must match JanusPath names
                                                    += sattr "from" to
                                                    += sattr "to" from
                                    returnA                                         -<  result
                                    )                                                       -<  conf'
            new_state_maps  <- listA $
                                (proc xml -> do
                                    xml'    <- getTree _shader_config_statemap      -<  xml
                                    from    <- getVal _statemap_from                -<  xml'
                                    to      <- getVal _statemap_to                  -<  xml'
                                    let result = eelem "statemap"				-- names must match JanusPath names
                                            += sattr "from" to
                                            += sattr "to" from
                                    returnA                                         -<  result
                                    )                                                       -<  conf'
            ( delTree _shader_config_tamap
              >>>
              delTree _shader_config_statemap
              >>>
              foldl (\arrow tree -> arrow >>> addTree _shader_config tree) this (new_ta_maps ++ new_state_maps)
	      )                                                                           -<< conf'

{- |
Registers an arbitrary Shader in the ShaderCreator-Repository. During the load-sequence of the server, ServerCreators are loaded
into the repository with the given names. These creators have to exist completely in code, it is not possible to make a specific
combination of creators known with a name. However, the alias-Shader allows registrating an arbitrary Shader combination
in the Repository to eliminate unnecessary redundancy in the configuration file.
The registration name of the new Shader is defined by \/shader\/config\/\@alias with no default.
The Shader to register is the sequence of subshaders of the alias-Shader.
Please note that this operation is local to the current system entity - when executed in the server's init-sequence,
the new alias will be visible throughout the whole system. When executed in a handler's init-sequence, the
new alias will be local to that handler.
-}
aliasShader :: ShaderCreator
aliasShader =
    mkDynamicCreator $ arr $ \(conf, associations) ->
    proc through -> do
        alias   <- getVal _shader_config_alias
                   <+!> ( "aliasShader"
                        , TAValueNotFound
                        , "No alias name."
                        , [("value", show _shader_config_alias)]
			)                                                   -<  conf
        let shader = seqShader associations
        addShaderCreator alias (mkStaticCreator shader)                     -<< ()
        returnA                                                             -<  through

{- |
Sets a value in the current Transaction. The node to be set is identified by \/shader\/config\/\@node. The Shader fails if this attribute is
missing or the respective node qualifier is malformed. The value to insert is computed by an Expression subshader (a Shader child
node with id \"expr\"). The Shader fails if the Expression subshader is not present or fails.
-}
setValShader :: ShaderCreator
setValShader =
    mkDynamicCreator $ arr $ \(conf, associations) ->
    proc in_ta -> do
        node    <- getVal _shader_config_node
                   <+!> ( "setValShader"
			, TAValueNotFound
			, "No target node specified."
			, [("value", show _shader_config_node)]
			)                                                   -<  conf
        let exprShader = lookupShader "expr" zeroArrow associations
        value   <- exprShader >>> getVal _value                             -<< in_ta
        ta      <- setVal (jp node) value                                   -<< in_ta
        returnA                                                             -<  ta

{- |
Sets a value in a state scope. The node to be set is identified by \/shader\/config\/\@node. The Shader fails if the node-attribute is
missing or the respective node qualifier is malformed. The value to insert is computed by an Expression subshader (a shader child node
with id \"expr\"). The Shader fails if the expression subshader is not present or fails.
-}
setStateShader :: ShaderCreator
setStateShader =
    mkDynamicCreator $ arr $ \(conf, associations) ->
    proc in_ta -> do
        node    <- getVal _shader_config_node
                   <+!> ( "setStateShader"
                        , TAValueNotFound
			, "No target node specified."
			, [("value", show _shader_config_node)]
			)                                                   -<  conf
        let exprShader = lookupShader "expr" zeroArrow associations
        value   <- exprShader >>> getVal _value                             -<< in_ta
        node <-! value                                                      -<< ()
        returnA                                                             -<  in_ta


{- |
Removes empty lists from a list of lists.
-}
removeEmpty :: [[a]] -> [[a]]
removeEmpty (x:xs)  = if (Prelude.null x) then (removeEmpty xs) else (x:removeEmpty xs)
removeEmpty []      = []
