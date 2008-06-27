-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.SystemShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: SystemShader.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus System Shader Definitions

   Definition of the Janus system Shaders, which provide functionality to dynamically
   configure the server state and.get loaded statically by the server startup facility.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.SystemShader
    (
      changeServerRoot
    , loadShaderCreator
    , loadHandlerCreator
    , loadStateHandler
    , loadHandler
    -- core shaders
    -- variable and channel control
    -- message control
    -- loader: Handler -> Context, Repository-Shader load/unload
    )
where

import Text.XML.HXT.Arrow

import Network.Server.Janus.Core
import Network.Server.Janus.DynamicLoader
import Network.Server.Janus.Messaging
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

import System.Directory

{- |
TODO
-}
changeServerRoot :: ShaderCreator
changeServerRoot =
    mkDynamicCreator $ proc (conf, _) -> do
        (SimpleVal oldroot ) <- getSV ("/global/system/serverroot")                  -<< ()
        newroot     <- getVal _shader_config_serverroot                     -<  conf
        currentdir  <- exceptZeroA_ getCurrentDirectory
                <+!> ("SystemShader.hs:changeServerRoot", GenericMessage, "Failed to get current directory.", [])
                                                                            -<  ()

        globalMsg $ "changing server root path to '" ++
                            newroot ++ "')... "                             -<< ()
        (if currentdir /= oldroot
          then (globalMsg $ "Warning: System root variable differs from actual current directory.")
          else this)                                                        -<<  ()

        (exceptZeroA $ setCurrentDirectory)
                <+!> ("SystemShader.hs:changeServerRoot", GenericMessage, "Failed to set new directory '" ++
                      newroot ++ "'.", [])                                  -<< newroot
        "/global/system/serverroot" <-! newroot                             -<< ()

        globalMsg $ "done\n"                               -<< ()

        returnA                                                             -<  this

{- |
TODO
-}
loadShaderCreator :: ShaderCreator
loadShaderCreator =
    mkDynamicCreator $ proc (conf, _) -> do
        ref         <- getVal _shader_config_reference                      -<  conf
        obj         <- getVal _shader_config_object                         -<  conf
        file        <- getVal _shader_config_module                         -<  conf
        globalMsg $ "loading shader creator '" ++
                            ref ++ "' (object '" ++ obj ++
                            "' in module '" ++ file ++ "')... "             -<< ()
        sRep        <- getShaderCreators                                    -<  ()
        sRep'       <-
            (loadComponent ref file obj
                <+!> ("SystemShader.hs:loadShaderCreator", GenericMessage, "Failed to load object " ++ ref ++ ".", [("object", ref)])
                )                                                           -<< sRep
        swapShaderCreators                                                  -<  sRep'
        globalMsg $ "done\n"                               -<< ()
        returnA                                                             -<  this

{- |
TODO
-}
loadHandlerCreator :: ShaderCreator
loadHandlerCreator =
    mkDynamicCreator $ proc (conf, _) -> do
        ref     <- getVal _shader_config_reference                          -<  conf
        obj     <- getVal _shader_config_object                             -<  conf
        file    <- getVal _shader_config_module                             -<  conf
        globalMsg $ "loading handler creator '" ++
                            ref ++ "' (object '" ++ obj ++
                            "' in module '" ++ file ++ "')... "             -<< ()
        hRep    <- getHandlerCreators                                       -<  ()
        hRep'   <-
            (loadComponent ref file obj
                <+!> ("SystemShader.hs:loadHandlerCreator", GenericMessage, "Failed to load object " ++ ref ++ ".", [("object", ref)])
                )                                                           -<< hRep
        swapHandlerCreators                                                 -<  hRep'
        globalMsg $ "done\n"                               -<< ()
        returnA                                                             -<  this

{- |
TODO
-}
loadHandler :: ShaderCreator
loadHandler =
    proc conf -> do
        hConf       <- getTree _shader_config_handler_config            -<  conf
        hId         <- getVal _config_id                                -<  hConf
        globalMsg $ "constructing handler '" ++
                            hId ++ "'... "                              -<< ()
        hType       <- getVal _config_type                              -<  hConf
        let shader = createThread "/global/threads" hId (proc in_ta -> do
            globalMsg $ "starting handler '" ++
                            hId ++ "'...\n"                         -<  ()
            swapChannel chLocal                                     -<  ()
            swapScope "local"                                       -<  ()
            context     <- getContext                               -<  ()
            ("/global/consoles/handlers/" ++ hId)
                <$! (ContextVal context)                            -<< ()
            let creator = mkDynamicCreator $ proc (_, associations) -> do
                let shader' = head $ shaderList associations
                hCreator    <- getHandlerCreator hType          -<< ()
                handler     <- hCreator                         -<< (hConf, shader')
                handler                                         -<< ()
                returnA                                         -<  this
            creator                                                 -<< conf
            returnA                                                 -<  in_ta
            )
        globalMsg $ "done\n"                           -<< ()
        returnA                                                         -<  shader

{- |
TODO
-}
loadStateHandler :: ShaderCreator
loadStateHandler =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc in_ta -> do
        ref     <- getVal _shader_config_reference                  -<  conf
        obj     <- getVal _shader_config_object                     -<  conf
        file    <- getVal _shader_config_module                     -<  conf
        sRep    <- getShaderCreators                                -<  ()
        sRep'   <-
            (loadComponent ref file obj
                <+!> ("SystemShader.hs:loadStateHandler", GenericMessage, "Failed to load object " ++ ref ++ ".", [("object", ref)])
                )                                                   -<< sRep
        swapShaderCreators                                          -<  sRep'
        returnA                                                     -<  in_ta


{-
ref     <- newRepositoryA classStr                                  -<  ()
mod     <- listA
            (constL configs
             >>>
              (proc config -> do
                ref     <- getAttrValue "reference"     -<  config
                name    <- getAttrValue "name"          -<  config
                modstr  <- getAttrValue "module"        -<  config
                returnA                                 -<  (ref, name, modstr)
                )
             )                                                      -<< ()
-- arrIO $ putStrLn -< "Loading " ++ objname ++ " in module " ++ modname ++ " (" ++ modname' ++ ")"
add tupels                                                          -<< rep
    where
        add ((ref, name, modstr):xs) =
            forwardError chGlobal (add xs)
            >>>
            (globalMsg $ "Loading object '" ++ ref ++ "' (value '" ++ name ++ "' in module '" ++ modstr ++ "')")
            >>>
            (loadComponent ref modstr name
                <+!> ("Server.hs:buildRepository", GenericMessage, "Failed to load object " ++ ref ++ ".", [("object", ref)])
                )
        add [] = returnA
-}

