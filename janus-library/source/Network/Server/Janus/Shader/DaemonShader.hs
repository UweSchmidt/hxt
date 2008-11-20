-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.DaemonShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: DaemonShader.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus Daemon Library

   A Daemon Shader is a Shader forking a new process and immediately returning control to the system. Usually they have no effect on
   the invoking Transaction, although this behaviour is no necessity. By defining Daemon Shaders in the server's or a handler's init
   element, daemons may be installed before any request is served. As daemons remain in possession of the Context state, they may
   still affect system operation.
-}

-- ------------------------------------------------------------

module Network.Server.Janus.Shader.DaemonShader
    (
    -- daemon shaders
      daemonCreator
    , testDaemon
    , logControlDaemon
    , sessionDaemon
    )
where

import Control.Concurrent
import Text.XML.HXT.Arrow

import Network.Server.Janus.Core as Shader
import Network.Server.Janus.Messaging
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

{- |
Creates a Daemon Shader executing an arbitrary Arrow based on the Context state if invoked. The Arrow does not need to depend on an input and
does not need to deliver an output.
-}
daemonCreator :: JanusStateArrow () () -> ShaderCreator
daemonCreator daemonArrow =
    mkDynamicCreator $ proc (conf, _) -> do
        ident   <- getVal _shader_config_id                     -<  conf
        let shader = proc in_ta -> do
            createThread "/global/threads" ident daemonArrow    -<  ()
            returnA                                             -<  in_ta
        returnA                                                     -<  shader

{- |
Creates a Daemon forwarding the messages stored in the channel chLocal to the channel chGlobal on the l_debug level every second.
-}
testDaemon :: ShaderCreator
testDaemon =
    daemonCreator thedaemon
    where
        thedaemon = proc _ -> do
            chGlobal <-@ mkSimpleLog "DaemonShader.hs:testDaemon" ("testDaemon invoked...") l_debug     -< ()
            arrIO $ threadDelay                                                                         -< 1000000
            msg <- listA $ getMsg chLocal >>> showMsg                                                   -< ()
            chGlobal <-@ mkSimpleLog "DaemonShader.hs:testDaemon" ("local messages: " ++ (show msg)) l_debug -<< ()
            thedaemon                                                                                   -< ()

{- |
Creates a Daemon blocking on channel \"control\" for arriving messages. When a message arrives it is processed if it is a Control Message. In this
case the \"messagelevel\" state attribute is read and used to reset the message handler of channel chGlobal with a filter based on the message level.
Afterwards all Control Messages are removed from the \"control\" channel.
This Daemon allows changing the message level the \"global\" message channel forwards to the console. This is utilized by the current Janus Console.
Of course more simple implementation were possible - this is only to demonstrate the work of Daemons and Control Messages.
-}
logControlDaemon :: ShaderCreator
logControlDaemon =
    daemonCreator thedaemon
    where
        thedaemon = proc _ -> do
            msg     <- listA $ listenChannel chControl                                                  -<  ()
            msg'    <- listA $ constL msg >>> showMsg                                                   -<< ()
            chGlobal <-@ mkSimpleLog "DaemonShader.hs:logControlDaemon" ("logControlDaemon invoked...") l_debug -< ()
            level   <- listA $
                        constL msg
                        >>>
                        getMsgTypeFilter ControlMsg
                        >>>
                        getMsgState "messagelevel"                                                      -<< ()
            case msg' of
                []     -> this                                                                  -<  ()
                (x:_)  ->
                    (proc _ -> do
                        chGlobal <-@ mkSimpleLog "DaemonShader.hs:logControlDaemon" ("control message detected: " ++ x) l_info -< ()
                        let myhandler   = (filterHandler (getMsgLevelFilter (read $ head level)) >>> consoleHandler)
                        changeHandler chGlobal (\_ -> myhandler) -<< ()
                        )                                                                       -<< ()
            filterMsg chControl (neg $ getMsgTypeFilter ControlMsg)                                     -<  ()
            thedaemon                                                                                   -<  ()

{- |
Periodically (every 10 seconds) checks the timestamps of all sessions (stored under \/session in the \"global\" scope). If a session state
change is older than 60 seconds, the respective session is removed from the \"global\" scope.
Updated to understand a parameter (validTime, milliseconds) for the time a session is kept. 0 means indefinitely, so sessions are not removed automatically at all. Negative values remove them on as soon as  checked.
-}
sessionDaemon :: ShaderCreator
sessionDaemon =
    mkDynamicCreator $ proc (conf, _) -> do
        ident   <- getVal _shader_config_id                                                             -<  conf
        validTime <- getValDef (_shader_config_ "@validTime") "60000" >>> parseA                        -<  conf
        let shader = proc in_ta -> do
            createThread "/global/threads" ident (thedaemon validTime)                                  -<  ()
            returnA                                                                                     -<  in_ta
        returnA                                                                                         -<  shader
    where
        thedaemon validTime =
            proc _ -> do
                chGlobal <-@ mkSimpleLog "sessionDaemon" ("sessionDaemon invoked...") l_debug           -<  ()
                arrIO $ threadDelay                                                                     -<  10000000
                (
                    (proc _ -> do
                        sessions <- listA $ listStateTrees "/global/session"                    -<  ()
                        checkSessions sessions validTime                                        -<< ()
                        )
                    `orElse`
                    this
                    )                                                                                   -<  ()
                chGlobal <-@ mkSimpleLog "sessionDaemon" ("sessionDaemon completed...") l_debug         -<  ()
                thedaemon validTime                                                                     -<  ()
        checkSessions [] _    = this
        checkSessions (x:xs) validTime =
            proc _ -> do
                current_ts  <- getCurrentTS                                                             -<  ()
                session_ts  <- getSC ("/global/session/" ++ x) >>> getCellTS                            -<  ()
                let diff_ts = current_ts - session_ts
                chGlobal <-@ mkSimpleLog "sessionDaemon" ("Session " ++ x ++ " inactive for " ++ (show diff_ts) ++ " ms.") l_info -<< ()
                (chGlobal <-@ mkSimpleLog "sessionDaemon" ("Removing session " ++ x) l_info
                    >>>
                    delStateTree  ("/global/session/" ++ x)
                    )
                    `whenP`
                    (\_ -> (validTime /= 0 && (diff_ts > validTime)))                                             -<< ()
                checkSessions xs validTime                                                                        -<  ()
