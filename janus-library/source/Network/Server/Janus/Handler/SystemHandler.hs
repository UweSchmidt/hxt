-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Handler.SystemHandler
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ConsoleHandler.hs, v1.1 2007/03/27 00:00:00 janus Exp $

-}

-- ------------------------------------------------------------

module Network.Server.Janus.Handler.SystemHandler
    ( daemonHandler
# if PLUGINS
    , dynHandler
# endif
    )
where

import Network.Server.Janus.Core
import Network.Server.Janus.Transaction as TA

# if PLUGINS
import Data.Maybe

import Network.Server.Janus.JanusPaths
import Network.Server.Janus.XmlHelper

import System.Eval (unsafeEval)
import System.IO

# endif

import Text.XML.HXT.Arrow


# if PLUGINS
{- |
The dynamic Handler compiles Haskell code delivered by \/config\/\@haskell at runtime. The result is considered to be a HandlerCreator value.
The actual Handler is created by configuring the compiled HandlerCreator with the configuration and Shader of the dynamic HandlerCreator.
-}
dynHandler :: HandlerCreator
dynHandler = proc (conf, shader) -> do
    let result = proc _ -> do
        source  <- getValDef _config_haskell ""                 -<  conf
        creator <- arrIO0 $
            (unsafeEval ("(" ++ source ++ ") :: HandlerCreator") [".", "./hs-plugins/src", "./HXT-6.0/src"] :: IO (Maybe HandlerCreator)) -<< ()
        (if isJust creator
            then (fromJust creator)
            else (constA nullHandler)
            )                                                   -<< (conf, shader)
        returnA                                                 -<  ()
    returnA                                                             -<  result
# endif

{- |
The daemon Handler is meant to host daemon Shaders. The Handler simply invokes its Shader with an empty Transaction. By providing a sequence
of Shaders one may use a daemon Handler to start a set of daemons.
-}
daemonHandler :: HandlerCreator
daemonHandler = proc (_, shader) -> do
    let handler = proc _ -> do
        ta    <- createTA 0 Processing                        -<  ()
        globalMsg "starting daemon handler..."                -<< ()
        shader                                                -<  ta
        globalMsg "daemon handler completed"                  -<< ()
    returnA                                                   -< handler
