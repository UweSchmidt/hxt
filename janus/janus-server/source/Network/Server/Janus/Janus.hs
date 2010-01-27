-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Main
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Janus.hs, v1.1 2008/04/12 18:00:00 janus Exp $

   Janus Main Module
-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Main where

import System.IO

import Network.Server.Janus.Core as Shader
import Network.Server.Janus.Server
import Network.Server.Janus.XmlHelper

import System.Environment

-- ------------------------------------------------------------

default_conf_file :: String
default_conf_file = "./conf/server.xml"

{- |
The main function creates an initial Context and evaluates the Server Arrow starting with this state.
-}
main :: IO ()
main = do
   -- Kontext bereitstellen
   initContext <- emptyContext
   args        <- getArgs
   let conf_file = if length args > 0 then head args else default_conf_file
   evalXml (proc _ -> serverArrow "" conf_file -< ()) initContext
   return ()

