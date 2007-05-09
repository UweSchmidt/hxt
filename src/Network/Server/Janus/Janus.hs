-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Main
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Janus.hs, v1.0 2006/11/02 18:00:00 janus Exp $

   Janus Main Module
-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Main where

import System.IO

import Network.Server.Janus.Core as Shader
import Network.Server.Janus.Server
import Network.Server.Janus.XmlHelper

-- ------------------------------------------------------------

conf_file :: String
conf_file       = "./conf/server.xml"

{- |
The main function creates an initial Context and evaluates the Server Arrow starting with this state.
-}
main :: IO ()
main = do

	-- Kontext bereitstellen
	initContext <- emptyContext

	evalXml (proc _ -> serverArrow conf_file -< ()) initContext

	return ()	

-- ------------------------------------------------------------
