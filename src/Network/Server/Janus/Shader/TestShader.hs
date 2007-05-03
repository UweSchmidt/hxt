-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.ExprShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ExprShader.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus Expression Shaders
   
   These Shaders represent expressions, delivering string values. They are especially useful to compute values to be used 
   by Control Shaders. The string value denoted by an Expression Shader is delivered by means of an XML tree with a root node
   \"value\" and a text node child containing the value.
-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.TestShader
    (
    -- expression shaders
      testShader
    , testShader2
    , testShader3
    )
where

import Text.XML.HXT.Arrow
        
import Network.Server.Janus.Core as Shader
import Network.Server.Janus.XmlHelper

import Control.Concurrent

testShader :: ShaderCreator
testShader = 
    mkDynamicCreator $ proc (_, _) -> do
        x   <- arrIO $ newMVar                                          -<  (0 :: Int)
        let shader = proc in_ta -> do
            val <- arrIO $ takeMVar                                     -<  x
            arrIO $ putMVar x                                           -<< (val + 1)
            setVal "/transaction/http/response/body" (show $ val + 1)   -<< in_ta
        returnA                                                         -<  shader

testShader2 :: ShaderCreator
testShader2 =
    mkDynamicCreator $ proc (_, _) -> do
        "test" <*! (0 :: Int)  -< ()
        let shader = proc in_ta -> do
            val :: Int <- getSVP "test" -< ()
            "test" <*! (val + 1) -<< ()
            setVal "/transaction/http/response/body" (show $ val + 1) -<< in_ta
        returnA -< shader

testShader3 :: ShaderCreator
testShader3 = 
    mkDynamicCreator $ proc (_, _) -> do
        x   <- arrIO $ newMVar                                          -<  (0 :: Int)
	pg  <- runInLocalURIContext
	       ( readDocument [(a_parse_html,v_1)
			      ,(a_indent,v_1)
			      ,(a_trace,v_0)
			      ] "../wwwpages/JanusCounter.html" )       -< ()
        let shader = proc in_ta -> do
            val <- arrIO $ takeMVar                                     -<  x
            arrIO $ putMVar x                                           -<< (val + 1)
	    pg1 <-  mkText >>> insertCounter pg                         -<< showCnt (val + 1)
	    txt1 <- addXHtmlDoctypeTransitional
		    >>> writeDocumentToString [(a_output_html, v_1)
					      ,(a_no_xml_pi,v_1)
					      ]                             -< pg1
            setVal "/transaction/http/response/body" txt1               -<< in_ta
        returnA                                                         -<  shader

insertCounter :: ArrowXml a => XmlTree -> a XmlTree XmlTree
insertCounter pg
    = insertTreeTemplate (constA pg)
      [ hasAttrValue "id" (== "count")
	:-> this
      ]

showCnt	:: Int -> String
showCnt cnt
    | cnt == 1 = "the first time"
    | otherwise  = show cnt ++ " times" 
      