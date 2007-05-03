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
    , counterPageShader
    )
where

import Text.XML.HXT.Arrow

import Network.Server.Janus.Core as Shader
import Network.Server.Janus.XmlHelper

import Control.Concurrent

-- ------------------------------------------------------------

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

-- ------------------------------------------------------------

-- | example shader for demonstrating the use of a local state for
-- a shader.
--
-- When the shader is created, a new MVar is allocated and initialized,
-- This MVar contains the state for this shader, in this case a single counter.
-- Further a template page is loaded from the wwwpages dir.
--
-- Every time the shader is called, the counter is incremented and
-- the value is inerted into the template page.

counterPageShader :: ShaderCreator
counterPageShader =
    mkDynamicCreator $ proc (_, _) -> do
        x   <- arrIO $ newMVar  -< (0 :: Int)
	pg  <- readTemplate     -< "wwwpages/JanusCounter.html"
	returnA                 -< counterPage x pg


readTemplate	:: XmlSource s String
readTemplate
    = runInLocalURIContext $
      readFromDocument [ (a_parse_html,v_1)
		       , (a_indent,v_1)
		       , (a_trace,v_0)
		       ]
      >>> strictA				-- evaluate the template page for space saving

counterPage	:: MVar Int -> XmlTree -> Shader
counterPage cnt pg
    = setVal "/transaction/http/response/body"	-- insert result into transaction body
      $< ( processState
	   >>> genPage
	 )
    where
    -- manipulate the state for this request and deliver result
    processState
	= arrIO0 incrCnt
	where
	incrCnt
	    = do
	      v <- takeMVar cnt
	      let res = v + 1
	      putMVar cnt res
	      return res

    -- insert result into template page
    genPage
	= arr showRes
	  >>> mkText
	  >>> insertTreeTemplate (constA pg)
		  [ hasAttrValue "id" (== "count")  -- substitute node with id "count"
		    :-> this
						    -- insert other substitutions here
		  ]
	  >>> addXHtmlDoctypeTransitional	    -- add a DTD decl and convert to text
          >>> writeDocumentToString [(a_output_html, v_1)
				    ,(a_no_xml_pi,v_1)
				    ]
    showRes	:: Int -> String
    showRes res
	| res == 1  = "the first time"
	| otherwise = show res ++ " times"
