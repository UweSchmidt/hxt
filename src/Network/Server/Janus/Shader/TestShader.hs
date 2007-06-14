-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.TestShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ExprShader.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus Test Shaders

   These Shaders represent simple servlets for demonstrating the janus functionality

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.TestShader
    ( testShader
    , testShader2
    , transactionStatusShader
    , counterPageShader
    , sessionDemo
    )
where

import Text.XML.HXT.Arrow

import Network.Server.Janus.Core as Shader
import Network.Server.Janus.XmlHelper

import Control.Concurrent
import Data.List

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
            (val :: Int) <- getSVP "test" -< ()
            "test" <*! (val + 1) -<< ()
            setVal "/transaction/http/response/body" (show $ val + 1) -<< in_ta
        returnA -< shader

-- ------------------------------------------------------------

transactionStatusShader	:: ShaderCreator
transactionStatusShader
    = mkStaticCreator
      ( setVal "/transaction/http/response/body" $< statusPage )

statusPage	:: XmlAccess s String
statusPage
    = insertTreeTemplate statusPageTemplate
      [ hasAttrValue "id" (== "status")
	:-> ( formatRequestFragment
	      >>> xshow indentDoc
	      >>> mkText
	    )
      ]
      >>> showPage
    where
    statusPageTemplate
	= constA "wwwpages/JanusStatus.html"
	  >>> readTemplate

    formatRequestFragment
	= processTopDown
	  ( replaceChildren
	    ( xshow getChildren
	      >>> arr formatRequest
	      >>> mkText
	    )
	    `when` hasName "request_fragment"
	  )
	where
	formatRequest
	    = ("\n" ++) . (++ "\n") . stringTrim

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
	pg  <- ( readTemplate
		 >>> strictA )  -< "wwwpages/JanusCounter.html"
	returnA                 -< counterPage x pg


counterPage	:: MVar Int -> XmlTree -> Shader
counterPage cnt pg
    = setVal "/transaction/http/response/body"	-- insert result into transaction body
      $< ( processState
	   >>> genOutPage
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
    genOutPage
	= arr showRes
	  >>> mkText
	  >>> insertTreeTemplate (constA pg)
		  [ hasAttrValue "id" (== "count")  -- substitute node with id "count"
		    :-> this
						    -- insert other substitutions here
		  ]
	  >>> showPage
    showRes	:: Int -> String
    showRes res
	| res == 1  = "the first time"
	| otherwise = show res ++ " times"

-- ------------------------------------------------------------

-- | read a HTML template page

readTemplate	:: XmlSource s String
readTemplate
    = runInLocalURIContext $
      readFromDocument [ (a_parse_html,v_1)
		       , (a_indent,v_1)
		       , (a_trace,v_1)
		       ]

showPage	:: IOStateArrow s XmlTree String
showPage
    = addXHtmlDoctypeTransitional	    -- add a DTD decl and convert to text
      >>> writeDocumentToString [ (a_output_html, v_1)
				, (a_no_xml_pi,v_1)
				]

genPage		:: String -> IOStateArrow a XmlTree XmlTree -> IOStateArrow a XmlTree String
genPage path processPage
    = constA path
      >>> readTemplate						-- read the template page
      >>> processPage						-- insert the data
      >>> showPage

-- ------------------------------------------------------------

sessionDemo :: ShaderCreator
sessionDemo =
    mkStaticCreator $ proc in_ta -> do
        sid     <- getVal "/transaction/session/@sessionid"         -<  in_ta
        myname  <- getVal "/transaction/http/request/@uri_path"     -<  in_ta
        (count :: Int) 
                <- getValDef "/transaction/session/state/@count" "0" 
                    >>> parseA                                      -<  in_ta
        in_ta'  <- setVal "/transaction/session/state/@count" (show $ count + 1)      
                                                                    -<< in_ta
        sessionPage myname sid (count + 1)                          -<< in_ta'

sessionPage	:: String -> String -> Int -> Shader
sessionPage path sid count
    = setVal "/transaction/http/response/body"
      $< genPage
	     ("wwwpages" ++ path)
	     ( editRefs
	       >>> insertCount )
    where
    editRefs				-- append session id to all pages of session demo
	= processTopDown $
	  appendSessionId `when` (hasName "a" >>> hasAttr "href")
	where
	appendSessionId
	    = (addAttr "href" $< editUrl)
	      `orElse`
	      this
	editUrl
	    = ( getAttrValue0 "href"
		&&&
		constA ("http://localhost" ++ path)
	      )
              >>> expandURI
	      >>> ( isA isLocalRef
		    `guards`
		    ( getPathFromURI
		      >>> arr (++ ("?sessionid=" ++ sid))
		    )
		  )
	isLocalRef = ("http://localhost/JanusSession/" `isPrefixOf`)

    insertCount				-- insert the session local counter
	=  processTopDown $
	   choiceA
	   [ hasAttrValue "id" (== "counter")
	     :-> txt (show count)
	   , hasAttrValue "id" (== "sessionid")
	     :-> txt sid
	   , this
	     :-> this
	   ]

-- ------------------------------------------------------------
