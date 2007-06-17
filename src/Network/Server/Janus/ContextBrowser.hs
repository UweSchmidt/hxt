-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.ContextBrowser
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ContextBrowser.hs, v0.8 2007/03/26 00:00:00 janus Exp $

   Janus Context Browser
   
   A small web application based on Shaders to browse the shared mutable states
   in the Janus Context.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.ContextBrowser
   (
   -- shaders
     doorShader
   , scopeShader
   -- , addValShader
   -- , delValShader
   -- , setValShader
   )
where

import Text.XML.HXT.Arrow
      
import Network.Server.Janus.Core
import Network.Server.Janus.HTMLBuilder
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

{- |
A Shader to return the front page. On this page the available scopes are listed. The user may select a scope, the according 
request will be directed to a Shader handling the "browse" operation ("operation" parameter in the URI).
-}
doorShader :: ShaderCreator
doorShader =
   mkStaticCreator $
   proc in_ta -> do
      sid      <- getValDef _transaction_session_sessionid "0"                       -<  in_ta
      scopes   <- listA $ listScopes                                                 -<  ()
      html_str <- browserPage [] [] (content sid (list sid scopes)) [] >>> html2Str  -<< undefined
      setVal _transaction_http_response_body html_str                                -<< in_ta   
   where
      content _ elements = 
         [ 
           heading 1 "Janus Context Browser"
         , heading 3 "Available scopes"
         , elements
         ]
      list sid scopes = 
         table ""
            += (row "" +>> [cell "" += text "Scope" "", cell "" += text "&nbsp;" ""])
            +>> list' sid scopes
      list' _ []      = 
         [none]
      list' sid (name:xs)  =
         (row "" +>> [cell "" += text name ""
               , cell "" += link ("/browser?node=/" ++ name ++ "&operation=browse&sessionid=" ++ sid) "Browse"])
         :(list' sid xs)
            
{- |
A Shader to return the nodes of a selected scope or subtree. The listed nodes can be clicked again to recursively 
traverse the shared mutable state.
-}
scopeShader :: ShaderCreator
scopeShader =
   mkStaticCreator $
   proc in_ta -> do
      sid   <- getValDef _transaction_session_sessionid "0"                -<  in_ta
      node  <- getValDef _transaction_http_request_cgi_node ""             -<  in_ta
      nodes <- listA $ listStateTrees node                                 -<<  ()
      
      html_str  <- browserPage [] [] (content sid node (list sid node nodes)) [] >>> html2Str    -<< undefined
      setVal _transaction_http_response_body html_str                    -<< in_ta
   where
      content _ node elements = 
         [ 
           heading 1 ("The following states are available at " ++ node)
         , elements
         ]
      list sid current nodes = 
         table ""
            += ( row ""
		 +>> [ cell "" += text "Node" ""
		     , cell "" += text "Cell" ""
		     , cell "" += text "&nbsp;" ""
		     ]
	       )
            +>> list' sid current nodes
      list' _ _ [] = 
          [none]
      list' sid current (name:xs) =
          ( row ""
	    +>> [ cell "" += text name ""
		, cell "" += text name ""
		, cell "" += link ( "/browser?node=" ++ current ++ "/" ++ name ++ 
				    "&operation=browse&sessionid=" ++ sid) "Browse"
		]
          ) : (list' sid current xs)

{-
A template Arrow to easily create a web form.
browserSubmit :: String -> String -> String -> XmlTransform s
browserSubmit sid operation caption =
   (form operation "/shop"
      += (block "" += formHidden "sessionid" sid)
      += (block "" += formHidden "operation" operation)
      += (block "" += formButton "action" caption)
      )
-}
      
{-
TODO: A Shader to add a string value to the shared mutable state.
addValShader :: ShaderCreator
addValShader =
   mkStaticCreator $
   proc in_ta -> do
      sid   <- getValDef "/transaction/session/@sessionid" "0"             -<  in_ta
      article  <- getValDef "/transaction/http/request/cgi/@article" ""    -<  in_ta
      price    <- "local" !-> ("/catalogue/" ++ article ++ "/price")       -<< ()

      htmltree <- browserPage [] [] 
            (content sid article (describe article price)) []  -<< undefined
      html_str <- html2Str                                     -<  htmltree
      setVal "/transaction/http/response/body" html_str        -<< in_ta
   where
      content sid name desc = 
         [ 
           heading 1 ("Article '" ++ name ++ "'")
         , desc
         , (form "add" "/shop"
            += (block "" += formHidden "sessionid" sid)
            += (block "" += formHidden "operation" "addItem")
            += (block "" += formHidden "article" name)
            += (block "" += formText "amount" "0" 2)
            += (block "" += formButton "action" "Add")
            )
         , browserSubmit sid "catalogue" "Catalogue"
         ]
      describe name price  = 
         table ""
            += (row "" +>> [cell "" += text "Article" "", cell "" += text name ""])
            += (row "" +>> [cell "" += text "Price" "", cell "" += text price ""])
-}

{-
TODO: A Shader to remove a node from the shared mutable state.
delValShader :: ShaderCreator
delValShader =
   mkStaticCreator $
   proc in_ta -> do
      sid      <- getValDef "/transaction/session/@sessionid" "0"          -<  in_ta
      article  <- getValDef "/transaction/http/request/cgi/@article" ""    -<  in_ta
      amount   <- getValDef "/transaction/http/request/cgi/@amount" ""     -<  in_ta
      price    <- "local" !-> ("/catalogue/" ++ article ++ "/price")       -<< ()

      (setVal ("/transaction/session/state/cart/" ++ article ++ "/@amount") amount  
         >>>
         setVal ("/transaction/session/state/cart/" ++ article ++ "/@price") price
         >>>
         setVal "/transaction/http/request/cgi/@operation" "cart"
         ) -<< in_ta
-}

{-
TODO: A Shader to change the string value of a node from the shared mutable state.
setValShader :: ShaderCreator
setValShader =
   mkStaticCreator $
   proc in_ta -> do
      sid   <- getValDef "/transaction/session/@sessionid" "0"           -<  in_ta
      article  <- getValDef "/transaction/http/request/cgi/@article" ""  -<  in_ta
      
      (delVal ("/transaction/session/state/cart/" ++ article) 
         >>>
         setVal "/transaction/http/request/cgi/@operation" "cart"
         ) -<< in_ta
-}

{- |
A template Arrow to easily create instances of the context browser's HTML layout.
-}
browserPage :: [XmlTransform s] -> [XmlTransform s] -> [XmlTransform s] -> [XmlTransform s] -> XmlTransform s
browserPage nav page_head page_content page_bottom =
   html
      += (headers
         += style_def "css/uhl.css"
         += title "Janus Browser Servlet"
         )
      += (htmlbody 
         += (table "" 
            += (row "" +>> [cell "", cell "" +>> page_head])
            += (row "" +>> [cell "" +>> nav, cell "" +>> page_content])
            += (row "" +>> [cell "", cell "" +>> page_bottom])
            )        
         )

