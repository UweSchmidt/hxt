-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.ShopExample
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ShopExample.hs,v 0.2 2007/03/26 00:00:00 janus Exp $

   Janus Example Shop Servlet

   A web application to provide a simple web shop application. This application manages sessions, user accounts,
   shopping carts and check out functionality.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.ShopExample
    (
    -- shaders
      doorShader
    , registerShader
    , loginShader
    , catalogueShader
    , articleShader
    , addItemShader
    , delItemShader
    , cartShader
    , checkoutShader
    , paymentShader
    , logoutShader
    )
where

import Text.XML.HXT.Arrow

import Network.Server.Janus.Core
import Network.Server.Janus.HTMLBuilder
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

-- ------------------------------------------------------------

cart		:: String
cart		= "cart"

_cart		:: JanusPath
_cart		=  jp $ show _transaction_session_state ++ "/" ++ cart

_cart_		:: String -> JanusPath
_cart_ n	=  jp $ show _cart ++ n

-- ------------------------------------------------------------

{- |
A door page to provide forms to register or to login with an existing account.
-}
doorShader :: ShaderCreator
doorShader =
    mkStaticCreator $
    proc in_ta -> do
        sid      <- getValDef _transaction_session_sessionid "0"    -<  in_ta
        html_str <- shopPage [] [] (content sid ) [] >>> html2Str   -<< undefined
        setVal _transaction_http_response_body   html_str           -<< in_ta
    where
        content sid =
            [
              heading 1 "Janus Shop Servlet"
            , paragraph "This is the shop example start page. Please choose to login, if you already registered with us, otherwise please enter your registration data." ""
            , heading 3 "New Customer"
            , (form "reg" "/shop"
                += (block "" += formHidden "sessionid" sid)
                += (block "" += formHidden "operation" "reg")
                += (block "" +>> [text "User" "", formText "regname" "Name" 25])
                += (block "" +>> [text "Password" "", formPass "regpass" 25])
                += (block "" += formButton "action" "Register")
                )
            , heading 3 "Existing Customer"
            , (form "login" "/shop"
                += (block "" += formHidden "sessionid" sid)
                += (block "" += formHidden "operation" "login")
                += (block "" +>> [text "User" "", formText "username" "" 25])
                += (block "" +>> [text "Password" "", formPass "password" 25])
                += (block "" += formButton "action" "Login")
                )
            ]

{- |
A shader to register a new user and immediately log him in.
-}
registerShader :: ShaderCreator
registerShader =
    mkStaticCreator $
    proc in_ta -> do
        user    <- getValDef (_transaction_http_request_cgi_ "@regname") ""  -<  in_ta
        pass    <- getValDef (_transaction_http_request_cgi_ "@regpass") ""  -<  in_ta
        ( if user /= ""
          then ("/global/userdb/" ++ user ++ "/password") <-! pass
          else this
          )                                                                   -<< ()
        ( setVal (_transaction_http_request_cgi_ "@username") user
	  >>>
          setVal (_transaction_http_request_cgi_ "@password") pass
          >>>
          setVal (_transaction_http_request_cgi_ "@operation") "login"
          )                                                                    -<< in_ta

{- |
A shader to log an existing user in.
-}
loginShader :: ShaderCreator
loginShader =
    mkStaticCreator $
    proc in_ta -> do
        user    <- getValDef (_transaction_http_request_cgi_ "@username") ""   -< in_ta
        pass    <- getValDef (_transaction_http_request_cgi_ "@password") ""   -< in_ta

        ifA (proc in_ta' -> do
                pass' <- getSVS ("/global/userdb/" ++ user ++ "/password")  -<< ()
                (if pass == pass'
                    then this
                    else zeroArrow) -<< in_ta'
                )
            ( setVal (_transaction_http_request_cgi_ "@operation") "catalogue"
              >>>
              setVal (_transaction_http_request_cgi_ "@authuser") user
            )
            ( setVal (_transaction_http_request_cgi_ "@operation") "init"
            )                                                                  -<< in_ta

{- |
A shader to log an existing user out.
-}
logoutShader :: ShaderCreator
logoutShader =
    mkStaticCreator $
    proc in_ta -> do
        delVal _transaction_session_state_authuser
        >>>
        setVal (_transaction_http_request_cgi_ "@operation") "init"    -<  in_ta

{- |
A shader to display the shop's catalogue. The catalogue is taken from the /catalogue node in the "local" scope.
-}
catalogueShader :: ShaderCreator
catalogueShader =
    mkStaticCreator $
    proc in_ta -> do
        sid      <- getValDef _transaction_session_sessionid "0"         -<  in_ta
        user     <- getValDef _transaction_session_state_authuser ""     -<  in_ta
        prods    <- listA $ listStateTrees "/local/catalogue"             -<  ()

        htmltree <- shopPage [] [] (content sid user (list sid prods)) []   -<< undefined
        htmlStr <- html2Str                                                 -<  htmltree
        setVal _transaction_http_response_body htmlStr                      -<< in_ta
    where
        content sid user elements =
            [
              heading 1 ("Welcome user '" ++ user ++ "'. Please select your product of interest:")
            , elements
            , shopSubmit sid "cart" "Cart"
            , shopSubmit sid "logout" "Logout"
            ]
        list sid products   =
            table ""
                += (row "" +>> [cell "" += text "Article" "", cell "" += text "&nbsp;" ""])
                +>> list' sid products
        list' _ []          =
            [none]
        list' sid (name:xs) =
            (row "" +>> [cell "" += text name ""
                    , cell "" += link ("/shop?article=" ++ name ++ "&operation=article&sessionid=" ++ sid) "Details"])
                :(list' sid xs)

{- |
A shader to display a single article.
-}
articleShader :: ShaderCreator
articleShader =
    mkStaticCreator $
    proc in_ta -> do
        sid     <- getValDef _transaction_session_sessionid "0"          -<  in_ta
        article <- getValDef (_transaction_http_request_cgi_ "@article") ""    -<  in_ta
        price   <- getSVS ("/local/catalogue/" ++ article ++ "/price")      -<< ()

        htmltree    <- shopPage [] []
                (content sid article (describe article price)) []           -<< undefined
        htmlStr <- html2Str                                                 -<  htmltree
        setVal _transaction_http_response_body htmlStr                      -<< in_ta
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
            , shopSubmit sid "catalogue" "Catalogue"
            ]
        describe name price =
            table ""
                += (row "" +>> [cell "" += text "Article" "", cell "" += text name ""])
                += (row "" +>> [cell "" += text "Price" "", cell "" += text price ""])

{- |
A shader to add an article (with amount) to the shopping cart.
-}
addItemShader :: ShaderCreator
addItemShader =
    mkStaticCreator $
    proc in_ta -> do
        article <- getValDef (_transaction_http_request_cgi_ "@article") ""      -<  in_ta
        (amount :: Int)
                <- ( getVal (_transaction_http_request_cgi_ "@amount") >>> parseA )
                   `orElse` constA 1                                            -<  in_ta
        (price :: Int) <- getSVP ("/local/catalogue/" ++ article ++ "/price")   -<< ()

        ( setVal (_cart_ $ article ++ "/@amount") (show amount)
          >>>
          setVal (_cart_ $ article ++ "/@price") (show price)
          >>>
          setVal (_transaction_http_request_cgi_ "@operation") cart
          )                                                                   -<< in_ta

{- |
A shader to remove an article from the shopping cart.
-}
delItemShader :: ShaderCreator
delItemShader =
    mkStaticCreator $
    proc in_ta -> do
        article <- getValDef (_transaction_http_request_cgi_ "@article") ""    -<  in_ta

        ( delVal (_cart_ article)
          >>>
          setVal (_transaction_http_request_cgi_ "@operation") cart
            )                                                               -<< in_ta

{- |
A shader to display the shopping cart.
-}
cartShader :: ShaderCreator
cartShader =
    mkStaticCreator $
    proc in_ta -> do
        sid     <- getValDef _transaction_session_sessionid "0"       -<  in_ta
        user    <- getValDef _transaction_session_state_authuser ""   -<  in_ta
        prods   <- listA $ (proc in_ta' -> do
                name    <- listVals (_cart_ "*")                                    -<  in_ta'
                (amount :: Int) <- getVal (_cart_ $ name ++ "/@amount") >>> parseA  -<< in_ta'
                (price :: Int)  <- getVal (_cart_ $ name ++ "/@price")  >>> parseA  -<< in_ta'
                returnA                                                  -<  (name, price, amount)
                )                                                        -<  in_ta
        let value = foldr (\(_, price, amount) cum -> cum + price * amount) 0 prods
        htmltree <- shopPage [] [] (content sid user (list sid prods) value) [] -<< undefined
        htmlStr  <- html2Str                                             -<  htmltree
        setVal _transaction_http_response_body htmlStr                   -<< in_ta
    where
        content sid user elements value =
            [
              heading 1 ("Cart of user '" ++ user ++ "'")
            , elements
            , heading 3 ("Shopping cart value: " ++ (show value))
            , shopSubmit sid "catalogue" "Catalogue"
            , shopSubmit sid "checkout" "Checkout"
            ]
        list sid products =
            table ""
	    += ( row ""
		 +>> [ cell "" += text "Article" ""
		     , cell "" += text "Price" ""
		     , cell "" += text "amount" ""
		     , cell "" += text "&nbsp;" ""
		     ]
	       )
            +>> list' sid products
        list' _ []        =
            [none]
        list' sid ((name,price,amount):xs)  =
            (row "" +>> [cell "" += text name "", cell "" += text (show price) "", cell "" += text (show amount) ""
                    , cell "" += link ("/shop?article=" ++ name ++ "&operation=delItem&sessionid=" ++ sid) "Remove"])
                :(list' sid xs)

{- |
A shader to display the checkout page.
-}
checkoutShader :: ShaderCreator
checkoutShader =
    mkStaticCreator $
    proc in_ta -> do
        sid     <- getValDef _transaction_session_sessionid "0"          -<  in_ta
        user    <- getValDef _transaction_session_state_authuser ""      -<  in_ta
        prods   <- listA $ (proc in_ta' -> do
                name    <- listVals (_cart_ "*")                                   -<  in_ta'
                (amount :: Int) <- getVal (_cart_ $ name ++ "/@amount") >>> parseA -<< in_ta'
                (price :: Int)  <- getVal (_cart_ $ name ++ "/@price")  >>> parseA -<< in_ta'
                returnA                                                     -<  (name, price, amount)
                )                                                           -<  in_ta
        let value = foldr (\(_, price, amount) cum -> cum + price * amount) 0 prods
        htmltree <- shopPage [] [] (content sid user value) []              -<< undefined
        htmlStr <- html2Str                                                 -<  htmltree
        setVal _transaction_http_response_body htmlStr                      -<< in_ta
    where
        content sid user value =
            [
              heading 1 ("Checkout for user '" ++ user ++ "'")
            , heading 3 ("Shopping cart value: " ++ (show value))
            , (form "checkout" "/shop"
                += (block "" += formHidden "sessionid" sid)
                += (block "" += formHidden "operation" "pay")
                += (block "" +>> [text "Credit Card" "", formText "cardnumber" "" 10])
                += (block "" += formButton "action" "Pay")
                )
            ]

{- |
A shader to perform the payment of a shopping cart.
-}
paymentShader :: ShaderCreator
paymentShader =
    mkStaticCreator $
    proc in_ta -> do
        sid     <- getValDef _transaction_session_sessionid "0"      -<  in_ta
        state   <- getTree   _transaction_session_state                 -<  in_ta
        ("/local/orders/_" ++ sid) <$! (XmlVal state)                   -<< ()
        ( delTree _cart
          >>>
          setVal (_transaction_http_request_cgi_ "@operation") "catalogue"
            )                               -<  in_ta

{- |
A template Arrow to easily create instances of the shop's HTML layout.
-}
shopPage :: [XmlTransform s] -> [XmlTransform s] -> [XmlTransform s] -> [XmlTransform s] -> XmlTransform s
shopPage nav page_head page_content page_bottom =
    html
        += (headers
            += style_def "css/uhl.css"
            += title "Janus Shop Application Demo"
            )
        += (htmlbody
            += (table ""
                += (row "" +>> [cell "", cell "" +>> page_head])
                += (row "" +>> [cell "" +>> nav, cell "" +>> page_content])
                += (row "" +>> [cell "", cell "" +>> page_bottom])
                )
            )

{- |
A template Arrow to easily create a web form.
-}
shopSubmit :: String -> String -> String -> XmlTransform s
shopSubmit sid operation caption =
    (form operation "/shop"
        += (block "" += formHidden "sessionid" sid)
        += (block "" += formHidden "operation" operation)
        += (block "" += formButton "action" caption)
        )
