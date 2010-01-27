-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.HTMLBuilder
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: HTMLBuilder.hs, v1.0 2006/11/02 00:00:00 janus Exp $

   Janus HTML Builder

   A set of Arrow functions to create an abstract representation of HTML documents. These Arrows base on
   HXT XML Arrows to represent XHTML.

-}

-- ------------------------------------------------------------

{-# LANGUAGE Arrows#-}

module Network.Server.Janus.HTMLBuilder
   (
   -- combinators and converters
     html2Str
   , (+>>)

   -- constructors
   , html
   , headers
   , style_def
   , title
   , htmlbody
   , (|-|)
   , block
   , paragraph
   , link
   , text
   , heading
   , image
   , table
   , row
   , cell
   , form
   , formHidden
   , formText
   , formPass
   , formArea
   , formButton
   , formSelection
   )
where

import Text.XML.HXT.Arrow

import Network.Server.Janus.XmlHelper

infixl 7 +>>

{- |
Converts a XHTML tree to a string
-}
html2Str :: XmlAccess s String
html2Str =
    proc htmltree -> do
        str   <- xshow (constA htmltree) -<< ()
        returnA -< "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n" ++ str

{- |
Inserts a list of XML subtree arrows into an XML arrow
-}
(+>>) :: XmlTransform s -> [XmlTransform s] -> XmlTransform s
(+>>) father children =
    foldl (\arrow element -> arrow += element) father children

{- |
Constructs an XHTML root node arrow.
-}
html :: XmlSource s a
html =
    (eelem "html"
        += sattr "xmlns" "http://www.w3.org/1999/xhtml"
        += sattr "xml:lang" "en"
        += sattr "lang" "en"
        )

{- |
Constructs an XHTML head node Arrow.
-}
headers :: XmlTransform s
headers =
    (eelem "head")

{- |
Constructs a style definition node, where the argument defines the stylefile in question.
-}
style_def :: String -> XmlTransform s
style_def stylefile =
    (eelem "link"
        += sattr "href" stylefile
        += sattr "rel"  "stylesheet"
        += sattr "type" "text/css"
        )

{- |
Constructs a title definition node, where the argument defines the title.
-}
title :: String -> XmlTransform s
title title_str =
    (eelem "title"
        += txt title_str
        )

{- |
Constructs an XHTML body node.
-}
htmlbody :: XmlTransform s
htmlbody =
    (eelem "body")

{- |
Operator to construct a block constituted by a list of XML Arrows, which is enclosed by a DIV
element. The argument defines the DIV's element style class.
-}
(|-|) :: String -> [XmlTransform s] -> XmlTransform s
(|-|) style styled_block =
    (eelem "div"
        += (if style == "" then none else sattr "class" style)
        +>> styled_block
        )

{- |
Constructs a DIV node. The argument defines the DIV's element style class.
-}
block :: String -> XmlTransform s
block style =
    (eelem "div"
        += (if style == "" then none else sattr "class" style)
        )

{- |
Constructs a P node. The first argument defines the style class, the second argument
defines the text content.
-}
paragraph :: String -> String -> XmlTransform s
paragraph content style =
    (eelem "p"
        += (if style == "" then none else sattr "class" style)
        += (if content  == "" then none else txt content)
        )

{- |
Constructs a hyperlink (A) node. The first argument defines the link URI, the second argument
defines the link's text.
-}
link :: String -> String -> XmlTransform s
link uri caption =
    (eelem ("a")
        += sattr "href"  uri
        += txt caption
        )

{- |
Constructs a text node, where the content is denoted by the first argument. If a second argument \/= \"\" is provided,
a span element with the second argument as its style encloses the new text.
-}
text :: String -> String -> XmlTransform s
text content style =
    (if style == "" then txt content else eelem "span" += sattr "class" style += txt content)

{- |
Constructs a head node, where the content is denoted by the second argument. The first argument defines the head level,
e.g. delivering a H1 element for a first argument of value 1..
-}
heading :: Int -> String -> XmlTransform s
heading level title_str =
    (eelem ("h" ++ (show level))
        += txt title_str
        )

{- |
Constructs an image node, where the file is denoted by the first argument. The second argument provides a description
(translates into an alt attribute), the third argument provides a style definition (ignored if it equals the empty string).
-}
image :: String -> String -> String -> XmlTransform s
image file desc style =
    (eelem "img"
        += sattr "href"  file
        += sattr "alt"   desc
        += (if style == "" then none else sattr "class" style)
        )

{- |
Constructs a table with no border, where the argument provides a style definition (ignored if it equals the empty string).
-}
table :: String -> XmlTransform s
table style =
    (eelem "table"
        += sattr "border" "0"
        += (if style == "" then none else sattr "class" style)
        )

{- |
Constructs a table row, where the argument provides a style definition (ignored if it equals the empty string).
-}
row :: String -> XmlTransform s
row style =
    (eelem "tr"
        += (if style == "" then none else sattr "class" style)
        )

{- |
Constructs a table cell, where the argument provides a style definition (ignored if it equals the empty string).
-}
cell :: String -> XmlTransform s
cell style =
    (eelem "td"
        += (if style == "" then none else sattr "class" style)
        )

{- |
Constructs an HTML GET form, where the first argument defines the name and the second the action.
-}
form :: String -> String -> XmlTransform s
form name action =
    (eelem "form"
        += sattr "method" "get"
        += sattr "name"   name
        += (if action == "" then none else sattr "action" action)
        )

{- |
Constructs a hidden input value for an HTML form, where the first argument defines the name and the second the value.
-}
formHidden :: String -> String -> XmlTransform s
formHidden name value =
    (eelem "input"
        += sattr "type"  "hidden"
        += sattr "name"  name
        += sattr "value" value
        )

{- |
Constructs a text input value for an HTML form, where the first argument defines the name, the second the value and the
third the maximum length.
-}
formText :: String -> String -> Int -> XmlTransform s
formText name value size =
    (eelem "input"
        += sattr "type"  "text"
        += sattr "size"  (show size)
        += sattr "name"  name
        += sattr "value" value
        )

{- |
Constructs a password input value for an HTML form, where the first argument defines the name and the second the maximum length.
-}
formPass :: String -> Int -> XmlTransform s
formPass name size =
    (eelem "input"
        += sattr "type"  "password"
        += sattr "size"  (show size)
        += sattr "name"  name
        )

{- |
Constructs an area input value for an HTML form, where the first argument defines the name, the second the value, the third
the number of columns and the fourth the number of rows.
-}
formArea :: String -> String -> Int -> Int -> XmlTransform s
formArea name value cols rows =
    (eelem "textarea"
        += sattr "cols"  (show cols)
        += sattr "rows"  (show rows)
        += sattr "name"  name
        += txt   value
        )

{- |
Constructs a submit button for an HTML form, where the first argument defines the name and the second the value.
-}
formButton :: String -> String -> XmlTransform s
formButton name value =
    (eelem "input"
        += sattr "type"  "submit"
        += sattr "name"  name
        += sattr "value" value
        )

{- |
Constructs a selection field for an HTML form, where the first argument defines the name, the second the size and the
third a list of name-value pairs.
-}
formSelection :: String -> Int -> [(String, String)] -> XmlTransform s
formSelection name size opts =
    (eelem "select"
        += sattr "name"  name
        += sattr "size"  (show size)
        += options opts
        )
    where
        options []              = this
        options ((opt, val):_)  =
                (eelem "option"
                    += sattr "value" val
                    += txt opt
                    )
