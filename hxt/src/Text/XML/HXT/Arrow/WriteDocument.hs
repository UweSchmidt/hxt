-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.WriteDocument
   Copyright  : Copyright (C) 2005-9 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Compound arrow for writing XML documents

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.WriteDocument
    ( writeDocument
    , writeDocumentToString
    , prepareContents
    )
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.Edit                  ( escapeHtmlDoc
                                                , escapeXmlDoc
                                                , haskellRepOfXmlDoc
                                                , indentDoc
                                                , addDefaultDTDecl
                                                , preventEmptyElements
                                                , removeDocWhiteSpace
                                                , treeRepOfXmlDoc
                                                )

import Text.XML.HXT.Arrow.DocumentOutput        ( putXmlDocument
                                                , encodeDocument
                                                , encodeDocument'
                                                )

-- ------------------------------------------------------------

{- |
the main filter for writing documents

this filter can be configured by an option list like 'Text.XML.HXT.Arrow.ReadDocument.readDocument'

usage: @ writeDocument optionList destination @

if @ destination @ is the empty string or \"-\", stdout is used as output device

available options are

* 'a_indent' : indent document for readability, (default: no indentation)

- 'a_remove_whitespace' : remove all redundant whitespace for shorten text (default: no removal)

- 'a_output_encoding' : encoding of document, default is 'a_encoding' or 'utf8'

- 'a_output_xml' : (default) issue XML: quote special XML chars \>,\<,\",\',& where neccessary
                   add XML processing instruction
                   and encode document with respect to 'a_output_encoding',
                   if explicitly switched of, the plain text is issued, this is useful
                   for non XML output, e.g. generated Haskell code, LaTex, Java, ...

- 'a_output_html' : issue XHTML: quote alle XML chars, use HTML entity refs or char refs for none ASCII chars

- 'a_no_empty_elements' : do not write the short form \<name .../\> for empty elements. When 'a_output_html' is set,
                          the always empty HTML elements are still written in short form, but not the others, as e.g. the script element.
                          Empty script elements, like \<script href=\"...\"/\>, are always a problem for firefox and others.
                          When XML output is generated with this option, all empty elements are written in the long form.

- 'a_no_empty_elem_for' : do not generate empty elements for the element names given in the comma separated list of this option value.
                          This option overwrites the above described 'a_no_empty_elements' option

- 'a_add_default_dtd' : if the document to be written was build by reading another document containing a Document Type Declaration,
                        this DTD is inserted into the output document (default: no insert)

- 'a_no_xml_pi' : suppress generation of \<?xml ... ?\> processing instruction

- 'a_show_tree' : show tree representation of document (for debugging)

- 'a_show_haskell' : show Haskell representaion of document (for debugging)

 a minimal main program for copying a document
 has the following structure:

> module Main
> where
>
> import Text.XML.HXT.Arrow
>
> main        :: IO ()
> main
>     = do
>       runX ( readDocument  [] "hello.xml"
>              >>>
>              writeDocument [] "bye.xml"
>            )
>       return ()

an example for copying a document to standard output with tracing and evaluation of
error code is:

> module Main
> where
>
> import Text.XML.HXT.Arrow
> import System.Exit
>
> main        :: IO ()
> main
>     = do
>       [rc] <- runX ( readDocument  [ (a_trace, "1")
>                                    ] "hello.xml"
>                      >>>
>                      writeDocument [ (a_output_encoding, isoLatin1)
>                                    ] "-"        -- output to stdout
>                      >>>
>                      getErrStatus
>                    )
>       exitWith ( if rc >= c_err
>                  then ExitFailure 1
>                  else ExitSuccess
>                )
-}

writeDocument   :: Attributes -> String -> IOStateArrow s XmlTree XmlTree
writeDocument userOptions dst
    = perform ( traceMsg 1 ("writeDocument: destination is " ++ show dst)
                >>>
                prepareContents userOptions encodeDocument
                >>>
                putXmlDocument textMode dst
                >>>
                traceMsg 1 "writeDocument: finished"
              )
      `when`
      documentStatusOk
    where
    textMode    = optionIsSet a_text_mode userOptions

-- ------------------------------------------------------------

-- |
-- Convert a document into a string. Formating is done the same way
-- and with the same options as in 'writeDocument'. Default output encoding is
-- no encoding, that means the result is a normal unicode encode haskell string.
-- The default may be overwritten with the 'Text.XML.HXT.XmlKeywords.a_output_encoding' option.
-- The XML PI can be suppressed by the 'Text.XML.HXT.XmlKeywords.a_no_xml_pi' option.
--
-- This arrow fails, when the encoding scheme is not supported.
-- The arrow is pure, it does not run in the IO monad.
-- The XML PI is suppressed, if not explicitly turned on with an
-- option @ (a_no_xml_pi, v_0) @

writeDocumentToString   :: ArrowXml a => Attributes  -> a XmlTree String
writeDocumentToString userOptions
    = prepareContents ( addEntries
                        userOptions
                        [ (a_output_encoding, unicodeString)
                        , (a_no_xml_pi, v_1)
                        ]
                      ) encodeDocument'
      >>>
      xshow getChildren

-- ------------------------------------------------------------

-- |
-- indent and format output

prepareContents :: ArrowXml a => Attributes -> (Bool -> String -> a XmlTree XmlTree) -> a XmlTree XmlTree
prepareContents userOptions encodeDoc
    = indent
      >>>
      addDtd
      >>>
      format
    where
    formatEmptyElems
        | not (null noEmptyElemFor)     = preventEmptyElements noEmptyElemFor
        | hasOption a_no_empty_elements
          ||
          hasOption a_output_xhtml      = preventEmptyElements []
        | otherwise                     = const this
    addDtd
        | hasOption a_add_default_dtd   = addDefaultDTDecl
        | otherwise                     = this
    indent
        | hasOption a_indent            = indentDoc                     -- document indentation
        | hasOption a_remove_whitespace = removeDocWhiteSpace           -- remove all whitespace between tags
        | otherwise                     = this

    format
        | hasOption a_show_tree         = treeRepOfXmlDoc
        | hasOption a_show_haskell      = haskellRepOfXmlDoc
        | hasOption a_output_html       = formatEmptyElems True
                                          >>>
                                          escapeHtmlDoc                 -- escape al XML and HTML chars >= 128
                                          >>>
                                          encodeDoc                     -- convert doc into text with respect to output encoding with ASCII as default
                                            suppressXmlPi ( lookupDef usAscii a_output_encoding options )
        | hasOption a_output_xml        = formatEmptyElems (hasOption a_output_xhtml)
                                          >>>
                                          escapeXmlDoc                  -- escape lt, gt, amp, quot,
                                          >>>
                                          encodeDoc                     -- convert doc into text with respect to output encoding
                                            suppressXmlPi ( lookupDef "" a_output_encoding options )
        | otherwise                     = this

    suppressXmlPi                                                       -- remove <?xml ... ?> when set
        = hasOption a_no_xml_pi

    noEmptyElemFor
        = words
          . map (\ c -> if c == ',' then ' ' else c)
          . lookup1 a_no_empty_elem_for
          $ options

    hasOption n
        = optionIsSet n options

    options = addEntries
              userOptions
              [ ( a_indent,             v_0 )
              , ( a_remove_whitespace,  v_0 )
              , ( a_output_xml,         v_1 )
              , ( a_show_tree,          v_0 )
              , ( a_show_haskell,       v_0 )
              , ( a_output_html,        v_0 )
              , ( a_output_xhtml,       v_0 )
              , ( a_no_xml_pi,          v_0 )
              , ( a_no_empty_elements,  v_0 )
              , ( a_no_empty_elem_for,  ""  )
              , ( a_add_default_dtd,    v_0 )
              ]

-- ------------------------------------------------------------
