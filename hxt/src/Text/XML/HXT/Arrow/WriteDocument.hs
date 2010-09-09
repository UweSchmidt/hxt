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
    , writeDocument'
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
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
                                                ( initialSysState
                                                )
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
-- TODO

{- |
the main filter for writing documents

this filter can be configured by an option list like 'Text.XML.HXT.Arrow.ReadDocument.readDocument'

usage: @ writeDocument optionList destination @

if @ destination @ is the empty string or \"-\", stdout is used as output device

for available options see 'Text.XML.HXT.Arrow.XmlState.SystemConfig'


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
> import Text.XML.HXT.Core
>
> main        :: IO ()
> main
>     = do
>       runX ( readDocument  [] "hello.xml"
>              >>>
>              writeDocument [] "bye.xml"
>            )
>       return ()

an example for copying a document to standard output with global trace level 1, input trace level 2,
output encoding isoLatin1,
and evaluation of
error code is:

> module Main
> where
>
> import Text.XML.HXT.Core
> import System.Exit
>
> main        :: IO ()
> main
>     = do
>       [rc] <- runX ( configSysVars [ withTrace 1 ]
                       >>>
                       readDocument    [ withTrace    2
                                       , withValidate no
>                                      ] "hello.xml"
>                      >>>
>                      writeDocument [ withOutputEncoding isoLatin1
>                                    ] "-"        -- output to stdout
>                      >>>
>                      getErrStatus
>                    )
>       exitWith ( if rc >= c_err
>                  then ExitFailure 1
>                  else ExitSuccess
>                )
-}

writeDocument   	:: SysConfigList -> String -> IOStateArrow s XmlTree XmlTree
writeDocument config dst
    = localSysEnv
      $
      configSysVars config
      >>>
      perform ( (flip writeDocument') dst $< getSysVar theTextMode )

writeDocument'  	:: Bool -> String -> IOStateArrow s XmlTree XmlTree
writeDocument' textMode dst
    = ( traceMsg 1 ("writeDocument: destination is " ++ show dst)
        >>>
        ( (flip prepareContents) encodeDocument $< getSysVar idS )
        >>>
        putXmlDocument textMode dst
        >>>
        traceMsg 1 "writeDocument: finished"
      )
      `when`
      documentStatusOk

-- ------------------------------------------------------------

-- |
-- Convert a document into a string. Formating is done the same way
-- and with the same options as in 'writeDocument'. Default output encoding is
-- no encoding, that means the result is a normal unicode encode haskell string.
-- The default may be overwritten with the 'Text.XML.HXT.Arrow.XmlState.SystemConfig.withOutputEncoding' option.
-- The XML PI can be suppressed by the 'Text.XML.HXT.XmlKeywords.a_no_xml_pi' option.
--
-- This arrow fails, when the encoding scheme is not supported.
-- The arrow is pure, it does not run in the IO monad.
-- The XML PI is suppressed, if not explicitly turned on with an
-- option @ (a_no_xml_pi, v_0) @

writeDocumentToString   :: ArrowXml a => SysConfigList  -> a XmlTree String
writeDocumentToString config
    = prepareContents ( foldr (>>>) id (withOutputEncoding unicodeString :
                                        withNoXmlPi        yes           :
                                        config
                                       )
                        $ initialSysState
                      ) encodeDocument'
      >>>
      xshow getChildren

-- ------------------------------------------------------------

-- |
-- indent and format output

prepareContents :: ArrowXml a => XIOSysState -> (Bool -> String -> a XmlTree XmlTree) -> a XmlTree XmlTree
prepareContents config encodeDoc
    = indent
      >>>
      addDtd
      >>>
      format
    where
    indent'	 = getS theIndent      config
    removeWS'    = getS theRemoveWS    config
    showTree'    = getS theShowTree    config
    showHaskell' = getS theShowHaskell config
    outHtml'     = getS theOutputFmt   config ==  HTMLoutput
    outXhtml'    = getS theOutputFmt   config == XHTMLoutput
    outXml'      = getS theOutputFmt   config ==   XMLoutput
    noPi'        = getS theNoXmlPi     config
    noEEs'       = getS theNoEmptyElements config
    noEEsFor'    = getS theNoEmptyElemFor  config
    addDDTD'     = getS theAddDefaultDTD   config
    outEnc'      = getS theOutputEncoding  config

    formatEmptyElems
        | not (null noEEsFor')          = preventEmptyElements noEEsFor'
        | noEEs'
          ||
          outXhtml'                     = preventEmptyElements []
        | otherwise                     = const this
    addDtd
        | addDDTD'                      = addDefaultDTDecl
        | otherwise                     = this
    indent
        | indent'                       = indentDoc                     -- document indentation
        | removeWS'                     = removeDocWhiteSpace           -- remove all whitespace between tags
        | otherwise                     = this

    format
        | showTree'                     = treeRepOfXmlDoc
        | showHaskell'                  = haskellRepOfXmlDoc
        | outHtml'                      = formatEmptyElems True
                                          >>>
                                          escapeHtmlDoc                 -- escape al XML and HTML chars >= 128
                                          >>>
                                          encodeDoc                     -- convert doc into text with respect to output encoding with ASCII as default
                                            noPi' ( if null outEnc' then usAscii else outEnc' )
        | outXml'                       = formatEmptyElems outXhtml'
                                          >>>
                                          escapeXmlDoc                  -- escape lt, gt, amp, quot,
                                          >>>
                                          encodeDoc                     -- convert doc into text with respect to output encoding
                                            noPi' outEnc'
        | otherwise                     = this

-- ------------------------------------------------------------
