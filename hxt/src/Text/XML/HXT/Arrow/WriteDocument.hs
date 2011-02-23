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
import Text.XML.HXT.Arrow.Edit                  ( haskellRepOfXmlDoc
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
--

{- |
the main filter for writing documents

this filter can be configured by an option list like 'Text.XML.HXT.Arrow.ReadDocument.readDocument'

usage: @ writeDocument optionList destination @

if @ destination @ is the empty string or \"-\", stdout is used as output device

for available options see 'Text.XML.HXT.Arrow.XmlState.SystemConfig'


- @withOutputXML@ :
 (default) issue XML: quote special XML chars \>,\<,\",\',& where neccessary
                   add XML processing instruction
                   and encode document with respect to output encoding,

- @withOutputHTML@ :
 issue HTML: translate all special XML chars and all HTML chars with a corresponding entity reference
 into entity references. Do not generate empty elements, e.g. @<script .../>@ for HTML elements, that are allowed
 to contain a none empty body. Result is for the example is @<script ...></script>@.
 The short form introduces trouble in various browsers.

- @withOutputXHTML@ :
 same as @withOutputHTML@, but all none ASCII chars are substituted by char references.

- @withOutputPLAIN@ :
 Do not substitute any chars. This is useful when generating something else than XML/HTML, e.g. Haskell source code.

- @withXmlPi yes/no@ :
 Add a @<?xml version=... encoding=... ?>@ processing instruction to the beginning of the document.
 Default is yes.

- @withAddDefaultDTD@ :
  if the document to be written was build by reading another document containing a Document Type Declaration,
  this DTD is inserted into the output document (default: no insert)

- @withShowTree yes/no@ :
  show DOM tree representation of document (for debugging)

- @withShowHaskell yes/no@ :
  show Haskell representaion of document (for debugging)

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

an example for copying a document from the web to standard output with global trace level 1, input trace level 2,
output encoding isoLatin1,
and evaluation of
error code is:

> module Main
> where
>
> import Text.XML.HXT.Core
> import Text.XML.HXT.Curl
> -- or
> -- import Text.XML.HXT.HTTP
> import System.Exit
>
> main        :: IO ()
> main
>     = do
>       [rc] <- runX
>               ( configSysVars [ withTrace 1          -- set the defaults for all read-,
>                               , withCurl []          -- write- and other operations
>                                 -- or withHTTP []
>                               ]
>                 >>>
>                 readDocument  [ withTrace     2      -- use these additional
>                               , withParseHTML yes    -- options only for this read
>                               ]
>                               "http://www.haskell.org/"
>                 >>>
>                 writeDocument [ withOutputEncoding isoLatin1
>                               ]
>                               ""                     -- output to stdout
>                 >>>
>                 getErrStatus
>               )
>       exitWith ( if rc >= c_err
>                  then ExitFailure 1
>                  else ExitSuccess
>                )
-}

writeDocument           :: SysConfigList -> String -> IOStateArrow s XmlTree XmlTree
writeDocument config dst
    = localSysEnv
      $
      configSysVars config
      >>>
      perform ( (flip writeDocument') dst $< getSysVar theTextMode )

writeDocument'          :: Bool -> String -> IOStateArrow s XmlTree XmlTree
writeDocument' textMode dst
    = ( traceMsg 1 ("writeDocument: destination is " ++ show dst)
        >>>
        ( (flip prepareContents) encodeDocument $< getSysVar idS )
        >>>
        traceDoc "document after encoding"
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
                                        withXmlPi          no            :
                                        config
                                       )
                        $ initialSysState
                      ) encodeDocument'
      >>>
      xshow getChildren

-- ------------------------------------------------------------

-- |
-- indent and format output

prepareContents :: ArrowXml a => XIOSysState -> (Bool -> Bool -> String -> a XmlTree XmlTree) -> a XmlTree XmlTree
prepareContents config encodeDoc
    = indent
      >>>
      addDtd
      >>>
      format
    where
    indent'      = getS theIndent      config
    removeWS'    = getS theRemoveWS    config
    showTree'    = getS theShowTree    config
    showHaskell' = getS theShowHaskell config
    outHtml'     = getS theOutputFmt   config ==  HTMLoutput
    outXhtml'    = getS theOutputFmt   config == XHTMLoutput
    outXml'      = getS theOutputFmt   config ==   XMLoutput
    noPi'        = not $ getS theXmlPi       config
    noEEsFor'    = getS theNoEmptyElemFor  config
    addDDTD'     = getS theAddDefaultDTD   config
    outEnc'      = getS theOutputEncoding  config

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
        | outHtml'                      = preventEmptyElements noEEsFor' True
                                          >>>
                                          encodeDoc                     -- convert doc into text with respect to output encoding with ASCII as default
                                            False noPi' ( if null outEnc' then usAscii else outEnc' )

        | outXhtml'                     = preventEmptyElements noEEsFor' True
                                          >>>
                                          encodeDoc                     -- convert doc into text with respect to output encoding
                                            True noPi' outEnc'
        | outXml'                       = ( if null noEEsFor'
                                            then this
                                            else preventEmptyElements noEEsFor' False
                                          )
                                          >>>
                                          encodeDoc                     -- convert doc into text with respect to output encoding
                                            True noPi' outEnc'
        | otherwise                     = this

-- ------------------------------------------------------------
