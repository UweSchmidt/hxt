-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.WriteDocument
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: WriteDocument.hs,v 1.8 2006/11/09 20:27:42 hxml Exp $

Compound arrow for writing XML documents

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.WriteDocument
    ( writeDocument
    , writeDocumentToString
    )
where

import Control.Arrow				-- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.Edit
    ( indentDoc
    , removeDocWhiteSpace
    , treeRepOfXmlDoc
    , haskellRepOfXmlDoc
    , escapeXmlDoc
    , escapeHtmlDoc
    )

import Text.XML.HXT.Arrow.DocumentOutput
    ( putXmlDocument
    , encodeDocument
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

writeDocument	:: Attributes -> String -> IOStateArrow s XmlTree XmlTree
writeDocument userOptions dst
    = perform ( traceMsg 1 ("writeDocument: destination is " ++ show dst)
		>>>
		prepareContents userOptions
		>>>
		putXmlDocument dst
		>>>
		traceMsg 1 "writeDocument: finished"
	      )
      `when`
      documentStatusOk

-- ------------------------------------------------------------

-- |
-- Convert a document into a string. Formating is done the same way
-- and with the same options as in 'writeDocument'.

writeDocumentToString	:: Attributes  ->  IOStateArrow s XmlTree String
writeDocumentToString userOptions
    = prepareContents userOptions
      >>>
      xshow getChildren

-- ------------------------------------------------------------

prepareContents	:: Attributes -> IOStateArrow s XmlTree XmlTree
prepareContents userOptions
    = indent
      >>>
      format
    where
    indent
	| hasOption a_indent		= indentDoc			-- document indentation
	| hasOption a_remove_whitespace	= removeDocWhiteSpace		-- remove all whitespace between tags
	| otherwise			= this

    format
	| hasOption a_show_tree		= treeRepOfXmlDoc
	| hasOption a_show_haskell	= haskellRepOfXmlDoc
	| hasOption a_output_html	= escapeHtmlDoc			-- escape al XML and HTML chars >= 128
					  >>>
					  encodeDocument		-- convert doc into text with respect to output encoding with ASCII as default
					    suppressXmlPi ( lookupDef usAscii a_output_encoding options )
	| hasOption a_output_xml	= escapeXmlDoc			-- escape lt, gt, amp, quot, 
					  >>>
					  encodeDocument		-- convert doc into text with respect to output encoding
					    suppressXmlPi ( lookupDef "" a_output_encoding options )
	| otherwise			= this

    suppressXmlPi							-- remove <?xml ... ?> when set
	= hasOption a_no_xml_pi

    hasOption n
	= optionIsSet n options

    options = addEntries 
              userOptions 
	      [ ( a_indent,		v_0 )
	      , ( a_remove_whitespace,	v_0 )
	      , ( a_output_xml,		v_1 )
	      , ( a_show_tree,		v_0 )
	      , ( a_show_haskell,	v_0 )
	      , ( a_output_html,	v_0 )
	      , ( a_no_xml_pi,          v_0 )
	      ]

-- ------------------------------------------------------------
