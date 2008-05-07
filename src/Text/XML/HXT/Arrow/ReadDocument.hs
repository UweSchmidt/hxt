-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.ReadDocument
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ReadDocument.hs,v 1.10 2006/11/24 07:41:37 hxml Exp $

Compound arrows for reading an XML\/HTML document or an XML\/HTML string

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.ReadDocument
    ( readDocument
    , readFromDocument
    , readString
    , readFromString
    , hread
    , xread
    )
where

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.Edit
    ( canonicalizeAllNodes
    , canonicalizeForXPath
    , canonicalizeContents
    , removeDocWhiteSpace
    )

import Text.XML.HXT.Arrow.ParserInterface
    
import Text.XML.HXT.Arrow.ProcessDocument
    ( getDocumentContents
    , parseXmlDocument
    , parseHtmlDocument
    , propagateAndValidateNamespaces
    )

import Text.XML.HXT.RelaxNG.Validator
    ( validateDocumentWithRelaxSchema
    )

import Data.Char
    ( toLower
    )

-- ------------------------------------------------------------

{- |
the main document input filter

this filter can be configured by an option list, a value of type "Attributes"

available options:

* 'a_parse_html': use HTML parser, else use XML parser (default)

- 'a_tagsoup' : use light weight and lasy parser based on tagsoup lib

- 'a_parse_by_mimetype' : select the parser by the mime type of the document
                          (pulled out of the HTTP header). When the mime type is set to \"text\/html\"
			  the HTML parser (parsec or tagsoup) is taken, when it\'s set to
			  \"text\/xml\" or \"text\/xhtml\" the XML parser (parsec or tagsoup) is taken.
			  If the mime type is something else no further processing is performed,
			  the contents is given back to the application in form of a single text node.
			  If the default document encoding ('a_encoding') is set to isoLatin1, this even enables processing
			  of arbitray binary data.

- 'a_validate' : validate document againsd DTD (default), else skip validation

- 'a_relax_schema' : validate document with Relax NG, the options value is the schema URI
                     this implies using XML parser, no validation against DTD, and canonicalisation

- 'a_check_namespaces' : check namespaces, else skip namespace processing (default)

- 'a_canonicalize' : canonicalize document (default), else skip canonicalization

- 'a_preserve_comment' : preserve comments during canonicalization, else remove comments (default)

- 'a_remove_whitespace' : remove all whitespace, used for document indentation, else skip this step (default)

- 'a_indent' : indent document by inserting whitespace, else skip this step (default)

- 'a_issue_warnings' : issue warnings, when parsing HTML (default), else ignore HTML parser warnings

- 'a_issue_errors' : issue all error messages on stderr (default), or ignore all error messages

- 'a_ignore_encoding_errors': ignore all encoding errors, default is issue all encoding errors

- 'a_trace' : trace level: values: 0 - 4

- 'a_proxy' : proxy for http access, e.g. www-cache:3128

- 'a_use_curl' : for http access via external programm curl, default is native HTTP access

- 'a_options_curl' : more options for external program curl

- 'a_encoding' : default document encoding ('utf8', 'isoLatin1', 'usAscii', 'iso8859_2', ... , 'iso8859_16', ...)

All attributes not evaluated by readDocument are stored in the created document root node for easy access of the various
options in e.g. the input\/output modules

If the document name is the empty string or an uri of the form \"stdin:\", the document is read from standard input.

examples:

> readDocument [ ] "test.xml"

reads and validates a document \"test.xml\", no namespace propagation, only canonicalization is performed 

> readDocument [ (a_validate, "0")
>              , (a_encoding, isoLatin1)
>              ] "test.xml"

reads document \"test.xml\" without validation, default encoding 'isoLatin1'.


> readDocument [ (a_parse_html, "1")
>              , (a_encoding, isoLatin1)
>              , (a_tagsoup,    "1")
>              ] ""

reads a HTML document from standard input, no validation is done when parsing HTML, default encoding is 'isoLatin1',
parsing is done with tagsoup parser

> readDocument [ (a_parse_html,     "1")
>              , (a_proxy,          "www-cache:3128")
>              , (a_curl,           "1")
>              , (a_issue_warnings, "0")
>              ] "http://www.haskell.org/"

reads Haskell homepage with HTML parser ignoring any warnings, with http access via external program curl and proxy \"www-cache\" at port 3128

> readDocument [ (a_validate,          "1")
>              , (a_check_namespace,   "1")
>              , (a_remove_whitespace, "1")
>              , (a_trace,             "2")
>              ] "http://www.w3c.org/"

read w3c home page (xhtml), validate and check namespaces, remove whitespace between tags, trace activities with level 2

for minimal complete examples see 'Text.XML.HXT.Arrow.WriteDocument.writeDocument' and 'runX', the main starting point for running an XML arrow.
-}

readDocument	:: Attributes -> String -> IOStateArrow s b XmlTree
readDocument userOptions src
    = traceLevel
      >>>
      addInputOptionsToSystemState
      >>>
      getDocumentContents remainingOptions src
      >>>
      ( processDoc $< getMimeType )
      >>>
      traceMsg 1 ("readDocument: " ++ show src ++ " processed")
      >>>
      traceSource
      >>>
      traceTree
    where
    options
 	= addEntries userOptions defaultOptions

    defaultOptions
	= [ ( a_parse_html,		v_0 )
	  , ( a_tagsoup,		v_0 )
	  , ( a_validate,		v_1 )
	  , ( a_issue_warnings,		v_1 )
	  , ( a_check_namespaces,	v_0 )
	  , ( a_canonicalize,		v_1 )
	  , ( a_preserve_comment,	v_0 )
	  , ( a_remove_whitespace,	v_0 )
	  , ( a_parse_by_mimetype,	v_0 )
	  , ( a_ignore_encoding_errors, v_0 )
	  ]

    httpOptions
	= [a_proxy, a_use_curl, a_options_curl]

    remainingOptions
	 = options -- filter (not . flip hasEntry defaultOptions . fst) options

    traceLevel
	= maybe this (setTraceLevel . read) . lookup a_trace $ options

    addInputOptionsToSystemState
	= addSysOptions (filter ((`elem` httpOptions) . fst) remainingOptions)
	  where
	  addSysOptions
	      = seqA . map (uncurry setParamString)

    getMimeType
	= getAttrValue transferMimeType >>^ map toLower

    processDoc mimeType
	= traceMsg 1 ("readDocument: " ++ show src ++ " (mime type: " ++ show mimeType ++ ") will be processed")
	  >>>
	  parse
	  >>>
	  ( if isXmlOrHtml
	    then ( checknamespaces
		   >>>
		   canonicalize
		   >>>
		   whitespace
		   >>>
		   relax
		 )
	    else this
	  )
	where
	parse
	    | isHtml
	      || 
	      withTagSoup		= parseHtmlDocument			-- parse as HTML or with tagsoup XML
					  withTagSoup
					  withNamespaces
					  issueW
					  (not (hasOption a_canonicalize) && preserveCmt)
					  removeWS
					  isHtml
	    | validateWithRelax		= parseXmlDocument False		-- for Relax NG use XML parser without validation
	    | isXml			= parseXmlDocument validate		-- parse as XML
	    | otherwise			= this					-- don't parse, mime type is not XML nor HTML
	checknamespaces
	    | (withNamespaces && not withTagSoup)
	      ||
	      validateWithRelax		= propagateAndValidateNamespaces
	    | otherwise			= this
	canonicalize
	    | withTagSoup		= this					-- tagsoup already removes redundant struff
	    | validateWithRelax		= canonicalizeAllNodes
	    | hasOption a_canonicalize
	      &&
	      preserveCmt			= canonicalizeForXPath
	    | hasOption a_canonicalize	= canonicalizeAllNodes
	    | otherwise			= this
	relax
	    | validateWithRelax		= validateDocumentWithRelaxSchema remainingOptions relaxSchema
	    | otherwise			= this
	whitespace
	    | removeWS
	      &&
	      not withTagSoup		= removeDocWhiteSpace			-- tagsoup already removes whitespace
	    | otherwise			= this
	validateWithRelax	= hasEntry a_relax_schema options
	relaxSchema		= lookup1 a_relax_schema options
	parseHtml		= hasOption a_parse_html
	parseXml		= hasOption a_parse_xml
	isHtml			= parseHtml				-- force HTML
				  ||
				  mimeTypeIsHtml
	isXml			= parseXml				-- force XML parsing
				  ||
				  ( not parseHtml
				    &&
				    ( mimeTypeIsXml || null mimeType )	-- mime type is XML or not there
				  )
	isXmlOrHtml	= isHtml || isXml
	parseByMimeType	= hasOption a_parse_by_mimetype
	validate	= hasOption a_validate
	withNamespaces	= hasOption a_check_namespaces
	withTagSoup	= hasOption a_tagsoup
	issueW		= hasOption a_issue_warnings
	removeWS	= hasOption a_remove_whitespace
	preserveCmt	= hasOption a_preserve_comment
	mimeTypeIsHtml	= parseByMimeType && isHtmlMimeType mimeType
	mimeTypeIsXml	= parseByMimeType && isXmlMimeType  mimeType
	hasOption n	= optionIsSet n options

-- ------------------------------------------------------------

-- |
-- the arrow version of 'readDocument', the arrow input is the source URI

readFromDocument	:: Attributes -> IOStateArrow s String XmlTree
readFromDocument userOptions
    = applyA ( arr $ \ s -> readDocument userOptions s )

-- ------------------------------------------------------------

-- |
-- read a document that is stored in a normal Haskell String
--
-- the same function as readDocument, but the parameter forms the input.
-- All options available for 'readDocument' are applicable for readString.
--
-- Default encoding: No encoding is done, the String argument is taken as Unicode string

readString	:: Attributes -> String -> IOStateArrow s b XmlTree
readString userOptions content
    = readDocument ( (a_encoding, unicodeString) : userOptions ) (stringProtocol ++ content)

-- ------------------------------------------------------------

-- |
-- the arrow version of 'readString', the arrow input is the source URI

readFromString	:: Attributes -> IOStateArrow s String XmlTree
readFromString userOptions
    = applyA ( arr $ \ s -> readString userOptions s )

-- ------------------------------------------------------------

-- |
-- parse a string as HTML content, substitute all HTML entity refs and canonicalize tree
-- (substitute char refs, ...). Errors are ignored.
-- 
-- A simpler version of 'readFromString' but with less functionality.
-- Does not run in the IO monad

hread			:: ArrowXml a => a String XmlTree
hread
    = parseHtmlContent
      >>>
      substHtmlEntityRefs
      >>>
      processTopDown ( none `when` isError )
      >>>
      canonicalizeContents

-- |
-- parse a string as XML content, substitute all predefined XML entity refs and canonicalize tree
-- (substitute char refs, ...)

xread			:: ArrowXml a => a String XmlTree
xread
    = parseXmlContent
      >>>
      substXmlEntityRefs
      >>>
      canonicalizeContents

-- ------------------------------------------------------------

