-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.ReadDocument
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

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
    , readDocumentOld	-- TODO throw away
    , readFromDocumentOld
    , readStringOld
    , readFromStringOld
    )
where

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.Edit                  ( canonicalizeAllNodes
                                                , canonicalizeForXPath
                                                , canonicalizeContents
                                                , rememberDTDAttrl
                                                , removeDocWhiteSpace
                                                )

import Text.XML.HXT.Arrow.ParserInterface

import Text.XML.HXT.Arrow.ProcessDocument       ( getDocumentContents
                                                , parseXmlDocument
                                                , parseHtmlDocument
                                                , propagateAndValidateNamespaces
                                                )

import Text.XML.HXT.RelaxNG.Validator           ( validateDocumentWithRelaxSchema )

-- ------------------------------------------------------------

{- |
the main document input filter

this filter can be configured by an option list, a value of type 'Text.XML.HXT.DOM.TypeDefs.Attributes'

available options:

* 'a_parse_html': use HTML parser, else use XML parser (default)

- 'a_tagsoup' : use light weight and lazy parser based on tagsoup lib

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

- 'a_ignore_none_xml_contents': ignore document contents of none XML\/HTML documents.
                                This option can be useful for implementing crawler like applications, e.g. an URL checker.
                                In those cases net traffic can be reduced.

- 'a_trace' : trace level: values: 0 - 4

- 'a_proxy' : proxy for http access, e.g. www-cache:3128

- 'a_redirect' : automatically follow redirected URIs, default is yes

- 'a_use_curl' : obsolete and ignored, HTTP acccess is always done with curl bindings for libcurl

- 'a_strict_input' : file input is done strictly using the 'Data.ByteString' input functions. This ensures correct closing of files, especially when working with
                     the tagsoup parser and not processing the whole input data. Default is off. The @ByteString@ input usually is not faster than the buildin @hGetContents@
                     for strings.

- 'a_options_curl' : deprecated but for compatibility reasons still supported.
                     More options passed to the curl binding.
                     Instead of using this option to set a whole bunch of options at once for curl
                     it is recomended to use the @curl-.*@ options syntax described below.

- 'a_encoding' : default document encoding ('utf8', 'isoLatin1', 'usAscii', 'iso8859_2', ... , 'iso8859_16', ...).
                 Only XML, HTML and text documents are decoded,
                 default decoding for XML\/HTML is utf8, for text iso latin1 (no decoding).
                 The whole content is returned in a single text node.

- 'a_mime_types' : set the mime type table for file input with given file. The format of this config file must be in the syntax of a debian linux \"mime.types\" config file

- 'a_if_modified_since' : read document conditionally, only if the document is newer than the given date and time argument, the contents is delivered,
                          else just the root node with the meta data is returned. The date and time must be given in "System.Locale.rfc822DateFormat".

- curl options : the HTTP interface with libcurl can be configured with a lot of options. To support these options in an easy way, there is a naming convetion:
                 Every option, which has the prefix @curl@ and the rest of the name forms an option as described in the curl man page, is passed to the curl binding lib.
                 See 'Text.XML.HXT.IO.GetHTTPLibCurl.getCont' for examples. Currently most of the options concerning HTTP requests are implemented.

All attributes not evaluated by readDocument are stored in the created document root node for easy access of the various
options in e.g. the input\/output modules

If the document name is the empty string or an uri of the form \"stdin:\", the document is read from standard input.

examples:

> readDocument [ ] "test.xml"

reads and validates a document \"test.xml\", no namespace propagation, only canonicalization is performed

> readDocument [ (a_validate, "0")
>              , (a_encoding, isoLatin1)
>              , (a_parse_by_mimetype, "1")
>              ] "http://localhost/test.php"

reads document \"test.php\", parses it as HTML or XML depending on the mimetype given from the server, but without validation, default encoding 'isoLatin1'.


> readDocument [ (a_parse_html, "1")
>              , (a_encoding, isoLatin1)
>              ] ""

reads a HTML document from standard input, no validation is done when parsing HTML, default encoding is 'isoLatin1',
parsing is done with tagsoup parser, but input is read strictly

> readDocument [ (a_encoding, isoLatin1)
>              , (a_mime_type,    "/etc/mime.types")
>              , (a_tagsoup,      "1")
>              , (a_strict_input, "1")
>              ] "test.svg"

reads an SVG document from standard input, sets the mime type by looking in the system mimetype config file, default encoding is 'isoLatin1',
parsing is done with the lightweight tagsoup parser, which implies no validation.

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

readDocument    :: SysConfigList -> String -> IOStateArrow s b XmlTree
readDocument config src
    = localSysParam (theTrace `pairS` theReadConfig) $
      readDocument' config src

readDocument'   :: SysConfigList -> String -> IOStateArrow s b XmlTree
readDocument' config src
    = configSysParams config
      >>>
      ( getCont $< getAllParams ) -- TODO
      >>>
      ( processDoc $<<< ( getMimeType
                          &&&
                          getSysParam (theParseByMimeType `pairS`
                                       (theParseHTML `pairS`
                                        theAcceptedMimeTypes
                                       )
                                      )
                          &&&
                          getAllParams	-- TODO
                        )
      )
      >>>
      traceMsg 1 ("readDocument: " ++ show src ++ " processed")
      >>>
      traceSource
      >>>
      traceTree
    where
    getCont userOptions					-- TODO
        = getDocumentContents (addEntries userOptions defaultOptions) src
        where
        defaultOptions
            = [ ( a_strict_input,             v_0 )
              , ( a_ignore_encoding_errors,   v_0 )
              ]

    getMimeType
        = getAttrValue transferMimeType >>^ stringToLower

    processDoc mimeType (parseByMimeType, (parseHtml, acceptedMimeTypes)) options
        = traceMsg 1 (unwords [ "readDocument:", show src
                              , "(mime type:", show mimeType, ") will be processed"])
          >>>
          ( if isAcceptedMimeType acceptedMimeTypes mimeType
            then ( ifA (fromLA hasEmptyBody)
                   ( replaceChildren none )                                     -- empty response, e.g. in if-modified-since request
                   ( ( parse $< getSysParam (theValidate `pairS`
                                             (theIgnoreNoneXmlContents `pairS`
                                              theTagSoup
                                             )
                                            )
                     )
                     >>>
                     ( if isXmlOrHtml
                       then ( ( checknamespaces $< getSysParam (theCheckNamespaces `pairS`
                                                                theTagSoup
                                                               )
                              )
                              >>>
                              rememberDTDAttrl
                              >>>
                              ( canonicalize $< getSysParam (thePreserveComment `pairS`
                                                             (theCanonicalize `pairS`
                                                              theTagSoup
                                                             )
                                                            )
                              )
                              >>>
                              ( whitespace $< getSysParam (theRemoveWS `pairS`
                                                           theTagSoup
                                                          )
                              )
                              >>>
                              relax
                            )
                       else this
                     )
                   )
                 )
            else ( traceMsg 1 (unwords [ "readDocument:", show src
                                       , "mime type:", show mimeType, "not accepted"])
                   >>>
                   replaceChildren none
                 )                                                                      -- remove contents of not accepted mimetype
          )
        where
        hasEmptyBody                    :: LA XmlTree XmlTree
        hasEmptyBody                    = hasAttrValue transferStatus (/= "200")        -- test on empty response body for not o.k. responses
                                          `guards`                                      -- e.g. 3xx status values
                                          ( neg getChildren
                                            <+>
                                            ( getChildren >>> isWhiteSpace )
                                          )

        isAcceptedMimeType              :: [String] -> String -> Bool
        isAcceptedMimeType mts mt
            | null mts
              ||
              null mt                   = True
            | otherwise                 = foldr (matchMt mt') False $ mts'
            where
            mt'                         = parseMt mt
            mts'                        = map parseMt
                                          $
                                          mts
            parseMt                     = break (== '/')
                                          >>>
                                          second (drop 1)
            matchMt (ma,mi) (mas,mis) r = ( (ma == mas || mas == "*")
                                            &&
                                            (mi == mis || mis == "*")
                                          )
                                          || r
        parse (validate, (removeNoneXml, withTagSoup'))
            | isHtml
              ||
              withTagSoup'              = configSysParam (putS theLowerCaseNames isHtml)
                                          >>>
                                          parseHtmlDocument                     -- parse as HTML or with tagsoup XML
            | validateWithRelax         = parseXmlDocument False                -- for Relax NG use XML parser without validation
            | isXml                     = parseXmlDocument validate             -- parse as XML
            | removeNoneXml             = replaceChildren none                  -- don't parse, if mime type is not XML nor HTML
            | otherwise                 = this                                  -- but remove contents when option is set

        checknamespaces (withNamespaces, withTagSoup')
            | (withNamespaces && not withTagSoup')
              ||
              validateWithRelax         = propagateAndValidateNamespaces
            | otherwise                 = this

        canonicalize (preserveCmt, (canonicalize', withTagSoup'))
            | withTagSoup'              = this                                  -- tagsoup already removes redundant stuff
            | validateWithRelax         = canonicalizeAllNodes
            | canonicalize'
              &&
              preserveCmt               = canonicalizeForXPath
            | canonicalize'             = canonicalizeAllNodes
            | otherwise                 = this

        relax
            | validateWithRelax         = validateDocumentWithRelaxSchema options relaxSchema
            | otherwise                 = this

        whitespace (removeWS, withTagSoup')
            | removeWS
              &&
              not withTagSoup'          = removeDocWhiteSpace                   -- tagsoup already removes whitespace
            | otherwise                 = this

        validateWithRelax       = hasEntry a_relax_schema options
        relaxSchema             = lookup1 a_relax_schema options

        isHtml                  = parseHtml                                     -- force HTML
                                  ||
                                  ( parseByMimeType && isHtmlMimeType mimeType )

        isXml                   = ( not parseByMimeType && not parseHtml )
                                  ||
                                  ( parseByMimeType
                                    &&
                                    ( isXmlMimeType mimeType
                                      ||
                                      null mimeType
                                    )                                           -- mime type is XML or not known
                                  )

        isXmlOrHtml     = isHtml || isXml

-- ------------------------------------------------------------

-- |
-- the arrow version of 'readDocument', the arrow input is the source URI

readFromDocument        :: SysConfigList -> IOStateArrow s String XmlTree
readFromDocument config	= applyA ( arr $ readDocument config )

-- ------------------------------------------------------------

-- |
-- read a document that is stored in a normal Haskell String
--
-- the same function as readDocument, but the parameter forms the input.
-- All options available for 'readDocument' are applicable for readString.
--
-- Default encoding: No encoding is done, the String argument is taken as Unicode string

readString      	:: SysConfigList -> String -> IOStateArrow s b XmlTree
readString config content
    			= readDocument (optionToSysConfig (a_encoding, unicodeString) : config)
                          (stringProtocol ++ content)

-- ------------------------------------------------------------

-- |
-- the arrow version of 'readString', the arrow input is the source URI

readFromString  	:: SysConfigList -> IOStateArrow s String XmlTree
readFromString config
    			= applyA ( arr $ readString config )

-- ------------------------------------------------------------

-- |
-- parse a string as HTML content, substitute all HTML entity refs and canonicalize tree
-- (substitute char refs, ...). Errors are ignored.
--
-- A simpler version of 'readFromString' but with less functionality.
-- Does not run in the IO monad

hread                   :: ArrowXml a => a String XmlTree
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

xread                   :: ArrowXml a => a String XmlTree
xread
    = parseXmlContent
      >>>
      substXmlEntityRefs
      >>>
      canonicalizeContents

-- ------------------------------------------------------------

{-# DEPRECATED readDocumentOld, readFromDocumentOld, readStringOld, readFromStringOld "Please use the new read functions" #-}

readDocumentOld    :: Attributes -> String -> IOStateArrow s b XmlTree
readDocumentOld userOptions src
    = readDocument (map optionToSysConfig $ userOptions) src

readFromDocumentOld        :: Attributes -> IOStateArrow s String XmlTree
readFromDocumentOld userOptions
    = applyA ( arr $ readDocumentOld userOptions )

readStringOld      :: Attributes -> String -> IOStateArrow s b XmlTree
readStringOld userOptions content
    = readDocumentOld ( (a_encoding, unicodeString) : userOptions ) (stringProtocol ++ content)

readFromStringOld  :: Attributes -> IOStateArrow s String XmlTree
readFromStringOld userOptions
    = applyA ( arr $ readStringOld userOptions )


-- ------------------------------------------------------------

