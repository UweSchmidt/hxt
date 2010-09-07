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
    )
where

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
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
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs

-- ------------------------------------------------------------
-- TODO
{- |
the main document input filter

this filter can be configured by a list of configuration options, a value of type 'Text.XML.HXT.XmlState.TypeDefs.SysConfig'

for available options see module 'Text.XML.HXT.XmlState.SystemConfig'

- @withTagSoup@ : use light weight and lazy parser based on tagsoup lib.
                  This is only available when hxt-tagsoup is installed and
                  'Text.XML.HXT.TagSoup' is imported


- 'a_relax_schema' : validate document with Relax NG, the options value is the schema URI
                     this implies using XML parser, no validation against DTD, and canonicalisation


- 'a_indent' : indent document by inserting whitespace, else skip this step (default)

- 'a_use_curl' : obsolete and ignored, HTTP acccess is always done with curl bindings for libcurl

- 'a_if_modified_since' : read document conditionally, only if the document is newer than the given date and time argument, the contents is delivered,
                          else just the root node with the meta data is returned. The date and time must be given in "System.Locale.rfc822DateFormat".

- curl options : the HTTP interface with libcurl can be configured with a lot of options. To support these options in an easy way, there is a naming convetion:
                 Every option, which has the prefix @curl@ and the rest of the name forms an option as described in the curl man page, is passed to the curl binding lib.
                 See 'Text.XML.HXT.IO.GetHTTPLibCurl.getCont' for examples. Currently most of the options concerning HTTP requests are implemented.

examples:

> readDocument [ ] "test.xml"

reads and validates a document \"test.xml\", no namespace propagation, only canonicalization is performed

> readDocument [ withValidate        no
>              , withInputEncoding   isoLatin1
>              , withParseByMimeType yes
>              ] "http://localhost/test.php"

reads document \"test.php\", parses it as HTML or XML depending on the mimetype given from the server, but without validation, default encoding 'isoLatin1'.


> readDocument [ withParseHTML       yes
>              , withInputEncoding   isoLatin1
>              ] ""

reads a HTML document from standard input, no validation is done when parsing HTML, default encoding is 'isoLatin1',

> readDocument [ withInputEncoding  isoLatin1
>              , withValidate       no
>              , withMimeTypeFile   "/etc/mime.types"
>              , withStrictInput    yes
>              ] "test.svg"

reads an SVG document from \"test.svg\", sets the mime type by looking in the system mimetype config file,
default encoding is 'isoLatin1',

> import Text.XML.HXT.Curl
> import Text.XML.HXT.TagSoup
>
> ...
>
> readDocument [ withParseHTML      yes
>              , withTagSoup
>              , withProxy          "www-cache:3128"
>              , withCurl           []
>              , withWarnings       no
>              ] "http://www.haskell.org/"

reads Haskell homepage with HTML parser ignoring any warnings
(at the time of writing, there were some HTML errors),
with http access via curl interface
and proxy \"www-cache\" at port 3128,
parsing is done with tagsoup HTML parser.
This requires packages \"hxt-curl\" and \"hxt-tagsoup\" to be installed

> readDocument [ withValidate          yes
>              , withCheckNamespaces   yes
>              , withRemoveWS          yes
>              , withTrace             2
>              ] "http://www.w3c.org/"

read w3c home page (xhtml), validate and check namespaces, remove whitespace between tags, trace activities with level 2

for minimal complete examples see 'Text.XML.HXT.Arrow.WriteDocument.writeDocument' and 'runX', the main starting point for running an XML arrow.
-}

readDocument    :: SysConfigList -> String -> IOStateArrow s b XmlTree
readDocument config src
    = localSysEnv
      $
      readDocument' config src

readDocument'   :: SysConfigList -> String -> IOStateArrow s b XmlTree
readDocument' config src
    = configSysParams config
      >>>
      getDocumentContents src
      >>>
      ( processDoc
        $<<
        ( getMimeType
          &&&
          getSysParam (theParseByMimeType `pairS`
                       (theParseHTML `pairS`
                        (theAcceptedMimeTypes `pairS`
                         theRelaxValidate
                        )
                       )
                      )
        )
      )
      >>>
      traceMsg 1 ("readDocument: " ++ show src ++ " processed")
      >>>
      traceSource
      >>>
      traceTree
    where
    getMimeType
        = getAttrValue transferMimeType >>^ stringToLower

    processDoc mimeType (parseByMimeType, (parseHtml, (acceptedMimeTypes, validateWithRelax)))
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
            | ( withNamespaces
		&&
		not withTagSoup'
	      )
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
            | validateWithRelax         = withoutUserState $< getSysParam theRelaxValidator
            | otherwise                 = this

        whitespace (removeWS, withTagSoup')
            | removeWS
              &&
              not withTagSoup'          = removeDocWhiteSpace                   -- tagsoup already removes whitespace
            | otherwise                 = this

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
    			= readDocument (withInputEncoding unicodeString : config)
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

