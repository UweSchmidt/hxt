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
                                                , andValidateNamespaces
                                                )
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs

-- ------------------------------------------------------------
--
{- |
the main document input filter

this filter can be configured by a list of configuration options, a value of type 'Text.XML.HXT.XmlState.TypeDefs.SysConfig'

for all available options see module 'Text.XML.HXT.XmlState.SystemConfig'

- @withValidate yes/no@ :
  switch on/off DTD validation. Only for XML parsed documents, not for HTML parsing.

- @withParseHTML yes/no@ :
  switch on HTML parsing.

- @withParseByMimeType yes/no@ :
  select XML/HTML parser by document mime type.
  text/xml and text/xhtml are parsed as XML, text/html as HTML.

- @withCheckNamespaces yes/no@ :
  Switch on/off namespace propagation and checking

- @withInputEncoding <encoding-spec>@ :
  Set default encoding.

- @withTagSoup@ :
  use light weight and lazy parser based on tagsoup lib.
  This is only available when package hxt-tagsoup is installed and
  'Text.XML.HXT.TagSoup' is imported

- @withRelaxNG <schema.rng>@ :
  validate document with Relax NG, the parameter is for the schema URI.
  This implies using XML parser, no validation against DTD, and canonicalisation.


- @withCurl [<curl-option>...]@ :
  Use the libCurl binding for HTTP access.
  This is only available when package hxt-curl is installed and
  'Text.XML.HXT.Curl' is imported
                   
- @withHTTP [<http-option>...]@ :
  Use the Haskell HTTP package for HTTP access.
  This is only available when package hxt-http is installed and
  'Text.XML.HXT.HTTP' is imported

examples:

> readDocument [] "test.xml"

reads and validates a document \"test.xml\", no namespace propagation, only canonicalization is performed

> ...
> import Text.XML.HXT.Curl
> ...
>
> readDocument [ withValidate        no
>              , withInputEncoding   isoLatin1
>              , withParseByMimeType yes
               , withCurl []
>              ] "http://localhost/test.php"

reads document \"test.php\", parses it as HTML or XML depending on the mimetype given from the server, but without validation, default encoding 'isoLatin1'.
HTTP access is done via libCurl.

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

> ...
> import Text.XML.HXT.Curl
> import Text.XML.HXT.TagSoup
> ...
>
> readDocument [ withParseHTML      yes
>              , withTagSoup
>              , withProxy          "www-cache:3128"
>              , withCurl           []
>              , withWarnings       no
>              ] "http://www.haskell.org/"

reads Haskell homepage with HTML parser, ignoring any warnings
(at the time of writing, there were some HTML errors),
with http access via libCurl interface
and proxy \"www-cache\" at port 3128,
parsing is done with tagsoup HTML parser.
This requires packages \"hxt-curl\" and \"hxt-tagsoup\" to be installed

> readDocument [ withValidate          yes
>              , withCheckNamespaces   yes
>              , withRemoveWS          yes
>              , withTrace             2
>              , withHTTP              []
>              ] "http://www.w3c.org/"

read w3c home page (xhtml), validate and check namespaces, remove whitespace between tags, trace activities with level 2.
HTTP access is done with Haskell HTTP package

for minimal complete examples see 'Text.XML.HXT.Arrow.WriteDocument.writeDocument' and 'runX', the main starting point for running an XML arrow.
-}

readDocument    :: SysConfigList -> String -> IOStateArrow s b XmlTree
readDocument config src
    = localSysEnv
      $
      readDocument' config src

readDocument'   :: SysConfigList -> String -> IOStateArrow s b XmlTree
readDocument' config src
    = configSysVars config
      >>>
      readD $< getSysVar theWithCache
    where
    readD True  = constA undefined		-- just for generalizing the signature to: IOStateArrow s b       XmlTree
                  >>>                           -- instead of                              IOStateArrow s XmlTree XmlTree
                  (withoutUserState $< (getSysVar theCacheRead >>^ ($ src)))
    readD False	= readDocument'' src

readDocument''   :: String -> IOStateArrow s b XmlTree
readDocument'' src
    = getDocumentContents src
      >>>
      ( processDoc
        $<<
        ( getMimeType
          &&&
          getSysVar (theParseByMimeType   `pairS`
                     theParseHTML         `pairS`
                     theAcceptedMimeTypes `pairS`
                     theRelaxValidate
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

    processDoc mimeType (((parseByMimeType, parseHtml), acceptedMimeTypes), validateWithRelax)
        = traceMsg 1 (unwords [ "readDocument:", show src
                              , "(mime type:", show mimeType, ") will be processed"])
          >>>
          ( if isAcceptedMimeType acceptedMimeTypes mimeType
            then ( ifA (fromLA hasEmptyBody)
                   ( replaceChildren none )                                     -- empty response, e.g. in if-modified-since request
                   ( ( parse $< getSysVar (theValidate              `pairS`
                                           theIgnoreNoneXmlContents `pairS`
                                           theTagSoup
                                          )
                     )
                     >>>
                     ( if isXmlOrHtml
                       then ( ( checknamespaces $< getSysVar (theCheckNamespaces `pairS`
                                                              theTagSoup
                                                             )
                              )
                              >>>
                              rememberDTDAttrl
                              >>>
                              ( canonicalize $< getSysVar (thePreserveComment `pairS`
                                                           theCanonicalize    `pairS`
                                                           theTagSoup
                                                          )
                              )
                              >>>
                              ( whitespace $< getSysVar (theRemoveWS `pairS`
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
        parse ((validate, removeNoneXml), withTagSoup')
	    | not isXmlOrHtml           = if removeNoneXml
					  then replaceChildren none             -- don't parse, if mime type is not XML nor HTML
					  else this                             -- but remove contents when option is set

            | isHtml
	      ||
              withTagSoup'              = configSysVar (putS theLowerCaseNames isHtml)
                                          >>>
                                          parseHtmlDocument                     -- parse as HTML or with tagsoup XML

            | isXml                     = parseXmlDocument (not validateWithRelax && validate)           -- parse as XML
            | otherwise                 = this                                  -- suppress warning

        checknamespaces (withNamespaces, withTagSoup')
            | withNamespaces
	      &&
	      withTagSoup'		= andValidateNamespaces			-- propagation is done in tagsoup

            | withNamespaces
              ||
              validateWithRelax         = propagateAndValidateNamespaces	-- RelaxNG requires correct namespaces

            | otherwise                 = this

        canonicalize ((preserveCmt, canonicalize'), withTagSoup')
            | withTagSoup'              = this                                  -- tagsoup already removes redundant stuff
            | validateWithRelax         = canonicalizeAllNodes
            | canonicalize'
              &&
              preserveCmt               = canonicalizeForXPath
            | canonicalize'             = canonicalizeAllNodes
            | otherwise                 = this

        relax
            | validateWithRelax         = withoutUserState $< getSysVar theRelaxValidator
            | otherwise                 = this

        whitespace (removeWS, withTagSoup')
            | removeWS
              &&
              not withTagSoup'          = removeDocWhiteSpace                   -- tagsoup already removes whitespace
            | otherwise                 = this

        isHtml                          = ( not parseByMimeType && parseHtml )  -- force HTML
					  ||
					  ( parseByMimeType && isHtmlMimeType mimeType )

        isXml                           = ( not parseByMimeType && not parseHtml )
					  ||
					  ( parseByMimeType
					    &&
					    ( isXmlMimeType mimeType
					      ||
					      null mimeType
					    )                                   -- mime type is XML or not known
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

