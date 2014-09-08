-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.ReadDocument
   Copyright  : Copyright (C) 2005-2013 Uwe Schmidt
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
    , hreadDoc
    , xread
    , xreadDoc
    )
where

import Control.Arrow.ListArrows

import Data.Maybe                               ( fromMaybe )
import qualified Data.Map                       as M


import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.Edit                  ( canonicalizeAllNodes
                                                , canonicalizeForXPath
                                                , rememberDTDAttrl
                                                , removeDocWhiteSpace
                                                )
import qualified Text.XML.HXT.Arrow.ParserInterface as PI
import Text.XML.HXT.Arrow.ProcessDocument       ( getDocumentContents
                                                , parseXmlDocument
                                                , parseXmlDocumentWithExpat
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

this filter can be configured by a list of configuration options,
a value of type 'Text.XML.HXT.XmlState.TypeDefs.SysConfig'

for all available options see module 'Text.XML.HXT.Arrow.XmlState.SystemConfig'

- @withValidate yes\/no@ :
  switch on\/off DTD validation. Only for XML parsed documents, not for HTML parsing.

- @withSubstDTDEntities yes\/no@ :
  switch on\/off entity substitution for general entities defined in DTD validation.
  Default is @yes@.
  Switching this option and the validation off can lead to faster parsing, in that case
  reading the DTD documents is not longer necessary.
  Only used with XML parsed documents, not with HTML parsing.

- @withSubstHTMLEntities yes\/no@ :
  switch on\/off entity substitution for general entities defined in HTML validation.
  Default is @no@.
  Switching this option on and the validation and substDTDEntities off can lead to faster parsing,
  in that case
  reading the DTD documents is not longer necessary, HTML general entities are still substituted.
  Only used with XML parsed documents, not with HTML parsing.

- @withParseHTML yes\/no@ :
  switch on HTML parsing.

- @withParseByMimeType yes\/no@ :
  select XML\/HTML parser by document mime type.
  text\/xml and text\/xhtml are parsed as XML, text\/html as HTML.

- @withCheckNamespaces yes\/no@ :
  Switch on\/off namespace propagation and checking

- @withInputEncoding \<encoding-spec\>@ :
  Set default encoding.

- @withTagSoup@ :
  use light weight and lazy parser based on tagsoup lib.
  This is only available when package hxt-tagsoup is installed and
  the source contains an @import Text.XML.HXT.TagSoup@.

- @withRelaxNG \<schema.rng\>@ :
  validate document with Relax NG, the parameter is for the schema URI.
  This implies using XML parser, no validation against DTD, and canonicalisation.

- @withCurl [\<curl-option\>...]@ :
  Use the libCurl binding for HTTP access.
  This is only available when package hxt-curl is installed and
  the source contains an @import Text.XML.HXT.Curl@.
                   
- @withHTTP [\<http-option\>...]@ :
  Use the Haskell HTTP package for HTTP access.
  This is only available when package hxt-http is installed and
  the source contains an @import Text.XML.HXT.HTTP@.

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
>              , withCurl []
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

read w3c home page (xhtml), validate and check namespaces, remove whitespace between tags,
trace activities with level 2.
HTTP access is done with Haskell HTTP package

> readDocument [ withValidate          no
>              , withSubstDTDEntities  no
>              ...
>              ] "http://www.w3c.org/"

read w3c home page (xhtml), but without accessing the DTD given in that document.
Only the predefined XML general entity refs are substituted.

> readDocument [ withValidate          no
>              , withSubstDTDEntities  no
>              , withSubstHTMLEntities yes
>              ...
>              ] "http://www.w3c.org/"

same as above, but with substituion of all general entity refs defined in XHTML.

for minimal complete examples see 'Text.XML.HXT.Arrow.WriteDocument.writeDocument'
and 'runX', the main starting point for running an XML arrow.
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
    readD True
        = constA undefined              -- just for generalizing the signature to: IOStateArrow s b       XmlTree
          >>>                           -- instead of                              IOStateArrow s XmlTree XmlTree
          (withoutUserState $< (getSysVar theCacheRead >>^ ($ src)))
    readD False
        = readDocument'' src

readDocument''   :: String -> IOStateArrow s b XmlTree
readDocument'' src
    = getDocumentContents src
      >>>
      ( processDoc
        $<<
        ( getMimeType
          &&&
          getSysVar (theParseByMimeType   .&&&.
                     theParseHTML         .&&&.
                     theAcceptedMimeTypes .&&&.
                     theRelaxValidate     .&&&.
                     theXmlSchemaValidate
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
    processNoneEmptyDoc p
        = ifA (fromLA hasEmptyBody)
              (replaceChildren none)
              p
        where
          hasEmptyBody
              = hasAttrValue transferStatus (/= "200")        -- test on empty response body for not o.k. responses
                `guards`                                      -- e.g. 3xx status values
                ( neg getChildren
                  <+>
                  ( getChildren >>> isWhiteSpace )
                )

    getMimeType
        = getAttrValue transferMimeType >>^ stringToLower

    applyMimeTypeHandler mt
        = withoutUserState (applyMTH $< getSysVar theMimeTypeHandlers)
        where
          applyMTH mtTable
              = fromMaybe none $
                fmap (\ f -> processNoneEmptyDoc
                             (traceMimeStart >>> f >>> traceMimeEnd)
                     ) $
                M.lookup mt mtTable
          traceMimeStart
              = traceMsg 2 $
                "readDocument: calling user defined document parser"
          traceMimeEnd
              = traceMsg 2 $
                "readDocument: user defined document parser finished"

    processDoc mimeType options
        = traceMsg 1 (unwords [ "readDocument:", show src
                              , "(mime type:", show mimeType, ") will be processed"
                              ]
                     )
          >>>
          ( applyMimeTypeHandler mimeType       -- try user defined document handlers
            `orElse`
            processDoc' mimeType options
          )

    processDoc' mimeType ( parseByMimeType
                         , ( parseHtml
                           , ( acceptedMimeTypes
                             , ( validateWithRelax
                               , validateWithXmlSchema
                               ))))
        = ( if isAcceptedMimeType acceptedMimeTypes mimeType
            then ( processNoneEmptyDoc
                   ( ( parse $< getSysVar (theValidate              .&&&.
                                           theSubstDTDEntities      .&&&.
                                           theSubstHTMLEntities     .&&&.
                                           theIgnoreNoneXmlContents .&&&.
                                           theTagSoup               .&&&.
                                           theExpat
                                          )
                     )
                     >>>
                     ( if isXmlOrHtml
                       then ( ( checknamespaces $< getSysVar (theCheckNamespaces .&&&.
                                                              theTagSoup
                                                             )
                              )
                              >>>
                              rememberDTDAttrl
                              >>>
                              ( canonicalize $< getSysVar (thePreserveComment .&&&.
                                                           theCanonicalize    .&&&.
                                                           theTagSoup
                                                          )
                              )
                              >>>
                              ( whitespace $< getSysVar (theRemoveWS .&&&.
                                                         theTagSoup
                                                        )
                              )
                              >>>
                              relaxOrXmlSchema
                            )
                       else this
                     )
                   )
                 )
            else ( traceMsg 1 (unwords [ "readDocument:", show src
                                       , "mime type:", show mimeType, "not accepted"])
                   >>>
                   replaceChildren none         -- remove contents of not accepted mimetype
                 )
          )
        where
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
        parse ( validate
              , ( substDTD
                , ( substHTML
                  , ( removeNoneXml
                    , ( withTagSoup'
                      , withExpat'
                      )))))
            | not isXmlOrHtml           = if removeNoneXml
                                          then replaceChildren none             -- don't parse, if mime type is not XML nor HTML
                                          else this                             -- but remove contents when option is set

            | isHtml
              ||
              withTagSoup'              = configSysVar (setS theLowerCaseNames isHtml)
                                          >>>
                                          parseHtmlDocument                     -- parse as HTML or with tagsoup XML

            | isXml                     = if withExpat'
                                          then parseXmlDocumentWithExpat
                                          else parseXmlDocument
                                               validate
                                               substDTD
                                               substHTML
                                               validateWithRelax
                                                                                -- parse as XML
            | otherwise                 = this                                  -- suppress warning

        checknamespaces (withNamespaces, withTagSoup')
            | withNamespaces
              &&
              withTagSoup'              = andValidateNamespaces                 -- propagation is done in tagsoup

            | withNamespaces
              ||
              validateWithRelax
              ||
              validateWithXmlSchema
                                        = propagateAndValidateNamespaces        -- RelaxNG and XML Schema require correct namespaces

            | otherwise                 = this

        canonicalize (preserveCmt, (canonicalize', withTagSoup'))
            | withTagSoup'              = this                                  -- tagsoup already removes redundant stuff
            | validateWithRelax
              ||
              validateWithXmlSchema     = canonicalizeAllNodes                  -- no comments in schema validation

            | canonicalize'
              &&
              preserveCmt               = canonicalizeForXPath
            | canonicalize'             = canonicalizeAllNodes
            | otherwise                 = this

        relaxOrXmlSchema
            | validateWithXmlSchema     = withoutUserState $< getSysVar theXmlSchemaValidator
            | validateWithRelax         = withoutUserState $< getSysVar theRelaxValidator
            | otherwise                 = this

        whitespace (removeWS, withTagSoup')
            | ( removeWS
                ||
                validateWithXmlSchema                                           -- XML Schema does not like WS
              )
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

readFromDocument :: SysConfigList -> IOStateArrow s String XmlTree
readFromDocument config
    = applyA ( arr $ readDocument config )

-- ------------------------------------------------------------

-- |
-- read a document that is stored in a normal Haskell String
--
-- the same function as readDocument, but the parameter forms the input.
-- All options available for 'readDocument' are applicable for readString,
-- except input encoding options.
--
-- Encoding: No decoding is done, the String argument is taken as Unicode string
-- All decoding must be done before calling readString, even if the
-- XML document contains an encoding spec.

readString :: SysConfigList -> String -> IOStateArrow s b XmlTree
readString config content
    = readDocument config (stringProtocol ++ content)

-- ------------------------------------------------------------

-- |
-- the arrow version of 'readString', the arrow input is the source URI

readFromString :: SysConfigList -> IOStateArrow s String XmlTree
readFromString config
    = applyA ( arr $ readString config )

-- ------------------------------------------------------------

-- |
-- parse a string as HTML content, substitute all HTML entity refs and canonicalize tree.
-- (substitute char refs, ...). Errors are ignored.
--
-- This arrow delegates all work to the parseHtmlContent parser in module HtmlParser.
--
-- This is a simpler version of 'readFromString' without any options,
-- but it does not run in the IO monad.

hread :: ArrowXml a => a String XmlTree
hread
    = fromLA $
      PI.hread                              -- substHtmlEntityRefs is done in parser
      >>>                                   -- as well as subst HTML char refs
      editNTreeA [isError :-> none]         -- ignores all errors
      >>>
      canonicalizeAllNodes                  -- combine text nodes, substitute char refs

-- | like hread, but accepts a whole document, not a HTML content

hreadDoc :: ArrowXml a => a String XmlTree
hreadDoc
    = fromLA $
      PI.hreadDoc                           -- substHtmlEntityRefs is done in parser
      >>>                                   -- as well as subst HTML char refs
      editNTreeA [isError :-> none]         -- ignores all errors
      >>>
      canonicalizeAllNodes
      
-- ------------------------------------------------------------

-- |
-- parse a string as XML CONTENT, (no xml decl or doctype decls are allowed),
-- substitute all predefined XML entity refs and canonicalize tree
-- This xread arrow delegates all work to the xread parser function in module XmlParsec

xread :: ArrowXml a => a String XmlTree
xread = PI.xreadCont

-- |
-- a more general version of xread which
-- parses a whole document including a prolog
-- (xml decl, doctype decl) and processing
-- instructions. Doctype decls remain uninterpreted,
-- but are in the list of results trees.

xreadDoc :: ArrowXml a => a String XmlTree
xreadDoc = PI.xreadDoc

{- -- the old version, where the parser does not subst char refs and cdata
xread                   = root [] [parseXmlContent]       -- substXmlEntityRefs is done in parser
                          >>>
                          canonicalizeContents
                          >>>
                          getChildren
-- -}

-- ------------------------------------------------------------

