-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.ProcessDocument
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Compound arrows for reading, parsing, validating and writing XML documents

   All arrows use IO and a global state for options, errorhandling, ...
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.ProcessDocument
    ( parseXmlDocument
    , parseXmlDocumentWithExpat
    , parseHtmlDocument
    , validateDocument
    , propagateAndValidateNamespaces
    , andValidateNamespaces
    , getDocumentContents
    )
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ListArrow                  ( fromLA )

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs

import Text.XML.HXT.Arrow.ParserInterface       ( parseXmlDoc
                                                , parseHtmlDoc
                                                , substXmlEntityRefs
                                                , substHtmlEntityRefs
                                                )

import Text.XML.HXT.Arrow.Edit                  ( transfAllCharRef
                                                )

import Text.XML.HXT.Arrow.GeneralEntitySubstitution
                                               ( processGeneralEntities
                                               )

import Text.XML.HXT.Arrow.DTDProcessing        ( processDTD
                                               )

import Text.XML.HXT.Arrow.DocumentInput        ( getXmlContents
                                               )

import Text.XML.HXT.Arrow.Namespace            ( propagateNamespaces
                                               , validateNamespaces
                                               )
import Text.XML.HXT.DTDValidation.Validation   ( validate
                                               , getDTDSubset
                                               , transform
                                               )

-- ------------------------------------------------------------

{- |
XML parser

Input tree must be a root tree with a text tree as child containing the document to be parsed.
The parser generates from the input string a tree of a wellformed XML document,
processes the DTD (parameter substitution, conditional DTD parts, ...) and
substitutes all general entity references. Next step is character reference substitution.
Last step is the document validation.
Validation can be controlled by an extra parameter.

Example:

> parseXmlDocument True    -- parse and validate document
>
> parseXmlDocument False   -- only parse document, don't validate

This parser is useful for applications processing correct XML documents.
-}

parseXmlDocument        :: Bool -> IOStateArrow s XmlTree XmlTree
parseXmlDocument validate'
    = ( replaceChildren ( ( getAttrValue a_source
                            &&&
                            xshow getChildren
                          )
                          >>>
                          parseXmlDoc
                          >>>
                          filterErrorMsg
                        )
        >>>
        setDocumentStatusFromSystemState "parse XML document"
        >>>
        processDTD
        >>>
        ( ifA (fromLA getDTDSubset)		-- DTD decl there: set of general entities must be computed
               ( processGeneralEntities
                 >>>
                 transfAllCharRef
               )
              ( substXmlEntityRefs		-- else only the 5 predefined entities are there
                -- TODO: error reporting when finding illegal entity references
                >>>
                transfAllCharRef		-- these 2 arrows can be merged into a single one
              )
        )
        >>>
        ( if validate'
          then validateDocument
          else this
        )
      )
      `when` documentStatusOk

-- ------------------------------------------------------------

parseXmlDocumentWithExpat        :: IOStateArrow s XmlTree XmlTree
parseXmlDocumentWithExpat
    = ( withoutUserState $< getSysVar theExpatParser
      )
      `when` documentStatusOk

-- ------------------------------------------------------------

{- |
HTML parser

Input tree must be a root tree with a text tree as child containing the document to be parsed.
The parser tries to parse everything as HTML, if the HTML document is not wellformed XML or if
errors occur, warnings are generated. The warnings can be issued, or suppressed.

Example: @ parseHtmlDocument True @ : parse document and issue warnings

This parser is useful for applications like web crawlers, where the pages may contain
arbitray errors, but the application is only interested in parts of the document, e.g. the plain text.

-}

parseHtmlDocument       :: IOStateArrow s XmlTree XmlTree
parseHtmlDocument
    = ( perform ( getAttrValue a_source >>> traceValue 1 (("parseHtmlDoc: parse HTML document " ++) . show) )
        >>>
        ( parseHtml $< getSysVar (theTagSoup .&&&. theExpat) )
        >>>
        ( removeWarnings $< getSysVar (theWarnings .&&&. theTagSoup )
        )
        >>>
        setDocumentStatusFromSystemState "parse HTML document"
        >>>
        traceTree
        >>>
        traceSource
        >>>
        perform ( getAttrValue a_source
                  >>>
                  traceValue 1 (\ src -> "parse HTML document " ++ show src ++ " finished")
                )
      )
      `when` documentStatusOk
    where
    parseHtml (withTagSoup', withExpat')
        | withExpat'    = withoutUserState $< getSysVar theExpatParser

        | withTagSoup'  = withoutUserState $< getSysVar theTagSoupParser

        | otherwise     = traceMsg 1 ("parse document with parsec HTML parser")
                          >>>
                          replaceChildren
                          ( ( getAttrValue a_source             -- get source name
                              &&&
                              xshow getChildren
                            )                                   -- get string to be parsed
                            >>>
                            parseHtmlDoc                        -- run parser
                          )
                          >>>
                          substHtmlEntityRefs                   -- substitute entity refs

    removeWarnings (warnings, withTagSoup')
        | withTagSoup'
          &&
          not warnings  = this
        | otherwise     = processTopDownWithAttrl
                          ( if warnings                         -- remove warnings inserted by parser and entity subst
                            then filterErrorMsg
                            else ( none
                                   `when`
                                   isError
                                 )
                          )

-- ------------------------------------------------------------

{- | Document validation

Input must be a complete document tree. The document
is validated with respect to the DTD spec.
Only useful for XML documents containing a DTD.

If the document is valid, it is transformed with respect to the DTD,
normalization of attribute values, adding default values, sorting attributes by name,...

If no error was found, result is the normalized tree,
else the error status is set in the list of attributes
of the root node \"\/\" and the document content is removed from the tree.

-}

validateDocument        :: IOStateArrow s XmlTree XmlTree
validateDocument
    = ( traceMsg 1 "validating document"
        >>>
        perform ( validateDoc
                  >>>
                  filterErrorMsg
                )
        >>>
        setDocumentStatusFromSystemState "document validation"
        >>>
        traceMsg 1 "document validated, transforming doc with respect to DTD"
        >>>
        transformDoc
        >>>
        traceMsg 1 "document transformed"
        >>>
        traceSource
        >>>
        traceTree
      )
      `when`
      documentStatusOk

-- ------------------------------------------------------------

{- | Namespace propagation

Input must be a complete document tree. The namespace declarations
are evaluated and all element and attribute names are processed by
splitting the name into prefix, local part and namespace URI.

Naames are checked with respect to the XML namespace definition

If no error was found, result is the unchanged input tree,
else the error status is set in the list of attributes
of the root node \"\/\" and the document content is removed from the tree.


-}

propagateAndValidateNamespaces  :: IOStateArrow s XmlTree XmlTree
propagateAndValidateNamespaces
    = ( traceMsg 1 "propagating namespaces"
        >>>
        propagateNamespaces
        >>>
        traceDoc "propagating namespaces done"
        >>>
        andValidateNamespaces
      )
      `when`
      documentStatusOk

andValidateNamespaces  :: IOStateArrow s XmlTree XmlTree
andValidateNamespaces
    = ( traceMsg 1 "validating namespaces"
        >>>
        ( setDocumentStatusFromSystemState "namespace propagation"
          `when`
          ( validateNamespaces >>> perform filterErrorMsg )
        )
        >>>
        traceMsg 1 "namespace validation finished"
      )
      `when`
      documentStatusOk

-- ------------------------------------------------------------

{- |
   creates a new document root, adds all options
   as attributes to the document root and calls 'getXmlContents'.

   If the document name is the empty string, the document will be read
   from standard input.

   For supported protocols see 'Text.XML.HXT.Arrow.DocumentInput.getXmlContents'
-}

getDocumentContents     :: String -> IOStateArrow s b XmlTree
getDocumentContents src
    = root [] []
      >>>
      addAttr a_source src
      >>>
      traceMsg 1 ("readDocument: start processing document " ++ show src)
      >>>
      getXmlContents

-- ------------------------------------------------------------

validateDoc                     :: ArrowList a => a XmlTree XmlTree
validateDoc                     = fromLA ( validate
                                           `when`
                                           getDTDSubset      -- validate only when DTD decl is present
                                         )

transformDoc                    :: ArrowList a => a XmlTree XmlTree
transformDoc                    = fromLA transform

-- ------------------------------------------------------------
