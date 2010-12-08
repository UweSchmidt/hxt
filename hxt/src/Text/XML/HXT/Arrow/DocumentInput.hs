-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.DocumentInput
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   State arrows for document input
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.DocumentInput
    ( getURIContents
    , getXmlContents
    , getXmlEntityContents
    , getEncoding
    , getTextEncoding
    , decodeDocument
    , addInputError
    )
where

import           Control.Arrow                            -- arrow classes
import           Control.Arrow.ArrowList
import           Control.Arrow.ArrowIf
import           Control.Arrow.ArrowTree
import           Control.Arrow.ArrowIO
import           Control.Arrow.ListArrow

import           Data.List                      ( isPrefixOf )
import           Data.String.Unicode            ( getDecodingFct
                                                , guessEncoding
                                                , normalizeNL
                                                )

import           System.FilePath                ( takeExtension )

import qualified Text.XML.HXT.IO.GetFILE        as FILE

import           Text.XML.HXT.DOM.Interface

import           Text.XML.HXT.Arrow.ParserInterface
                                                ( parseXmlDocEncodingSpec
                                                , parseXmlEntityEncodingSpec
                                                , removeEncodingSpec
                                                )
import           Text.XML.HXT.Arrow.XmlArrow
import           Text.XML.HXT.Arrow.XmlState
import           Text.XML.HXT.Arrow.XmlState.TypeDefs

-- ----------------------------------------------------------

protocolHandlers        :: AssocList String (IOStateArrow s XmlTree XmlTree)
protocolHandlers
    = [ ("file",        getFileContents)
      , ("http",        getHttpContents)
      , ("stdin",       getStdinContents)
      ]

getProtocolHandler      :: IOStateArrow s String (IOStateArrow s XmlTree XmlTree)
getProtocolHandler
    = arr (\ s -> lookupDef getUnsupported s protocolHandlers)

getUnsupported          :: IOStateArrow s XmlTree XmlTree
getUnsupported
    = perform ( getAttrValue a_source
                >>>
                arr (("unsupported protocol in URI " ++) . show)
                >>>
                applyA (arr issueFatal)
              )
      >>>
      setDocumentStatusFromSystemState "accessing documents"

getStringContents               :: IOStateArrow s XmlTree XmlTree
getStringContents
    = setCont $< getAttrValue a_source
      >>>
      addAttr transferMessage "OK"
      >>>
      addAttr transferStatus "200"
    where
    setCont contents
        = replaceChildren (txt contents')
          >>>
          addAttr transferURI (take 7 contents)                 -- the "string:" prefix is stored, this is required by setBaseURIFromDoc
          >>>
          addAttr a_source (show . prefix 48 $ contents')       -- a quoted prefix of the content, max 48 chars is taken as source name
        where
        contents'  = drop (length stringProtocol) contents
        prefix l s
            | length s' > l = take (l - 3) s' ++ "..."
            | otherwise     = s'
            where
            s' = take (l + 1) s

getFileContents         :: IOStateArrow s XmlTree XmlTree
getFileContents
    = applyA ( ( getSysVar theStrictInput
                 &&&
                 ( getAttrValue transferURI
                   >>>
                   getPathFromURI
                 )
               )
               >>>
               traceValue 2 (\ (b, f) -> "read file " ++ show f ++ " (strict input = " ++ show b ++ ")")
               >>>
               arrIO (uncurry FILE.getCont)
               >>>
               ( arr (uncurry addInputError) -- io error occured
                 |||
                 arr addTxtContent      -- content read
               )
             )
      >>>
      addMimeType

getStdinContents                :: IOStateArrow s XmlTree XmlTree
getStdinContents
    = applyA (  getSysVar theStrictInput
                >>>
                arrIO FILE.getStdinCont
               >>>
               ( arr (uncurry addInputError) -- io error occured
                 |||
                 arr addTxtContent           -- content read
               )
             )

addInputError                :: Attributes -> String -> IOStateArrow s XmlTree XmlTree
addInputError al e
    = issueFatal e
      >>>
      seqA (map (uncurry addAttr) al)
      >>>
      setDocumentStatusFromSystemState "accessing documents"

addMimeType     :: IOStateArrow s XmlTree XmlTree
addMimeType
    = addMime $< ( ( getSysVar theFileMimeType
                     >>>
                     isA (not . null)
                   )
                   `orElse`
                   ( getAttrValue transferURI
                     >>>
                     ( uriToMime $< getMimeTypeTable )
                   )
                 )
    where
    addMime mt
        = addAttr transferMimeType mt
    uriToMime mtt
        = arr $ ( \ uri -> extensionToMimeType (drop 1 . takeExtension $ uri) mtt )

addTxtContent   :: Blob -> IOStateArrow s XmlTree XmlTree
addTxtContent bc
    = replaceChildren (blb bc)
      >>>
      addAttr transferMessage "OK"
      >>>
      addAttr transferStatus "200"

getHttpContents         :: IOStateArrow s XmlTree XmlTree
getHttpContents
    = withoutUserState $ applyA $ getSysVar theHttpHandler

getURIContents          :: IOStateArrow s XmlTree XmlTree
getURIContents
    = getContentsFromString
      `orElse`
      getContentsFromDoc
    where
    getContentsFromString
        = ( getAttrValue a_source
            >>>
            isA (isPrefixOf stringProtocol)
          )
          `guards`
          getStringContents

    getContentsFromDoc
        = ( ( addTransferURI $< getBaseURI
              >>>
              getCont
            )
            `when`
            ( setAbsURI $< ( getAttrValue a_source
                             >>^
                             ( \ src-> (if null src then "stdin:" else src) )   -- empty document name -> read from stdin
                           )
            )
          )
          >>>
          setDocumentStatusFromSystemState "getURIContents"

    setAbsURI src
        = ifA ( constA src >>> changeBaseURI )
          this
          ( issueFatal ("illegal URI : " ++ show src) )

    addTransferURI uri
        = addAttr transferURI uri

    getCont
        = applyA ( getBaseURI                           -- compute the handler and call it
                   >>>
                   traceValue 2 (("getURIContents: reading " ++) . show)
                   >>>
                   getSchemeFromURI
                   >>>
                   getProtocolHandler
                 )
          `orElse`
          this                                          -- don't change tree, when no handler can be found

setBaseURIFromDoc       :: IOStateArrow s XmlTree XmlTree
setBaseURIFromDoc
    = perform ( getAttrValue transferURI
                >>>
                isA (isPrefixOf stringProtocol)         -- do not change base URI when reading from a string
                >>>
                setBaseURI
              )

{- |
   Read the content of a document.

   This routine is usually called from 'Text.XML.HXT.Arrow.ProcessDocument.getDocumentContents'.

   The input must be a root node (constructed with 'Text.XML.HXT.Arrow.XmlArrow.root'), usually without children.
   The attribute list contains all input parameters, e.g. URI or source file name, encoding preferences, ...
   If the source name is empty, the input is read from standard input.

   The source is transformed into an absolute URI. If the source is a relative URI, or a file name,
   it is expanded into an absolut URI with respect to the current base URI.
   The default base URI is of protocol \"file\" and points to the current working directory.

   The currently supported protocols are \"http\", \"file\", \"stdin\" and \"string\".

   The latter two are internal protocols. An uri of the form \"stdin:\" stands for the content of
   the standard input stream.

   \"string:some text\" means, that \"some text\" is taken as input.
   This internal protocol is used for reading from normal 'String' values.

-}

getXmlContents          :: IOStateArrow s XmlTree XmlTree
getXmlContents
    = getXmlContents' parseXmlDocEncodingSpec
      >>>
      setBaseURIFromDoc

getXmlEntityContents            :: IOStateArrow s XmlTree XmlTree
getXmlEntityContents
    = getXmlContents' parseXmlEntityEncodingSpec
      >>>
      processChildren removeEncodingSpec
      >>>
      setBaseURIFromDoc

getXmlContents'         :: IOStateArrow s XmlTree XmlTree -> IOStateArrow s XmlTree XmlTree
getXmlContents' parseEncodingSpec
    = ( getURIContents
        >>>
        choiceA
        [ isXmlHtmlDoc  :-> ( parseEncodingSpec
                              >>>
                              filterErrorMsg
                              >>>
                              decodeDocument
                            )
        , isTextDoc     :->  decodeDocument
        , this          :-> this
        ]
        >>>
        perform ( getAttrValue transferURI
                  >>>
                  traceValue 1 (("getXmlContents: content read and decoded for " ++) . show)
                )
        >>>
        traceTree
        >>>
        traceSource
      )
      `when`
      isRoot

isMimeDoc               :: (String -> Bool) -> IOStateArrow s XmlTree XmlTree
isMimeDoc isMT          = fromLA $
                          ( ( getAttrValue transferMimeType >>^ stringToLower )
                            >>>
                            isA (\ t -> null t || isMT t)
                          )
                          `guards` this

isTextDoc, isXmlHtmlDoc :: IOStateArrow s XmlTree XmlTree

isTextDoc               = isMimeDoc isTextMimeType

isXmlHtmlDoc            = isMimeDoc (\ mt -> isHtmlMimeType mt || isXmlMimeType mt)

-- ------------------------------------------------------------

getEncoding     :: IOStateArrow s XmlTree String
getEncoding
    = catA [ xshow getChildren                  -- 1. guess: guess encoding by looking at the first few bytes
             >>>
             arr guessEncoding
           , getAttrValue transferEncoding      -- 2. guess: take the transfer encoding
           , getAttrValue a_encoding            -- 3. guess: take encoding parameter in root node
           , getSysVar  theInputEncoding        -- 4. guess: take encoding parameter in global state
           , constA utf8                        -- default : utf8
           ]
      >. (head . filter (not . null))           -- make the filter deterministic: take 1. entry from list of guesses

getTextEncoding :: IOStateArrow s XmlTree String
getTextEncoding
    = catA [ getAttrValue transferEncoding      -- 1. guess: take the transfer encoding
           , getAttrValue a_encoding            -- 2. guess: take encoding parameter in root node
           , getSysVar theInputEncoding         -- 3. guess: take encoding parameter in global state
           , constA isoLatin1                   -- default : no encoding
           ]
      >. (head . filter (not . null))           -- make the filter deterministic: take 1. entry from list of guesses


decodeDocument  :: IOStateArrow s XmlTree XmlTree
decodeDocument
    = choiceA
      [ ( isRoot >>> isXmlHtmlDoc )   :-> ( decodeX                $< getSysVar theExpat)
                                  -- old: ( decodeArr normalizeNL  $< getEncoding )
      , ( isRoot >>> isTextDoc )      :-> ( decodeArr id           $< getTextEncoding )
      , this                          :-> this
      ]
    where
    decodeX             :: Bool -> IOStateArrow s XmlTree XmlTree
    decodeX False       = decodeArr normalizeNL  $< getEncoding
    decodeX True        = noDecode               $< getEncoding         -- parse with expat

    noDecode enc        = traceMsg 2 ("no decoding (done by expat): encoding is " ++ show enc)
                          >>>
                          addAttr transferEncoding enc

    decodeArr   :: (String -> String) -> String -> IOStateArrow s XmlTree XmlTree
    decodeArr normalizeNewline enc
        = maybe notFound found . getDecodingFct $ enc
        where
        found df
            = traceMsg 2 ("decodeDocument: encoding is " ++ show enc)
              >>>
              ( decodeText df $< getSysVar theEncodingErrors )
              >>>
              addAttr transferEncoding enc

        notFound
            = issueFatal ("encoding scheme not supported: " ++ show enc)
              >>>
              setDocumentStatusFromSystemState "decoding document"

        decodeText df withEncErrors
            = processChildren
              ( getText                                                 -- get the document content
                >>> arr df                                              -- decode the text, result is (string, [errMsg])
                >>> ( ( (fst >>> normalizeNewline)                      -- take decoded string, normalize newline and build text node
                        ^>> mkText
                      )
                      <+>
                      ( if withEncErrors
                        then
                        ( arrL snd                                      -- take the error messages
                          >>>
                          arr ((enc ++) . (" encoding error" ++))       -- prefix with enc error
                          >>>
                          applyA (arr issueErr)                         -- build issueErr arrow and apply
                          >>>
                          none                                          -- neccessary for type match with <+>
                        )
                        else none
                      )
                    )
              )

-- ------------------------------------------------------------
