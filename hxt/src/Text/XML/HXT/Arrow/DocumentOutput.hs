-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.DocumentOutput
   Copyright  : Copyright (C) 2005-9 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   State arrows for document output

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.DocumentOutput
    ( putXmlDocument
    , putXmlTree
    , putXmlSource
    , encodeDocument
    , encodeDocument'
    )
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO
import Control.Arrow.ListArrow

import qualified
       Data.ByteString.Lazy                     as BS
import Data.Maybe
import Data.String.Unicode                      ( getOutputEncodingFct' )

import Text.XML.HXT.DOM.Interface
import qualified
       Text.XML.HXT.DOM.ShowXml                 as XS

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.Edit                  ( addHeadlineToXmlDoc
                                                , addXmlPi
                                                , addXmlPiEncoding
                                                , indentDoc
                                                , numberLinesInXmlDoc
                                                , treeRepOfXmlDoc
                                                , escapeHtmlRefs
                                                , escapeXmlRefs
                                                )
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.XmlState.TypeDefs

import System.IO                                ( Handle
                                                , IOMode(..)
                                                , openFile
                                                , openBinaryFile
                                                , hSetBinaryMode
                                                , hPutStrLn
                                                , hClose
                                                , stdout
                                                )
import System.IO.Error                          ( try )

-- ------------------------------------------------------------
--
-- | Write the contents of a document tree into an output stream (file or stdout).
--
-- If textMode is set, writing is done with Haskell string output, else (default)
-- writing is done with lazy ByteString output

putXmlDocument  :: Bool -> String -> IOStateArrow s XmlTree XmlTree
putXmlDocument textMode dst
    = perform putDoc
      where
      putDoc
          = ( if textMode
              then ( xshow getChildren
                     >>>
                     arrIO (\ s -> try ( hPutDocument (\h -> hPutStrLn h s)))
                   )
              else ( xshowBlob getChildren
                     >>>
                     arrIO (\ s -> try ( hPutDocument (\h -> do
                                                             BS.hPutStr h s
                                                             BS.hPutStr h (stringToBlob "\n")
                                                      )
                                       )
                           )
                   )
            )
            >>>
            ( ( traceMsg 1 ("io error, document not written to " ++ outFile)
                >>>
                arr show >>> mkError c_fatal
                >>>
                filterErrorMsg
              )
              |||
              ( traceMsg 2 ("document written to " ++ outFile ++ ", textMode = " ++ show textMode)
                >>>
                none
              )
            )
          where
          isStdout  = null dst || dst == "-"

          outFile   = if isStdout
                      then "stdout"
                      else show dst

          hPutDocument      :: (Handle -> IO ()) -> IO ()
          hPutDocument action
              | isStdout
                  = do
                    hSetBinaryMode stdout (not textMode)
                    action stdout
                    hSetBinaryMode stdout False
              | otherwise
                  = do
                    handle <- ( if textMode
                                then openFile
                                else openBinaryFile
                              ) dst WriteMode
                    action handle
                    hClose handle

-- |
-- write the tree representation of a document to a file

putXmlTree      :: String -> IOStateArrow s XmlTree XmlTree
putXmlTree dst
    = perform ( treeRepOfXmlDoc
                >>>
                addHeadlineToXmlDoc
                >>>
                putXmlDocument True dst
              )

-- |
-- write a document with indentaion and line numers

putXmlSource    :: String -> IOStateArrow s XmlTree XmlTree
putXmlSource dst
    = perform ( (this ) `whenNot` isRoot
                >>>
                indentDoc
                >>>
                numberLinesInXmlDoc
                >>>
                addHeadlineToXmlDoc
                >>>
                putXmlDocument True dst
              )

-- ------------------------------------------------------------

getEncodingParam        :: IOStateArrow s XmlTree String
getEncodingParam
    = catA [ getSysVar theOutputEncoding   -- 4. guess: take output encoding parameter from global state
           , getSysVar theInputEncoding    -- 5. guess: take encoding parameter from global state
           , constA utf8                   -- default : utf8
           ]
      >. (head . filter (not . null))

getOutputEncoding       :: String -> IOStateArrow s XmlTree String
getOutputEncoding defaultEnc
    = getEC $< getEncodingParam
    where
    getEC enc' = fromLA $ getOutputEncoding' defaultEnc enc'

encodeDocument  :: Bool -> Bool -> String -> IOStateArrow s XmlTree XmlTree
encodeDocument quoteXml supressXmlPi defaultEnc
    = encode $< getOutputEncoding defaultEnc
    where
    encode enc
        = traceMsg 2 ("encodeDocument: encoding is " ++ show enc)
          >>>
          ( encodeDocument' quoteXml supressXmlPi enc
            `orElse`
            ( issueFatal ("encoding scheme not supported: " ++ show enc)
              >>>
              setDocumentStatusFromSystemState "encoding document"
            )
          )

-- ------------------------------------------------------------

isBinaryDoc               :: LA XmlTree XmlTree
isBinaryDoc               = ( ( getAttrValue transferMimeType >>^ stringToLower )
                              >>>
                              isA (\ t -> not (null t || isTextMimeType t || isXmlMimeType t))
                            )
                            `guards` this

getOutputEncoding'      :: String -> String -> LA XmlTree String
getOutputEncoding' defaultEnc defaultEnc2
    =  catA [ isBinaryDoc
              >>>                               -- 0. guess: binary data found: no encoding at all
              constA isoLatin1                  --           the content should usually be a blob
                                                --           this handling is like the decoding in DocumentInput,
                                                --           there nothing is decoded for non text or non xml contents
            , getChildren                       -- 1. guess: evaluate <?xml ... encoding="..."?>
              >>>
              ( ( isPi >>> hasName t_xml )
                `guards`
                getAttrValue a_encoding
              )
            , constA defaultEnc                 -- 2. guess: explicit parameter, may be ""
            , getAttrValue a_output_encoding    -- 3. guess: take output encoding parameter in root node
            , constA defaultEnc2                -- default : UNICODE or utf8
            ]
      >. (head . filter (not . null))           -- make the filter deterministic: take 1. entry from list of guesses

encodeDocument' :: ArrowXml a => Bool -> Bool -> String -> a XmlTree XmlTree
encodeDocument' quoteXml supressXmlPi defaultEnc
    = fromLA (encode $< getOutputEncoding' defaultEnc utf8)
    where
    encode      :: String -> LA XmlTree XmlTree
    encode encodingScheme
        | encodingScheme == unicodeString
	    = replaceChildren
              ( (getChildren >. XS.xshow'' cQuot aQuot)
                >>>
                mkText
              )
        | isNothing encodeFct
            = none
        | otherwise
            = ( if supressXmlPi
                then processChildren (none `when` isXmlPi)
                else ( addXmlPi
                       >>>
                       addXmlPiEncoding encodingScheme
                     )
              )
              >>>
              ( isLatin1Blob
                `orElse`
                encodeDoc (fromJust encodeFct)
              )
              >>>
              addAttr a_output_encoding encodingScheme
        where
        (cQuot, aQuot)
            | quoteXml	= escapeXmlRefs
            | otherwise = escapeHtmlRefs

        encodeFct       = getOutputEncodingFct' encodingScheme

        encodeDoc ef    = replaceChildren
                          ( xshowBlobWithEnc cQuot aQuot ef getChildren
                            >>>
                            mkBlob
                          )
        xshowBlobWithEnc cenc aenc enc f
                        = f >. XS.xshow' cenc aenc enc 

        -- if encoding scheme is isolatin1 and the contents is a single blob (bytestring)
        -- the encoding is the identity.
        -- This optimization enables processing (copying) of none XML contents
        -- without any conversions from and to strings
        isLatin1Blob
            | encodingScheme /= isoLatin1
                        = none
            | otherwise = childIsSingleBlob `guards` this
            where
            childIsSingleBlob
                        = listA getChildren
                          >>>
                          isA (length >>> (== 1))
                          >>>
                          unlistA
                          >>>
                          isBlob

-- ------------------------------------------------------------
