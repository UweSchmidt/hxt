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

import Control.Arrow				-- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO
import Control.Arrow.ListArrow

import Text.XML.HXT.DOM.Unicode			( getOutputEncodingFct )
import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.Edit			( addHeadlineToXmlDoc
						, addXmlPi
						, addXmlPiEncoding
						, indentDoc
						, numberLinesInXmlDoc
						, treeRepOfXmlDoc
						)

import System.IO				( Handle
						, IOMode(..)
						, openFile
						, openBinaryFile
						, hPutStrLn
						, hClose
						, stdout
						)

import System.IO.Error				( try )

-- ------------------------------------------------------------
--
-- output arrows

putXmlDocument	:: Bool -> String -> IOStateArrow s XmlTree XmlTree
putXmlDocument textMode dst
    = perform ( xshow getChildren
		>>>
		arrIO (\ s -> try ( hPutDocument (\h -> hPutStrLn h s)))
		>>>
		( ( traceMsg 1 ("io error, document not written to " ++ outFile)
		    >>>
		    arr show >>> mkError c_fatal
		    >>>
		    filterErrorMsg
		  )
		  |||
		  ( traceMsg 2 ("document written to " ++ outFile)
		    >>>
		    none
		  )
		)
	      )
      where
      isStdout	= null dst || dst == "-"

      outFile	= if isStdout
		  then "stdout"
		       else show dst

      hPutDocument	:: (Handle -> IO ()) -> IO ()
      hPutDocument action
	  | isStdout
	      = action stdout
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

putXmlTree	:: String -> IOStateArrow s XmlTree XmlTree
putXmlTree dst
    = perform ( treeRepOfXmlDoc
		>>>
		addHeadlineToXmlDoc
	        >>>
		putXmlDocument True dst
	      )

-- |
-- write a document with indentaion and line numers

putXmlSource	:: String -> IOStateArrow s XmlTree XmlTree
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

getEncodingParam	:: IOStateArrow s XmlTree String
getEncodingParam
    = catA [ getParamString a_output_encoding	-- 4. guess: take output encoding parameter from global state
	   , getParamString a_encoding		-- 5. guess: take encoding parameter from global state
	   , constA utf8			-- default : utf8
           ]
      >. (head . filter (not . null))

getOutputEncoding	:: String -> IOStateArrow s XmlTree String
getOutputEncoding defaultEnc
    = getEC $< getEncodingParam
    where
    getEC enc' = fromLA $ getOutputEncoding' defaultEnc enc'

encodeDocument	:: Bool -> String -> IOStateArrow s XmlTree XmlTree
encodeDocument supressXmlPi defaultEnc
    = encode $< getOutputEncoding defaultEnc
    where
    encode enc
        = traceMsg 2 ("encodeDocument: encoding is " ++ show enc)
	  >>>
          ( encodeDocument' supressXmlPi enc
            `orElse`
            ( issueFatal ("encoding scheme not supported: " ++ show enc)
	      >>>
	      setDocumentStatusFromSystemState "encoding document"
            )
          )

-- ------------------------------------------------------------

getOutputEncoding'	:: String -> String -> LA XmlTree String
getOutputEncoding' defaultEnc defaultEnc2
    =  catA [ getChildren			-- 1. guess: evaluate <?xml ... encoding="..."?>
	      >>>
	      ( ( isPi >>> hasName t_xml )
		`guards`
		getAttrValue a_encoding
	      )
	    , constA defaultEnc			-- 2. guess: explicit parameter, may be ""
	    , getAttrValue a_output_encoding	-- 3. guess: take output encoding parameter in root node
	    , constA defaultEnc2		-- default : UNICODE or utf8
	    ]
      >. (head . filter (not . null))		-- make the filter deterministic: take 1. entry from list of guesses

encodeDocument'	:: ArrowXml a => Bool -> String -> a XmlTree XmlTree
encodeDocument' supressXmlPi defaultEnc
    = fromLA (encode $< getOutputEncoding' defaultEnc utf8)
    where
    encode	:: String -> LA XmlTree XmlTree
    encode encodingScheme
        = case getOutputEncodingFct encodingScheme of
          Nothing	-> none
          Just ef	-> ( if supressXmlPi
		             then processChildren (none `when` isXmlPi)
		             else ( addXmlPi
			            >>>
			            addXmlPiEncoding encodingScheme
			          )
		           )
		           >>>
		           replaceChildren ( xshow getChildren
				             >>>
				             arr ef
				             >>>
				             mkText
				           )
		           >>>
		           addAttr a_output_encoding encodingScheme

-- ------------------------------------------------------------
