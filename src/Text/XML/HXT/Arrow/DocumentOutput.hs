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
    ( module Text.XML.HXT.Arrow.DocumentOutput )
where

import Control.Arrow				-- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO

import Text.XML.HXT.DOM.Unicode
    ( getOutputEncodingFct )

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.Edit
    ( addHeadlineToXmlDoc
    , addXmlPi
    , addXmlPiEncoding
    , indentDoc
    , numberLinesInXmlDoc
    , treeRepOfXmlDoc
    )

import Data.Maybe

import System.IO
    ( Handle
    , IOMode(..)
    , openFile
    , hPutStrLn
    , hClose
    , stdout
    )

import System.IO.Error
    ( try )

-- ------------------------------------------------------------
--
-- output arrows

putXmlDocument	:: String -> IOStateArrow s XmlTree XmlTree
putXmlDocument dst
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

      hPutDocument	:: (Handle -> IO()) -> IO()
      hPutDocument action
	  | isStdout
	      = action stdout
	  | otherwise
	      = do
		handle <- openFile dst WriteMode
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
		putXmlDocument dst
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
		putXmlDocument dst
	      )

-- ------------------------------------------------------------

getOutputEncoding	:: String -> IOStateArrow s XmlTree String
getOutputEncoding defaultEnc
    =  catA [ getChildren			-- 1. guess: evaluate <?xml ... encoding="..."?>
	      >>>
	      ( ( isPi >>> hasName t_xml )
		`guards`
		getAttrValue a_encoding
	      )
	    , constA defaultEnc			-- 2. guess: explicit parameter, may be ""
	    , getAttrValue a_output_encoding	-- 3. guess: take output encoding parameter in root node
	    , getParamString a_output_encoding	-- 4. guess: take output encoding parameter in global state
	    , getParamString a_encoding		-- 5. guess: take encoding parameter in global state
	    , constA utf8			-- default : utf8
	    ]
      >. (head . filter (not . null))		-- make the filter deterministic: take 1. entry from list of guesses

encodeDocument	:: Bool -> String -> IOStateArrow s XmlTree XmlTree
encodeDocument supressXmlPi defaultEnc
    = applyA ( getOutputEncoding defaultEnc
	       >>>
	       arr encArr
	     )
      `when`
      isRoot
    where
    encArr	:: String -> IOStateArrow s XmlTree XmlTree
    encArr enc	= maybe notFound found . getOutputEncodingFct $ enc
	where
	found ef = traceMsg 2 ("encodeDocument: encoding is " ++ show enc)
		   >>>
		   ( if supressXmlPi
		     then processChildren (none `when` isXmlPi)
		     else ( addXmlPi
			    >>>
			    addXmlPiEncoding enc
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
		   addAttr a_output_encoding enc

	notFound = issueFatal ("encoding scheme not supported: " ++ show enc)
		   >>>
		   setDocumentStatusFromSystemState "encoding document"

-- ------------------------------------------------------------
