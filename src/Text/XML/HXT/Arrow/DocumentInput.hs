-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.DocumentInput
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   State arrows for document input
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.DocumentInput
    ( getURIContents
    , getXmlContents
    , getXmlEntityContents
    , getEncoding
    , decodeDocument
    )
where

import Control.Arrow				-- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO

import Text.XML.HXT.DOM.Unicode
    ( getDecodingFct
    , guessEncoding
    , normalizeNL
    )

import qualified Text.XML.HXT.IO.GetFILE	as FILE
import qualified Text.XML.HXT.IO.GetHTTPNative	as HTTP
import qualified Text.XML.HXT.IO.GetHTTPCurl	as CURL

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.ParserInterface
    ( parseXmlDocEncodingSpec
    , parseXmlEntityEncodingSpec
    , removeEncodingSpec
    )

import Data.List
    ( isPrefixOf
    )
import Data.Maybe

-- ----------------------------------------------------------

protocolHandlers	:: AssocList String (IOStateArrow s XmlTree XmlTree)
protocolHandlers
    = [ ("file",	getFileContents)
      , ("http",	getHttpContents)
      , ("stdin",	getStdinContents)
      ]

getProtocolHandler	:: IOStateArrow s String (IOStateArrow s XmlTree XmlTree)
getProtocolHandler
    = arr (\ s -> lookupDef getUnsupported s protocolHandlers)

getUnsupported		:: IOStateArrow s XmlTree XmlTree
getUnsupported
    = perform ( getAttrValue a_source
		>>>
		arr (("unsupported protocol in URI " ++) . show)
		>>>
		applyA (arr issueFatal)
	      )
      >>>
      setDocumentStatusFromSystemState "accessing documents"
	
getStringContents		:: IOStateArrow s XmlTree XmlTree
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
	  addAttr transferURI (take 7 contents)			-- the "string:" prefix is stored, this is required by setBaseURIFromDoc
	  >>>
	  addAttr a_source (show . prefix 48 $ contents')	-- a quoted prefix of the content, max 48 chars is taken as source name
	where
	contents'  = drop (length stringProtocol) contents
	prefix l s
	    | length s' > l = take (l - 3) s' ++ "..."
	    | otherwise     = s'
	    where
	    s' = take (l + 1) s

getFileContents		:: IOStateArrow s XmlTree XmlTree
getFileContents
    = applyA ( getAttrValue transferURI
	       >>>
	       getPathFromURI
	       >>>
	       arrIO FILE.getCont
	       >>>
	       ( arr addError		-- io error occured
		 |||
		 arr addTxtContent		-- content read
	       )
	     )

getStdinContents		:: IOStateArrow s XmlTree XmlTree
getStdinContents
    = applyA ( arrIO (\ _ -> FILE.getStdinCont)
	       >>>
	       ( arr addError		-- io error occured
		 |||
		 arr addTxtContent		-- content read
	       )
	     )

addError		:: String -> IOStateArrow s XmlTree XmlTree
addError e
    = issueFatal e
      >>>
      setDocumentStatusFromSystemState "accessing documents"

addTxtContent	:: String -> IOStateArrow s XmlTree XmlTree
addTxtContent c
    = replaceChildren (txt c)
      >>>
      addAttr transferMessage "OK"
      >>>
      addAttr transferStatus "200"

{-
getHttpContents		:: IOStateArrow s XmlTree XmlTree
getHttpContents
    = applyA ( ( getAttrValue transferURI
		 &&&
		 getParamString a_proxy
	       )
	       >>>
	       applyA ( getParamInt 0 a_use_curl
			>>>
			ifP (==0)
			  ( constA $ arrIO2 HTTP.getCont )		-- native http
			  ( constA $
			    ( getParamString a_options_curl &&& this )
			    >>>
			    arrIO3 CURL.getCont
			  )
		      )
	       >>>
	       ( arr addError
		 |||
		 arr addContent
	       )
	     )
      where
      addContent	:: (AssocList String String, String) -> IOStateArrow s XmlTree XmlTree
      addContent (al, c)
	  = replaceChildren (txt c)
	    >>>
	    seqA (map (uncurry addAttr) al)
-}

getHttpContents		:: IOStateArrow s XmlTree XmlTree
getHttpContents
    = getCont $<<<< ( getAttrValue transferURI
		      &&&
		      getOpt a_proxy
		      &&&
		      getOpt a_use_curl
		      &&&
		      getOpt a_options_curl
		    )
      where
      getOpt opt
	  = getAttrValue0 opt
	    `orElse`
	    getParamString opt

      getCont uri proxy curl curlOpt
	  = applyA ( ( if curl == v_1
		       then ( traceMsg 2 ( "get HTTP via "
					   ++ show ( "curl "
						     ++ (if null proxy then "" else "--proxy " ++ proxy)
						     ++ (if null curlOpt then "" else " " ++ curlOpt)
						     ++ " " ++ uri
						   )
					 )
			      >>>
			      arrIO0 ( CURL.getCont curlOpt uri proxy )
			    )
		       else arrIO0 ( HTTP.getCont         uri proxy )
		     )
		     >>>
		     ( arr addError
		       |||
		       arr addContent
		     )
		   )

      addContent	:: (AssocList String String, String) -> IOStateArrow s XmlTree XmlTree
      addContent (al, c)
	  = replaceChildren (txt c)
	    >>>
	    seqA (map (uncurry addAttr) al)

getURIContents		:: IOStateArrow s XmlTree XmlTree
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
			     ( \ src-> (if null src then "stdin:" else src) )	-- empty document name -> read from stdin
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
	= applyA ( getBaseURI				-- compute the handler and call it
		   >>>
		   traceString 2 (("getURIContents: reading" ++) . show)
		   >>>
		   getSchemeFromURI
		   >>>
		   getProtocolHandler
		 )
	  `orElse`
	  this						-- don't change tree, when no handler can be found
	  
setBaseURIFromDoc	:: IOStateArrow s XmlTree XmlTree
setBaseURIFromDoc
    = perform ( getAttrValue transferURI
		>>>
		isA (isPrefixOf stringProtocol)		-- do not change base URI when reading from a string
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

getXmlContents		:: IOStateArrow s XmlTree XmlTree
getXmlContents
    = getXmlContents' parseXmlDocEncodingSpec
      >>>
      setBaseURIFromDoc

getXmlEntityContents		:: IOStateArrow s XmlTree XmlTree
getXmlEntityContents
    = getXmlContents' parseXmlEntityEncodingSpec
      >>>
      processChildren removeEncodingSpec
      >>>
      setBaseURIFromDoc

getXmlContents'		:: IOStateArrow s XmlTree XmlTree -> IOStateArrow s XmlTree XmlTree
getXmlContents' parseEncodingSpec
    = ( getURIContents
	>>>
	parseEncodingSpec
	>>>
	filterErrorMsg
	>>>
	decodeDocument
	>>>
	perform ( getAttrValue transferURI
		  >>>
		  traceString 1 (("getXmlContents: content read and decoded for " ++) . show)
		)
	>>>
	traceTree
	>>>
	traceSource
      )
      `when`
      isRoot

-- ------------------------------------------------------------

getEncoding	:: IOStateArrow s XmlTree String
getEncoding
    = catA [ xshow getChildren			-- 1. guess: guess encoding by looking at the first few bytes
	     >>>
	     arr guessEncoding
	   , getAttrValue transferEncoding	-- 2. guess: take the transfer encoding
	   , getAttrValue a_encoding		-- 4. guess: take encoding parameter in root node
	   , getParamString a_encoding		-- 5. guess: take encoding parameter in global state
	   , constA utf8			-- default : utf8
	   ]
      >. (head . filter (not . null))		-- make the filter deterministic: take 1. entry from list of guesses


decodeDocument	:: IOStateArrow s XmlTree XmlTree
decodeDocument
    = ( decodeArr $< getEncoding )
      `when`
      isRoot
    where
    decodeArr	:: String -> IOStateArrow s XmlTree XmlTree
    decodeArr enc
	= maybe notFound found . getDecodingFct $ enc
	where
	found df
	    = traceMsg 2 ("decodeDocument: encoding is " ++ show enc)
	      >>>
	      processChildren (decodeText df)
	      >>>
	      addAttr transferEncoding enc

	notFound
	    = issueFatal ("encoding scheme not supported: " ++ show enc)
	      >>>
	      setDocumentStatusFromSystemState "decoding document"

	decodeText df
	    = getText
	      >>> arr df					-- result is (string, [errMsg])
	      >>> ( ( (normalizeNL . fst) ^>> mkText )		-- take string, normalize newline and build text node
		    <+>
		    ( arrL snd					-- take the error messages
		      >>>
		      arr ((enc ++) . (" encoding error" ++))	-- prefix with enc error
		      >>>
		      applyA (arr issueErr)			-- build issueErr arrow and apply
		      >>>
		      none					-- neccessary for type match with <+>
		    )
		  )
      
-- ------------------------------------------------------------
