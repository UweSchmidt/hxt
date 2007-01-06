-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlIOStateArrow
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: XmlIOStateArrow.hs,v 1.39 2006/11/09 20:27:42 hxml Exp $

the basic state arrows for XML processing

A state is needed for global processing options,
like encoding options, document base URI, trace levels
and error message handling

The state is separated into a user defined state
and a system state. The system state contains variables
for error message handling, for tracing, for the document base
for accessing XML documents with relative references, e.g. DTDs,
and a global key value store. This assoc list has strings as keys
and lists of XmlTrees as values. It is used to store arbitrary
XML and text values, e.g. user defined global options.

The user defined part of the store is in the default case empty, defined as ().
It can be extended with an arbitray data type

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlIOStateArrow
    ( -- * Data Types
      XIOState(..),
      XIOSysState(..),
      IOStateArrow,
      IOSArrow,

      -- * Running Arrows
      initialState,
      initialSysState,
      runX,

      -- * User State Manipulation
      getUserState,
      setUserState,
      changeUserState,
      withExtendedUserState,
      withOtherUserState,

      -- * Global System State Access
      getSysParam,
      changeSysParam,

      setParamList,
      setParam,
      unsetParam,
      getParam,
      getAllParams,
      setParamString,
      getParamString,
      setParamInt,
      getParamInt,
      
      -- * Error Message Handling
      clearErrStatus,
      setErrStatus,
      getErrStatus,
      setErrMsgStatus,
      setErrorMsgHandler,

      errorMsgStderr,
      errorMsgCollect,
      errorMsgStderrAndCollect,
      errorMsgIgnore,

      getErrorMessages,

      filterErrorMsg,
      issueWarn,
      issueErr,
      issueFatal,
      setDocumentStatus,
      setDocumentStatusFromSystemState,
      documentStatusOk,

      -- * Document Base
      setBaseURI,
      getBaseURI,
      changeBaseURI,
      setDefaultBaseURI,
      getDefaultBaseURI,
      runInLocalURIContext,

      -- * Tracing
      setTraceLevel,
      getTraceLevel,
      withTraceLevel,
      trace,
      traceMsg,
      traceString,
      traceSource,
      traceTree,
      traceDoc,
      traceState,

      -- * URI Manipulation
      expandURIString,
      expandURI,
      mkAbsURI,

      getFragmentFromURI,
      getPathFromURI,
      getPortFromURI,
      getQueryFromURI,
      getRegNameFromURI,
      getSchemeFromURI,
      getUserInfoFromURI
      )
where

import Control.Arrow				-- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO

import Control.Arrow.IOStateListArrow

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow

import Text.XML.HXT.Arrow.Edit
    ( addHeadlineToXmlDoc
    , treeRepOfXmlDoc
    )

import Data.Maybe

import Network.URI
    ( URI
    , escapeURIChar
    , isUnescapedInURI
    , nonStrictRelativeTo
    , parseURIReference
    , uriAuthority
    , uriFragment
    , uriPath
    , uriPort
    , uriQuery
    , uriRegName
    , uriScheme
    , uriUserInfo
    )

import System.IO
    ( hPutStrLn
    , hFlush
    , stderr
    )

import System.Directory
    ( getCurrentDirectory
    )

-- ------------------------------------------------------------
{- $datatypes -}


-- |
-- predefined system state data type with all components for the
-- system functions, like trace, error handling, ...

data XIOSysState	= XIOSys  { xio_trace			:: ! Int
				  , xio_errorStatus		:: ! Int
				  , xio_errorModule		:: ! String
				  , xio_errorMsgHandler		::   String -> IO ()
				  , xio_errorMsgCollect		:: ! Bool
				  , xio_errorMsgList		:: ! XmlTrees
				  , xio_baseURI			:: ! String
				  , xio_defaultBaseURI		:: ! String
				  , xio_attrList		:: ! (AssocList String XmlTrees)
				  }

-- |
-- state datatype consists of a system state and a user state
-- the user state is not fixed

data XIOState us	= XIOState { xio_sysState		:: ! XIOSysState
				   , xio_userState		:: ! us
				   }

-- |
-- The arrow type for stateful arrows

type IOStateArrow s b c	= IOSLA (XIOState s) b c

-- |
-- The arrow for stateful arrows with no user defined state

type IOSArrow b c	= IOStateArrow () b c

-- ------------------------------------------------------------


-- | the default global state, used as initial state when running an 'IOSArrow' with 'runIOSLA' or
-- 'runX'

initialState	:: us -> XIOState us
initialState s	= XIOState { xio_sysState 	= initialSysState
			   , xio_userState	= s
			   }

initialSysState	:: XIOSysState
initialSysState	= XIOSys { xio_trace		= 0
			 , xio_errorStatus	= c_ok
			 , xio_errorModule	= ""
			 , xio_errorMsgHandler	= hPutStrLn stderr
			 , xio_errorMsgCollect	= False
			 , xio_errorMsgList	= []
			 , xio_baseURI		= ""
			 , xio_defaultBaseURI	= ""
			 , xio_attrList		= []
			 }

-- ------------------------------------------------------------

-- |
-- apply an 'IOSArrow' to an empty root node with 'initialState' () as initial state
--
-- the main entry point for running a state arrow with IO
--
-- when running @ runX f@ an empty XML root node is applied to @f@.
-- usually @f@ will start with a constant arrow (ignoring the input), e.g. a 'Text.XML.HXT.Arrow.ReadDocument.readDocument' arrow.
--
-- for usage see examples with 'Text.XML.HXT.Arrow.WriteDocument.writeDocument'
--
-- if input has to be feed into the arrow use 'Control.Arrow.IOStateListArrow.runIOSLA' like in @ runIOSLA f emptyX inputDoc @

runX		:: IOSArrow XmlTree c -> IO [c]
runX		= runXIOState (initialState ())

runXIOState	:: XIOState s -> IOStateArrow s XmlTree c -> IO [c]
runXIOState s0 f
    = do
      (_finalState, res) <- runIOSLA (emptyRoot >>> f) s0 undefined
      return res
    where
    emptyRoot    = root [] []

-- ------------------------------------------------------------

{- user state -}

-- | read the user defined part of the state

getUserState	:: IOStateArrow s b s
getUserState
    = IOSLA $ \ s _ ->
      return (s, [xio_userState s])

-- | change the user defined part of the state

changeUserState		:: (b -> s -> s) -> IOStateArrow s b b
changeUserState cf
    = IOSLA $ \ s v ->
      let s' = s { xio_userState = cf v (xio_userState s) }
      in return (s', [v])

-- | set the user defined part of the state

setUserState		:: IOStateArrow s s s
setUserState
    = changeUserState const

-- | extend user state
--
-- Run an arrow with an extended user state component, The old component
-- is stored together with a new one in a pair, the arrow is executed with this
-- extended state, and the augmented state component is removed form the state
-- when the arrow has finished its execution

withExtendedUserState	:: s1 -> IOStateArrow (s1, s0) b c -> IOStateArrow s0 b c
withExtendedUserState initS1 f
    = IOSLA $ \ s0 x ->
      do
      ~(finalS, res) <- runIOSLA f ( XIOState { xio_sysState  =          xio_sysState  s0
					      , xio_userState = (initS1, xio_userState s0)
					      }
				   ) x
      return ( XIOState { xio_sysState  =      xio_sysState  finalS
			, xio_userState = snd (xio_userState finalS)
			}
	     , res
	     )

-- | change the type of user state
--
-- This conversion is useful, when running a state arrow with another
-- structure of the user state, e.g. with () when executing some IO arrows

withOtherUserState	:: s1 -> IOStateArrow s1 b c -> IOStateArrow s0 b c
withOtherUserState s1 f
    = IOSLA $ \ s x ->
      do
      (s', res) <- runIOSLA f ( XIOState { xio_sysState  = xio_sysState s
					 , xio_userState = s1
					 }
			      ) x
      return ( XIOState { xio_sysState  = xio_sysState  s'
			, xio_userState = xio_userState s
			}
	     , res
	     )

-- ------------------------------------------------------------

{- $system state params -}

getSysParam	:: (XIOSysState -> c) -> IOStateArrow s b c
getSysParam f
    = IOSLA $ \ s _x ->
      return (s, (:[]) . f . xio_sysState $ s)

changeSysParam		:: (b -> XIOSysState -> XIOSysState) -> IOStateArrow s b b
changeSysParam cf
    = ( IOSLA $ \ s v ->
	let s' = changeSysState (cf v) s
	in return (s', [v])
      )
    where
    changeSysState css s = s { xio_sysState = css (xio_sysState s) }

-- | store a single XML tree in global state under a given attribute name

setParam	:: String -> IOStateArrow s XmlTree XmlTree
setParam n
    = (:[]) ^>> setParamList n

-- | store a list of XML trees in global system state under a given attribute name

setParamList	:: String -> IOStateArrow s XmlTrees XmlTree
setParamList n
    = changeSysParam addE
      >>>
      arrL id
    where
    addE x s = s { xio_attrList = addEntry n x (xio_attrList s) }

-- | remove an entry in global state, arrow input remains unchanged

unsetParam	:: String -> IOStateArrow s b b
unsetParam n
    = changeSysParam delE
    where
    delE _ s = s { xio_attrList = delEntry n (xio_attrList s) }

-- | read an attribute value from global state

getParam	:: String -> IOStateArrow s b XmlTree
getParam n
    = getAllParams
      >>>
      arrL (lookup1 n)

-- | read all attributes from global state

getAllParams	:: IOStateArrow s b (AssocList String XmlTrees)
getAllParams
    = getSysParam xio_attrList

setParamString	:: String -> String -> IOStateArrow s b b
setParamString n v
    = perform ( txt v
		>>>
		setParam n
	      )

-- | read a string value from global state,
-- if parameter not set \"\" is returned

getParamString	:: String -> IOStateArrow s b String
getParamString n
    = xshow (getParam n)

-- | store an int value in global state

setParamInt	:: String -> Int -> IOStateArrow s b b
setParamInt n v
    = setParamString n (show v)

-- | read an int value from global state
-- 
-- > getParamInt 0 myIntAttr

getParamInt	:: Int -> String -> IOStateArrow s b Int
getParamInt def n
    = getParamString n
      >>^
      (\ x -> if null x then def else read x)

-- ------------------------------------------------------------

-- | reset global error variable

changeErrorStatus	:: (Int -> Int -> Int) -> IOStateArrow s Int Int
changeErrorStatus f
    = changeSysParam (\ l s -> s { xio_errorStatus = f l (xio_errorStatus s) })

clearErrStatus		:: IOStateArrow s b b
clearErrStatus
    = perform (constA 0 >>> changeErrorStatus min)

-- | set global error variable

setErrStatus		:: IOStateArrow s Int Int
setErrStatus
    = changeErrorStatus max

-- | read current global error status

getErrStatus		:: IOStateArrow s XmlTree Int
getErrStatus
    = getSysParam xio_errorStatus

-- | raise the global error status level to that of the input tree

setErrMsgStatus	:: IOStateArrow s XmlTree XmlTree
setErrMsgStatus
    = perform ( getErrorLevel
		>>>
		setErrStatus
	      )

-- | set the error message handler and the flag for collecting the errors

setErrorMsgHandler	:: Bool -> (String -> IO ()) -> IOStateArrow s b b
setErrorMsgHandler c f
    = changeSysParam cf
    where
    cf _ s = s { xio_errorMsgHandler = f
	       , xio_errorMsgCollect = c }

-- | error message handler for output to stderr
		  
sysErrorMsg		:: IOStateArrow s XmlTree XmlTree
sysErrorMsg
    = perform ( getErrorLevel &&& getErrorMsg
		>>>
		arr formatErrorMsg
		>>>
		( IOSLA $ \ s e ->
		  do
		  (xio_errorMsgHandler . xio_sysState $ s) e
		  return (s, undefined)
		)
	      )
    where
    formatErrorMsg (level, msg)	= "\n" ++ errClass level ++ ": " ++ msg
    errClass l
	= fromMaybe "fatal error" . lookup l $ msgList
	  where
	  msgList	= [ (c_ok,	"no error")
			  , (c_warn,	"warning")
			  , (c_err,	"error")
			  , (c_fatal,	"fatal error")
			  ]


-- | the default error message handler: error output to stderr

errorMsgStderr		:: IOStateArrow s b b
errorMsgStderr		= setErrorMsgHandler False (hPutStrLn stderr)

-- | error message handler for collecting errors

errorMsgCollect		:: IOStateArrow s b b
errorMsgCollect		= setErrorMsgHandler True (const $ return ())

-- | error message handler for output to stderr and collecting

errorMsgStderrAndCollect	:: IOStateArrow s b b
errorMsgStderrAndCollect	= setErrorMsgHandler True (hPutStrLn stderr)

-- | error message handler for ignoring errors

errorMsgIgnore		:: IOStateArrow s b b
errorMsgIgnore		= setErrorMsgHandler False (const $ return ())

-- |
-- if error messages are collected by the error handler for
-- processing these messages by the calling application,
-- this arrow reads the stored messages and clears the error message store

getErrorMessages	:: IOStateArrow s b XmlTree
getErrorMessages
    = getSysParam (reverse . xio_errorMsgList)		-- reverse the list of errors
      >>>
      clearErrorMsgList					-- clear the error list in the system state
      >>>
      arrL id

clearErrorMsgList	:: IOStateArrow s b b
clearErrorMsgList
    = changeSysParam (\ _ s -> s { xio_errorMsgList = [] } )

addToErrorMsgList	:: IOStateArrow s XmlTree XmlTree
addToErrorMsgList
    = changeSysParam cf
    where
    cf t s = if xio_errorMsgCollect s
	     then s { xio_errorMsgList = t : xio_errorMsgList s }
	     else s

-- ------------------------------------------------------------

-- |
-- filter error messages from input trees and issue errors

filterErrorMsg		:: IOStateArrow s XmlTree XmlTree
filterErrorMsg
    = ( setErrMsgStatus
	>>>
	sysErrorMsg
	>>>
	addToErrorMsgList
	>>>
	none
      )
      `when`
      isError

-- | generate a warnig message

issueWarn		:: String -> IOStateArrow s b b
issueWarn msg		= perform (warn msg  >>> filterErrorMsg)

-- | generate an error message
issueErr		:: String -> IOStateArrow s b b
issueErr msg		= perform (err msg   >>> filterErrorMsg)

-- | generate a fatal error message, e.g. document not found

issueFatal		:: String -> IOStateArrow s b b
issueFatal msg		= perform (fatal msg >>> filterErrorMsg)

-- |
-- add the error level and the module where the error occured
-- to the attributes of a document root node and remove the children when level is greater or equal to 'c_err'.
-- called by 'setDocumentStatusFromSystemState' when the system state indicates an error

setDocumentStatus	:: Int -> String -> IOStateArrow s XmlTree XmlTree
setDocumentStatus level msg
    = ( addAttrl ( sattr a_status (show level)
		   <+>
		   sattr a_module msg
		 )
	>>>
	( if level >= c_err
	  then setChildren []
	  else this
	)
      )
      `when`
      isRoot

-- |
-- check whether the error level attribute in the system state
-- is set to error, in this case the children of the document root are
-- removed and the module name where the error occured and the error level are added as attributes with 'setDocumentStatus'
-- else nothing is changed

setDocumentStatusFromSystemState		:: String -> IOStateArrow s XmlTree XmlTree
setDocumentStatusFromSystemState msg
    = setStatus $< getErrStatus
    where
    setStatus level
	| level <= c_warn	= this
	| otherwise		= setDocumentStatus level msg


-- |
-- check whether tree is a document root and the status attribute has a value less than 'c_err'

documentStatusOk	:: IOStateArrow s XmlTree XmlTree
documentStatusOk
    = isRoot
      >>>
      ( (getAttrValue a_status
	 >>>
	 isA (\ v -> null v || ((read v)::Int) <= c_warn)
	)
	`guards`
	this
      )

-- ------------------------------------------------------------

-- | set the base URI of a document, used e.g. for reading includes, e.g. external entities,
-- the input must be an absolute URI

setBaseURI		:: IOStateArrow s String String
setBaseURI
    = changeSysParam (\ b s -> s { xio_baseURI = b } )
      >>>
      traceString 2 (("setBaseURI: new base URI is " ++) . show)

-- | read the base URI from the globale state

getBaseURI		:: IOStateArrow s b String
getBaseURI
    = getSysParam xio_baseURI
      >>>
      ( ( getDefaultBaseURI
	  >>>
	  setBaseURI		
	  >>>
	  getBaseURI
	)
	`when`
	isA null				-- set and get it, if not yet done
      )

-- | change the base URI with a possibly relative URI, can be used for
-- evaluating the xml:base attribute. Returns the new absolute base URI.
-- Fails, if input is not parsable with parseURIReference
--
-- see also: 'setBaseURI', 'mkAbsURI'

changeBaseURI		:: IOStateArrow s String String
changeBaseURI
    = mkAbsURI
      >>>
      setBaseURI

-- | set the default base URI, if parameter is null, the system base (@ file:\/\/\/\<cwd\>\/ @) is used,
-- else the parameter, must be called before any document is read

setDefaultBaseURI	:: String -> IOStateArrow s b String
setDefaultBaseURI base
    = ( if null base
	then arrIO getDir
	else constA base
      )
      >>>
      changeSysParam (\ b s -> s { xio_defaultBaseURI = b } )
      >>>
      traceString 2 (("setDefaultBaseURI: new default base URI is " ++) . show)
    where
    getDir _ = do
	       cwd <- getCurrentDirectory
	       return ("file://" ++ normalize cwd ++ "/")

    -- under Windows getCurrentDirectory returns something like: "c:\path\to\file"
    -- backslaches are not allowed in URIs and paths must start with a /
    -- so this is transformed into "/c:/path/to/file"

    normalize wd'@(d : ':' : _)
	| d `elem` ['A'..'Z'] || d `elem` ['a'..'z']
	    = '/' : concatMap win32ToUriChar wd'
    normalize wd'
	= concatMap escapeNonUriChar wd'
				 
    win32ToUriChar '\\' = "/"
    win32ToUriChar c    = escapeNonUriChar c

    escapeNonUriChar c  = escapeURIChar isUnescapedInURI c   -- from Network.URI


-- | get the default base URI

getDefaultBaseURI	:: IOStateArrow s b String
getDefaultBaseURI
    = getSysParam xio_defaultBaseURI		-- read default uri in system  state
      >>>
      ( setDefaultBaseURI ""			-- set the default uri in system state
	>>>
	getDefaultBaseURI ) `when` isA null	-- when uri not yet set

-- ------------------------------------------------------------

-- | remember base uri, run an arrow and restore the base URI, used with external entity substitution

runInLocalURIContext	:: IOStateArrow s b c -> IOStateArrow s b c
runInLocalURIContext f
    = ( getBaseURI &&& this )
      >>>
      ( this *** listA f )
      >>>
      ( setBaseURI *** this )
      >>>
      arrL snd

-- ------------------------------------------------------------

-- | set the global trace level

setTraceLevel	:: Int -> IOStateArrow s b b
setTraceLevel l
    = changeSysParam (\ _ s -> s { xio_trace = l } )

-- | read the global trace level

getTraceLevel	:: IOStateArrow s b Int
getTraceLevel
    = getSysParam xio_trace

-- | run an arrow with a given trace level, the old trace level is restored after the arrow execution

withTraceLevel	:: Int -> IOStateArrow s b c -> IOStateArrow s b c
withTraceLevel level f
    = ( getTraceLevel       &&& this )
      >>>
      ( setTraceLevel level *** listA f )
      >>>
      ( restoreTraceLevel   *** this )
      >>>
      arrL snd
    where
    restoreTraceLevel	:: IOStateArrow s Int Int
    restoreTraceLevel
	= setTraceLevel $< this

-- | apply a trace arrow and issue message to stderr

trace		:: Int -> IOStateArrow s b String -> IOStateArrow s b b
trace level trc
    = perform ( trc
		>>>
		arrIO (\ s -> ( do
				hPutStrLn stderr s
				hFlush stderr
			      )
		      )
	      )
      `when` ( getTraceLevel
	       >>>
	       isA (>= level)
	     )

-- | issue a string message as trace 
traceMsg	:: Int -> String -> IOStateArrow s b b
traceMsg level msg
    = perform ( trace level $
		constA ('-' : "- (" ++ show level ++ ") " ++ msg)
	      )

-- | issue the string input of an arrow
traceString	:: Int -> (String -> String) -> IOStateArrow s String String
traceString level f
    = perform (applyA (arr f >>> arr (traceMsg level)))

-- | issue the source representation of a document if trace level >= 3
traceSource	:: IOStateArrow s XmlTree XmlTree
traceSource 
    = trace 3 $
      xshow this

-- | issue the tree representation of a document if trace level >= 4
traceTree	:: IOStateArrow s XmlTree XmlTree
traceTree
    = trace 4 $
      xshow ( treeRepOfXmlDoc
	      >>>
	      addHeadlineToXmlDoc
	      >>>
	      getChildren
	    )

-- | trace a main computation step
-- issue a message when trace level >= 1, issue document source if level >= 3, issue tree when level is >= 4

traceDoc	:: String -> IOStateArrow s XmlTree XmlTree
traceDoc msg
    = traceMsg 1 msg
      >>>
      traceSource
      >>>
      traceTree

-- | trace the global state

traceState	:: IOStateArrow s b b
traceState
    = perform ( xshow ( (getAllParams >>. concat)
			>>>
			applyA (arr formatParam)
		      )
		>>>
		traceString 2 ("global state:\n" ++)
	      )
      where
      -- formatParam	:: (String, XmlTrees) -> IOStateArrow s b1 XmlTree
      formatParam (n, v)
	  = mkelem "param" [sattr "name" n] [arrL (const v)] <+> txt "\n"

-- ----------------------------------------------------------

-- | compute the absolut URI for a given URI and a base URI

expandURIString	:: String -> String -> Maybe String
expandURIString uri base
    = do
      base' <- parseURIReference base
      uri'  <- parseURIReference uri
      abs'  <- nonStrictRelativeTo uri' base'
      return $ show abs'

-- | arrow variant of 'expandURIString', fails if 'expandURIString' returns Nothing

expandURI		:: ArrowXml a => a (String, String) String
expandURI
    = arrL (maybeToList . uncurry expandURIString)

-- | arrow for expanding an input URI into an absolute URI using global base URI, fails if input is not a legal URI

mkAbsURI		:: IOStateArrow s String String
mkAbsURI
    = ( this &&& getBaseURI ) >>> expandURI

-- | arrow for selecting the scheme (protocol) of the URI, fails if input is not a legal URI.
--
-- See Network.URI for URI components

getSchemeFromURI	:: ArrowList a => a String String
getSchemeFromURI	= getPartFromURI scheme
    where
    scheme = init . uriScheme

-- | arrow for selecting the registered name (host) of the URI, fails if input is not a legal URI

getRegNameFromURI	:: ArrowList a => a String String
getRegNameFromURI	= getPartFromURI host
    where
    host = maybe "" uriRegName . uriAuthority

-- | arrow for selecting the port number of the URI without leading \':\', fails if input is not a legal URI

getPortFromURI		:: ArrowList a => a String String
getPortFromURI		= getPartFromURI port
    where
    port = dropWhile (==':') . maybe "" uriPort . uriAuthority

-- | arrow for selecting the user info of the URI without trailing \'\@\', fails if input is not a legal URI

getUserInfoFromURI		:: ArrowList a => a String String
getUserInfoFromURI		= getPartFromURI ui
    where
    ui = reverse . dropWhile (=='@') . reverse . maybe "" uriUserInfo . uriAuthority

-- | arrow for computing the path component of an URI, fails if input is not a legal URI

getPathFromURI		:: ArrowList a => a String String
getPathFromURI		= getPartFromURI uriPath

-- | arrow for computing the query component of an URI, fails if input is not a legal URI

getQueryFromURI		:: ArrowList a => a String String
getQueryFromURI		= getPartFromURI uriQuery

-- | arrow for computing the fragment component of an URI, fails if input is not a legal URI

getFragmentFromURI	:: ArrowList a => a String String
getFragmentFromURI	= getPartFromURI uriFragment

-- | arrow for computing the path component of an URI, fails if input is not a legal URI

getPartFromURI		:: ArrowList a => (URI -> String) -> a String String
getPartFromURI sel
    = arrL (maybeToList . getPart)
      where
      getPart s = do
		  uri <- parseURIReference s
		  return (sel uri)

-- ------------------------------------------------------------
