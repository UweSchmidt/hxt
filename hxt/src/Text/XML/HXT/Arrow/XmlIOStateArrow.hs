-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlIOStateArrow
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

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
      SysConfig,
      SysConfigList,

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
      withoutUserState,

      -- * Global System State Access
      getSysParam,
      setSysParam,
      chgSysParam,
      configSysParam,
      configSysParams,
      localSysParam,
      incrSysParam,
      getSysConfigOption,

      setParam,
      unsetParam,
      getParam,
      getAllParams,
      setParamString,
      setParamInt,
      getParamInt,

      -- * RelaxNG Handling
      setRelaxParam,
      getRelaxParam,

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
      setTraceCmd,
      getTraceCmd,
      trace,
      traceMsg,
      traceValue,
      traceString,
      traceSource,
      traceTree,
      traceDoc,

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
      getUserInfoFromURI,

      -- * Mime Type Handling
      getMimeTypeTable,
      setMimeTypeTable,
      setMimeTypeTableFromFile


      -- * State selectors
      , theSysState
      , theUserState
      , theErrorStatus
      , theErrorMsgHandler
      , theErrorMsgCollect
      , theErrorMsgList
      , theBaseURI
      , theDefaultBaseURI
      , theReadConfig
      , theTraceLevel
      , theTraceCmd
      , theTrace
      , theMimeTypes
      , theMimeTypeFile
      , theParseByMimeType
      , theAcceptedMimeTypes
      , theAttrList
      , theWarnings
      , theRemoveWS
      , theParseHTML
      , theLowerCaseNames
      , thePreserveComment
      , theValidate
      , theCheckNamespaces
      , theCanonicalize
      , theIgnoreNoneXmlContents
      , theTagSoup
      , theTagSoupParser
      , theRelaxCollectErrors
      , theRelaxNoOfErrors
      , theRelaxDefineId

      , withAcceptedMimeTypes
      , withWarnings
      , withErrors
      , withMimeTypeFile
      , withParseByMimeType
      , withRemoveWS
      , withPreserveComment
      , withTrace
      , withParseHTML
      , withValidate
      , withCheckNamespaces
      , withCanonicalize
      , withIgnoreNoneXmlContents

      , optionToSysConfig

      , subS
      , pairS
      , putS
      )
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO
import Control.Arrow.IOStateListArrow
import Control.Arrow.ListArrow          ( runLA )

import Control.Monad                    ( mzero
                                        , mplus )
import Control.DeepSeq

import Data.Char                        ( isDigit )

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow

import Text.XML.HXT.Arrow.Edit          ( addHeadlineToXmlDoc
                                        , treeRepOfXmlDoc
                                        , indentDoc
                                        )

import Data.Maybe

import Network.URI                      ( URI
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

import System.IO                        ( hPutStrLn
                                        , hFlush
                                        , stderr
                                        )

import System.Directory                 ( getCurrentDirectory )

-- ------------------------------------------------------------
{- datatypes -}

-- |
-- state datatype consists of a system state and a user state
-- the user state is not fixed

data XIOState us        = XIOState { xio_sysState               :: ! XIOSysState
                                   , xio_userState              :: ! us
                                   }

instance (NFData us) => NFData (XIOState us) where
    rnf (XIOState sys usr)      = rnf sys `seq` rnf usr

-- |
-- The arrow type for stateful arrows

type IOStateArrow s b c = IOSLA (XIOState s) b c

-- |
-- The arrow for stateful arrows with no user defined state

type IOSArrow b c       = IOStateArrow () b c

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

runX            :: IOSArrow XmlTree c -> IO [c]
runX            = runXIOState (initialState ())


runXIOState     :: XIOState s -> IOStateArrow s XmlTree c -> IO [c]
runXIOState s0 f
    = do
      (_finalState, res) <- runIOSLA (emptyRoot >>> f) s0 undefined
      return res
    where
    emptyRoot    = root [] []


-- | the default global state, used as initial state when running an 'IOSArrow' with 'runIOSLA' or
-- 'runX'

initialState    :: us -> XIOState us
initialState s  = XIOState { xio_sysState       = initialSysState
                           , xio_userState      = s
                           }

-- ------------------------------------------------------------

-- user state functions

-- | read the user defined part of the state

getUserState    :: IOStateArrow s b s
getUserState
    = IOSLA $ \ s _ ->
      return (s, [xio_userState s])

-- | change the user defined part of the state

changeUserState         :: (b -> s -> s) -> IOStateArrow s b b
changeUserState cf
    = IOSLA $ \ s v ->
      let s' = s { xio_userState = cf v (xio_userState s) }
      in return (s', [v])

-- | set the user defined part of the state

setUserState            :: IOStateArrow s s s
setUserState
    = changeUserState const

-- | extend user state
--
-- Run an arrow with an extended user state component, The old component
-- is stored together with a new one in a pair, the arrow is executed with this
-- extended state, and the augmented state component is removed form the state
-- when the arrow has finished its execution

withExtendedUserState   :: s1 -> IOStateArrow (s1, s0) b c -> IOStateArrow s0 b c
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

withOtherUserState      :: s1 -> IOStateArrow s1 b c -> IOStateArrow s0 b c
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

withoutUserState      :: IOSArrow b c -> IOStateArrow s0 b c
withoutUserState      = withOtherUserState ()

-- ------------------------------------------------------------

type Selector s a       = (s -> a, a -> s -> s)

subS                    :: Selector b c -> Selector a b -> Selector a c
subS (g2, s2) (g1, s1)  = ( g2 . g1
                          , s1s2
                          )
                          where
                          s1s2 x s = s'
                              where
                              x1  = g1 s
                              x1' = s2 x x1
                              s'  = s1 x1' s

pairS                   :: Selector s a -> Selector s b -> Selector s (a, b)
pairS (g1, s1) (g2, s2) = ( g1 &&& g2
                          , \ (x, y) -> s2 y . s1 x
                          )

chgS                    :: Selector s a -> (a -> a) -> (s -> s)
chgS (g, s) f x         = s (f (g x)) x

getS                    :: Selector s a -> s -> a
getS                    = fst                           -- getS (g, _s) x = g x

putS                    :: Selector s a -> a -> (s -> s)
putS s v                = chgS s (const v)

-- ------------------------------------------------------------

-- system state structure and acces functions

-- |
-- predefined system state data type with all components for the
-- system functions, like trace, error handling, ...

data XIOSysState        = XIOSys  { xio_traceLevel              :: ! Int
                                  , xio_traceCmd                ::   Int -> String -> IO ()
                                  , xio_errorStatus             :: ! Int
                                  , xio_errorMsgHandler         ::   String -> IO ()
                                  , xio_errorMsgCollect         :: ! Bool
                                  , xio_errorMsgList            :: ! XmlTrees
                                  , xio_baseURI                 :: ! String
                                  , xio_defaultBaseURI          :: ! String
                                  , xio_attrList                :: ! Attributes
                                  , xio_readConfig              :: ! XIOParseConfig
                                  , xio_relaxConfig             ::   XIORelaxConfig
                                  }

data XIOParseConfig	= XIOPcfg { xio_mimeTypes               ::   MimeTypeTable
                                  , xio_mimeTypeFile            ::   String
                                  , xio_acceptedMimeTypes	::   [String]
                                  , xio_warnings                :: ! Bool
                                  , xio_removeWS                :: ! Bool
                                  , xio_parseByMimeType         :: ! Bool
                                  , xio_parseHTML               :: ! Bool
                                  , xio_lowerCaseNames          :: ! Bool
                                  , xio_preserveComment         :: ! Bool
                                  , xio_validate                :: ! Bool
                                  , xio_checkNamespaces         :: ! Bool
                                  , xio_canonicalize            :: ! Bool
                                  , xio_ignoreNoneXmlContents   :: ! Bool
                                  , xio_tagSoup                 :: ! Bool
                                  , xio_tagSoupParser           ::   IOSArrow XmlTree XmlTree 
                                  }

data XIORelaxConfig     = XIORxc  { xio_relaxTrees              ::   AssocList String XmlTrees
                                  , xio_relaxSchema             ::   String
                                  , xio_relaxValidate           :: ! Bool
                                  , xio_relaxCollectErrors      :: ! Bool
                                  , xio_relaxNoOfErrors         :: ! Int
                                  , xio_relaxDefineId           :: ! Int                                  }

instance NFData XIOSysState		-- all fields of interest are strict

type SysConfig          	= XIOSysState -> XIOSysState
type SysConfigList		= [SysConfig]

-- ------------------------------

theSysState                     :: Selector (XIOState us) XIOSysState
theSysState                     = ( xio_sysState,        \ x s -> s { xio_sysState = x} )

theUserState                    :: Selector (XIOState us) us
theUserState                    = ( xio_userState,       \ x s -> s { xio_userState = x} )

theReadConfig                   :: Selector XIOSysState XIOParseConfig
theReadConfig                   = ( xio_readConfig,      \ x s -> s { xio_readConfig = x} )

theRelaxConfig                  :: Selector XIOSysState XIORelaxConfig
theRelaxConfig                  = ( xio_relaxConfig,      \ x s -> s { xio_relaxConfig = x} )

theRelaxAttrList                :: Selector XIOSysState (AssocList String XmlTrees)
theRelaxAttrList                = ( xio_relaxTrees,      \ x s -> s { xio_relaxTrees = x} )
                                  `subS` theRelaxConfig

theRelaxCollectErrors           :: Selector XIOSysState Bool
theRelaxCollectErrors           = ( xio_relaxCollectErrors, \ x s -> s { xio_relaxCollectErrors = x} )
                                  `subS` theRelaxConfig

theRelaxNoOfErrors              :: Selector XIOSysState Int
theRelaxNoOfErrors              = ( xio_relaxNoOfErrors, \ x s -> s { xio_relaxNoOfErrors = x} )
                                  `subS` theRelaxConfig

theRelaxDefineId                :: Selector XIOSysState Int
theRelaxDefineId                = ( xio_relaxDefineId, \ x s -> s { xio_relaxDefineId = x} )
                                  `subS` theRelaxConfig

theErrorStatus                  :: Selector XIOSysState Int
theErrorStatus                  = ( xio_errorStatus,     \ x s -> s { xio_errorStatus = x } )

theErrorMsgHandler              :: Selector XIOSysState (String -> IO ())
theErrorMsgHandler              = ( xio_errorMsgHandler, \ x s -> s { xio_errorMsgHandler = x } )

theErrorMsgCollect              :: Selector XIOSysState Bool
theErrorMsgCollect              = ( xio_errorMsgCollect, \ x s -> s { xio_errorMsgCollect = x } )

theErrorMsgList                 :: Selector XIOSysState XmlTrees
theErrorMsgList                 = ( xio_errorMsgList,    \ x s -> s { xio_errorMsgList = x } )

theBaseURI			:: Selector XIOSysState String
theBaseURI                      = ( xio_baseURI,         \ x s -> s { xio_baseURI = x } )

theDefaultBaseURI		:: Selector XIOSysState String
theDefaultBaseURI               = ( xio_defaultBaseURI,  \ x s -> s { xio_defaultBaseURI = x } )

theTraceLevel			:: Selector XIOSysState Int
theTraceLevel                   = ( xio_traceLevel,      \ x s -> s { xio_traceLevel = x } )

theTraceCmd			:: Selector XIOSysState (Int -> String -> IO ())
theTraceCmd                     = ( xio_traceCmd,        \ x s -> s { xio_traceCmd = x } )

theTrace			:: Selector XIOSysState (Int, Int -> String -> IO ())
theTrace                        = theTraceLevel `pairS` theTraceCmd

theAttrList			:: Selector XIOSysState Attributes
theAttrList                     = ( xio_attrList,        \ x s -> s { xio_attrList = x } )

theMimeTypes			:: Selector XIOSysState MimeTypeTable
theMimeTypes                    = ( xio_mimeTypes,       \ x s -> s { xio_mimeTypes = x } )
                                  `subS` theReadConfig

theMimeTypeFile			:: Selector XIOSysState String
theMimeTypeFile                    = ( xio_mimeTypeFile, \ x s -> s { xio_mimeTypeFile = x } )
                                  `subS` theReadConfig

theAcceptedMimeTypes		:: Selector XIOSysState [String]
theAcceptedMimeTypes            = ( xio_acceptedMimeTypes,       \ x s -> s { xio_acceptedMimeTypes = x } )
                                  `subS` theReadConfig

theWarnings			:: Selector XIOSysState Bool
theWarnings                     = ( xio_warnings,        \ x s -> s { xio_warnings = x } )
                                  `subS` theReadConfig

theRemoveWS			:: Selector XIOSysState Bool
theRemoveWS                     = ( xio_removeWS,        \ x s -> s { xio_removeWS = x } )
                                  `subS` theReadConfig

thePreserveComment		:: Selector XIOSysState Bool
thePreserveComment              = ( xio_preserveComment, \ x s -> s { xio_preserveComment = x } )
                                  `subS` theReadConfig

theParseByMimeType		:: Selector XIOSysState Bool
theParseByMimeType              = ( xio_parseByMimeType, \ x s -> s { xio_parseByMimeType = x } )
                                  `subS` theReadConfig

theParseHTML			:: Selector XIOSysState Bool
theParseHTML                    = ( xio_parseHTML, \ x s -> s { xio_parseHTML = x } )
                                  `subS` theReadConfig

theLowerCaseNames	        :: Selector XIOSysState Bool
theLowerCaseNames               = ( xio_lowerCaseNames, \ x s -> s { xio_lowerCaseNames = x } )
                                  `subS` theReadConfig

theValidate			:: Selector XIOSysState Bool
theValidate                     = ( xio_validate, \ x s -> s { xio_validate = x } )
                                  `subS` theReadConfig

theCheckNamespaces		:: Selector XIOSysState Bool
theCheckNamespaces              = ( xio_checkNamespaces, \ x s -> s { xio_checkNamespaces = x } )
                                  `subS` theReadConfig

theCanonicalize			:: Selector XIOSysState Bool
theCanonicalize                 = ( xio_canonicalize, \ x s -> s { xio_canonicalize = x } )
                                  `subS` theReadConfig

theIgnoreNoneXmlContents	:: Selector XIOSysState Bool
theIgnoreNoneXmlContents        = ( xio_ignoreNoneXmlContents, \ x s -> s { xio_ignoreNoneXmlContents = x } )
                                  `subS` theReadConfig

theTagSoup			:: Selector XIOSysState Bool
theTagSoup                      = ( xio_tagSoup,        \ x s -> s { xio_tagSoup = x } )
                                  `subS` theReadConfig

theTagSoupParser		:: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theTagSoupParser                = ( xio_tagSoupParser,  \ x s -> s { xio_tagSoupParser = x } )
                                  `subS` theReadConfig

-- ------------------------------

-- config options

yes				:: Bool
yes				= True

no				:: Bool
no				= False

withTrace			:: Int -> SysConfig
withTrace			= putS theTraceLevel

withAcceptedMimeTypes		:: [String] -> SysConfig
withAcceptedMimeTypes           = putS theAcceptedMimeTypes

withMimeTypeFile		:: String -> SysConfig
withMimeTypeFile                = putS theMimeTypeFile

withWarnings			:: Bool -> SysConfig
withWarnings			= putS theWarnings

withErrors			:: Bool -> SysConfig
withErrors b			= putS theErrorMsgHandler h
    where
    h | b                       = hPutStrLn stderr
      | otherwise               = const $ return ()

withRemoveWS			:: Bool -> SysConfig
withRemoveWS			= putS theRemoveWS

withPreserveComment		:: Bool -> SysConfig
withPreserveComment		= putS thePreserveComment

withParseByMimeType		:: Bool -> SysConfig
withParseByMimeType		= putS theParseByMimeType

withParseHTML    		:: Bool -> SysConfig
withParseHTML    		= putS theParseHTML

withValidate    		:: Bool -> SysConfig
withValidate    		= putS theValidate

withCheckNamespaces    		:: Bool -> SysConfig
withCheckNamespaces    		= putS theCheckNamespaces

withCanonicalize    		:: Bool -> SysConfig
withCanonicalize    		= putS theCanonicalize

withIgnoreNoneXmlContents    	:: Bool -> SysConfig
withIgnoreNoneXmlContents    	= putS theIgnoreNoneXmlContents

optionToSysConfig		:: (String, String) -> SysConfig
optionToSysConfig (n, v)
    | n == a_trace              	= putS theTraceLevel      	(toInt 0 v)
    | n == a_issue_warnings		= putS theWarnings        	(isTrueValue v)
    | n == a_remove_whitespace		= putS theRemoveWS        	(isTrueValue v)
    | n == a_preserve_comment   	= putS thePreserveComment 	(isTrueValue v) 
    | n == a_parse_by_mimetype  	= putS theParseByMimeType 	(isTrueValue v)
    | n == a_mime_types                 = putS theMimeTypeFile          v
    | n == a_parse_html         	= putS theParseHTML       	(isTrueValue v)
    | n == a_validate           	= putS theValidate        	(isTrueValue v)
    | n == a_check_namespaces   	= putS theCheckNamespaces 	(isTrueValue v)
    | n == a_canonicalize       	= putS theCanonicalize    	(isTrueValue v)
    | n == a_ignore_none_xml_contents   = putS theIgnoreNoneXmlContents (isTrueValue v)
    | n == a_accept_mimetypes           = putS theAcceptedMimeTypes     (words v)
    | n == a_tagsoup                    = putS theTagSoup               (isTrueValue v)
    | otherwise				= chgS theAttrList        	(addEntry n v)
    where
    a_tagsoup				= "tagsoup"
    -- mkt                                 = runLA mkText	-- TODO

toInt				:: Int -> String -> Int
toInt def s
        | not (null s) && all isDigit s	= read s
        | otherwise                     = def

-- ------------------------------------------------------------

getSysConfigOption	:: String -> SysConfigList -> String
getSysConfigOption n c	= lookup1 n $ tl
    where
    s  = (foldr (.) id c) initialSysState
    tl = getS theAttrList s
    -- t2s = concat . runLA ( xshow unlistA ) -- TODO

-- ------------------------------------------------------------

initialSysState 		:: XIOSysState
initialSysState 		= XIOSys
                                  { xio_traceLevel       = 0
                                  , xio_traceCmd         = traceOutputToStderr
                                  , xio_errorStatus      = c_ok
                                  , xio_errorMsgHandler  = hPutStrLn stderr
                                  , xio_errorMsgCollect  = False
                                  , xio_errorMsgList     = []
                                  , xio_baseURI          = ""
                                  , xio_defaultBaseURI   = ""
                                  , xio_attrList         = []
                                  , xio_readConfig       = initialReadConfig
                                  , xio_relaxConfig      = initialRelaxConfig
                                  }

initialReadConfig		:: XIOParseConfig
initialReadConfig		= XIOPcfg
                                  { xio_mimeTypes        	= defaultMimeTypeTable
                                  , xio_mimeTypeFile            = ""
                                  , xio_acceptedMimeTypes	= []
                                  , xio_warnings         	= True
                                  , xio_removeWS         	= True
                                  , xio_parseByMimeType  	= False
                                  , xio_parseHTML        	= False
                                  , xio_lowerCaseNames          = False
                                  , xio_tagSoup                 = False
                                  , xio_preserveComment  	= False
                                  , xio_validate         	= True
                                  , xio_checkNamespaces  	= False
                                  , xio_canonicalize     	= True
                                  , xio_ignoreNoneXmlContents   = False
                                  , xio_tagSoupParser           = dummyTagSoupParser
                                  }
    where
    dummyTagSoupParser		= issueFatal $
                                  "TagSoup parser not configured, " ++
                                  "please install package hxt-tagsoup and use 'withTagSoup' system config option"

initialRelaxConfig              :: XIORelaxConfig
initialRelaxConfig              = XIORxc
                                  { xio_relaxTrees              = []
                                  , xio_relaxSchema             = ""
                                  , xio_relaxValidate           = False
                                  , xio_relaxCollectErrors      = True
                                  , xio_relaxNoOfErrors         = 0
                                  , xio_relaxDefineId           = 0
                                  }

-- ------------------------------

getSysParam                     :: Selector XIOSysState c -> IOStateArrow s b c
getSysParam sel                 = IOSLA $ \ s _x ->
                                  return (s, (:[]) . getS (sel `subS` theSysState) $ s)

setSysParam                     :: Selector XIOSysState c -> IOStateArrow s c c
setSysParam sel                 = (\ v -> configSysParam $ putS sel v) $< this

chgSysParam                    :: Selector XIOSysState c -> (b -> c -> c) -> IOStateArrow s b b
chgSysParam sel op             = (\ v -> configSysParam $ chgS sel (op v)) $< this

configSysParam                  :: SysConfig -> IOStateArrow s c c
configSysParam cf               = IOSLA $ \ s v ->
                                  return (chgS theSysState cf s, [v])

configSysParams                 :: SysConfigList -> IOStateArrow s c c
configSysParams cfs             = configSysParam $ foldr (.) id $ cfs

localSysParam			:: Selector XIOSysState c -> IOStateArrow s a b -> IOStateArrow s a b
localSysParam sel f		= IOSLA $ \ s0 v ->
                                  let sel' = sel `subS` theSysState in
                                  let c0   = getS sel' s0 in
                                  do
                                  (s1, res) <- runIOSLA f s0 v
                                  return (putS sel' c0 s1, res)

incrSysParam                    :: Selector XIOSysState Int -> IOStateArrow s a Int
incrSysParam cnt                = getSysParam cnt
                                  >>>
                                  arr (+1)
                                  >>>
                                  setSysParam cnt
                                  >>>
                                  arr (\ x -> x - 1)
                                  
-- ------------------------------

-- | store a string in global state under a given attribute name

setParam        	:: String -> IOStateArrow s String String
setParam n              = chgSysParam theAttrList (addEntry n)

-- | remove an entry in global state, arrow input remains unchanged

unsetParam      	:: String -> IOStateArrow s b b
unsetParam n    	= configSysParam $ chgS theAttrList (delEntry n)

-- | read an attribute value from global state

getParam        	:: String -> IOStateArrow s b String
getParam n	        = getSysParam theAttrList
                          >>^
                          lookup1 n

-- | read all attributes from global state

getAllParams    	:: IOStateArrow s b Attributes
getAllParams		= getSysParam theAttrList


setParamString  	:: String -> String -> IOStateArrow s b b
setParamString n v      = perform ( constA v
                                    >>>
                                    setParam n
                                  )

-- | store an int value in global state

setParamInt     	:: String -> Int -> IOStateArrow s b b
setParamInt n v         = setParamString n (show v)

-- | read an int value from global state
--
-- > getParamInt 0 myIntAttr

getParamInt     	:: Int -> String -> IOStateArrow s b Int
getParamInt def n       = getParam n
                          >>^
                          toInt def

setRelaxParam        	:: String -> IOStateArrow s XmlTrees XmlTree
setRelaxParam n         = chgSysParam theRelaxAttrList (addEntry n)
                          >>>
                          arrL id

getRelaxParam        	:: String -> IOStateArrow s b XmlTree
getRelaxParam n	        = getSysParam theRelaxAttrList
                          >>>
                          arrL (lookup1 n)

-- ------------------------------------------------------------

changeErrorStatus       :: (Int -> Int -> Int) -> IOStateArrow s Int Int
changeErrorStatus f     = chgSysParam theErrorStatus f

-- | reset global error variable

clearErrStatus          :: IOStateArrow s b b
clearErrStatus          = configSysParam $ putS theErrorStatus 0

-- | set global error variable

setErrStatus            :: IOStateArrow s Int Int
setErrStatus            = changeErrorStatus max

-- | read current global error status

getErrStatus            :: IOStateArrow s XmlTree Int
getErrStatus            = getSysParam theErrorStatus

-- ------------------------------------------------------------

-- | raise the global error status level to that of the input tree

setErrMsgStatus         :: IOStateArrow s XmlTree XmlTree
setErrMsgStatus         = perform
                          ( getErrorLevel >>> setErrStatus )

-- | set the error message handler and the flag for collecting the errors

setErrorMsgHandler      :: Bool -> (String -> IO ()) -> IOStateArrow s b b
setErrorMsgHandler c f  = configSysParam $ putS (theErrorMsgCollect `pairS` theErrorMsgHandler) (c, f)

-- | error message handler for output to stderr

sysErrorMsg             :: IOStateArrow s XmlTree XmlTree
sysErrorMsg             = perform
                          ( getErrorLevel &&& getErrorMsg
                            >>>
                            arr formatErrorMsg
                            >>>
                            getSysParam theErrorMsgHandler &&& this
                            >>>
                            arrIO (\ (h, msg) -> h msg)
                          )
    where
    formatErrorMsg (level, msg) = "\n" ++ errClass level ++ ": " ++ msg
    errClass l          = fromMaybe "fatal error" . lookup l $ msgList
        where
        msgList         = [ (c_ok,      "no error")
                          , (c_warn,    "warning")
                          , (c_err,     "error")
                          , (c_fatal,   "fatal error")
                          ]


-- | the default error message handler: error output to stderr

errorMsgStderr          :: IOStateArrow s b b
errorMsgStderr          = setErrorMsgHandler False (hPutStrLn stderr)

-- | error message handler for collecting errors

errorMsgCollect         :: IOStateArrow s b b
errorMsgCollect         = setErrorMsgHandler True (const $ return ())

-- | error message handler for output to stderr and collecting

errorMsgStderrAndCollect        :: IOStateArrow s b b
errorMsgStderrAndCollect        = setErrorMsgHandler True (hPutStrLn stderr)

-- | error message handler for ignoring errors

errorMsgIgnore          :: IOStateArrow s b b
errorMsgIgnore          = setErrorMsgHandler False (const $ return ())

-- |
-- if error messages are collected by the error handler for
-- processing these messages by the calling application,
-- this arrow reads the stored messages and clears the error message store

getErrorMessages        :: IOStateArrow s b XmlTree
getErrorMessages        = getSysParam theErrorMsgList
                          >>>
                          configSysParam (putS theErrorMsgList [])
                          >>>
                          arrL reverse

addToErrorMsgList       :: IOStateArrow s XmlTree XmlTree
addToErrorMsgList       = chgSysParam
                          ( theErrorMsgCollect `pairS` theErrorMsgList )
                          ( \ e (cs, es) -> (cs, if cs then e : es else es) )

-- ------------------------------------------------------------

-- |
-- filter error messages from input trees and issue errors

filterErrorMsg          :: IOStateArrow s XmlTree XmlTree
filterErrorMsg          = ( setErrMsgStatus
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

issueWarn               :: String -> IOStateArrow s b b
issueWarn msg           = perform (warn msg  >>> filterErrorMsg)

-- | generate an error message
issueErr                :: String -> IOStateArrow s b b
issueErr msg            = perform (err msg   >>> filterErrorMsg)

-- | generate a fatal error message, e.g. document not found

issueFatal              :: String -> IOStateArrow s b b
issueFatal msg          = perform (fatal msg >>> filterErrorMsg)

-- |
-- add the error level and the module where the error occured
-- to the attributes of a document root node and remove the children when level is greater or equal to 'c_err'.
-- called by 'setDocumentStatusFromSystemState' when the system state indicates an error

setDocumentStatus       :: Int -> String -> IOStateArrow s XmlTree XmlTree
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

setDocumentStatusFromSystemState        :: String -> IOStateArrow s XmlTree XmlTree
setDocumentStatusFromSystemState msg
                                = setStatus $< getErrStatus
    where
    setStatus level
        | level <= c_warn       = this
        | otherwise             = setDocumentStatus level msg


-- |
-- check whether tree is a document root and the status attribute has a value less than 'c_err'

documentStatusOk        :: ArrowXml a => a XmlTree XmlTree
documentStatusOk        = isRoot
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

setBaseURI              :: IOStateArrow s String String
setBaseURI		= setSysParam theBaseURI
                          >>>
                          traceValue 2 (("setBaseURI: new base URI is " ++) . show)

-- | read the base URI from the globale state

getBaseURI              :: IOStateArrow s b String
getBaseURI              = getSysParam theBaseURI
                          >>>
                          ( ( getDefaultBaseURI
                              >>>
                              setBaseURI
                              >>>
                              getBaseURI
                            )
                            `when`
                            isA null                                -- set and get it, if not yet done
                          )

-- | change the base URI with a possibly relative URI, can be used for
-- evaluating the xml:base attribute. Returns the new absolute base URI.
-- Fails, if input is not parsable with parseURIReference
--
-- see also: 'setBaseURI', 'mkAbsURI'

changeBaseURI           :: IOStateArrow s String String
changeBaseURI           = mkAbsURI >>> setBaseURI

-- | set the default base URI, if parameter is null, the system base (@ file:\/\/\/\<cwd\>\/ @) is used,
-- else the parameter, must be called before any document is read

setDefaultBaseURI       :: String -> IOStateArrow s b String
setDefaultBaseURI base  = ( if null base
                            then arrIO getDir
                            else constA base
                          )
                          >>>
                          setSysParam theDefaultBaseURI
                          >>>
                          traceValue 2 (("setDefaultBaseURI: new default base URI is " ++) . show)
    where
    getDir _ 		= do
                          cwd <- getCurrentDirectory
                          return ("file://" ++ normalize cwd ++ "/")

    -- under Windows getCurrentDirectory returns something like: "c:\path\to\file"
    -- backslaches are not allowed in URIs and paths must start with a /
    -- so this is transformed into "/c:/path/to/file"

    normalize wd'@(d : ':' : _)
        | d `elem` ['A'..'Z']
          ||
          d `elem` ['a'..'z']
                        = '/' : concatMap win32ToUriChar wd'
    normalize wd'       = concatMap escapeNonUriChar wd'

    win32ToUriChar '\\' = "/"
    win32ToUriChar c    = escapeNonUriChar c

    escapeNonUriChar c  = escapeURIChar isUnescapedInURI c   -- from Network.URI


-- | get the default base URI

getDefaultBaseURI       :: IOStateArrow s b String
getDefaultBaseURI       = getSysParam theDefaultBaseURI            -- read default uri in system  state
                          >>>
                          ( ( setDefaultBaseURI ""                  -- set the default uri in system state
                              >>>
                              getDefaultBaseURI
                            )
                            `when` isA null
                          )                                         -- when uri not yet set

-- ------------------------------------------------------------

-- | remember base uri, run an arrow and restore the base URI, used with external entity substitution

runInLocalURIContext    :: IOStateArrow s b c -> IOStateArrow s b c
runInLocalURIContext f  = localSysParam theBaseURI f

-- ------------------------------------------------------------

-- | set the global trace level

setTraceLevel   	:: Int -> IOStateArrow s b b
setTraceLevel l		= configSysParam $ withTrace l

-- | read the global trace level

getTraceLevel   	:: IOStateArrow s b Int
getTraceLevel           = getSysParam theTraceLevel

-- | set the global trace command. This command does the trace output

setTraceCmd     	:: (Int -> String -> IO ()) -> IOStateArrow s b b
setTraceCmd c		= configSysParam $ putS theTraceCmd c

-- | acces the command for trace output

getTraceCmd     	:: IOStateArrow a b (Int -> String -> IO ())
getTraceCmd             = getSysParam theTraceCmd

-- | run an arrow with a given trace level, the old trace level is restored after the arrow execution

withTraceLevel  	:: Int -> IOStateArrow s b c -> IOStateArrow s b c
withTraceLevel level f  = localSysParam theTraceLevel $ setTraceLevel level >>> f

-- | apply a trace arrow and issue message to stderr

trace           	:: Int -> IOStateArrow s b String -> IOStateArrow s b b
trace level trc         = perform ( trc
                                    >>>
                                    ( getTraceCmd &&& this )
                                    >>>
                                    arrIO (\ (cmd, msg) -> cmd level msg)
                                  )
                          `when` ( getTraceLevel
                                   >>>
                                   isA (>= level)
                                 )

traceOutputToStderr     :: Int -> String -> IO ()
traceOutputToStderr _level msg
                        = do
                          hPutStrLn stderr msg
                          hFlush stderr

-- | trace the current value transfered in a sequence of arrows.
--
-- The value is formated by a string conversion function. This is a substitute for
-- the old and less general traceString function

traceValue              :: Int -> (b -> String) -> IOStateArrow s b b
traceValue level trc    = trace level (arr $ (('-' : "- (" ++ show level ++ ") ") ++) . trc)

-- | an old alias for 'traceValue'

traceString             :: Int -> (b -> String) -> IOStateArrow s b b
traceString             = traceValue

-- | issue a string message as trace

traceMsg        	:: Int -> String -> IOStateArrow s b b
traceMsg level msg      = traceValue level (const msg)

-- | issue the source representation of a document if trace level >= 3
--
-- for better readability the source is formated with indentDoc

traceSource     	:: IOStateArrow s XmlTree XmlTree
traceSource             = trace 3 $
                          xshow $
                          choiceA [ isRoot :-> ( indentDoc
                                                 >>>
                                                 getChildren
                                               )
                                  , isElem :-> ( root [] [this]
                                                 >>> indentDoc
                                                 >>> getChildren
                                                 >>> isElem
                                               )
                                  , this   :-> this
                                  ]

-- | issue the tree representation of a document if trace level >= 4
traceTree       	:: IOStateArrow s XmlTree XmlTree
traceTree               = trace 4 $
                          xshow $
                          treeRepOfXmlDoc
                          >>>
                          addHeadlineToXmlDoc
                          >>>
                          getChildren

-- | trace a main computation step
-- issue a message when trace level >= 1, issue document source if level >= 3, issue tree when level is >= 4

traceDoc        	:: String -> IOStateArrow s XmlTree XmlTree
traceDoc msg            = traceMsg 1 msg
                          >>>
                          traceSource
                          >>>
                          traceTree

-- ----------------------------------------------------------

-- | parse a URI reference, in case of a failure,
-- try to escape unescaped chars, convert backslashes to slashes for windows paths,
-- and try parsing again

parseURIReference'      :: String -> Maybe URI
parseURIReference' uri
    = parseURIReference uri
      `mplus`
      ( if unesc
        then parseURIReference uri'
        else mzero
      )
    where
    unesc       = not . all isUnescapedInURI $ uri

    escape '\\' = "/"
    escape c    = escapeURIChar isUnescapedInURI c

    uri'        = concatMap escape uri

-- | compute the absolut URI for a given URI and a base URI

expandURIString :: String -> String -> Maybe String
expandURIString uri base
    = do
      base' <- parseURIReference' base
      uri'  <- parseURIReference' uri
      abs'  <- nonStrictRelativeTo uri' base'
      return $ show abs'

-- | arrow variant of 'expandURIString', fails if 'expandURIString' returns Nothing

expandURI               :: ArrowXml a => a (String, String) String
expandURI
    = arrL (maybeToList . uncurry expandURIString)

-- | arrow for expanding an input URI into an absolute URI using global base URI, fails if input is not a legal URI

mkAbsURI                :: IOStateArrow s String String
mkAbsURI
    = ( this &&& getBaseURI ) >>> expandURI

-- | arrow for selecting the scheme (protocol) of the URI, fails if input is not a legal URI.
--
-- See Network.URI for URI components

getSchemeFromURI        :: ArrowList a => a String String
getSchemeFromURI        = getPartFromURI scheme
    where
    scheme = init . uriScheme

-- | arrow for selecting the registered name (host) of the URI, fails if input is not a legal URI

getRegNameFromURI       :: ArrowList a => a String String
getRegNameFromURI       = getPartFromURI host
    where
    host = maybe "" uriRegName . uriAuthority

-- | arrow for selecting the port number of the URI without leading \':\', fails if input is not a legal URI

getPortFromURI          :: ArrowList a => a String String
getPortFromURI          = getPartFromURI port
    where
    port = dropWhile (==':') . maybe "" uriPort . uriAuthority

-- | arrow for selecting the user info of the URI without trailing \'\@\', fails if input is not a legal URI

getUserInfoFromURI              :: ArrowList a => a String String
getUserInfoFromURI              = getPartFromURI ui
    where
    ui = reverse . dropWhile (=='@') . reverse . maybe "" uriUserInfo . uriAuthority

-- | arrow for computing the path component of an URI, fails if input is not a legal URI

getPathFromURI          :: ArrowList a => a String String
getPathFromURI          = getPartFromURI uriPath

-- | arrow for computing the query component of an URI, fails if input is not a legal URI

getQueryFromURI         :: ArrowList a => a String String
getQueryFromURI         = getPartFromURI uriQuery

-- | arrow for computing the fragment component of an URI, fails if input is not a legal URI

getFragmentFromURI      :: ArrowList a => a String String
getFragmentFromURI      = getPartFromURI uriFragment

-- | arrow for computing the path component of an URI, fails if input is not a legal URI

getPartFromURI          :: ArrowList a => (URI -> String) -> a String String
getPartFromURI sel
    = arrL (maybeToList . getPart)
      where
      getPart s = do
                  uri <- parseURIReference' s
                  return (sel uri)

-- ------------------------------------------------------------

-- | set the table mapping of file extensions to mime types in the system state
--
-- Default table is defined in 'Text.XML.HXT.DOM.MimeTypeDefaults'.
-- This table is used when reading loacl files, (file: protocol) to determine the mime type

setMimeTypeTable        	:: MimeTypeTable -> IOStateArrow s b b
setMimeTypeTable mtt		= configSysParam $ putS (theMimeTypes `pairS` theMimeTypeFile) (mtt, "")

-- | set the table mapping of file extensions to mime types by an external config file
--
-- The config file must follow the conventions of /etc/mime.types on a debian linux system,
-- that means all empty lines and all lines starting with a # are ignored. The other lines
-- must consist of a mime type followed by a possible empty list of extensions.
-- The list of extenstions and mime types overwrites the default list in the system state
-- of the IOStateArrow

setMimeTypeTableFromFile        :: FilePath -> IOStateArrow s b b
setMimeTypeTableFromFile file   = configSysParam $ putS theMimeTypeFile file

-- | read the system mimetype table

getMimeTypeTable		:: IOStateArrow s b MimeTypeTable
getMimeTypeTable		= getMime $< getSysParam (theMimeTypes `pairS` theMimeTypeFile)
    where
    getMime (mtt, "")           = constA mtt
    getMime (_,  mtf)           = perform (setMimeTypeTable $< arrIO0 ( readMimeTypeTable mtf))
                                  >>>
                                  getMimeTypeTable

-- ------------------------------------------------------------
