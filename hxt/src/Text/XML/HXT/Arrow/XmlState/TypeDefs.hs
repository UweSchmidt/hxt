-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlState.TypeDefs
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

module Text.XML.HXT.Arrow.XmlState.TypeDefs
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.IOStateListArrow
import Control.DeepSeq

import Data.ByteString.Lazy             ( ByteString )
import Data.Char                        ( isDigit )

import Text.XML.HXT.DOM.Interface

-- ------------------------------------------------------------
{- datatypes -}

-- |
-- state datatype consists of a system state and a user state
-- the user state is not fixed

data XIOState us        = XIOState { xioSysState               :: ! XIOSysState
                                   , xioUserState              :: ! us
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

-- user state functions

-- | read the user defined part of the state

getUserState    :: IOStateArrow s b s
getUserState
    = IOSLA $ \ s _ ->
      return (s, [xioUserState s])

-- | change the user defined part of the state

changeUserState         :: (b -> s -> s) -> IOStateArrow s b b
changeUserState cf
    = IOSLA $ \ s v ->
      let s' = s { xioUserState = cf v (xioUserState s) }
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
      ~(finalS, res) <- runIOSLA f ( XIOState { xioSysState  =          xioSysState  s0
                                              , xioUserState = (initS1, xioUserState s0)
                                              }
                                   ) x
      return ( XIOState { xioSysState  =      xioSysState  finalS
                        , xioUserState = snd (xioUserState finalS)
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
      (s', res) <- runIOSLA f ( XIOState { xioSysState  = xioSysState s
                                         , xioUserState = s1
                                         }
                              ) x
      return ( XIOState { xioSysState  = xioSysState  s'
                        , xioUserState = xioUserState s
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

idS                     :: Selector s s
idS                     = (id, const)

-- ------------------------------------------------------------

-- system state structure and acces functions

-- |
-- predefined system state data type with all components for the
-- system functions, like trace, error handling, ...

data XIOSysState        = XIOSys  { xioSysWriter                :: ! XIOSysWriter
                                  , xioSysEnv                   :: ! XIOSysEnv
                                  }

instance NFData XIOSysState             -- all fields of interest are strict

data XIOSysWriter       = XIOwrt  { xioErrorMsgList             :: ! XmlTrees
                                  }

data XIOSysEnv          = XIOEnv  { xioTraceLevel               :: ! Int
                                  , xioTraceCmd                 ::   Int -> String -> IO ()
                                  , xioErrorStatus              :: ! Int
                                  , xioErrorMsgHandler          ::   String -> IO ()
                                  , xioErrorMsgCollect          :: ! Bool
                                  , xioBaseURI                  :: ! String
                                  , xioDefaultBaseURI           :: ! String
                                  , xioAttrList                 :: ! Attributes
                                  , xioInputConfig              :: ! XIOInputConfig
                                  , xioParseConfig              :: ! XIOParseConfig
                                  , xioOutputConfig             :: ! XIOOutputConfig
                                  , xioRelaxConfig              :: ! XIORelaxConfig
                                  , xioBinaryConfig             :: ! XIOBinaryConfig
                                  }

data XIOInputConfig     = XIOIcgf { xioStrictInput              :: ! Bool
                                  , xioEncodingErrors           :: ! Bool
                                  , xioInputEncoding            ::   String
                                  , xioUseCurl                  :: ! Bool
                                  , xioHttpHandler              ::   IOSArrow XmlTree XmlTree
                                  , xioInputOptions             :: ! Attributes
                                  , xioRedirect                 :: ! Bool
                                  , xioProxy                    ::   String
                                  }

data XIOParseConfig     = XIOPcfg { xioMimeTypes                ::   MimeTypeTable
                                  , xioMimeTypeFile             ::   String
                                  , xioAcceptedMimeTypes        ::   [String]
                                  , xioWarnings                 :: ! Bool
                                  , xioRemoveWS                 :: ! Bool
                                  , xioParseByMimeType          :: ! Bool
                                  , xioParseHTML                :: ! Bool
                                  , xioLowerCaseNames           :: ! Bool
                                  , xioPreserveComment          :: ! Bool
                                  , xioValidate                 :: ! Bool
                                  , xioCheckNamespaces          :: ! Bool
                                  , xioCanonicalize             :: ! Bool
                                  , xioIgnoreNoneXmlContents    :: ! Bool
                                  , xioTagSoup                  :: ! Bool
                                  , xioTagSoupParser            ::   IOSArrow XmlTree XmlTree
                                  }

data XIOOutputConfig    = XIOOcfg { xioIndent                   :: ! Bool
                                  , xioOutputEncoding           :: ! String
                                  , xioOutputFmt                :: ! XIOXoutConfig
                                  , xioNoXmlPi                  :: ! Bool
                                  , xioNoEmptyElemFor           :: ! [String]
                                  , xioNoEmptyElements          :: ! Bool
                                  , xioAddDefaultDTD            :: ! Bool
                                  , xioTextMode                 :: ! Bool
                                  , xioShowTree                 :: ! Bool
                                  , xioShowHaskell              :: ! Bool
                                  }
data XIOXoutConfig      = XMLoutput | XHTMLoutput | HTMLoutput | PLAINoutput
                          deriving (Eq)

data XIORelaxConfig     = XIORxc  { xioRelaxValidate            :: ! Bool
                                  , xioRelaxSchema              ::   String
                                  , xioRelaxCheckRestr          :: ! Bool
                                  , xioRelaxValidateExtRef      :: ! Bool
                                  , xioRelaxValidateInclude     :: ! Bool
                                  , xioRelaxCollectErrors       :: ! Bool
                                  , xioRelaxNoOfErrors          :: ! Int
                                  , xioRelaxDefineId            :: ! Int
                                  , xioRelaxAttrList            ::   AssocList String XmlTrees
				  , xioRelaxValidator           ::   IOSArrow XmlTree XmlTree
                                  }

data XIOBinaryConfig    = XIOBin  { xioBinaryCompression        ::   CompressionFct
                                  , xioBinaryDeCompression      ::   DeCompressionFct
                                  }

type CompressionFct     = ByteString -> ByteString
type DeCompressionFct   = ByteString -> ByteString

type SysConfig                  = XIOSysState -> XIOSysState
type SysConfigList              = [SysConfig]

-- ------------------------------

theSysState                     :: Selector (XIOState us) XIOSysState
theSysState                     = ( xioSysState,         \ x s -> s { xioSysState = x} )

theUserState                    :: Selector (XIOState us) us
theUserState                    = ( xioUserState,        \ x s -> s { xioUserState = x} )

-- ----------------------------------------

theSysWriter                    :: Selector XIOSysState XIOSysWriter
theSysWriter                    = ( xioSysWriter, \ x s -> s { xioSysWriter = x} )

theErrorMsgList                 :: Selector XIOSysState XmlTrees
theErrorMsgList                 = ( xioErrorMsgList,    \ x s -> s { xioErrorMsgList = x } )
                                   `subS` theSysWriter

-- ----------------------------------------

theSysEnv                       :: Selector XIOSysState XIOSysEnv
theSysEnv                       = ( xioSysEnv, \ x s -> s { xioSysEnv = x} )

theInputConfig                  :: Selector XIOSysState XIOInputConfig
theInputConfig                  = ( xioInputConfig,      \ x s -> s { xioInputConfig = x} )
                                   `subS` theSysEnv

theStrictInput                  :: Selector XIOSysState Bool
theStrictInput                  = ( xioStrictInput,      \ x s -> s { xioStrictInput = x} )
                                  `subS` theInputConfig

theEncodingErrors               :: Selector XIOSysState Bool
theEncodingErrors               = ( xioEncodingErrors,   \ x s -> s { xioEncodingErrors = x} )
                                  `subS` theInputConfig

theInputEncoding                :: Selector XIOSysState String
theInputEncoding                = ( xioInputEncoding,   \ x s -> s { xioInputEncoding = x} )
                                  `subS` theInputConfig

theUseCurl                      :: Selector XIOSysState Bool
theUseCurl                      = ( xioUseCurl,      \ x s -> s { xioUseCurl = x} )
                                  `subS` theInputConfig

theHttpHandler                  :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theHttpHandler                  = ( xioHttpHandler,      \ x s -> s { xioHttpHandler = x} )
                                  `subS` theInputConfig

theInputOptions                 :: Selector XIOSysState Attributes
theInputOptions                 = ( xioInputOptions,      \ x s -> s { xioInputOptions = x} )
                                  `subS` theInputConfig

theRedirect                     :: Selector XIOSysState Bool
theRedirect                     = ( xioRedirect,      \ x s -> s { xioRedirect = x} )
                                  `subS` theInputConfig

theProxy                        :: Selector XIOSysState String
theProxy                        = ( xioProxy,      \ x s -> s { xioProxy = x} )
                                  `subS` theInputConfig
-- ----------------------------------------

theOutputConfig                 :: Selector XIOSysState XIOOutputConfig
theOutputConfig                 = ( xioOutputConfig, \ x s -> s { xioOutputConfig = x} )
                                   `subS` theSysEnv

theIndent                       :: Selector XIOSysState Bool
theIndent                       = ( xioIndent,      \ x s -> s { xioIndent = x} )
                                  `subS` theOutputConfig

theOutputEncoding               :: Selector XIOSysState String
theOutputEncoding               = ( xioOutputEncoding,      \ x s -> s { xioOutputEncoding = x} )
                                  `subS` theOutputConfig

theOutputFmt                    :: Selector XIOSysState XIOXoutConfig
theOutputFmt                    = ( xioOutputFmt,      \ x s -> s { xioOutputFmt = x} )
                                  `subS` theOutputConfig

theNoXmlPi                      :: Selector XIOSysState Bool
theNoXmlPi                      = ( xioNoXmlPi,      \ x s -> s { xioNoXmlPi = x} )
                                  `subS` theOutputConfig

theNoEmptyElemFor               :: Selector XIOSysState [String]
theNoEmptyElemFor               = ( xioNoEmptyElemFor,      \ x s -> s { xioNoEmptyElemFor = x} )
                                  `subS` theOutputConfig

theNoEmptyElements              :: Selector XIOSysState Bool
theNoEmptyElements              = ( xioNoEmptyElements,      \ x s -> s { xioNoEmptyElements = x} )
                                  `subS` theOutputConfig

theAddDefaultDTD                :: Selector XIOSysState Bool
theAddDefaultDTD                = ( xioAddDefaultDTD,      \ x s -> s { xioAddDefaultDTD = x} )
                                  `subS` theOutputConfig

theTextMode                     :: Selector XIOSysState Bool
theTextMode                     = ( xioTextMode,      \ x s -> s { xioTextMode = x} )
                                  `subS` theOutputConfig

theShowTree                     :: Selector XIOSysState Bool
theShowTree                     = ( xioShowTree,      \ x s -> s { xioShowTree = x} )
                                  `subS` theOutputConfig

theShowHaskell                  :: Selector XIOSysState Bool
theShowHaskell                  = ( xioShowHaskell,      \ x s -> s { xioShowHaskell = x} )
                                  `subS` theOutputConfig

-- ----------------------------------------

theRelaxConfig                  :: Selector XIOSysState XIORelaxConfig
theRelaxConfig                  = ( xioRelaxConfig,      \ x s -> s { xioRelaxConfig = x} )
                                   `subS` theSysEnv

theRelaxValidate                :: Selector XIOSysState Bool
theRelaxValidate                = ( xioRelaxValidate, \ x s -> s { xioRelaxValidate = x} )
                                  `subS` theRelaxConfig

theRelaxSchema                  :: Selector XIOSysState String
theRelaxSchema                  = ( xioRelaxSchema, \ x s -> s { xioRelaxSchema = x} )
                                  `subS` theRelaxConfig

theRelaxCheckRestr              :: Selector XIOSysState Bool
theRelaxCheckRestr              = ( xioRelaxCheckRestr, \ x s -> s { xioRelaxCheckRestr = x} )
                                  `subS` theRelaxConfig

theRelaxValidateExtRef          :: Selector XIOSysState Bool
theRelaxValidateExtRef          = ( xioRelaxValidateExtRef, \ x s -> s { xioRelaxValidateExtRef = x} )
                                  `subS` theRelaxConfig

theRelaxValidateInclude         :: Selector XIOSysState Bool
theRelaxValidateInclude         = ( xioRelaxValidateInclude, \ x s -> s { xioRelaxValidateInclude = x} )
                                  `subS` theRelaxConfig

theRelaxCollectErrors           :: Selector XIOSysState Bool
theRelaxCollectErrors           = ( xioRelaxCollectErrors, \ x s -> s { xioRelaxCollectErrors = x} )
                                  `subS` theRelaxConfig

theRelaxNoOfErrors              :: Selector XIOSysState Int
theRelaxNoOfErrors              = ( xioRelaxNoOfErrors, \ x s -> s { xioRelaxNoOfErrors = x} )
                                  `subS` theRelaxConfig

theRelaxDefineId                :: Selector XIOSysState Int
theRelaxDefineId                = ( xioRelaxDefineId, \ x s -> s { xioRelaxDefineId = x} )
                                  `subS` theRelaxConfig

theRelaxAttrList                :: Selector XIOSysState (AssocList String XmlTrees)
theRelaxAttrList                = ( xioRelaxAttrList,      \ x s -> s { xioRelaxAttrList = x} )
                                  `subS` theRelaxConfig

theRelaxValidator                :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theRelaxValidator                = ( xioRelaxValidator,      \ x s -> s { xioRelaxValidator = x} )
                                  `subS` theRelaxConfig

-- ----------------------------------------

theParseConfig                  :: Selector XIOSysState XIOParseConfig
theParseConfig                  = ( xioParseConfig,      \ x s -> s { xioParseConfig = x} )
                                   `subS` theSysEnv

theErrorStatus                  :: Selector XIOSysState Int
theErrorStatus                  = ( xioErrorStatus,     \ x s -> s { xioErrorStatus = x } )
                                   `subS` theSysEnv

theErrorMsgHandler              :: Selector XIOSysState (String -> IO ())
theErrorMsgHandler              = ( xioErrorMsgHandler, \ x s -> s { xioErrorMsgHandler = x } )
                                   `subS` theSysEnv

theErrorMsgCollect              :: Selector XIOSysState Bool
theErrorMsgCollect              = ( xioErrorMsgCollect, \ x s -> s { xioErrorMsgCollect = x } )
                                   `subS` theSysEnv

theBaseURI                      :: Selector XIOSysState String
theBaseURI                      = ( xioBaseURI,         \ x s -> s { xioBaseURI = x } )
                                   `subS` theSysEnv

theDefaultBaseURI               :: Selector XIOSysState String
theDefaultBaseURI               = ( xioDefaultBaseURI,  \ x s -> s { xioDefaultBaseURI = x } )
                                   `subS` theSysEnv

theTraceLevel                   :: Selector XIOSysState Int
theTraceLevel                   = ( xioTraceLevel,      \ x s -> s { xioTraceLevel = x } )
                                   `subS` theSysEnv

theTraceCmd                     :: Selector XIOSysState (Int -> String -> IO ())
theTraceCmd                     = ( xioTraceCmd,        \ x s -> s { xioTraceCmd = x } )
                                   `subS` theSysEnv

theTrace                        :: Selector XIOSysState (Int, Int -> String -> IO ())
theTrace                        = theTraceLevel `pairS` theTraceCmd

theAttrList                     :: Selector XIOSysState Attributes
theAttrList                     = ( xioAttrList,        \ x s -> s { xioAttrList = x } )
                                   `subS` theSysEnv

theMimeTypes                    :: Selector XIOSysState MimeTypeTable
theMimeTypes                    = ( xioMimeTypes,       \ x s -> s { xioMimeTypes = x } )
                                  `subS` theParseConfig

theMimeTypeFile                 :: Selector XIOSysState String
theMimeTypeFile                    = ( xioMimeTypeFile, \ x s -> s { xioMimeTypeFile = x } )
                                  `subS` theParseConfig

theAcceptedMimeTypes            :: Selector XIOSysState [String]
theAcceptedMimeTypes            = ( xioAcceptedMimeTypes,       \ x s -> s { xioAcceptedMimeTypes = x } )
                                  `subS` theParseConfig

theWarnings                     :: Selector XIOSysState Bool
theWarnings                     = ( xioWarnings,        \ x s -> s { xioWarnings = x } )
                                  `subS` theParseConfig

theRemoveWS                     :: Selector XIOSysState Bool
theRemoveWS                     = ( xioRemoveWS,        \ x s -> s { xioRemoveWS = x } )
                                  `subS` theParseConfig

thePreserveComment              :: Selector XIOSysState Bool
thePreserveComment              = ( xioPreserveComment, \ x s -> s { xioPreserveComment = x } )
                                  `subS` theParseConfig

theParseByMimeType              :: Selector XIOSysState Bool
theParseByMimeType              = ( xioParseByMimeType, \ x s -> s { xioParseByMimeType = x } )
                                  `subS` theParseConfig

theParseHTML                    :: Selector XIOSysState Bool
theParseHTML                    = ( xioParseHTML, \ x s -> s { xioParseHTML = x } )
                                  `subS` theParseConfig

theLowerCaseNames               :: Selector XIOSysState Bool
theLowerCaseNames               = ( xioLowerCaseNames, \ x s -> s { xioLowerCaseNames = x } )
                                  `subS` theParseConfig

theValidate                     :: Selector XIOSysState Bool
theValidate                     = ( xioValidate, \ x s -> s { xioValidate = x } )
                                  `subS` theParseConfig

theCheckNamespaces              :: Selector XIOSysState Bool
theCheckNamespaces              = ( xioCheckNamespaces, \ x s -> s { xioCheckNamespaces = x } )
                                  `subS` theParseConfig

theCanonicalize                 :: Selector XIOSysState Bool
theCanonicalize                 = ( xioCanonicalize, \ x s -> s { xioCanonicalize = x } )
                                  `subS` theParseConfig

theIgnoreNoneXmlContents        :: Selector XIOSysState Bool
theIgnoreNoneXmlContents        = ( xioIgnoreNoneXmlContents, \ x s -> s { xioIgnoreNoneXmlContents = x } )
                                  `subS` theParseConfig

theTagSoup                      :: Selector XIOSysState Bool
theTagSoup                      = ( xioTagSoup,        \ x s -> s { xioTagSoup = x } )
                                  `subS` theParseConfig

theTagSoupParser                :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theTagSoupParser                = ( xioTagSoupParser,  \ x s -> s { xioTagSoupParser = x } )
                                  `subS` theParseConfig

-- ----------------------------------------

theBinaryConfig                  :: Selector XIOSysState XIOBinaryConfig
theBinaryConfig                  = ( xioBinaryConfig,      \ x s -> s { xioBinaryConfig = x} )
                                   `subS` theSysEnv

theBinaryCompression             :: Selector XIOSysState (ByteString -> ByteString)
theBinaryCompression             = ( xioBinaryCompression,      \ x s -> s { xioBinaryCompression = x} )
                                   `subS` theBinaryConfig

theBinaryDeCompression           :: Selector XIOSysState (ByteString -> ByteString)
theBinaryDeCompression           = ( xioBinaryDeCompression,      \ x s -> s { xioBinaryDeCompression = x} )
                                   `subS` theBinaryConfig

-- ------------------------------------------------------------

getSysVar                       :: Selector XIOSysState c -> IOStateArrow s b c
getSysVar sel                   = IOSLA $ \ s _x ->
                                  return (s, (:[]) . getS (sel `subS` theSysState) $ s)

setSysVar                       :: Selector XIOSysState c -> IOStateArrow s c c
setSysVar sel                   = (\ v -> configSysVar $ putS sel v) $< this

chgSysVar                       :: Selector XIOSysState c -> (b -> c -> c) -> IOStateArrow s b b
chgSysVar sel op                = (\ v -> configSysVar $ chgS sel (op v)) $< this

configSysVar                    :: SysConfig -> IOStateArrow s c c
configSysVar cf                 = IOSLA $ \ s v ->
                                  return (chgS theSysState cf s, [v])

configSysVars                   :: SysConfigList -> IOStateArrow s c c
configSysVars cfs               = configSysVar $ foldr (>>>) id $ cfs

localSysVar                     :: Selector XIOSysState c -> IOStateArrow s a b -> IOStateArrow s a b
localSysVar sel f               = IOSLA $ \ s0 v ->
                                  let sel' = sel `subS` theSysState in
                                  let c0   = getS sel' s0 in
                                  do
                                  (s1, res) <- runIOSLA f s0 v
                                  return (putS sel' c0 s1, res)

localSysEnv                     :: IOStateArrow s a b -> IOStateArrow s a b
localSysEnv                     = localSysVar theSysEnv

incrSysVar                      :: Selector XIOSysState Int -> IOStateArrow s a Int
incrSysVar cnt                  = getSysVar cnt
                                  >>>
                                  arr (+1)
                                  >>>
                                  setSysVar cnt
                                  >>>
                                  arr (\ x -> x - 1)

-- ------------------------------

-- | store a string in global state under a given attribute name

setSysAttr              :: String -> IOStateArrow s String String
setSysAttr n            = chgSysVar theAttrList (addEntry n)

-- | remove an entry in global state, arrow input remains unchanged

unsetSysAttr            :: String -> IOStateArrow s b b
unsetSysAttr n            = configSysVar $ chgS theAttrList (delEntry n)

-- | read an attribute value from global state

getSysAttr                :: String -> IOStateArrow s b String
getSysAttr n              = getSysVar theAttrList
                          >>^
                          lookup1 n

-- | read all attributes from global state

getAllSysAttrs            :: IOStateArrow s b Attributes
getAllSysAttrs            = getSysVar theAttrList


setSysAttrString        :: String -> String -> IOStateArrow s b b
setSysAttrString n v    = perform ( constA v
                                    >>>
                                    setSysAttr n
                                  )

-- | store an int value in global state

setSysAttrInt           :: String -> Int -> IOStateArrow s b b
setSysAttrInt n v       = setSysAttrString n (show v)

-- | read an int value from global state
--
-- > getSysAttrInt 0 myIntAttr

getSysAttrInt           :: Int -> String -> IOStateArrow s b Int
getSysAttrInt def n     = getSysAttr n
                          >>^
                          toInt def

toInt                   :: Int -> String -> Int
toInt def s
        | not (null s)
          &&
          all isDigit s = read s
        | otherwise     = def

-- ------------------------------------------------------------
