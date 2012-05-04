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
    ( module Text.XML.HXT.Arrow.XmlState.TypeDefs
    , Selector(..)
    , chgS
    , idS
    , (.&&&.)
    )
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.IOStateListArrow
import Control.DeepSeq

import Data.ByteString.Lazy             ( ByteString )
import Data.Char                        ( isDigit )
import Data.Function.Selector           ( Selector(..)
                                        , chgS
                                        , idS
                                        , (.&&&.)
                                        )
import qualified Data.Map               as M

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

-- system state structure and acces functions

-- |
-- predefined system state data type with all components for the
-- system functions, like trace, error handling, ...

data XIOSysState        = XIOSys  { xioSysWriter                :: ! XIOSysWriter
                                  , xioSysEnv                   :: ! XIOSysEnv
                                  }

instance NFData XIOSysState             -- all fields of interest are strict

data XIOSysWriter       = XIOwrt  { xioErrorStatus              :: ! Int
                                  , xioErrorMsgList             :: ! XmlTrees
                                  , xioExpatErrors              ::   IOSArrow XmlTree XmlTree
                                  , xioRelaxNoOfErrors          :: ! Int
                                  , xioRelaxDefineId            :: ! Int
                                  , xioRelaxAttrList            ::   AssocList String XmlTrees
                                  }

data XIOSysEnv          = XIOEnv  { xioTraceLevel               :: ! Int
                                  , xioTraceCmd                 ::   Int -> String -> IO ()
                                  , xioErrorMsgHandler          ::   String -> IO ()
                                  , xioErrorMsgCollect          :: ! Bool
                                  , xioBaseURI                  :: ! String
                                  , xioDefaultBaseURI           :: ! String
                                  , xioAttrList                 :: ! Attributes
                                  , xioInputConfig              :: ! XIOInputConfig
                                  , xioParseConfig              :: ! XIOParseConfig
                                  , xioOutputConfig             :: ! XIOOutputConfig
                                  , xioRelaxConfig              :: ! XIORelaxConfig
                                  , xioXmlSchemaConfig          :: ! XIOXmlSchemaConfig
                                  , xioCacheConfig              :: ! XIOCacheConfig
                                  }

data XIOInputConfig     = XIOIcgf { xioStrictInput              :: ! Bool
                                  , xioEncodingErrors           :: ! Bool
                                  , xioInputEncoding            ::   String
                                  , xioHttpHandler              ::   IOSArrow XmlTree XmlTree
                                  , xioInputOptions             :: ! Attributes
                                  , xioRedirect                 :: ! Bool
                                  , xioProxy                    ::   String
                                  }

data XIOParseConfig     = XIOPcfg { xioMimeTypes                ::   MimeTypeTable
                                  , xioMimeTypeHandlers         ::   MimeTypeHandlers
                                  , xioMimeTypeFile             ::   String
                                  , xioAcceptedMimeTypes        ::   [String]
                                  , xioFileMimeType             ::   String
                                  , xioWarnings                 :: ! Bool
                                  , xioRemoveWS                 :: ! Bool
                                  , xioParseByMimeType          :: ! Bool
                                  , xioParseHTML                :: ! Bool
                                  , xioLowerCaseNames           :: ! Bool
                                  , xioPreserveComment          :: ! Bool
                                  , xioValidate                 :: ! Bool
                                  , xioSubstDTDEntities         :: ! Bool
                                  , xioSubstHTMLEntities        :: ! Bool
                                  , xioCheckNamespaces          :: ! Bool
                                  , xioCanonicalize             :: ! Bool
                                  , xioIgnoreNoneXmlContents    :: ! Bool
                                  , xioTagSoup                  :: ! Bool
                                  , xioTagSoupParser            ::   IOSArrow XmlTree XmlTree
                                  , xioExpat                    :: ! Bool
                                  , xioExpatParser              ::   IOSArrow XmlTree XmlTree
                                  }

data XIOOutputConfig    = XIOOcfg { xioIndent                   :: ! Bool
                                  , xioOutputEncoding           :: ! String
                                  , xioOutputFmt                :: ! XIOXoutConfig
                                  , xioXmlPi                    :: ! Bool
                                  , xioNoEmptyElemFor           :: ! [String]
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
                                  , xioRelaxValidator           ::   IOSArrow XmlTree XmlTree
                                  }

data XIOXmlSchemaConfig = XIOScc  { xioXmlSchemaValidate        :: ! Bool
                                  , xioXmlSchemaSchema          ::   String
                                  , xioXmlSchemaValidator       ::   IOSArrow XmlTree XmlTree
                                  }

data XIOCacheConfig     = XIOCch  { xioBinaryCompression        ::   CompressionFct
                                  , xioBinaryDeCompression      ::   DeCompressionFct
                                  , xioWithCache                :: ! Bool
                                  , xioCacheDir                 :: ! String
                                  , xioDocumentAge              :: ! Int
                                  , xioCache404Err              :: ! Bool
                                  , xioCacheRead                ::   String -> IOSArrow XmlTree XmlTree
                                  , xioStrictDeserialize        :: ! Bool
                                  }

type MimeTypeHandlers   = M.Map String (IOSArrow XmlTree XmlTree)

type CompressionFct     = ByteString -> ByteString
type DeCompressionFct   = ByteString -> ByteString

type SysConfig          = XIOSysState -> XIOSysState
type SysConfigList      = [SysConfig]

-- ----------------------------------------

theSysState                     :: Selector (XIOState us) XIOSysState
theSysState                     = S { getS = xioSysState
                                    , setS = \ x s -> s { xioSysState = x}
                                    }

theUserState                    :: Selector (XIOState us) us
theUserState                    = S { getS = xioUserState
                                    , setS = \ x s -> s { xioUserState = x}
                                    }

-- ----------------------------------------

theSysWriter                    :: Selector XIOSysState XIOSysWriter
theSysWriter                    = S { getS = xioSysWriter
                                    , setS = \ x s -> s { xioSysWriter = x}
                                    }

theErrorStatus                  :: Selector XIOSysState Int
theErrorStatus                  = theSysWriter
                                  >>>
                                  S { getS = xioErrorStatus
                                    , setS = \ x s -> s { xioErrorStatus = x }
                                    }

theErrorMsgList                 :: Selector XIOSysState XmlTrees
theErrorMsgList                 = theSysWriter
                                  >>>
                                  S { getS = xioErrorMsgList
                                    , setS = \ x s -> s { xioErrorMsgList = x }
                                    }

theRelaxNoOfErrors              :: Selector XIOSysState Int
theRelaxNoOfErrors              = theSysWriter
                                  >>>
                                  S { getS = xioRelaxNoOfErrors
                                    , setS = \ x s -> s { xioRelaxNoOfErrors = x}
                                    }

theRelaxDefineId                :: Selector XIOSysState Int
theRelaxDefineId                = theSysWriter
                                  >>>
                                  S { getS = xioRelaxDefineId
                                    , setS = \ x s -> s { xioRelaxDefineId = x}
                                    }

theRelaxAttrList                :: Selector XIOSysState (AssocList String XmlTrees)
theRelaxAttrList                = theSysWriter
                                  >>>
                                  S { getS = xioRelaxAttrList
                                    , setS = \ x s -> s { xioRelaxAttrList = x}
                                    }

-- ----------------------------------------

theSysEnv                       :: Selector XIOSysState XIOSysEnv
theSysEnv                       = S { getS = xioSysEnv
                                    , setS = \ x s -> s { xioSysEnv = x}
                                    }

theInputConfig                  :: Selector XIOSysState XIOInputConfig
theInputConfig                  = theSysEnv
                                  >>>
                                  S { getS = xioInputConfig
                                    , setS = \ x s -> s { xioInputConfig = x}
                                    }

theStrictInput                  :: Selector XIOSysState Bool
theStrictInput                  = theInputConfig
                                  >>>
                                  S { getS = xioStrictInput
                                    , setS = \ x s -> s { xioStrictInput = x}
                                    }

theEncodingErrors               :: Selector XIOSysState Bool
theEncodingErrors               = theInputConfig
                                  >>>
                                  S { getS = xioEncodingErrors
                                    , setS = \ x s -> s { xioEncodingErrors = x}
                                    }

theInputEncoding                :: Selector XIOSysState String
theInputEncoding                = theInputConfig
                                  >>>
                                  S { getS = xioInputEncoding
                                    , setS = \ x s -> s { xioInputEncoding = x}
                                    }

theHttpHandler                  :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theHttpHandler                  = theInputConfig
                                  >>>
                                  S { getS = xioHttpHandler
                                    , setS = \ x s -> s { xioHttpHandler = x}
                                    }

theInputOptions                 :: Selector XIOSysState Attributes
theInputOptions                 = theInputConfig
                                  >>>
                                  S { getS = xioInputOptions
                                    , setS = \ x s -> s { xioInputOptions = x}
                                    }

theRedirect                     :: Selector XIOSysState Bool
theRedirect                     = theInputConfig
                                  >>>
                                  S { getS = xioRedirect
                                    , setS = \ x s -> s { xioRedirect = x}
                                    }

theProxy                        :: Selector XIOSysState String
theProxy                        = theInputConfig
                                  >>>
                                  S { getS = xioProxy
                                    , setS = \ x s -> s { xioProxy = x}
                                    }

-- ----------------------------------------

theOutputConfig                 :: Selector XIOSysState XIOOutputConfig
theOutputConfig                 = theSysEnv
                                  >>>
                                  S { getS = xioOutputConfig
                                    , setS = \ x s -> s { xioOutputConfig = x}
                                    }

theIndent                       :: Selector XIOSysState Bool
theIndent                       = theOutputConfig
                                  >>>
                                  S { getS = xioIndent
                                    , setS = \ x s -> s { xioIndent = x}
                                    }

theOutputEncoding               :: Selector XIOSysState String
theOutputEncoding               = theOutputConfig
                                  >>>
                                  S { getS = xioOutputEncoding
                                    , setS = \ x s -> s { xioOutputEncoding = x}
                                    }

theOutputFmt                    :: Selector XIOSysState XIOXoutConfig
theOutputFmt                    = theOutputConfig
                                  >>>
                                  S { getS = xioOutputFmt
                                    , setS = \ x s -> s { xioOutputFmt = x}
                                    }

theXmlPi                        :: Selector XIOSysState Bool
theXmlPi                        = theOutputConfig
                                  >>>
                                  S { getS = xioXmlPi
                                    , setS = \ x s -> s { xioXmlPi = x}
                                    }

theNoEmptyElemFor               :: Selector XIOSysState [String]
theNoEmptyElemFor               = theOutputConfig
                                  >>>
                                  S { getS = xioNoEmptyElemFor
                                    , setS = \ x s -> s { xioNoEmptyElemFor = x}
                                    }

theAddDefaultDTD                :: Selector XIOSysState Bool
theAddDefaultDTD                = theOutputConfig
                                  >>>
                                  S { getS = xioAddDefaultDTD
                                    , setS = \ x s -> s { xioAddDefaultDTD = x}
                                    }

theTextMode                     :: Selector XIOSysState Bool
theTextMode                     = theOutputConfig
                                  >>>
                                  S { getS = xioTextMode
                                    , setS = \ x s -> s { xioTextMode = x}
                                    }

theShowTree                     :: Selector XIOSysState Bool
theShowTree                     = theOutputConfig
                                  >>>
                                  S { getS = xioShowTree
                                    , setS = \ x s -> s { xioShowTree = x}
                                    }

theShowHaskell                  :: Selector XIOSysState Bool
theShowHaskell                  = theOutputConfig
                                  >>>
                                  S { getS = xioShowHaskell
                                    , setS = \ x s -> s { xioShowHaskell = x}
                                    }

-- ----------------------------------------

theRelaxConfig                  :: Selector XIOSysState XIORelaxConfig
theRelaxConfig                  = theSysEnv
                                  >>>
                                  S { getS = xioRelaxConfig
                                    , setS = \ x s -> s { xioRelaxConfig = x}
                                    }

theRelaxValidate                :: Selector XIOSysState Bool
theRelaxValidate                = theRelaxConfig
                                  >>>
                                  S { getS = xioRelaxValidate
                                    , setS = \ x s -> s { xioRelaxValidate = x}
                                    }

theRelaxSchema                  :: Selector XIOSysState String
theRelaxSchema                  = theRelaxConfig
                                  >>>
                                  S { getS = xioRelaxSchema
                                    , setS = \ x s -> s { xioRelaxSchema = x}
                                    }

theRelaxCheckRestr              :: Selector XIOSysState Bool
theRelaxCheckRestr              = theRelaxConfig
                                  >>>
                                  S { getS = xioRelaxCheckRestr
                                    , setS = \ x s -> s { xioRelaxCheckRestr = x}
                                    }

theRelaxValidateExtRef          :: Selector XIOSysState Bool
theRelaxValidateExtRef          = theRelaxConfig
                                  >>>
                                  S { getS = xioRelaxValidateExtRef
                                    , setS = \ x s -> s { xioRelaxValidateExtRef = x}
                                    }

theRelaxValidateInclude         :: Selector XIOSysState Bool
theRelaxValidateInclude         = theRelaxConfig
                                  >>>
                                  S { getS = xioRelaxValidateInclude
                                    , setS = \ x s -> s { xioRelaxValidateInclude = x}
                                    }

theRelaxCollectErrors           :: Selector XIOSysState Bool
theRelaxCollectErrors           = theRelaxConfig
                                  >>>
                                  S { getS = xioRelaxCollectErrors
                                    , setS = \ x s -> s { xioRelaxCollectErrors = x}
                                    }

theRelaxValidator               :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theRelaxValidator               = theRelaxConfig
                                  >>>
                                  S { getS = xioRelaxValidator
                                    , setS = \ x s -> s { xioRelaxValidator = x}
                                    }

-- ----------------------------------------

theXmlSchemaConfig              :: Selector XIOSysState XIOXmlSchemaConfig
theXmlSchemaConfig              = theSysEnv
                                  >>>
                                  S { getS = xioXmlSchemaConfig
                                    , setS = \ x s -> s { xioXmlSchemaConfig = x}
                                    }

theXmlSchemaValidate            :: Selector XIOSysState Bool
theXmlSchemaValidate            = theXmlSchemaConfig
                                  >>>
                                  S { getS = xioXmlSchemaValidate
                                    , setS = \ x s -> s { xioXmlSchemaValidate = x}
                                    }

theXmlSchemaSchema              :: Selector XIOSysState String
theXmlSchemaSchema              = theXmlSchemaConfig
                                  >>>
                                  S { getS = xioXmlSchemaSchema
                                    , setS = \ x s -> s { xioXmlSchemaSchema = x}
                                    }

theXmlSchemaValidator           :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theXmlSchemaValidator           = theXmlSchemaConfig
                                  >>>
                                  S { getS = xioXmlSchemaValidator
                                    , setS = \ x s -> s { xioXmlSchemaValidator = x}
                                    }

-- ----------------------------------------

theParseConfig                  :: Selector XIOSysState XIOParseConfig
theParseConfig                  = theSysEnv
                                  >>>
                                  S { getS = xioParseConfig
                                    , setS = \ x s -> s { xioParseConfig = x}
                                    }

theErrorMsgHandler              :: Selector XIOSysState (String -> IO ())
theErrorMsgHandler              = theSysEnv
                                  >>>
                                  S { getS = xioErrorMsgHandler
                                    , setS = \ x s -> s { xioErrorMsgHandler = x }
                                    }

theErrorMsgCollect              :: Selector XIOSysState Bool
theErrorMsgCollect              = theSysEnv
                                  >>>
                                  S { getS = xioErrorMsgCollect
                                    , setS = \ x s -> s { xioErrorMsgCollect = x }
                                    }

theBaseURI                      :: Selector XIOSysState String
theBaseURI                      = theSysEnv
                                  >>>
                                  S { getS = xioBaseURI
                                    , setS = \ x s -> s { xioBaseURI = x }
                                    }

theDefaultBaseURI               :: Selector XIOSysState String
theDefaultBaseURI               = theSysEnv
                                  >>>
                                  S { getS = xioDefaultBaseURI
                                    , setS = \ x s -> s { xioDefaultBaseURI = x }
                                    }

theTraceLevel                   :: Selector XIOSysState Int
theTraceLevel                   = theSysEnv
                                  >>>
                                  S { getS = xioTraceLevel
                                    , setS = \ x s -> s { xioTraceLevel = x }
                                    }

theTraceCmd                     :: Selector XIOSysState (Int -> String -> IO ())
theTraceCmd                     = theSysEnv
                                  >>>
                                  S { getS = xioTraceCmd
                                    , setS = \ x s -> s { xioTraceCmd = x }
                                    }

theTrace                        :: Selector XIOSysState (Int, Int -> String -> IO ())
theTrace                        = theTraceLevel .&&&. theTraceCmd

theAttrList                     :: Selector XIOSysState Attributes
theAttrList                     = theSysEnv
                                  >>>
                                  S { getS = xioAttrList
                                    , setS = \ x s -> s { xioAttrList = x }
                                    }

theMimeTypes                    :: Selector XIOSysState MimeTypeTable
theMimeTypes                    = theParseConfig
                                  >>>
                                  S { getS = xioMimeTypes
                                    , setS = \ x s -> s { xioMimeTypes = x }
                                    }

theMimeTypeHandlers             :: Selector XIOSysState MimeTypeHandlers
theMimeTypeHandlers             = theParseConfig
                                  >>>
                                  S { getS = xioMimeTypeHandlers
                                    , setS = \ x s -> s { xioMimeTypeHandlers = x }
                                    }

theMimeTypeFile                 :: Selector XIOSysState String
theMimeTypeFile                 = theParseConfig
                                  >>>
                                  S { getS = xioMimeTypeFile
                                    , setS = \ x s -> s { xioMimeTypeFile = x }
                                    }

theAcceptedMimeTypes            :: Selector XIOSysState [String]
theAcceptedMimeTypes            = theParseConfig
                                  >>>
                                  S { getS = xioAcceptedMimeTypes
                                    , setS = \ x s -> s { xioAcceptedMimeTypes = x }
                                    }

theFileMimeType                 :: Selector XIOSysState String
theFileMimeType                 = theParseConfig
                                  >>>
                                  S { getS = xioFileMimeType
                                    , setS = \ x s -> s { xioFileMimeType = x }
                                    }

theWarnings                     :: Selector XIOSysState Bool
theWarnings                     = theParseConfig
                                  >>>
                                  S { getS = xioWarnings
                                    , setS = \ x s -> s { xioWarnings = x }
                                    }

theRemoveWS                     :: Selector XIOSysState Bool
theRemoveWS                     = theParseConfig
                                  >>>
                                  S { getS = xioRemoveWS
                                    , setS = \ x s -> s { xioRemoveWS = x }
                                    }

thePreserveComment              :: Selector XIOSysState Bool
thePreserveComment              = theParseConfig
                                  >>>
                                  S { getS = xioPreserveComment
                                    , setS = \ x s -> s { xioPreserveComment = x }
                                    }

theParseByMimeType              :: Selector XIOSysState Bool
theParseByMimeType              = theParseConfig
                                  >>>
                                  S { getS = xioParseByMimeType
                                    , setS = \ x s -> s { xioParseByMimeType = x }
                                    }

theParseHTML                    :: Selector XIOSysState Bool
theParseHTML                    = theParseConfig
                                  >>>
                                  S { getS = xioParseHTML
                                    , setS = \ x s -> s { xioParseHTML = x }
                                    }

theLowerCaseNames               :: Selector XIOSysState Bool
theLowerCaseNames               = theParseConfig
                                  >>>
                                  S { getS = xioLowerCaseNames
                                    , setS = \ x s -> s { xioLowerCaseNames = x }
                                    }

theValidate                     :: Selector XIOSysState Bool
theValidate                     = theParseConfig
                                  >>>
                                  S { getS = xioValidate
                                    , setS = \ x s -> s { xioValidate = x }
                                    }

theSubstDTDEntities            :: Selector XIOSysState Bool
theSubstDTDEntities            = theParseConfig
                                  >>>
                                  S { getS = xioSubstDTDEntities
                                    , setS = \ x s -> s { xioSubstDTDEntities = x }
                                    }

theSubstHTMLEntities            :: Selector XIOSysState Bool
theSubstHTMLEntities            = theParseConfig
                                  >>>
                                  S { getS = xioSubstHTMLEntities
                                    , setS = \ x s -> s { xioSubstHTMLEntities = x }
                                    }

theCheckNamespaces              :: Selector XIOSysState Bool
theCheckNamespaces              = theParseConfig
                                  >>>
                                  S { getS = xioCheckNamespaces
                                    , setS = \ x s -> s { xioCheckNamespaces = x }
                                    }

theCanonicalize                 :: Selector XIOSysState Bool
theCanonicalize                 = theParseConfig
                                  >>>
                                  S { getS = xioCanonicalize
                                    , setS = \ x s -> s { xioCanonicalize = x }
                                    }

theIgnoreNoneXmlContents        :: Selector XIOSysState Bool
theIgnoreNoneXmlContents        = theParseConfig
                                  >>>
                                  S { getS = xioIgnoreNoneXmlContents
                                    , setS = \ x s -> s { xioIgnoreNoneXmlContents = x }
                                    }

theTagSoup                      :: Selector XIOSysState Bool
theTagSoup                      = theParseConfig
                                  >>>
                                  S { getS = xioTagSoup
                                    , setS = \ x s -> s { xioTagSoup = x }
                                    }

theTagSoupParser                :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theTagSoupParser                = theParseConfig
                                  >>>
                                  S { getS = xioTagSoupParser
                                    , setS = \ x s -> s { xioTagSoupParser = x }
                                    }

theExpat                        :: Selector XIOSysState Bool
theExpat                        = theParseConfig
                                  >>>
                                  S { getS = xioExpat
                                    , setS = \ x s -> s { xioExpat = x }
                                    }

theExpatParser                  :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theExpatParser                  = theParseConfig
                                  >>>
                                  S { getS = xioExpatParser
                                    , setS = \ x s -> s { xioExpatParser = x }
                                    }

theExpatErrors                  :: Selector XIOSysState (IOSArrow XmlTree XmlTree)
theExpatErrors                  = theSysWriter
                                  >>>
                                  S { getS = xioExpatErrors
                                    , setS = \ x s -> s { xioExpatErrors = x }
                                    }

-- ----------------------------------------

theCacheConfig                  :: Selector XIOSysState XIOCacheConfig
theCacheConfig                  = theSysEnv
                                  >>>
                                  S { getS = xioCacheConfig
                                    , setS = \ x s -> s { xioCacheConfig = x}
                                    }

theBinaryCompression            :: Selector XIOSysState (ByteString -> ByteString)
theBinaryCompression            = theCacheConfig
                                  >>>
                                  S { getS = xioBinaryCompression
                                    , setS = \ x s -> s { xioBinaryCompression = x}
                                    }

theBinaryDeCompression          :: Selector XIOSysState (ByteString -> ByteString)
theBinaryDeCompression          = theCacheConfig
                                  >>>
                                  S { getS = xioBinaryDeCompression
                                    , setS = \ x s -> s { xioBinaryDeCompression = x}
                                    }

theWithCache                    :: Selector XIOSysState Bool
theWithCache                    = theCacheConfig
                                  >>>
                                  S { getS = xioWithCache
                                    , setS = \ x s -> s { xioWithCache = x}
                                    }

theCacheDir                     :: Selector XIOSysState String
theCacheDir                     = theCacheConfig
                                  >>>
                                  S { getS = xioCacheDir
                                    , setS = \ x s -> s { xioCacheDir = x}
                                    }

theDocumentAge                  :: Selector XIOSysState Int
theDocumentAge                  = theCacheConfig
                                  >>>
                                  S { getS = xioDocumentAge
                                    , setS = \ x s -> s { xioDocumentAge = x}
                                    }

theCache404Err                  :: Selector XIOSysState Bool
theCache404Err                  = theCacheConfig
                                  >>>
                                  S { getS = xioCache404Err
                                    , setS = \ x s -> s { xioCache404Err = x}
                                    }

theCacheRead                    :: Selector XIOSysState (String -> IOSArrow XmlTree XmlTree)
theCacheRead                    = theCacheConfig
                                  >>>
                                  S { getS = xioCacheRead
                                    , setS = \ x s -> s { xioCacheRead = x}
                                    }

theStrictDeserialize            :: Selector XIOSysState Bool
theStrictDeserialize            = theCacheConfig
                                  >>>
                                  S { getS = xioStrictDeserialize
                                    , setS = \ x s -> s { xioStrictDeserialize = x}
                                    }

-- ------------------------------------------------------------

getSysVar                       :: Selector XIOSysState c -> IOStateArrow s b c
getSysVar sel                   = IOSLA $ \ s _x ->
                                  return (s, (:[]) . getS (theSysState >>> sel) $ s)

setSysVar                       :: Selector XIOSysState c -> IOStateArrow s c c
setSysVar sel                   = (\ v -> configSysVar $ setS sel v) $< this

chgSysVar                       :: Selector XIOSysState c -> (b -> c -> c) -> IOStateArrow s b b
chgSysVar sel op                = (\ v -> configSysVar $ chgS sel (op v)) $< this

configSysVar                    :: SysConfig -> IOStateArrow s c c
configSysVar cf                 = IOSLA $ \ s v ->
                                  return (chgS theSysState cf s, [v])

configSysVars                   :: SysConfigList -> IOStateArrow s c c
configSysVars cfs               = configSysVar $ foldr (>>>) id $ cfs

localSysVar                     :: Selector XIOSysState c -> IOStateArrow s a b -> IOStateArrow s a b
localSysVar sel f               = IOSLA $ \ s0 v ->
                                  let sel' = theSysState >>> sel in
                                  let c0   = getS sel' s0 in
                                  do
                                  (s1, res) <- runIOSLA f s0 v
                                  return (setS sel' c0 s1, res)

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
      all isDigit s     = read s
    | otherwise         = def

-- ------------------------------------------------------------
