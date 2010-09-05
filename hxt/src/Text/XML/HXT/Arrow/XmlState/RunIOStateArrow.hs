-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   run an io state arrow
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.IOStateListArrow

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState.ErrorHandling
import Text.XML.HXT.Arrow.XmlState.TraceHandling
import Text.XML.HXT.Arrow.XmlState.TypeDefs

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
initialState s  = XIOState { xioSysState       = initialSysState
                           , xioUserState      = s
                           }

-- ------------------------------------------------------------

initialSysState                 :: XIOSysState
initialSysState                 = XIOSys
                                  { xioTraceLevel        = 0
                                  , xioTraceCmd          = traceOutputToStderr
                                  , xioErrorStatus       = c_ok
                                  , xioErrorMsgHandler   = errorOutputToStderr
                                  , xioErrorMsgCollect   = False
                                  , xioErrorMsgList      = []
                                  , xioBaseURI           = ""
                                  , xioDefaultBaseURI    = ""
                                  , xioAttrList          = []
                                  , xioInputConfig       = initialInputConfig
                                  , xioParseConfig       = initialParseConfig
                                  , xioOutputConfig      = initialOutputConfig
                                  , xioRelaxConfig       = initialRelaxConfig
                                  }

initialInputConfig              :: XIOInputConfig
initialInputConfig              = XIOIcgf
                                  { xioStrictInput       = False
                                  , xioEncodingErrors    = True
                                  , xioInputEncoding     = ""
                                  , xioUseCurl           = False
                                  , xioHttpHandler       = dummyHTTPHandler
                                  , xioInputOptions      = []
                                  , xioRedirect          = False
                                  , xioProxy             = ""
                                  }

initialParseConfig              :: XIOParseConfig
initialParseConfig              = XIOPcfg
                                  { xioMimeTypes                = defaultMimeTypeTable
                                  , xioMimeTypeFile             = ""
                                  , xioAcceptedMimeTypes        = []
                                  , xioWarnings                 = True
                                  , xioRemoveWS                 = True
                                  , xioParseByMimeType          = False
                                  , xioParseHTML                = False
                                  , xioLowerCaseNames           = False
                                  , xioTagSoup                  = False
                                  , xioPreserveComment          = False
                                  , xioValidate                 = True
                                  , xioCheckNamespaces          = False
                                  , xioCanonicalize             = True
                                  , xioIgnoreNoneXmlContents    = False
                                  , xioTagSoupParser            = dummyTagSoupParser
                                  }

initialOutputConfig             :: XIOOutputConfig
initialOutputConfig             = XIOOcfg
                                  { xioIndent                   = False
                                  , xioOutputEncoding           = ""
                                  , xioOutputFile               = ""
                                  , xioOutputFmt                = XMLoutput
                                  , xioNoXmlPi                  = False
                                  , xioNoEmptyElemFor           = []
                                  , xioNoEmptyElements          = False
                                  , xioAddDefaultDTD            = False
                                  , xioTextMode                 = False
                                  , xioShowTree                 = False
                                  , xioShowHaskell              = False
                                  }

initialRelaxConfig              :: XIORelaxConfig
initialRelaxConfig              = XIORxc
                                  { xioRelaxValidate            = False
                                  , xioRelaxSchema              = ""
                                  , xioRelaxCheckRestr          = True
                                  , xioRelaxValidateExtRef      = True
                                  , xioRelaxValidateInclude     = True
                                  , xioRelaxCollectErrors       = True
                                  , xioRelaxNoOfErrors          = 0
                                  , xioRelaxDefineId            = 0
                                  , xioRelaxAttrList            = []
                                  }

-- ------------------------------------------------------------

dummyHTTPHandler        :: IOSArrow b b
dummyHTTPHandler        = issueFatal $
                          "HTTP handler not configured, " ++
                          "please install package hxt-curl and use 'withCurl' input config option"

dummyTagSoupParser        :: IOSArrow b b
dummyTagSoupParser      =  issueFatal $
                           "TagSoup parser not configured, " ++
                           "please install package hxt-tagsoup and use 'withTagSoup' parser config option"

-- ------------------------------------------------------------

getSysConfigOption      :: String -> SysConfigList -> String
getSysConfigOption n c  = lookup1 n $ tl
    where
    s  = (foldr (>>>) id c) initialSysState
    tl = getS theAttrList s

-- ------------------------------------------------------------
