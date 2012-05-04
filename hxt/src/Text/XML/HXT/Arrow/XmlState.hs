-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlState
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   the interface for the basic state maipulation functions
-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlState
    ( -- * Data Types
      XIOState
    , XIOSysState
    , IOStateArrow
    , IOSArrow
    , SysConfig
    , SysConfigList
    ,
      -- * User State Manipulation
      getUserState
    , setUserState
    , changeUserState
    , withExtendedUserState
    , withOtherUserState
    , withoutUserState
    ,
      -- * Run IO State arrows
      runX
    ,
      -- * Global System State Configuration and Access
      configSysVars
    , setSysAttr
    , unsetSysAttr
    , getSysAttr
    , getAllSysAttrs
    , setSysAttrString
    , setSysAttrInt
    , getSysAttrInt
    , getConfigAttr
    ,
      -- * Error Handling
      clearErrStatus
    , setErrStatus
    , getErrStatus
    , setErrMsgStatus
    , setErrorMsgHandler
    , errorMsgStderr
    , errorMsgCollect
    , errorMsgStderrAndCollect
    , errorMsgIgnore
    , getErrorMessages
    , filterErrorMsg
    , issueWarn
    , issueErr
    , issueFatal
    , issueExc
    , setDocumentStatus
    , setDocumentStatusFromSystemState
    , documentStatusOk

    , -- * Tracing
      setTraceLevel
    , getTraceLevel
    , withTraceLevel
    , setTraceCmd
    , getTraceCmd
    , trace
    , traceMsg
    , traceValue
    , traceString
    , traceSource
    , traceTree
    , traceDoc

    , -- * Document Base
      setBaseURI
    , getBaseURI
    , changeBaseURI
    , setDefaultBaseURI
    , getDefaultBaseURI
    , runInLocalURIContext

    ,  -- * URI Manipulation
      expandURIString
    , expandURI
    , mkAbsURI
    , getFragmentFromURI
    , getPathFromURI
    , getPortFromURI
    , getQueryFromURI
    , getRegNameFromURI
    , getSchemeFromURI
    , getUserInfoFromURI

    , -- * Mime Type Handling
      getMimeTypeTable
    , setMimeTypeTable
    , setMimeTypeTableFromFile

    , -- * System Configuration and Options
      yes
    , no

    , withAcceptedMimeTypes
    , withAddDefaultDTD
    , withSysAttr
    , withCanonicalize
    , withCompression
    , withCheckNamespaces
    , withDefaultBaseURI
    , withStrictDeserialize
    , withEncodingErrors
    , withErrors
    , withFileMimeType
    , withIgnoreNoneXmlContents
    , withIndent
    , withInputEncoding
    , withInputOption
    , withInputOptions
    , withMimeTypeFile
    , withMimeTypeHandler
    , withNoEmptyElemFor
    , withXmlPi
    , withOutputEncoding
    , withOutputXML
    , withOutputHTML
    , withOutputXHTML
    , withOutputPLAIN
    , withParseByMimeType
    , withParseHTML
    , withPreserveComment
    , withProxy
    , withRedirect
    , withRemoveWS
    , withShowHaskell
    , withShowTree
    , withStrictInput
    , withSubstDTDEntities
    , withSubstHTMLEntities
    , withTextMode
    , withTrace
    , withValidate
    , withWarnings
    )
where

import Text.XML.HXT.Arrow.XmlState.ErrorHandling
import Text.XML.HXT.Arrow.XmlState.MimeTypeTable
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlState.SystemConfig
import Text.XML.HXT.Arrow.XmlState.TraceHandling
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.XmlState.URIHandling

-- ------------------------------------------------------------
