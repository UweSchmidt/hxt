-- ------------------------------------------------------------

module Text.XML.HXT.Monad.XmlState
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

import           Text.XML.HXT.Monad.XmlState.ErrorHandling
import           Text.XML.HXT.Monad.XmlState.MimeTypeTable
import           Text.XML.HXT.Monad.XmlState.RunIOStateArrow
import           Text.XML.HXT.Monad.XmlState.SystemConfig
import           Text.XML.HXT.Monad.XmlState.TraceHandling
import           Text.XML.HXT.Monad.XmlState.TypeDefs
import           Text.XML.HXT.Monad.XmlState.URIHandling

-- ------------------------------------------------------------
