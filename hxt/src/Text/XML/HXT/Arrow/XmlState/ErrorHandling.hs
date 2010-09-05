-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlState.ErrorHandling
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

module Text.XML.HXT.Arrow.XmlState.ErrorHandling
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO

import Data.Maybe

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs

import System.IO                        ( hPutStrLn
                                        , hFlush
                                        , stderr
                                        )

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
errorMsgStderr          = setErrorMsgHandler False (\ x ->
                                                    do hPutStrLn stderr x
                                                       hFlush    stderr
                                                   )

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

errorOutputToStderr     :: String -> IO ()
errorOutputToStderr msg
                        = do
                          hPutStrLn stderr msg
                          hFlush stderr

-- ------------------------------------------------------------
