-- ------------------------------------------------------------

module Text.XML.HXT.Monad.XmlState.ErrorHandling
where

import           Control.Exception                    (SomeException)
import           Control.Monad.Arrow

import           Data.Maybe

import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.Monad.ArrowXml
import           Text.XML.HXT.Monad.XmlState.TypeDefs

import           System.IO                            (hFlush, hPutStrLn,
                                                       stderr)

-- ------------------------------------------------------------

changeErrorStatus       :: (Int -> Int -> Int) -> IOStateArrow s Int Int
changeErrorStatus f     = chgSysVar theErrorStatus f

-- | reset global error variable

clearErrStatus          :: IOStateArrow s b b
clearErrStatus          = configSysVar $ setS theErrorStatus 0

-- | set global error variable

setErrStatus            :: IOStateArrow s Int Int
setErrStatus            = changeErrorStatus max

-- | read current global error status

getErrStatus            :: IOStateArrow s XmlTree Int
getErrStatus            = getSysVar theErrorStatus

-- ------------------------------------------------------------

-- | raise the global error status level to that of the input tree

setErrMsgStatus         :: IOStateArrow s XmlTree XmlTree
setErrMsgStatus         = perform
                          ( getErrorLevel >=> setErrStatus )

-- | set the error message handler and the flag for collecting the errors

setErrorMsgHandler      :: Bool -> (String -> IO ()) -> IOStateArrow s b b
setErrorMsgHandler c f  = configSysVar $ setS (theErrorMsgCollect .&&&. theErrorMsgHandler) (c, f)

-- | error message handler for output to stderr

sysErrorMsg             :: IOStateArrow s XmlTree XmlTree
sysErrorMsg             = perform
                          ( getErrorLevel &=& getErrorMsg
                            >=>
                            return . formatErrorMsg
                            >=>
                            getSysVar theErrorMsgHandler &=& this
                            >=>
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
getErrorMessages        = getSysVar theErrorMsgList
                          >=>
                          configSysVar (setS theErrorMsgList [])
                          >=>
                          arrL reverse

addToErrorMsgList       :: IOStateArrow s XmlTree XmlTree
addToErrorMsgList       = chgSysVar
                          ( theErrorMsgCollect .&&&. theErrorMsgList )
                          ( \ e (cs, es) -> (cs, if cs then e : es else es) )

-- ------------------------------------------------------------

-- |
-- filter error messages from input trees and issue errors

filterErrorMsg          :: IOStateArrow s XmlTree XmlTree
filterErrorMsg          = ( setErrMsgStatus
                            >=>
                            sysErrorMsg
                            >=>
                            addToErrorMsgList
                            >=>
                            none
                          )
                          `when`
                          isError

-- | generate a warnig message

issueWarn               :: String -> IOStateArrow s b b
issueWarn msg           = perform (warn msg  >=> filterErrorMsg)

-- | generate an error message
issueErr                :: String -> IOStateArrow s b b
issueErr msg            = perform (err msg   >=> filterErrorMsg)

-- | generate a fatal error message, e.g. document not found

issueFatal              :: String -> IOStateArrow s b b
issueFatal msg          = perform (fatal msg >=> filterErrorMsg)

-- | Default exception handler: issue a fatal error message and fail.
--
-- The parameter can be used to specify where the error occured

issueExc                :: String -> IOStateArrow s SomeException b
issueExc m              = ( issueFatal $< return . (msg ++) . show )
                          >=>
                          none
    where
    msg | null m        = "Exception: "
        | otherwise     = "Exception in " ++ m ++ ": "

-- |
-- add the error level and the module where the error occured
-- to the attributes of a document root node and remove the children when level is greater or equal to 'c_err'.
-- called by 'setDocumentStatusFromSystemState' when the system state indicates an error

setDocumentStatus       :: Int -> String -> IOStateArrow s XmlTree XmlTree
setDocumentStatus level msg
                        = ( addAttrl ( sattr a_status (show level)
                                       <++>
                                       sattr a_module msg
                                     )
                            >=>
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

documentStatusOk        :: MonadSeq m => XmlTree -> m XmlTree
documentStatusOk        = isRoot
                          >=>
                          ( (getAttrValue a_status
                             >=>
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
