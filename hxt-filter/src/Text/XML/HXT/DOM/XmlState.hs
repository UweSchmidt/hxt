-- |
-- This module provides a Monad for an internal state and IO commands.
-- The state consists of two parts, the user state and the system state
-- user state ist a type parameter, the system state is a list
-- name-value pair. If the user state is not needed, the type parameter
-- can be instantiated with @()@.
--
-- Furthermore there are types for Xml filter working on this monad
-- and functions for manipulating the state components
-- and for lifting i\/o commands and XmlFilter to monad filters.
--
-- Error reporting functions are also located in this module.

module Text.XML.HXT.DOM.XmlState
    ( module Text.XML.HXT.DOM.XmlState
    )
where

import Text.XML.HXT.DOM.XmlTree

import qualified Control.Monad.MonadStateIO as MonadStateIO

import System.IO
import Data.Maybe

-- ------------------------------------------------------------

-- |
-- The internal system state consists of a list of name-value pairs
-- of type @(String, XmlTrees)@, so arbitrary lists of trees can be stored.
-- For options, often only strings are used as values, so a set of access
-- functions with string values is available
-- The error handling method can be controlled by an error handler filter,
-- the default filter issues the errors on stderr

data SysState			= SysState { sysStateAttrs		:: ! SysStateAttrs
					   , sysStateErrorHandler	:: ! (XmlStateFilter ())
					   }

type SysStateAttrs		= AssocList String XmlTrees

-- |
-- The State has a system and a user part
-- the user state is a type parameter

data XmlState state		= XmlState { sysState	:: ! SysState
					   , userState	:: ! state
					   }

-- |
-- The monad type for commands. It is an instance of "StateIO" from the
-- general module "Control.Monad.MonadStateIO".

type XState state res		= MonadStateIO.StateIO (XmlState state) res

-- |
-- The "XmlFilter" type for filters working on a state

type XmlStateFilter state	= XmlTree -> XState state XmlTrees

-- ------------------------------------------------------------
--
-- user defined state
-- access functions

-- |
-- change the user state
--
--    * 1.parameter fct :  the user state change function
--
--    - returns : the new state

changeState	:: (state -> state) -> XState state state
changeState f
    = do
      ns <- MonadStateIO.changeState f'
      return (userState ns)
      where
      f' s = s { userState = f (userState s) }

-- |
-- set the user state.
--
--    * 1.parameter s :  the new state
--
--    - returns : the new state

setState	:: state -> XState state state
setState s
    = changeState ( \_ -> s )

-- |
-- read the user state
--
--    - returns : the current state

getState	:: XState state state
getState
    = changeState id

-- ------------------------------------------------------------

-- |
-- change the system part of the state.
--
-- see also : 'changeState'

changeSysState	:: (SysState -> SysState) -> XState state SysState
changeSysState f
    = do
      ns <- MonadStateIO.changeState f'
      return (sysState ns)
      where
      f' s = s { sysState = f (sysState s) }

-- |
-- set the system part of the state.
--
-- see also : 'setState'

setSysState		:: SysState -> XState state SysState
setSysState s
    = changeSysState ( \_ -> s )

-- |
-- read the system part of the state.
--
-- see also : 'getState'

getSysState		:: XState state SysState
getSysState
    = changeSysState id

-- |
-- the initial system state
--
-- an empty list of attribute value pairs

initialSysState		:: SysState
initialSysState
    = SysState { sysStateAttrs        = []
	       , sysStateErrorHandler = errorMsgToStderr
	       }

-- |
-- change the attributes in the system state

changeSysStateAttrs		:: (SysStateAttrs -> SysStateAttrs) -> (SysState -> SysState)
changeSysStateAttrs cf sstate
    = sstate { sysStateAttrs = cf (sysStateAttrs sstate) }


-- |
-- set the error message handler

setSysErrorHandler		:: XmlStateFilter () -> XState state ()
setSysErrorHandler ehf
    = changeSysState (\ s -> s { sysStateErrorHandler = ehf })
      >> return ()

-- |
-- get the error handler

getSysErrorHandler		:: XState state (XmlStateFilter ())
getSysErrorHandler
    = do
      s <- getSysState
      return (sysStateErrorHandler s)

-- ------------------------------------------------------------

-- |
-- set or change a single system parameter.
--
--    * 1.parameter name :  the name of the parameter
--
--    - 2.parameter value :  the list of associated trees
--
--    - returns : nothing
--
-- see also : 'setSysParam', 'setSysParamInt'

setSysParamTree		:: String -> XmlTrees -> XState state ()
setSysParamTree name val
    = changeSysState (changeSysStateAttrs (addEntry name val))
      >> return ()

-- |
-- set or change a single system parameter of type string.
--
--    * 1.parameter name :  the name of the parameter
--
--    - 2.parameter value :  the (new) string value
--
--    - returns : nothing
--
-- see also : 'setSysParamTree', setSysParamInt

setSysParam		:: String -> String -> XState state ()
setSysParam name val
    = setSysParamTree name (xtext val)

-- |
-- set or change a single integer type system parameter
--
-- see also : 'setSysParam'

setSysParamInt		:: String -> Int -> XState state ()
setSysParamInt name val	= setSysParam name (show val)

-- |
-- add (or change) all attributes of the document root to the system state
--     - returns : this

setSystemParams	:: XmlStateFilter state
setSystemParams t
    = changeSysState (changeSysStateAttrs (addEntries (toTreel . getAttrl $ t)))
      >> thisM t

-- ------------------------------------------------------------

-- |
-- read a system parameter
--
--    * 1.parameter name :  the name of the parameter
--
--    - returns : the list of tres associated with the key, or the empty list for unknown parameters

getSysParamTree		:: String -> XState state XmlTrees
getSysParamTree name
    = do
      s <- getSysState
      return (lookup1 name (sysStateAttrs s))

-- |
-- read a system string parameter
--
--    * 1.parameter name :  the name of the parameter
--
--    - returns : the value, or the empty string for unknown parameters

getSysParam		:: String -> XState state String
getSysParam name
    = do
      ts <- getSysParamTree name
      return (xshow ts)

-- |
-- read a system parameter or return a default value
--
--    * 1.parameter name :  the name of the parameter
--
--    - 2.parameter default :  the default value
--
--    - returns : the value if found, else the default

getSysParamWithDefault	:: String -> String -> XState state String
getSysParamWithDefault name def
    = do
      val <- getSysParam name
      return ( if null val
	       then def
	       else val
	     )

-- |
-- read an integer system parameter
--
--    * 1.parameter name : 
--
--    - 2.parameter default : 
--
-- see also : 'getSysParamWithDefault'

getSysParamInt	:: String -> Int -> XState state Int
getSysParamInt var def
    = do
      val <- getSysParamWithDefault var (show def)
      return (read val)

-- ------------------------------------------------------------

-- |
-- exec a XState command with initial state.
--
--    * 1.parameter initalState :  the inital user state
--
--    - 2.parameter cmd :  the command
--
--    - returns : the i\/o command with result and user state

run0		:: XmlState state -> XState state res -> IO (res, XmlState state)
run0 initialState (MonadStateIO.STIO cmd)
    = do
      (res, finalState) <- cmd initialState
      return (res, finalState)

-- |
-- exec a XState command with initial user state.
-- ignore final user state.
-- like run0, but ignore the resulting user state

run		:: state -> XState state res -> IO res
run initialUserState cmd
    = do
      (res, _finalState) <- run0 (XmlState initialSysState initialUserState) cmd
      return res

-- |
-- exec a XState command in th IO monad.
-- like run with the empty state ().

run'		:: XState () res -> IO res
run'		= run ()

-- ------------------------------------------------------------

-- |
-- run a command in a new user state.
-- chain the system state part,
-- init new system state with the current one, run the command and
-- update the old system state with the resulting new system state
--
--    * 1.parameter initialUserState :  the initial user state
--
--    - 2.parameter cmd :  the command
--
--    - returns : the result of executing cmd and the final state

chain'		:: state1 -> XState state1 res -> XState state0 (res, state1)
chain' initialUserState1 cmd1
    = do
      sysState0 <- getSysState
      (res, finalState1) <- io $ run0 (XmlState sysState0 initialUserState1) cmd1
      _ <- setSysState (sysState finalState1)
      return (res, (userState finalState1))

-- |
-- like chain' but forget the final user state
--
--    * 1.parameter initialUserState :  the initial user state
--
--    - 2.parameter cmd :  the command
--
--    - returns : only the result of executing cmd

chain		:: state1 -> XState state1 res -> XState state0 res
chain initialUserState1 cmd1
    = do
      (res, _) <- chain' initialUserState1 cmd1
      return res

-- ------------------------------------------------------------
--
-- lift functions

-- |
-- lift a XmlFilter to a XmlStateFilter filter
-- issue all error nodes as error messages
-- and remove the error nodes from the result
--
--    * 1.parameter f :  the filter
--
--    - returns : the filter running in the state monad
--
--		  all errors are filtered from the result and issued on stderr

liftF		:: XmlFilter -> XmlStateFilter state
liftF f		= liftMf f .>> issueError

-- |
-- lift an I\/O command
--
--    * 1.parameter cmd :  the i\/o command
--
--    - returns : the i\/o command lifted to the XML state monad

io		:: IO a -> XState state a
io		= MonadStateIO.io

-- ------------------------------------------------------------
--

-- |
-- set the trace level.
--
-- convention:
--
-- 0: no trace output (default)
--
-- 1: trace important computation steps, e.g. accessing a document
--
-- 2: trace small computation steps
--
-- 3: output an intermediate result XmlTree in XML source format
--
-- 4: output an intermediate result XmlTree in tree representation
--
--    * 1.parameter level :  the trace level
--
--    - returns : nothing

setTraceLevel	:: Int -> XState state ()
setTraceLevel l	= setSysParamInt a_trace l

-- |
-- get the current trace level.
--
--    - returns : the current trace level

getTraceLevel	:: XState state Int
getTraceLevel	= getSysParamInt a_trace 0

-- |
-- trace output for arbitray commands.
--
--    * 1.parameter level :  the trace level,
--			  for which the command will be execuded
--			  if level \<= current trace level
--
--    - 2.parameter cmd :  the command to be executed
--
--    - returns : nothing

traceCmd	:: Int -> XState state a -> XState state ()
traceCmd level cmd
    = do
      trcLevel <- getTraceLevel
      if level <= trcLevel
        then do
	     _ <- cmd
	     return ()
        else return ()

-- |
-- trace output function for simple text.
--
--    * 1.parameter level :  like in traceCmd
--
--    - 2.parameter str :  the test
--
--    - returns : nothing

trace		:: Int -> String -> XState state ()
trace level str
    = traceCmd level
      $ do
	io $ hPutStrLn stderr ("-- (" ++ show level ++ ") " ++ str)

-- |
-- trace output of the user part of the program state.
--
--    * 1.parameter level :  like in traceCmd
--
--    - 2.parameter showFct :  the toString function
--
--    - returns : nothing

traceState	:: Int -> (state -> String) -> XState state ()
traceState level fct
    = traceCmd level
      $ do
	s <- getState
	io $ hPutStrLn stderr (fct s)

-- ------------------------------------------------------------
--
-- error functions

-- |
-- filter to reset the state attribute 'a_status'
--    - returns : this

clearStatus	:: XmlStateFilter state
clearStatus t
    = do
      setSysParamInt a_status c_ok
      thisM t

-- |
-- report an error message.
--
--    - returns : if the input tree n represents an error, @res = []@
--		  and the error is processed by the errror handler filter (default: error is issued on stderr)
--		  else @res = [n]@
--
-- see also : 'issueErr'

issueError	:: XmlStateFilter state
issueError
    = (setErrorMsgLevel .>> errorMsgHandler .>> noneM)
      `whenM`
      isXError

errorMsgHandler	:: XmlStateFilter state
errorMsgHandler
    = performAction
      ( \ t -> chain' () ( do
			   ehf <- getSysErrorHandler
			   _ <- ehf t
			   return ()
			 )
      )

-- |
-- set the error level in system state

setErrorMsgLevel	:: XmlStateFilter state
setErrorMsgLevel
    = performAction
      ( \ (NTree (XError level _str) _cs) ->
	do
	errLevel <- getSysParamInt a_status 0
	setSysParamInt a_status (max errLevel level)
      )

-- |
-- default error handler for writing errors to stderr

errorMsgToStderr	:: XmlStateFilter state
errorMsgToStderr
    = performAction
      ( \ (NTree (XError level str) _cs) ->
	io $ hPutStrLn stderr ("\n" ++ errClass level ++ ": " ++ str)
      )

-- |
-- error message handler for collecting all error messages
-- all messages are stored under attribute 'a_error_log'
-- they can be read with @getSysParamTree a_error_log@ or by
-- applying the filter 'getErrorMsg' to the root node

errorMsgLogging		:: XmlStateFilter state
errorMsgLogging
    = performAction
      ( \ t ->
	do
	errLog <- getSysParamTree a_error_log
	setSysParamTree a_error_log (t : errLog)
      )

errorMsgLoggingAndToStderr      :: XmlStateFilter state
errorMsgLoggingAndToStderr	= errorMsgLogging.>> errorMsgToStderr

-- |
-- the filter for reading all collected error mesages
--
-- result is the list of error messages, the input tree is ignored

getErrorMsg	:: XmlStateFilter state
getErrorMsg _t
    = do
      el <- getSysParamTree a_error_log
      setSysParamTree a_error_log []
      return (reverse el)

-- |
-- error level translation
-- 'c_warn' (1) : warning,
-- 'c_err' (2): error (e.g. parse error, validation error, ...),
-- 'c_fatal' (3) : fatal error (document access error, internal error, ...)

errClass	:: Int -> String
errClass l
    = fromMaybe "fatal error" . lookup l $ msgList
      where
      msgList	= [ (c_ok,	"no error")
		  , (c_warn,	"warning")
		  , (c_err,	"error")
		  , (c_fatal,	"fatal error")
		  ]

-- |
-- short cut for issuing a warning
--
-- see also : 'issueError', 'issueErr'

issueWarn	:: String -> XmlStateFilter state
issueWarn msg	= liftMf (warn msg) .>> issueError

-- |
-- short cut for issuing an error
--
-- see also : 'issueError'

issueErr	:: String -> XmlStateFilter state
issueErr msg	= liftMf (err msg) .>> issueError

-- |
-- short cut for issuing a fatal error
--
-- see also : 'issueError', 'issueErr'

issueFatal	:: String -> XmlStateFilter state
issueFatal msg	= liftMf (fatal msg) .>> issueError


-- ------------------------------------------------------------
--
-- issue an error, add the error to the document root tree
-- and return the tree

addFatal	:: String -> XmlStateFilter state
addFatal msg
    = liftF ( fatal msg
	      +++
	      setStatus c_fatal "accessing documents"
	    )

-- ------------------------------------------------------------

-- |
-- checks the value of the attribute 'a_status' in a document root.
-- if it contains a value greater or equal to 'c_err', an error with error message
-- stored in attribute 'a_module' is issued and the filter acts as the 'noneM' filter
-- else its the 'thisM' filter

checkStatus	:: XmlStateFilter state
checkStatus t
    = if status >= c_err
      then errorMsgHandler (mkXErrorTree c_warn (errClass status ++ "s detected in " ++ msg) [])
	   >> noneM t
	else
           thisM t
    where
    status = intValueOf a_status t
    msg    =    valueOf a_module t

-- |
-- add the error level and the module where the error occured
-- to the attributes of a document root node and remove the children when level is greater or equal to 'c_err'

setStatus	:: Int -> String -> XmlFilter
setStatus level msg
    = ( addAttrInt a_status level
	.>
	addAttr a_module msg
	.>
	( if level >= c_err
	  then replaceChildren []
	  else this
	)
      )
      `when` isRoot

-- |
-- check whether tree is a document root and the status attribute has a value less than 'c_err'

statusOk	:: XmlFilter
statusOk
    = isRoot
      .>
      isOf (\ t -> not (intValueOf a_status t >= c_err))
-- |
-- check whether the error level attribute in the system state
-- is set to error, in this case the children of the document root are
-- removed and error info is added as attributes with 'setStatus'
-- else nothing is changed

checkResult	:: String -> XmlStateFilter state
checkResult msg t
    = do
      level <- getSysParamInt a_status 0
      ( if level <= c_warn
	then thisM
	else liftMf ( setStatus level msg )
        ) t

-- |
-- monadic filter for processing the attribute list of a tag.
-- for other trees this filter acts like 'noneM'
--
-- see also : 'processAttr', 'processAttrl'

processAttrM				:: XmlStateFilter a -> XmlStateFilter a
processAttrM f t
    = do
      res <- f $$< al
      return $ replaceAttrl res t
    where
    al = getAttrl t

-- ------------------------------------------------------------
