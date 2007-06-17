-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Messaging
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Messaging.hs, v1.0 2006/10/28 00:00:00 janus Exp $

   Janus Messaging
   
   Provides the data types for Janus messages and message filters as well as construction,
   access and filter functions.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Messaging
	(
	-- data types
	  Message
	, Messages
	, MessageArrow
	, MessageFilter
	, MessageType (..)
	, MessageCode (..)
	, MessageLevel
	, MessageSource
	, MessageValue
	, MessageState
	, MessageTime
	, l_control
	, l_debug
	, l_info
	, l_warn
	, l_error
	, l_mandatory

	-- message creation
	, mkControlMsg
	, mkPlainMsg
	, mkSimpleLog
	, mkLog
	, mkErr
	, mkWarn

	-- message access
	, getMsgType
	, getMsgSource
	, getMsgCode
	, getMsgValue
	, getMsgLevel
	, getMsgTS
	, showMsg

	-- message state
	, addMsgState
	, delMsgState
	, clearMsgState
	, getMsgState
	, listMsgStates

	-- message list transform
	, getMsgFilter
	, getMsgTypeFilter
	, getMsgLevelFilter
	, getMsgCodeFilter
	, getMsgStateFilter
	)
where

import Data.Map
import System.Time
import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.XmlTree (XmlTree)

import Network.Server.Janus.JanusPaths
import Network.Server.Janus.XmlHelper

type Message  		= XmlTree
type Messages 		= [Message]
type MessageFilter s	= XmlTransform s
type MessageArrow s	= XmlConstSource s

data MessageType 
	= ControlMsg
	| ErrorMsg 
	| EventMsg
	| LogMsg 
	| PlainMsg
	| WarningMsg 
	deriving (Show, Eq, Read)

-- message codes
data MessageCode 
	= FileNotFound
	| FormatError
	| GenericMessage
	| LoaderError
	| NewMessageLevel
	| Reload
	| ShaderFailed
	| TAValueNotFound
	| Terminate
	| TypeError
	| ValueNotFound
	deriving (Show, Eq, Read)

type MessageLevel 	= Integer
type MessageSource 	= String
type MessageValue 	= String
type MessageState 	= Map String String
type MessageTime   	= ClockTime

-- Constants for log levels
l_control,
 l_debug,
 l_info,
 l_warn,
 l_error,
 l_mandatory :: MessageLevel

l_control   = 50
l_debug     = 30
l_info      = 20
l_warn      = 10
l_error     = 1
l_mandatory = 0



-- Message Construction
{- |
Creates a new message Arrow, providing all fields.
-}
createMsg :: MessageType -> MessageSource -> MessageCode -> MessageValue -> MessageLevel -> MessageTime -> XmlSource s a
createMsg typ src code val level _ = 
	proc _ -> do
		ts	<- getCurrentTS 	-< ()
		(eelem "message" 
			+= sattr "type" (show typ)				
			+= sattr "timestamp" (show ts)				
			+= sattr "source" src
			+= sattr "level" (show level)				
			+= sattr "code" (show code)				
			+= sattr "value" val
			+= eelem "state"
			)			-<< ()

{- |
Creates a new control message, only providing the message state and its code.
-}
mkControlMsg :: [(String, String)] -> MessageCode -> XmlSource s a
mkControlMsg state code = 
	proc _ -> do
		ts <- arrIO0 $ getClockTime -< ()
		msg <- createMsg ControlMsg "" code "" l_control ts -<< ()
		addState state -< msg
	where
		addState             [] = this
		addState ((key,val):xs) = addMsgState key val 
					  >>> 
					  addState xs

{- |
Creates a new plain message to get displayed to a human user without annotations. The message is created on the mandatory
level and is therefore shown in any case.
-}
mkPlainMsg :: MessageValue -> XmlSource s a
mkPlainMsg val = 
	proc _ -> do
		ts <- arrIO0 $ getClockTime -< ()
		createMsg PlainMsg "" GenericMessage val l_mandatory ts -<< ()

{- |
Creates a new simplified log message, adding the message source and level to a plain message.
-}
mkSimpleLog :: MessageSource -> MessageValue -> MessageLevel -> XmlSource s a
mkSimpleLog src val level = 
	proc _ -> do
		ts <- arrIO0 $ getClockTime -< ()
		createMsg LogMsg src GenericMessage val level ts -<< ()

{- |
Creates a new log message, adding the message code to a simple log message.
-}
mkLog :: MessageSource -> MessageCode -> MessageValue -> MessageLevel -> XmlSource s a
mkLog src code val level = 
	proc _ -> do
		ts <- arrIO0 $ getClockTime -< ()
		createMsg LogMsg src code val level ts -<< ()

{- |
Creates a new error message. The message is created on the error level. Beside the value the source, code and
state have to be defined (translating into readability by both human and automatic processors).
-}
mkErr :: MessageSource -> MessageCode -> MessageValue -> [(String, String)] -> XmlSource s a
mkErr src code val state = 
	proc _ -> do
		ts <- arrIO0 $ getClockTime -< ()
		msg <- createMsg ErrorMsg src code val l_error ts -<< ()
		addState state -< msg
	where
		addState             [] = this
		addState ((key,val'):xs) = addMsgState key val' 
					   >>> 
					   addState xs

{- |
Creates a new warning message. The message is created on the warning level. Beside the value the source and code 
have to be defined.
-}
mkWarn :: MessageSource -> MessageCode -> MessageValue -> XmlSource s a
mkWarn src code val = 
	proc _ -> do
		ts <- arrIO0 $ getClockTime -< ()
		createMsg WarningMsg src code val l_warn ts -<< ()



-- Message Access
{- |
An Arrow delivering the type of a Message.
-}
getMsgType :: XmlAccess s MessageType
getMsgType =
	proc msg -> do
		typ <- getVal _message_type -< msg
		returnA -< (read typ)

{- |
An Arrow delivering the source of a Message.
-}
getMsgSource  :: XmlAccess s MessageSource
getMsgSource =
	getVal _message_source

{- |
An Arrow delivering the code of a Message.
-}
getMsgCode :: XmlAccess s MessageCode
getMsgCode =
	proc msg -> do
		code <- getVal _message_code -< msg
		returnA -< (read code)

{- |
An Arrow delivering the value of a Message.
-}
getMsgValue :: XmlAccess s MessageValue
getMsgValue =
	getVal _message_value

{- |
An Arrow delivering the level of a Message.
-}
getMsgLevel :: XmlAccess s MessageLevel
getMsgLevel =
	proc msg -> do
		level <- getVal _message_level -< msg
		returnA -< (read level)

{- |
An Arrow delivering the timestamp of a Message.
-}
getMsgTS :: XmlAccess s JanusTimestamp
getMsgTS =
	proc msg -> do
		ts <- getVal _message_timestamp -< msg
		returnA -< (read ts)

{- |
An Arrow delivering a string representation of a Message.
-}
showMsg :: XmlAccess s String
showMsg =
	proc msg -> do
		typ <- getMsgType -< msg
		source <- getMsgSource -< msg
		level <- getMsgLevel -< msg
		code <- getMsgCode -< msg
		value <- getMsgValue -< msg
		returnA -< if typ == PlainMsg
				then value
				else "[" ++ show typ ++ ": " ++ value ++ " (" ++ source ++ ", code " ++ show code ++ ", level " ++ show level ++ ")]\n"



{- |
An Arrow adding a name-value pair to the state of a message.
-}
addMsgState :: String -> String -> XmlTransform s
addMsgState key val = 
	setVal (_message_state_ key) val

{- |
An Arrow removing a name-value pair from the state of a message by means of the name.
-}
delMsgState :: String -> XmlTransform s
delMsgState key = 
	delVal (_message_state_ key)

{- |
An Arrow deleting the whole state of a message.
-}
clearMsgState :: XmlTransform s
clearMsgState = 
	delTree _message_state
	>>> 
	insEmptyTree _message_state

{- |
An Arrow delivering the value of a name-value pair in a message's state.
-}
getMsgState :: String -> XmlAccess s String
getMsgState key = 
	getVal (_message_state_ key)

{- |
An Arrow delivering the names of name-value pairs in a message's state. This Arrow is non-deterministic.
-}
listMsgStates :: XmlAccess s String
listMsgStates = 
	listVals (_message_state_ "*")




-- message list transform
{- |
An Arrow delivering a message filter based on an access function, a binary comparison function and a comparison 
value. Messages may pass the filter if they satisfy the comparison function.
-}
getMsgFilter :: XmlAccess s a -> (a -> a -> Bool) -> a -> MessageFilter s
getMsgFilter access comp value =
	proc msg -> do
		value' <- access -< msg
		if comp value' value
			then returnA -< msg
			else zeroArrow -< ()

{- |
An Arrow delivering a message filter to let only messages of a given type pass.
-}
getMsgTypeFilter :: MessageType -> MessageFilter s
getMsgTypeFilter typ = 
	getMsgFilter getMsgType (==) typ

{- |
An Arrow delivering a message filter to let only messages less or equal to a given level pass.
-}
getMsgLevelFilter :: MessageLevel -> MessageFilter s
getMsgLevelFilter level = 
	getMsgFilter getMsgLevel (<=) level

{- |
An Arrow delivering a message filter to let only messages of a given message code pass.
-}
getMsgCodeFilter :: MessageCode -> MessageFilter s
getMsgCodeFilter code = 
	getMsgFilter getMsgCode (==) code

{- |
An Arrow delivering a message filter to let only messages with a given name-value pair in their
state pass.
-}
getMsgStateFilter :: String -> String -> MessageFilter s
getMsgStateFilter key val = 
	getMsgFilter (getMsgState key) (==) val
