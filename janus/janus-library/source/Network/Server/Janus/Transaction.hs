-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Transaction
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Transaction.hs, v1.0 2006/12/17 00:00:00 janus Exp $

   Janus Transactions

   Beside the according data types, this module provides functions to create and access Transaction
   XML values.

-}

-- ------------------------------------------------------------

module Network.Server.Janus.Transaction
    (
    -- transaction xpath constants
      ta_http_request
    , ta_http_response
    , ta_http_url

    -- data types
    , Transaction
    , Transactions
    , TransactionId
    , TransactionState (..)

    -- transaction construction and access
    , createTA
    , getTAId
    , setTAId
    , getTAState
    , setTAState
    , setTAStart
    , getTAStart
    , setTAEnd
    , getTAEnd
    , getTARunTime

    -- message operations on a transaction
    , sendTAMsg
    , getTAMsg
    , clearTAMsg
    )
where

import Text.XML.HXT.Arrow

import Network.Server.Janus.JanusPaths
import Network.Server.Janus.Messaging
import Network.Server.Janus.XmlHelper

type TransactionId      = Int

data TransactionState   =
      Init
    | Processing
    | Reinvoke
    | Completed
    | Failure
    deriving (Eq, Show, Read)

type Transaction    = XmlTree
type Transactions   = [Transaction]
type Timestamp      = Integer

-- transaction xpath constants
ta_http_request,
 ta_http_response,
 ta_http_url 			:: String

ta_http_request			= "/transaction/http/request/body"
ta_http_response		= "/transaction/http/response/body"
ta_http_url			= "/transaction/http/request/@url"

{- |
Creates an empty Transaction with a given id and in a given state.
-}
createTA :: TransactionId -> TransactionState -> XmlSource s a
createTA ident state =
    eelem "transaction" 				-- these names must corrsepond to the
        += sattr "transaction_id" (show ident)          -- names in JanusPaths
        += sattr "transaction_state" (show state)
        += eelem "messages"
        += eelem "request_fragment"
        += eelem "response_fragment"

-- Access functions
{- |
Returns a Transaction's id
-}
getTAId :: XmlAccess s TransactionId
getTAId =
    getValDef _transaction_transactionId "0"
    >>>
    parseA

{- |
Replaces a Transaction's id
-}
setTAId :: TransactionId -> XmlTransform s
setTAId ident =
    setVal _transaction_transactionId (show ident)

{- |
Returns a Transaction's state
-}
getTAState :: XmlAccess s TransactionState
getTAState =
    getVal _transaction_transactionState
    >>>
    parseA

{- |
Replaces a Transaction's state
-}
setTAState :: TransactionState -> XmlTransform s
setTAState state =
    setVal _transaction_transactionState (show state)

{- |
Returns a Transaction's start timestamp
-}
getTAStart :: XmlAccess s Timestamp
getTAStart =
    getVal _transaction_start
    >>>
    parseA

{- |
Replaces a Transaction's start timestamp
-}
setTAStart :: Timestamp -> XmlTransform s
setTAStart ts =
    setVal _transaction_start (show ts)

{- |
Returns a Transaction's end timestamp
-}
getTAEnd :: XmlAccess s Timestamp
getTAEnd =
    getVal _transaction_end
    >>>
    parseA

{- |
Replaces a Transaction's end timestamp
-}
setTAEnd :: Timestamp -> XmlTransform s
setTAEnd ts =
    setVal _transaction_end (show ts)

{- |
Returns a Transaction's runtime
-}
getTARunTime :: XmlAccess s Integer
getTARunTime =
    proc ta -> do
        start   <- getTAStart   -<  ta
        end     <- getTAEnd     -<  ta
        returnA                 -<  (end - start)

{- |
Stores a message in a Transaction's \/transaction\/messages node
-}
sendTAMsg :: MessageArrow s -> XmlTransform s
sendTAMsg msg =
    insTree _transaction_messages (liftConstSource msg)

{- |
Returns the messages stored in a Transaction's \/transaction\/messages node. This Arrow is non-deterministic.
-}
getTAMsg :: XmlAccess s Message
getTAMsg =
    getTree (_transaction_messages_ "*")

{- |
Removes all messages from a Transaction's \/transaction\/messages node
-}
clearTAMsg :: XmlTransform s
clearTAMsg =
    delTree _transaction_messages
    >>>
    insEmptyTree _transaction_messages
