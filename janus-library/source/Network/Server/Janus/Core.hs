-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Core
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Core.hs,v 1.1 2007/03/26 00:00:00 janus Exp $

   Janus Shader and Handler Definitions

   Definition of the Shader arrow, its Context state and the Handler arrow. An interface to use the Context state and its
   facilities is provided.

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Core
    (
    -- data types
      JanusArrow
    , JanusStateArrow
    , Handler
    , HandlerCreator
    , Associations
    , ConfigArrow
    , Context
    , MessageHandler
    , Shader
    , ShaderCreator
    , StateHandler (..)
    , StateValue (..)
    , StateOperation (..)
    , ChannelId
    , UID

    -- state handling (persistence layer configuration)
    , defHandler

    -- interface for the associations (subshaders) of a shader
    , createAssocs
    , addShader
    , shaderList
    , selectList
    , lookupShader

    -- thread pool
    , addThread
    , delThread
    , listThreads
    , listThreadPairs
    , stopThread

    -- TODO
    , getContext
    , setContext
    , getCursor
    , setCursor
    , runInContext

    -- some predefined shaders
    , mkCreator
    , mkStaticCreator
    , mkDynamicCreator
    , mkFallibleCreator
    , loadShader
    , loadAssocs
    , executeShader
    , dynShader
    , nullShader
    , idShader
    , idErrorShader
    , seqShader

    -- Context constructor
    , emptyContext

    -- Operations on the server configuration in the Context
    , getConfig
    , swapConfig

    -- Operations on repositories in the Context
    , getShaderCreators
    , swapShaderCreators
    , delShaderCreators
    , getShaderCreator
    , addShaderCreator
    , getHandlerCreators
    , swapHandlerCreators
    , delHandlerCreators
    , getHandlerCreator
    , addHandlerCreator

    -- Operations on the shadow facility (to allow for construction of a new shader while processing one) in the Context
    , getShadow
    , swapShadow
    , extendShadow

    -- Operations on state scopes in the Context
    , listScopes
    , addScope
    , swapScope
    , delScope

    -- Operations on the values contained in the state scopes in the Context
    , getCellTS
    , getSC
    , getSV
    , getSVP
    , getSVS
    , getStateCell
    , listStateTrees
    , listStatePairsStr
    , listStatePairs
    , getStateTreeTS
    , (<#!)
    , (<=!)
    , (<$!)
    , (<*!)
    , (<-!)
    , setStateCell
    , setStateCellSafe
    , delStateTree
    , swapStateStr
    , swapStateCell
    , swapStateCellSafe
    , (<=!>)
    , (<$!>)
    , (<*!>)
    , (<-!>)
    , mapStateCell
    , listenStateTree
    , invokeStateTree

    -- Operations to deliver unique identifiers
    , getUID
    , getQualifiedUID

    -- Operations concerning channel ids and message channels
    , mkChannelId
    , chGlobal
    , chLocal
    , chControl
    , listChannels
    , addChannel
    , swapChannel
    , delChannel

    -- Operations on a single channel
    , listenChannel
    , invokeChannel
    , changeHandler
    , addHandler
    , clearHandler
    , sendMsg
    , sendPlain
    , globalMsg
    , localMsg
    , controlMsg
    , (<-@)
    , getMsg
    , filterMsg
    , clearMsg
    , (<*>)
    , (<!>)

    -- Operations on the message buffer and related Transaction messaging
    , bufferMsg
    , forwardMsgBuf
    , forwardError
    , maskError
    , getMsgBuf
    , clearMsgBuf
    , (<+*>)
    , (<+!>)

    -- Handlers for message channels
    , defaultHandler
    , storeHandler
    , consoleHandler
    , logFileHandler
    , logHandler
    , filterHandler
    , invokeHandler
    , nullHandler
    , createThread

    )
where

-- ------------------------------------------------------------

import Control.Concurrent

import Data.Char
import Data.Dynamic
import Data.Either
import Data.Map
import Data.Maybe
import Data.Typeable

import Network.Server.Janus.DynamicLoader
import Network.Server.Janus.Messaging
import Network.Server.Janus.Transaction as TA
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

import System.IO
import System.Eval (unsafeEval)
import System.Time

import Text.ParserCombinators.Parsec

import Text.XML.HXT.Arrow
import Text.XML.HXT.XPath
import Text.XML.HXT.XPath.XPathDataTypes
       ( Expr (..)
       , LocationPath (..)
       , Path (..)
       , AxisSpec (..)
       , NodeTest (..)
       , XStep (..)
       )

-- ------------------------------------------------------------

infixr 0  <-@, <-!

type JanusArrow    s b c	= IOStateArrow     s b c
type JanusStateArrow b c	= JanusArrow Context b c
type Handler			= JanusStateArrow () ()
type HandlerCreator		= JanusStateArrow (XmlTree, Shader) Handler

-- TODO
-- type StateHandler       = (JanusStateArrow String String, JanusStateArrow String String)
data StateHandler       = StateHandler {
    sh_load     :: JanusStateArrow StateIdentifier (StateHandler, StateCell),
    sh_store    :: JanusStateArrow (StateIdentifier, StateCell) StateHandler,
    sh_traverse :: JanusStateArrow StateIdentifier StateTree,
    sh_create   :: JanusStateArrow (StateIdentifier, StateCell) StateHandler
}

defHandler :: StateIdentifier -> StateCell -> StateHandler
defHandler _ cell =
    StateHandler {
          sh_load = proc _ -> do
                    returnA -< (undefined, cell)
        , sh_store = proc (_, _) -> do
                    returnA -< undefined
        , sh_traverse = proc _ -> do
                    returnA -< undefined
        , sh_create = proc (_, _) -> do
                    returnA -< undefined
    }

-- ------------------------------------------------------------

type Shader             = JanusStateArrow XmlTree XmlTree
type ShaderCreator      = JanusStateArrow XmlTree Shader

data Associations       = Assoc ([Shader], Map String Shader)

data Context            = Context { c_cfg       :: ConfigArrow
				  , c_threads   :: MVar (Map String ThreadId)
				  , c_sRep      :: Repository ShaderCreator
				  , c_hRep      :: Repository HandlerCreator
				  -- , c_stRep     :: Repository StateHandlerCreator
				  , c_scopes    :: Map String SharedScope
				  , c_channels  :: Map ChannelId SharedChannel
				  , c_shadow    :: Shader
				  , c_cursor    :: StateIdentifier
				  , c_msgbuf    :: Messages
				  }
			  deriving Typeable

-- ------------------------------------------------------------

type MessageHandler     = JanusStateArrow (Channel, Messages) (Channel, Messages)
type ConfigArrow        = JanusStateArrow () XmlTree -- XmlConstSource Context

-- ------------------------------------------------------------

newtype Channel         = Ch (Messages, MessageHandler, [ChannelListener])

type SharedChannel      = MVar Channel

type ChannelListener    = MVar Messages

newtype ChannelId	= CId { ch_id :: String
			      }
                          deriving (Eq, Ord)

instance Show ChannelId where show = ch_id

mkChannelId		:: String -> ChannelId
mkChannelId		= CId . (Prelude.map toLower)

chGlobal
  , chLocal
  , chControl		:: ChannelId
chGlobal		= mkChannelId "global"
chLocal			= mkChannelId "local"
chControl		= mkChannelId "control"

-- ------------------------------------------------------------

type SharedScope        = SharedTree
type SharedTree         = MVar StateTree
-- type SharedCell      = MVar StateCell
type StateIdentifier    = [XStep]
type StateListener      = MVar StateOperation

-- ------------------------------------------------------------

type UID                = Int

data StateOperation
    = Delete        StateIdentifier
    | Insert        StateIdentifier
    | Update        StateIdentifier StateCell
    | Unspecified   StateIdentifier

data StateTree          = StateTree {
    t_id        :: String,
    t_cell      :: StateCell,
    t_ts        :: JanusTimestamp,
    t_children  :: Map String SharedTree,
    t_listeners :: [StateListener]
}

data StateCell          = StateCell {
    c_val       :: StateValue,
    c_ts        :: JanusTimestamp
}

data StateValue
    = NullVal
    | PolyVal       Dynamic
    | SimpleVal     String
    | ListVal       [String]
    | MapVal        (Map String String)
    | ActionVal     (IO ())
    | HandleVal     Handle
    | HdlOpVal      (Handle -> IO ())
    | MessageVal    Message
    | ShaderVal     Shader
    | UIDVal        UID
    | XmlVal        XmlTree
    | ContextVal    Context
    | ThreadVal     ThreadId

instance Show StateValue where
    show (NullVal)          = "{Nil}"
    show (PolyVal _)        = "{Dynamic Value}"
    show (SimpleVal str)    = str
    show (ListVal strs)     = show strs
    show (MapVal strs)      = show strs
    show (ActionVal _)      = "{IO}"
    show (HandleVal _)      = "{Handle}"
    show (HdlOpVal _)       = "{HandleAction}"
    show (MessageVal msg)   = show msg
    show (ShaderVal _)      = "{Shader}"
    show (UIDVal uid)       = "{UID " ++ (show uid) ++ "}"
    show (XmlVal tree)      = show tree
    show (ContextVal _)     = "{Context}"
    show (ThreadVal tid)     = "{" ++ (show tid) ++ "}"

-- ------------------------------------------------------------

-- Associations

{- |
Constructor to create an Associations value based on a list of id-Shader pairs. The Associations type encapsulates the
sub-Shaders of a given Shader and provides access to them by their identifiers. The interface preserves the definition
order in the configuration file.
-}
createAssocs :: [(String, Shader)] -> Associations
createAssocs list =
    Assoc (Prelude.map snd list, fromList list)

{- |
Adds a Shader to an Associations value. The first argument represents the Shader's id, the second the Shader itself.
-}
addShader :: String -> Shader -> Associations -> Associations
addShader key shader (Assoc (list, assocMap)) =
    Assoc (new_list, new_map)
    where
        new_list = reverse $ (head list):(reverse list)
        new_map = insert key shader assocMap

{- |
Returns the Shaders contained in an Associations value. This list preserves the definition order in the configuration
file.
-}
shaderList :: Associations -> [Shader]
shaderList (Assoc (list, _)) =
    list

{- |
Returns the identifiers contained in an Associations value. This list does NOT preserve the definition order in the configuration
file.
-}
selectList :: Associations -> [String]
selectList (Assoc (_, assocMap)) =
    keys assocMap

{- |
Returns a Shader by its identifier. If the identifier cannot be found, a default Shader (second argument) is returned.
-}
lookupShader :: String -> Shader -> Associations -> Shader
lookupShader key def (Assoc (_, assocMap)) =
    findWithDefault def key assocMap






-- StateHandler
-- TODO










-- Thread Pool
{- |
TODO
-}
addThread :: String -> ThreadId -> JanusStateArrow a a
addThread name ident =
    proc through -> do
        context     <- getContext               -<  ()
        let threads      = c_threads context
        threads'    <- arrIO $ takeMVar         -<  threads
        let threads''    = insert name ident threads'
        arrIO $ putMVar threads                 -<< threads''
        returnA                                 -<  through

{- |
TODO
-}
delThread :: String -> JanusStateArrow a a
delThread name =
    proc through -> do
        context     <- getContext               -<  ()
        let threads      = c_threads context
        threads'    <- arrIO $ takeMVar         -<  threads
        let threads''    = delete name threads'
        arrIO $ putMVar threads                 -<< threads''
        returnA                                 -<  through

{- |
TODO
-}
listThreads :: JanusStateArrow a String
listThreads =
    proc _ -> do
        context     <- getContext               -<  ()
        let threads     = c_threads context
        threads'    <- arrIO $ takeMVar         -<  threads
        constL $ keys threads'                  -<< ()

{- |
TODO
-}
listThreadPairs :: JanusStateArrow a (String, ThreadId)
listThreadPairs =
    proc _ -> do
        context     <- getContext               -<  ()
        let threads     = c_threads context
        threads'    <- arrIO $ takeMVar         -<  threads
        constL $ toList threads'                -<< ()

{- |
TODO
-}
stopThread :: String -> JanusStateArrow a a
stopThread name =
    proc through -> do
        context     <- getContext               -<  ()
        let threads     = c_threads context
        threads'    <- arrIO $ takeMVar         -<  threads
        if member name threads'
            then arrIO $ killThread             -<  threads' ! name
            else zeroArrow                      -<  ()
        delThread name                          -<  through





-- Shader Construction

{- |
Like mkCreator, but this function takes a Shader instead of a ShaderCreator. This Shader is static in that it does not depend on
Associations or configuration.
-}
mkStaticCreator :: Shader -> ShaderCreator
mkStaticCreator shader =
    mkDynamicCreator (constA shader)

{- |
TODO
A wrapper for a ShaderCreator. This wrapper adds error handling by means of zeroArrow to a ShaderCreator. It removes messages from
the Context message buffer (where some operations may store Transaction messages before failing) to put them in to Transaction.
-}
mkDynamicCreator :: JanusStateArrow (XmlTree, Associations) Shader -> ShaderCreator
mkDynamicCreator creator =
    mkCreator True creator

{- |
TODO
-}
mkFallibleCreator :: JanusStateArrow (XmlTree, Associations) Shader -> ShaderCreator
mkFallibleCreator creator =
    mkCreator False creator

{- |
TODO
-- mapping erzeugen
-- debugging optionen
-}
mkCreator :: Bool -> JanusStateArrow (XmlTree, Associations) Shader -> ShaderCreator
mkCreator errorhandling creator =
    proc conf -> do
        ident           <- getVal _shader_config_id                  -<  conf
        state           <- getVal _shader_config_state               -<  conf

        globalMsg $ "loading init shader for shader '" ++
                                ident ++ "'... "                        -<< ()
        iShader         <- ((single $ getTree _shader_init_shader)
                                >>>
                                loadShader
                                >>>
                                (arr snd)
                                ) `orElse` (constA this)                -<  conf
        globalMsg $ "done\n"                       -<< ()
        globalMsg $ "executing init shader for shader '" ++
                                ident ++ "'... "                        -<< ()
        (executeShader iShader) >>> (globalMsg $ "done\n")
            <*> (chGlobal, mkErr "Core.hs:mkCreator" GenericMessage ("Init shader failed for shader '" ++ ident ++ "'") [])
                                                                        -<< ()

        associations    <- loadAssocs                                   -<  conf
        root_cursor     <- getVal _shader_config_rootState
                           >>>
                           (arr $ \str -> toId str [])                  -<  conf
        let root_cursor' = case root_cursor of
                                Nothing      -> this
                                Just cursor  -> setCursor cursor
        (state <$! NullVal)                                             -<< ()

        shader      <- root_cursor' >>> creator
            <*> (chGlobal, mkErr "Core.hs:mkCreator" GenericMessage ("Shader creator failed for shader '" ++ ident ++ "'") [])
                                                                        -<< (conf, associations)
        let shader' = (proc in_ta -> do
                accepts     <- getVal _shader_config_accepts
                                >>>
                                parseA                          -<  conf
                ta_state    <- TA.getTAState                    -<  in_ta
                (if elem ta_state accepts
                    then
                        (chGlobal  <-@ mkSimpleLog ident (ident ++ " invoked.\n") l_debug)
                            >>>
                            root_cursor'
                            >>>
                            shader
                            >>>
                            (chGlobal  <-@ mkSimpleLog ident (ident ++ " completed.\n") l_debug)
                    else this)                                  -<< in_ta
                    )
        let shader'' = if errorhandling
                        then
                            (shader'
                            `orElse`
                                (proc ta -> do
                                    msg <- listA $ getMsgBuf    -<  ()
                                    (sendTAMsg (constL msg)
                                        >>>
                                        setTAState Failure
                                        )                       -<< ta
                                    )
                                `orElse`
                                    (chGlobal <-@ mkErr "mkCreator" ShaderFailed "shader failed." [])
                                )
                        else
                            shader'
        returnA                                                         -<  shader''


{- |
Creates a ShaderCreator by reading \/config\/haskell from its configuration and compiling the contained string. If reading of \/config\/haskell
or compiling the value fails, the result equals the identity ShaderCreator. The configuration and Associations of this ShaderCreator are
forwarded to the compiled one.
-}
dynShader :: ShaderCreator
dynShader =
    proc conf -> do
        source  <- getValDef _shader_config_haskell ""        -<  conf
        creator <- arrIO0 $
            (unsafeEval
                ("(" ++ source ++ ") :: ShaderCreator")
                ["Shader.Shader", "Control.Arrow", "Control.Arrow.ArrowIO", "System.IO", "Prelude"] :: IO (Maybe ShaderCreator)
                )                                               -<< ()
        shader  <-
            if isJust creator
                then fromJust creator                           -<< conf
                else returnA                                    -<  this
        returnA                                                 -<< shader

{- |
A Shader always failing (equals the zeroArrow).
-}
nullShader :: Shader
nullShader =
    zeroArrow

{- |
The identity Shader simply delivering its input Transaction unchanged.
-}
idShader :: Shader
idShader =
    returnA

{- |
Equals the identity Shader, but sends a fix error message to the \"global\" message channel as a side effect.
-}
idErrorShader :: String -> Shader
idErrorShader _ =
    proc ta -> do
        chGlobal <-@ (mkErr "Core.hs:idErrorShader " LoaderError ("idErrorShader invoked.") []) -< ()
        returnA -< ta

{- |
Builds a Shader representing the sequence of Shaders contained in an Associations value.
-}
seqShader :: Associations -> Shader
seqShader associations =
    seqA (shaderList associations)

{- |
TODO
-}
loadShader :: JanusStateArrow XmlTree (String, Shader)
loadShader =
    proc sTree -> do
        sConf           <- getTree _shader_config                       -<  sTree
        ident           <- getVal _config_id                            -<  sConf
        sType           <- getVal _config_type
                           `orElse`
                           (constA ""
                                >>> (chGlobal <-@ mkErr "Core.hs:loadShader" GenericMessage ("Type undefined for shader '" ++ ident ++ "'") [])
                                )                                       -<< sConf

        chGlobal <-@ mkSimpleLog "Core.hs:loadShader" ("Loading shader " ++ ident ++ " of type " ++ sType) l_debug -<< ()

        creator     <- getShaderCreator sType                           -<< ()
        shader      <- creator                                          -<< sTree
        returnA                                                         -<  (ident, shader)

{- |
TODO
-}
loadAssocs :: JanusStateArrow XmlTree Associations
loadAssocs =
    proc sTree -> do
        sConf           <- getTree _shader_config                   -<  sTree
        ident           <- getVal  _config_id                       -<  sConf
        associations <- listA (
            proc sTree' -> do
                ssTree              <- getTree _shader_shader       -<  sTree'
                (ident', shader')   <- loadShader                   -<  ssTree
                chGlobal <-@ mkSimpleLog "Core.hs:loadAssocs"
                             ("Associated shader '" ++ ident' ++ "' to parent shader '" ++ ident ++ "'")
                             l_debug                                -<< ()
                returnA                                             -<  (ident', shader')
            )                                                           -<< sTree
        returnA                                                         -<  createAssocs associations

{- |
TODO
-}
executeShader :: Shader -> JanusStateArrow a XmlTree
executeShader shader =
    proc _ -> do
        createTA 0 Processing >>> shader                                -<< ()

-- Context
-- -- Construction and General Operations

{- |
Provides the empty context.
-}
emptyContext :: IO Context
emptyContext =
    do
        ct              <- getClockTime
        let ts   = getTS ct
        let cell = StateCell { c_val = NullVal, c_ts = ts }
        let uid  = StateTree { t_id = "uid", t_cell = cell, t_ts = ts, t_children = empty, t_listeners = [] }
        mvar_uid        <- newMVar uid
        mvar_threads    <- newMVar empty
        let result = Context {
                        c_cfg       = eelem "config",
                        c_threads   = mvar_threads,
                        c_sRep      = newRepository "Shader.ShaderCreator",
                        c_hRep      = newRepository "Shader.HandlerCreator",
                        -- c_pRep      = newRepository "Shader.StateHandler",
                        c_shadow    = returnA,
                        c_cursor    = [],
                        c_scopes    = insert "uid" mvar_uid empty,
                        c_channels  = empty,
                        c_msgbuf    = []
                     }
        return result

{- |
Returns the current Context.
-}
getContext :: JanusStateArrow a Context
getContext =
    getUserState

{- |
Replaces the current Context and returns the input value.
-}
setContext :: Context -> JanusStateArrow a a
setContext context =
    proc through -> do
        setUserState        -<  context
        returnA             -<  through

{- |
Returns the current cursor.
-}
getCursor :: JanusStateArrow a StateIdentifier
getCursor =
    getContext >>> (arr c_cursor)

{- |
Replaces the current cursor and returns the input value.
-}
setCursor :: StateIdentifier -> JanusStateArrow a a
setCursor cursor =
    proc through -> do
        context <-  getContext          -<  ()
        let context' = context { c_cursor = cursor }
        setContext context'             -<< through

{- |
TODO
-}
runInContext :: Context -> JanusStateArrow a b -> JanusStateArrow a b
runInContext ctx arrow =
    proc in_val -> do
        prev_ctx    <- getContext               -<  ()
        setContext ctx                          -<  ()
        out_val     <- ( arrow
		         >>>
			 setContext prev_ctx
                       )
                       `orElse`
                       ( setContext prev_ctx
                         >>> zeroArrow
                       )                        -<< in_val
        returnA                                 -<  out_val

-- -- Identifier Operations

{- |
Builds a StateIdentifier value from an XPath expression string.
-}
toId :: String -> StateIdentifier -> Maybe StateIdentifier
toId str cursor =
    case (runParser parseXPath [] "" str) of
        Left _      -> Nothing
        Right (PathExpr _ (Just (LocPath Abs path)))    -> Just (path)
        Right (PathExpr _ (Just (LocPath Rel path)))    -> Just (cursor ++ path)
        _           -> Nothing

{- |
TODO
-}
toScopeId :: StateIdentifier -> Maybe (String, StateIdentifier)
toScopeId ident =
    if (length ident > 0)
        then Just (nameOf $ head ident, tail ident)
        else Nothing
    where
        nameOf (Step Child (NameTest name) [])  = qualifiedName name
        nameOf _                                = "."

{- |
TODO
-}
identArrow :: JanusStateArrow String (String, StateIdentifier)
identArrow =
    proc st_id -> do
        cursor      <- getCursor            -<  ()
        (maybeA $ (arr $ toId st_id))
            >>>
            (maybeA $ (arr toScopeId))      -<< cursor

{- |
Returns the father StateIdentifier to a given StateIdentifier.
-}
father :: StateIdentifier -> StateIdentifier
father =
    reverse . tail . reverse

{- |
Returns the local part string of a StateIdentifier value.
-}
local :: StateIdentifier -> String
local st_id =
    select_local (reverse $ st_id)
    where
        select_local ((Step Child (NameTest name) []):_)    = qualifiedName name
        select_local _                                      = "."

-- -- Config Operations

{- |
Returns the current configuration XmlTree from the Context.
-}
getConfig :: XmlSource Context a
getConfig =
    proc arb -> do
        context <- getContext                   -<  arb
        let config = c_cfg context
        config                                  -<< ()

{- |
The current configuration XmlTree is replaced by the input of the delivered Arrow. The previous configuration is
returned by the Arrow.
-}

swapConfig :: XmlTransform Context
swapConfig =
    proc new_config -> do
        context     <- getContext               -<  ()
        let config = c_cfg context
        let new_context = context { c_cfg = constA new_config }
        setContext new_context                  -<< ()
        config                                  -<< ()





-- -- Repository Operations

{- |
Returns a repository from the Context by means of a selector function.
-}
getRepository :: (Context -> Repository a) -> JanusStateArrow b (Repository a)
getRepository selector =
    proc _ -> do
        context     <- getContext               -<  ()
        let rep = selector context
        returnA                                 -<  rep

{- |
Replaces a repository by the input of the delivered Arrow. The previous repository is returned.
-}
swapRepository :: (Context -> Repository a) -> ((Context, Repository a) -> Context) -> JanusStateArrow (Repository a) (Repository a)
swapRepository selector swapper =
    proc new_rep -> do
        context     <- getContext               -<  ()
        let old_rep = selector context
        let new_context = swapper (context, new_rep)
        setContext new_context                  -<< ()
        returnA                                 -<  old_rep

{- |
Removes all values from a Context repository by inserting a new empty repository.
-}
delRepository :: String -> JanusStateArrow (Repository a) (Repository a) -> JanusStateArrow b b
delRepository rep_type swapper =
    proc through -> do
        (newRepositoryA rep_type
         >>>
         swapper)                               -<  ()
        returnA                                 -<  through

{- |
TODO
-}
getRepositoryValue :: String -> JanusStateArrow a (Repository b) -> b -> JanusStateArrow a b
getRepositoryValue name getter def =
        getter
        >>>
        getComponentDef name def

{- |
TODO
-}
addRepositoryValue :: JanusStateArrow a (Repository b) -> JanusStateArrow (Repository b) (Repository b) -> String -> b -> JanusStateArrow c c
addRepositoryValue getter swapper name value =
    proc through -> do
        rep         <- getter                   -<  undefined
        new_rep     <- addComponent name value  -<  rep
        swapper                                 -<  new_rep
        returnA                                 -<  through

{- |
Returns the ShaderCreator repository from the Context.
-}
getShaderCreators :: JanusStateArrow a (Repository ShaderCreator)
getShaderCreators = getRepository c_sRep

{- |
Replaces the ShaderCreator repository by the input of the delivered Arrow. The previous repository is returned.
-}
swapShaderCreators :: JanusStateArrow (Repository ShaderCreator) (Repository ShaderCreator)
swapShaderCreators = swapRepository c_sRep (\(context, new_rep) -> context { c_sRep = new_rep })

{- |
Removes all ShaderCreators from the Context's according repository (by inserting a new empty repository).
-}
delShaderCreators :: JanusStateArrow a a
delShaderCreators = delRepository "Shader.ShaderCreator" swapShaderCreators

{- |
Returns a particular ShaderCreator from the Context's repository by its reference identifier.
-}
getShaderCreator :: String -> JanusStateArrow a ShaderCreator
getShaderCreator name = getRepositoryValue name getShaderCreators (mkStaticCreator $ idShader)

{- |
Adds a ShaderCreator to the Context's repository by a reference identifier.
-}
addShaderCreator :: String -> ShaderCreator -> JanusStateArrow a a
addShaderCreator name shader = addRepositoryValue getShaderCreators swapShaderCreators name shader

{- |
Returns the HandlerCreator repository from the Context.
-}
getHandlerCreators :: JanusStateArrow a (Repository HandlerCreator)
getHandlerCreators = getRepository c_hRep


{- |
Replaces the HandlerCreator repository by the input of the delivered Arrow. The previous repository is returned.
-}
swapHandlerCreators :: JanusStateArrow (Repository HandlerCreator) (Repository HandlerCreator)
swapHandlerCreators = swapRepository c_hRep (\(context, new_rep) -> context { c_hRep = new_rep })

{- |
Removes all ShaderCreators from the Context's according repository (by inserting a new empty repository).
-}
delHandlerCreators :: JanusStateArrow a a
delHandlerCreators = delRepository "Shader.HandlerCreator" swapHandlerCreators


{- |
Returns a particular ShaderCreator from the Context's repository by its reference identifier.
-}
getHandlerCreator :: String -> JanusStateArrow a HandlerCreator
getHandlerCreator name = getRepositoryValue name getHandlerCreators (constA nullHandler)

{- |
Adds a HandlerCreator to the Context's repository by a reference identifier.
-}
addHandlerCreator :: String -> HandlerCreator -> JanusStateArrow a a
addHandlerCreator name handler = addRepositoryValue getHandlerCreators swapHandlerCreators name handler


-- -- Shadow Operations

{- |
Returns the current Context shadow.
-}
getShadow :: JanusStateArrow a Shader
getShadow =
    proc _ -> do
        context     <- getContext               -<  ()
        let shadow = c_shadow context
        returnA                                 -<  shadow

{- |
Replaces the current Context shadow by a new Shader. The resulting Arrow returns the previous shadow.
-}
swapShadow :: Shader -> JanusStateArrow a Shader
swapShadow shadow =
    proc _ -> do
        context     <- getContext               -<  ()
        let old_shadow = c_shadow context
        let new_context = context { c_shadow = shadow }
        setContext new_context                  -<< ()
        returnA                                 -<  old_shadow

{- |
Extends the current Context shadow by a new Shader. The new Shader is added behind the current shadow.
-}
extendShadow :: Shader -> JanusStateArrow a a
extendShadow extension =
    proc through -> do
        context     <- getContext               -<  ()
        let old_shadow = c_shadow context
        let new_shadow = old_shadow >>> extension
        let new_context = context { c_shadow = new_shadow }
        setContext new_context                  -<< ()
        returnA                                 -<  through


-- -- State Operations

-- -- -- Scope Operations

{- |
Creates a new scope with the argument as its name and returns it.
-}
newScope :: String -> JanusStateArrow a SharedScope
newScope ident =
    proc _ -> do
        component   <- newStateTree ident       -<  ()
        arrIO $ newMVar                         -<  component

{- |
Delivers a scope from the Context, denoted by its name.
-}
getScope :: String -> JanusStateArrow a SharedScope
getScope ident =
    proc _ -> do
        let ident' = Prelude.map toLower ident
        context     <- getContext               -<  ()
        let scopes = c_scopes context
        if member ident' scopes
            then returnA                        -<  scopes ! ident'
            else zeroArrow                      -<  ()

{- |
Lists the names of all scopes present in the Context. This Arrow is a non-deterministic one.
-}
listScopes :: JanusStateArrow a String
listScopes =
    proc _ -> do
        context <- getContext                   -<  ()
        let scopes = c_scopes context
        constL (keys scopes)                    -<< ()

{- |
Creates a new scope in the Context, referenced by a name.
-}
addScope :: String -> JanusStateArrow a a
addScope ident =
    proc through -> do
        new_scope   <- newScope ident           -< ()
        let ident' = Prelude.map toLower ident
        context     <- getContext               -< ()
        let scopes = c_scopes context
        if member ident' scopes
            then (zeroArrow)                    -< ()
            else (proc s_map -> do
                let new_context = context { c_scopes = s_map }
                setContext new_context          -<< ()
                returnA -< through)             -<< insert ident' new_scope scopes

{- |
Replaces an existing scope in the Context by a new one.
-}
swapScope :: String -> JanusStateArrow a a
swapScope ident =
    proc through -> do
        new_scope   <- newScope ident             -< ()
        let ident' = Prelude.map toLower ident
        context     <- getContext                 -< ()
        let scopes = c_scopes context
        if member ident' scopes
            then (proc s_map -> do
                -- let old_scope = scopes ! ident'
                let new_context = context { c_scopes = s_map }
                setContext new_context  -<< ()
                returnA -< through)         -<< insert ident' new_scope scopes
            else (zeroArrow)                -< ()

{- |
Removes a scope from the Context
-}
delScope :: String -> JanusStateArrow a a
delScope ident =
    proc through -> do
        let ident' = Prelude.map toLower ident
        context <- getContext -< ()
        let scopes = c_scopes context
        if member ident' scopes
            then (proc s_map -> do
                -- let old_scope = scopes ! ident'
                let new_context = context { c_scopes = s_map }
                setContext new_context  -<< ()
                returnA -< through)         -<< delete ident' scopes
            else (zeroArrow)                -< ()





-- -- -- State Cell/Value Operations
{- |
Returns an empty StateCell (with timestamp 0).
-}
newNullCell :: JanusStateArrow a StateCell
newNullCell =
    constA NullVal >>> newCell >>> setCellTS 0

{- |
Returns a new StateCell based on the string input of the Arrow.
-}
newCellS :: JanusStateArrow String StateCell
newCellS =
    arr (\str -> SimpleVal $ str) >>> newCell

{- |
Returns a new StateCell based on the polymorphic input of the Arrow.
-}
newCellP :: Typeable a => JanusStateArrow a StateCell
newCellP =
    arr (\val -> PolyVal $ toDyn val) >>> newCell

{- |
Returns a new StateCell based on a StateValue content.
-}
newCell :: JanusStateArrow StateValue StateCell
newCell =
    proc val -> do
        ts  <-  getCurrentTS                    -< ()
        returnA                                 -< StateCell { c_val = val, c_ts = ts }

{- |
Returns the StateValue from a StateCell.
-}
getCellVal :: JanusStateArrow StateCell StateValue
getCellVal =
    arr c_val

{- |
Returns the string representation from a StateCell's StateValue.
-}
getCellVS :: JanusStateArrow StateCell String
getCellVS =
    proc cell -> do
        let val = c_val cell
        case val of
            SimpleVal str   -> returnA          -<  str
            _               -> zeroArrow        -<  ()

{- |
Returns a polymorphic representation from a StateCell's StateValue.
-}
getCellVP :: Typeable a => JanusStateArrow StateCell a
getCellVP =
    proc cell -> do
        let val     = c_val cell
        let val'    = case val of
                        PolyVal dyn     -> fromDynamic dyn
                        _               -> Nothing
        (if isJust val'
            then constA $ fromJust val'
            else zeroArrow
            )                                   -<< ()

{- |
Returns a StateCell's timestamp.
-}
getCellTS :: JanusStateArrow StateCell JanusTimestamp
getCellTS =
    arr c_ts

{- |
Replaces a StateCell's StateValue.
-}
setCellVal :: StateValue -> JanusStateArrow StateCell StateCell
setCellVal val =
    getCurrentTS
    >>>
    (arr $ \ts -> StateCell { c_val = val, c_ts = ts })

{- |
Replaces a StateCell's StateValue string representation.
-}
setCellVS :: String -> JanusStateArrow StateCell StateCell
setCellVS strval =
    getCurrentTS
    >>>
    (arr $ \ts -> StateCell { c_val = SimpleVal strval, c_ts = ts } )

{- |
Replaces a StateCell's StateValue polymorphic representation.
-}
setCellVP :: Typeable a => a -> JanusStateArrow StateCell StateCell
setCellVP val =
    getCurrentTS
    >>>
    (arr $ \ts -> StateCell { c_val = PolyVal (toDyn val), c_ts = ts })

{- |
Sets the timestamp of a StateCell.
-}
setCellTS :: JanusTimestamp -> JanusStateArrow StateCell StateCell
setCellTS ts =
    (arr $ \cell -> cell { c_ts = ts })





-- -- -- State Access Operations
{- |
Delivers a StateCell from the Context. The argument denotes the node in question (by an XPath expression), including the
scope as its first branch.
-}
getSC :: String -> JanusStateArrow a StateCell
getSC st_id =
    getStateCell st_id

{- |
Delivers a StateValue from the Context. The argument denotes the node in question (by an XPath expression), including the
scope as its first branch.
-}
getSV :: String -> JanusStateArrow a StateValue
getSV st_id =
    getStateCell st_id >>> getCellVal

{- |
Delivers the polymorphic representation of a StateValue from the Context. The argument denotes the node in question (by an
XPath expression), including the scope as its first branch.
-}
getSVP :: Typeable b => String -> JanusStateArrow a b
getSVP st_id =
    getStateCell st_id >>> getCellVP

{- |
Delivers the string representation of a StateValue from the Context. The argument denotes the node in question (by an XPath
expression), including the scope as its first branch.
-}
getSVS :: String -> JanusStateArrow a String
getSVS st_id =
    getStateCell st_id >>> getCellVS

{- |
Delivers a StateCell from the Context. The argument denotes the node in question (by an XPath expression), including the
scope as its first branch.
-}
getStateCell :: String -> JanusStateArrow a StateCell
getStateCell st_id =
    proc _ -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> getStateCell' st_id')                   -<< ()

{- |
Equivalent to getStateCell, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
getStateCell' :: StateIdentifier -> JanusStateArrow SharedTree StateCell
getStateCell' st_id =
    mutateTree st_id zeroArrow (queryCell this) zeroArrow

{- |
Delivers the structural timestamp of a node from the Context. The argument denotes the node in question (by an XPath expression),
including the scope as its first branch.
-}
getStateTreeTS :: String -> JanusStateArrow a JanusTimestamp
getStateTreeTS st_id =
    proc _ -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> getStateTreeTS' st_id')                 -<< ()

{- |
Equivalent to getStateCellTS, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
getStateTreeTS' :: StateIdentifier -> JanusStateArrow SharedTree JanusTimestamp
getStateTreeTS' st_id =
    mutateTree st_id zeroArrow (arr $ \tree -> (tree, [t_ts tree])) zeroArrow

{- |
Returns the names of all child nodes of a node from the Context. The argument denotes the node in question (by an XPath expression),
including the scope as its first branch.
-}
listStateTrees :: String -> JanusStateArrow a String
listStateTrees st_id =
    proc _ -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> listStateTrees' st_id')                 -<< ()

{- |
Equivalent to listStateTrees, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
listStateTrees' :: StateIdentifier -> JanusStateArrow SharedTree String
listStateTrees' st_id =
    mutateTree st_id
        (zeroArrow)
        (proc tree -> do
            children <- listA $ listChildren -< tree
            returnA -< (tree, children)
            )
        (zeroArrow)

{- |
Pairwise returns the names and according string values of all child nodes of a node from the Context. The argument denotes the node in
question (by an XPath expression), including the scope as its first branch.
-}
listStatePairsStr :: String -> JanusStateArrow a (String, String)
listStatePairsStr st_id =
    listStatePairs st_id
    >>>
    second getCellVS

{- |
Pairwise returns the names and according StateCell values of all child nodes of a node from the Context. The argument denotes the node in
question (by an XPath expression), including the scope as its first branch.
-}
listStatePairs :: String -> JanusStateArrow a (String, StateCell)
listStatePairs st_id =
    proc _ -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> listStatePairs' st_id')                 -<< ()

{- |
Equivalent to listStatePairs, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
listStatePairs' :: StateIdentifier -> JanusStateArrow SharedTree (String, StateCell)
listStatePairs' st_id =
    mutateTree st_id
        (zeroArrow)
        (proc tree -> do
            children <- listA $
                    (proc tree' -> do
                        name            <- listChildren                   -<  tree'
                        cell            <- getChild name >>> arr t_cell   -<< tree'
                        returnA                                           -<  (name, cell)
                        ) -<  tree
            returnA -< (tree, children)
            )
        (zeroArrow)

{- |
Safely replaces a StateCell in the Context by a new one. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the old and the new StateCell. The old StateCell is compared with
the actual one to decide if the operation is based on a correct previous version of the StateCell. The operation fails if the given
old StateCell and the actual one do not match.
-}
(<#!) :: String -> (StateCell, StateCell) -> JanusStateArrow a a
(<#!) st_id (cell, ref_cell) =
    setStateCellSafe st_id cell ref_cell

{- |
Replaces a StateCell in the Context by a new one. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the new StateCell.
-}
(<=!) ::  String -> StateCell -> JanusStateArrow a a
(<=!) st_id cell =
    setStateCell st_id cell

{- |
Replaces a StateValue in the Context by a new one. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the new StateValue.
-}
(<$!) ::  String -> StateValue -> JanusStateArrow a a
(<$!) st_id val =
    proc through -> do
        cell    <- newCell                  -<  val
        setStateCell st_id cell             -<< through

{- |
Replaces a polymorphic representation in the Context by a new one. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the new value.
-}
(<*!) ::  Typeable b => String -> b -> JanusStateArrow a a
(<*!) st_id val =
    proc through -> do
        cell    <- newCellP                             -<  val
        setStateCell st_id cell                         -<< through

{- |
Replaces a string representation in the Context by a new one. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the new string.
-}
(<-!) ::  String -> String -> JanusStateArrow a a
(<-!) st_id str =
    proc through -> do
        cell    <- newCellS                             -<  str
        setStateCell st_id cell                         -<< through

{- |
Replaces a StateCell in the Context by a new one. The first argument denotes the node in question (by an XPath expression), including the
scope as its first branch. The second argument denotes the new StateCell.
-}
setStateCell :: String -> StateCell -> JanusStateArrow a a
setStateCell st_id st_cell =
    proc through -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> setStateCell' st_id' st_cell st_cell)   -<< ()
        returnA                                                     -<  through

{- |
Safely replaces a StateCell in the Context by a new one. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the old and the new StateCell. The old StateCell is compared with
the actual one to decide if the operation is based on a correct previous version of the StateCell. The operation fails if the given
old StateCell and the actual one do not match.
-}
setStateCellSafe :: String -> StateCell -> StateCell -> JanusStateArrow a a
setStateCellSafe st_id st_cell ref_cell =
    proc through -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> setStateCell' st_id' st_cell ref_cell)  -<< ()
        returnA                                                     -<  through

{- |
Equivalent to setStateCellSafe, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
setStateCell' :: StateIdentifier -> StateCell -> StateCell -> JanusStateArrow SharedTree SharedTree
setStateCell' st_id st_cell ref_cell =
    proc through -> do
        mutateTree st_id
            (proc st_id' -> do
                newStateTree (local st_id')             -<< ()
                )
            (mutateCell $   proc cell -> do
                    if (c_ts ref_cell) >= (c_ts cell)
                        then returnA                    -<  (st_cell, [through])
                        else zeroArrow                  -<  ()
                )
            (constA $ Update st_id st_cell)             -<< through

{- |
Removes a node including all subtrees from the Context. The argument denotes the node in question (by an XPath expression), including the
scope as its first branch.
-}
delStateTree :: String -> JanusStateArrow a a
delStateTree st_id =
    proc through -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> delStateTree' st_id')                   -<< ()
        returnA                                                     -<  through

{- |
Equivalent to delStateTree, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
delStateTree' :: StateIdentifier -> JanusStateArrow SharedTree SharedTree
delStateTree' st_id =
    proc through -> do
        mutateTree (father st_id)
            (zeroArrow)
            (proc tree -> do
                tree' <- delChild (local st_id)             -< tree
                returnA                                     -< (tree', [through])
                )
            (constA $ Delete st_id)                                 -<< through

{- |
Replaces a node value in the Context by a string representation. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the new string value.
-}
swapStateStr :: String -> String -> JanusStateArrow a String
swapStateStr st_id str =
    proc _ -> do
        cell        <- newCellS                                         -<  str
        cell'       <- swapStateCell st_id cell                         -<< ()
        getCellVS                                                       -<  cell'

{- |
Replaces a node value in the Context by a new StateCell. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the new StateCell value.
-}
swapStateCell :: String -> StateCell -> JanusStateArrow a StateCell
swapStateCell st_id st_cell =
    proc _ -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> swapStateCell' st_id' st_cell st_cell)  -<< ()

{- |
Safely replaces a node value in the Context by a new StateCell. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the new StateCell value and the third argument
denotes the old StateCell. The old StateCell is compared with the actual one to decide if the operation is based on a correct previous
version of the StateCell. The operation fails if the given old StateCell and the actual one do not match.
-}
swapStateCellSafe :: String -> StateCell -> StateCell -> JanusStateArrow a StateCell
swapStateCellSafe st_id st_cell ref_cell =
    proc _ -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> swapStateCell' st_id' st_cell ref_cell) -<< ()

{- |
Equivalent to swapStateCell, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
swapStateCell' :: StateIdentifier -> StateCell -> StateCell -> JanusStateArrow SharedTree StateCell
swapStateCell' st_id st_cell ref_cell =
    proc through -> do
        mutateTree st_id
            (proc st_id' -> do
                newStateTree (local st_id')     -<< ()
                )
            (mutateCell $ proc cell -> do
                    if (c_ts ref_cell) >= (c_ts cell)
                        then returnA            -<  (st_cell, [cell])
                        else zeroArrow          -<  ()
                )
            (constA $ Update st_id st_cell)     -<< through

{- |
Applies a StateCell transformation Arrow to a node in the Context. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the transformation Arrow.
-}
(<=!>) :: String -> JanusStateArrow StateCell StateCell -> JanusStateArrow a StateCell
(<=!>) st_id cell_mapper =
    mapStateCell st_id cell_mapper

{- |
Applies a StateValue transformation Arrow to a node in the Context. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the transformation Arrow.
-}
(<$!>) ::  String -> JanusStateArrow StateValue StateValue -> JanusStateArrow a StateValue
(<$!>) st_id val_mapper =
    let cell_mapper = proc cell -> do
                        ts  <- getCurrentTS                  -<  ()
                        val <- getCellVal >>> val_mapper     -<  cell
                        cell'   <- setCellTS ts              -<< cell
                        setCellVal val                       -<< cell'
                      in
                        mapStateCell st_id cell_mapper >>> getCellVal

{- |
Applies a polymorphic transformation Arrow to a node in the Context. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the transformation Arrow.
-}
(<*!>) :: Typeable b => String -> JanusStateArrow b b -> JanusStateArrow a b
(<*!>) st_id val_mapper =
    let cell_mapper = proc cell -> do
                        ts  <- getCurrentTS                 -<  ()
                        val <- getCellVP >>> val_mapper     -<  cell
                        cell'   <- setCellTS ts             -<< cell
                        setCellVP val                       -<< cell'
                      in
                        mapStateCell st_id cell_mapper >>> getCellVP

{- |
Applies a string transformation Arrow to a node in the Context. The first argument denotes the node in question (by an XPath expression),
including the scope as its first branch. The second argument denotes the transformation Arrow.
-}
(<-!>) ::  String -> JanusStateArrow String String -> JanusStateArrow a String
(<-!>) st_id str_mapper =
    let cell_mapper = proc cell -> do
                        ts  <- getCurrentTS                 -<  ()
                        str <- getCellVS >>> str_mapper     -<  cell
                        cell'   <- setCellTS ts             -<< cell
                        setCellVS str                       -<< cell'
                      in
                        mapStateCell st_id cell_mapper >>> getCellVS

{- |
Equivalent to <=!>: Applies a StateCell transformation Arrow to a node in the Context. The first argument denotes the node in question
(by an XPath expression), including the scope as its first branch. The second argument denotes the transformation Arrow.
-}
mapStateCell :: String -> JanusStateArrow StateCell StateCell -> JanusStateArrow a StateCell
mapStateCell st_id op =
    proc _ -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> mapStateCell' st_id' op)                -<< ()

{- |
Equivalent to mapStateCell, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
mapStateCell' :: StateIdentifier -> JanusStateArrow StateCell StateCell -> JanusStateArrow SharedTree StateCell
mapStateCell' st_id operation =
    proc through -> do
        mutateTree st_id
            (proc st_id' -> do
                newStateTree (local st_id')     -<< ()
                )
            (mutateCell $ proc cell -> do
                cell'   <- operation            -<  cell
                returnA -< (cell', [cell'])
                )
            (constA $ Unspecified st_id)        -<< through

{- |
Blocks over a node in the Context until a StateOperation occurs. The argument denotes the node in question (by an XPath expression),
including the scope as its first branch.
-}
listenStateTree :: String -> JanusStateArrow a a
listenStateTree st_id =
    proc through -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> listenStateTree' st_id')                -<< ()
        returnA                                                     -<  through

{- |
Equivalent to listenStateTree, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation. Furthermore this function returns the StateOperation which occured.
-}
listenStateTree' :: StateIdentifier -> JanusStateArrow SharedTree StateOperation
listenStateTree' st_id =
    proc mvar_tree -> do
        listener <-
            mutateTree st_id
                (zeroArrow)
                (proc tree -> do
                    let listeners       = t_listeners tree
                    new_listener <- arrIO0 $ newEmptyMVar -< ()
                    let tree' = tree { t_listeners = (new_listener:listeners) }
                    returnA -< (tree', [new_listener])
                    )
                (zeroArrow)                                         -<< mvar_tree
        arrIO $ takeMVar                                            -< listener

{- |
Wakes up all listeners blocking over a node in the Context with a given StateOperation. The first argument denotes the node in
question (by an XPath expression), including the scope as its first branch. The second argument denotes the StateOperation to send.
-}
invokeStateTree :: String -> StateOperation -> JanusStateArrow a a
invokeStateTree st_id op =
    proc through -> do
        (scope, st_id')     <- identArrow                           -<  st_id
        (getScope scope >>> invokeStateTree' st_id' op)             -<< ()
        returnA                                                     -<  through

{- |
Equivalent to invokeStateTree, but operating on a SharedTree value as its input and using a StateIdentifier instead of an XPath
string representation.
-}
invokeStateTree' :: StateIdentifier -> StateOperation -> JanusStateArrow SharedTree SharedTree
invokeStateTree' st_id op =
    proc through -> do
        mutateTree st_id
            (zeroArrow)
            (proc tree -> do
                let listeners       = t_listeners tree
                let tree' = tree { t_listeners = [] }
                arrIO $ invokeLs listeners -<< op
                returnA -< (tree', [through])
                )
            (zeroArrow)                                             -<< through
    where
        invokeLs (x:xs) op'  = putMVar x op' >> invokeLs xs op'
        invokeLs []     _   = return ()

{- |
Returns a new unique identifier from the \"general\" UID space. If no UID has been issued so far, the default value supplied is returned and
stored in the UID state.
-}
getUID :: UID -> JanusStateArrow a UID
getUID def =
    getQualifiedUID "general" def

{- |
Returns a new unique identifier from a given UID space. There exists a \"uid\" scope in the Context, in which there are nodes to represent
distinct UID spaces. If no UID has been issued so far from the space in question, the default value supplied is returned and
stored in the UID state.
-}
getQualifiedUID :: String -> UID -> JanusStateArrow a UID
getQualifiedUID uid_scope def =
    proc _ -> do
        (UIDVal uid) <- ("/uid/" ++ Prelude.map toLower uid_scope)
                        <$!> (arr $ (\val -> case val of
                                                    UIDVal prev_uid -> (UIDVal $ prev_uid + 1)
                                                    _               -> (UIDVal $ def)
                                                )
                                        ) -< ()
        returnA -< uid





-- internal state operations
{-
Lifts an Arrow transforming a StateValue and delivering a polymorphic result list to an Arrow operating on StateTrees.
mutateVal :: JanusStateArrow StateValue (StateValue, [a])
             -> JanusStateArrow StateTree (StateTree, [a])
mutateVal mutate =
    mutateCell
        (proc cell -> do
            let val = c_val cell
            ts  <- getCurrentTS                          -< ()
            (val', result)  <- mutate                    -< val
            let cell' = cell { c_val = val', c_ts = ts }
            returnA                                      -< (cell', result)
            )
-}

{- |
Lifts an Arrow transforming a StateCell and delivering a polymorphic result list to an Arrow operating on StateTrees.
-}
mutateCell :: JanusStateArrow StateCell (StateCell, [a])
             -> JanusStateArrow StateTree (StateTree, [a])
mutateCell mutate =
    proc tree -> do
        let cell    = t_cell tree
        (cell', result) <- mutate                         -< cell
        let ts = c_ts cell'
        let tree' = tree { t_cell = cell', t_ts = ts }
        returnA                                           -< (tree', result)

{-
Lifts an Arrow accessing a StateValue and delivering a polymorphic result to an Arrow operating on StateTrees.
queryVal :: JanusStateArrow StateValue a
             -> JanusStateArrow StateTree (StateTree, [a])
queryVal query =
        queryCell
            (proc cell -> do
                let val = c_val cell
                result  <- query     -< val
                returnA              -< result
                )
-}

{- |
Lifts an Arrow accessing a StateCell and delivering a polymorphic result to an Arrow operating on StateTrees.
-}
queryCell :: JanusStateArrow StateCell a
             -> JanusStateArrow StateTree (StateTree, [a])
queryCell query =
    proc tree -> do
        let cell    = t_cell tree
        result  <- listA $ query     -< cell
        returnA                      -< (tree, result)

{- |
Defines an Arrow transforming a SharedTree and delivering some polymorphic value. The Arrow is configured based on
a StateIdentifier, an Arrow to react to a missing intermediate node misses, an Arrow to react to a found intermediate node and
an Arrow denoting a StateOperation independent of the input.
-}
mutateTree :: StateIdentifier
             -> JanusStateArrow StateIdentifier StateTree
             -> JanusStateArrow StateTree (StateTree, [b])
             -> JanusStateArrow a StateOperation
             -> JanusStateArrow SharedTree b
mutateTree st_id@((Step Child (NameTest next) []):xs) miss_op found_op invoke =
    proc mvar_tree -> do
        let next' = qualifiedName next
        tree        <- arrIO $ takeMVar         -< mvar_tree
        let children = t_children tree
        (next_mvar, tree')
                    <- (if member next' children
                            then
                                constA $ (children ! next', tree)
                            else
                                proc tree' -> do
                                    new_tree     <- miss_op                      -<< st_id
                                    tree''       <- insertChild next' new_tree   -<< tree'
                                    let children' = t_children tree''
                                    returnA -<  (children' ! next', tree'')
                            ) `orElse` ((arrIO $ putMVar mvar_tree) >>> zeroArrow) -<< tree
        result  <- listA $ mutateTree xs miss_op found_op invoke    -<< next_mvar
        (if Prelude.null result
            then (arrIO0 $ putMVar mvar_tree tree) >>> zeroArrow
            else this
            )                                               -<< ()
        tree''  <- ifA (constA undefined >>> invoke)
                        (proc tree'' -> do
                            op  <- invoke       -<  undefined
                            invokeListeners op  -<< tree''
                            )
                        (this) -<  tree'
        arrIO $ putMVar mvar_tree                           -<< tree''
        constL result                                       -<< ()
mutateTree [] _ found_op invoke =
    proc mvar_tree -> do
        tree        <- arrIO $ takeMVar                         -<  mvar_tree
        (tree', result) <- found_op
                        `orElse`
                        ((arrIO $ putMVar mvar_tree) >>> zeroArrow) -<<  tree
        tree''  <- ifA (constA undefined >>> invoke)
                        (proc tree'' -> do
                            op  <- invoke       -<  undefined
                            invokeListeners op  -<< tree''
                            )
                        (this)   -<  tree'
        arrIO $ putMVar mvar_tree           -<< tree''
        constL result                       -<< ()
mutateTree _ _ _ _ =
    zeroArrow

{- |
Creates a new empty StateTree value of a given name.
-}
newStateTree :: String -> JanusStateArrow a StateTree
newStateTree name =
    proc _ -> do
        ts  <- getCurrentTS                 -< ()
        cell    <- newNullCell                      -< ()
        let result = StateTree {
                          t_id          = name
                        , t_cell        = cell
                        , t_ts          = ts
                        , t_children    = empty
                        , t_listeners   = []
                     }
        returnA                                         -< result

{- |
Returns a particular child denoted by its name from a given StateTree node.
-}
getChild :: String -> JanusArrow s StateTree StateTree
getChild name =
    proc tree -> do
        let children = t_children tree
        (if member name children
            then
                proc _ -> do
                    let child_mvar = children ! name
                    arrIO $ readMVar        -< child_mvar
            else
                zeroArrow
            ) -<< tree

{- |
Lists all children names of a given StateTree node.
-}
listChildren :: JanusArrow s StateTree String
listChildren =
    proc tree -> do
        let children    = t_children tree
        let ch_names    = keys children
        constL ch_names     -<< ()

{- |
Removes a child denoted by its name from a given StateTree node.
-}
delChild :: String -> JanusArrow s StateTree StateTree
delChild name =
    proc tree -> do
        ts  <- getCurrentTS                 -< ()
        let children    = t_children tree
        let result      = tree { t_children = delete name children, t_ts = ts }
        returnA -< result

{- |
Inserts a StateTree value at a given name into a given StateTree node.
-}
insertChild :: String -> StateTree -> JanusArrow s StateTree StateTree
insertChild name new_child =
    proc tree -> do
        ts  <- getCurrentTS                 -< ()
        let children    = t_children tree
        mvar_tree       <- arrIO0 $ newMVar new_child -< ()
        let result      = tree { t_children = insert name mvar_tree children, t_ts = ts }
        returnA -< result

{-
Applies a StateTree transformation Arrow to all children of a given StateTree node.
mapChildren :: JanusArrow s StateTree StateTree -> JanusArrow s StateTree StateTree
mapChildren operation =
    proc tree -> do
        ts  <- getCurrentTS                 -< ()
        let children        = t_children tree
        new_children    <-
            foldl (\arrow (key, child) ->
                        proc _ -> do
                            child'      <- arrIO $ readMVar     -< child
                            new_child   <- operation >>> arr Just
                                                    `orElse`
                                                    constA Nothing                          -<  child'
                            (if isJust new_child
                                then
                                    proc _ -> do
                                        new_child'  <- arrIO $ newMVar -< fromJust new_child
                                        arrow >>> arr (\map -> insert key new_child' map) -<< ()
                                else arrow
                                ) -<< ()
                        )
                        (constA empty)
                        (toList children)   -<< ()
        let result      = tree { t_children = new_children, t_ts = ts }
        returnA -< result
-}

{-
Applies a StateTree transformation Arrow to all nodes on a given path through the tree.
mapPath :: StateIdentifier -> JanusArrow s StateTree StateTree -> JanusArrow s StateTree StateTree
mapPath ((Step Child (NameTest next) []):xs) operation =
    proc tree -> do
        let next' = qualifiedName next
        tree'           <- operation              -<  tree
        child           <- getChild next'         -<< tree'
        new_child       <- mapPath xs operation   -<  child
        insertChild next' new_child               -<< tree'

mapPath (_:_) _      = zeroArrow
mapPath [] operation =
    operation

-}

{- |
Invokes all listeners of a given StateTree node with a given StateOperation.
-}
invokeListeners :: StateOperation -> JanusArrow s StateTree StateTree
invokeListeners op =
    proc tree -> do
        let listeners   = t_listeners tree
        arrIO $ invoke op -< listeners
        let result      = tree { t_listeners = [] }
        returnA                                     -< result
    where
        invoke op' (x:xs)   = putMVar x op' >> invoke op' xs
        invoke _  []        = return ()







-- -- Messaging Operations
-- -- -- Channel Operations
{- |
Returns a new empty channel.
-}
newChannel :: JanusArrow s a SharedChannel
newChannel =
    proc _ -> do
        let channel = Ch ([], defaultHandler, [])
        arrIO $ newMVar                             -<  channel

{- |
Returns a channel denoted by its name from the Context.
-}
getChannel :: ChannelId -> JanusStateArrow a SharedChannel
getChannel channel =
    proc _ -> do
        context     <- getContext                   -<  ()
        let channels = c_channels context
        if channel `member` channels
            then returnA                            -<  channels ! channel
            else zeroArrow                          -<  ()

{- |
Lists the names of all channels present in the Context.
-}
listChannels :: JanusStateArrow a ChannelId
listChannels =
    proc _ -> do
        context     <- getContext                   -<  ()
        let channels = c_channels context
        constL (keys channels)                      -<< ()

{- |
Adds a new channel denoted by its name to the Context.
-}
addChannel :: ChannelId -> JanusStateArrow a a
addChannel channel =
    proc through -> do
        new_channel <- newChannel                   -<  ()
        context     <- getContext                   -<  ()
        let channels = c_channels context
        if channel `member` channels
            then (zeroArrow)                        -< channels
            else (proc ch_map -> do
                let new_context = context { c_channels = ch_map }
                setContext new_context              -<< ()
                returnA -< through)                 -<< insert channel new_channel channels

{- |
Replaces an existing channel denoted by its name in the Context by a new one.
-}
swapChannel :: ChannelId -> JanusStateArrow a a
swapChannel channel =
    proc through -> do
        new_channel <- newChannel                   -<  ()
        context     <- getContext                   -<  ()
        let channels = c_channels context
        if channel `member` channels
            then (proc ch_map -> do
                -- let old_channel = channels ! id'
                let new_context = context { c_channels = ch_map }
                setContext new_context              -<< ()
                returnA -< through)                 -<< insert channel new_channel channels
            else (zeroArrow)                        -<  ()

{- |
Removes a channel denoted by its name from the Context.
-}
delChannel :: ChannelId -> JanusStateArrow a a
delChannel channel =
    proc through -> do
        context     <- getContext                   -<  ()
        let channels = c_channels context
        if channel `member` channels
            then (proc ch_map -> do
                -- let old_channel = channels ! id'
                let new_context = context { c_channels = ch_map }
                setContext new_context              -<< ()
                returnA -< through)                 -<< delete channel channels
            else (zeroArrow)                        -<  ()

{- |
Blocks over a channel denoted by its name in the Context. The unlocking messages are returned.
-}
listenChannel :: ChannelId -> JanusStateArrow a Message
listenChannel channel =
    proc _ -> do
        mvar_channel    <- getChannel channel           -<  ()
        listener <- arrIO0 $ newEmptyMVar               -<  ()
        Ch (msgs, hdl, lst) <- arrIO $ takeMVar         -<  mvar_channel
        let new_channel = Ch (msgs, hdl, listener:lst)
        arrIO $ putMVar mvar_channel                    -<< new_channel
        msg         <- arrIO $ takeMVar                 -<  listener
        constL msg                                      -<< ()

{- |
Invokes all listeners of a channel denoted by its name in the Context with a list of messages.
-}
invokeChannel :: ChannelId -> [Message] -> JanusStateArrow a a
invokeChannel channel msg =
    proc through -> do
        mvar_channel    <- getChannel channel               -<  ()
        Ch (msgs, hdl, lsts) <- arrIO $ takeMVar            -<  mvar_channel
        arrIO $ invoke                                      -<  lsts
        let new_channel = Ch (msgs, hdl, [])
        arrIO $ putMVar mvar_channel                        -<< new_channel
        returnA                                             -<  through
    where
        invoke (x:xs) = putMVar x msg >> invoke xs
        invoke [] = return ()

{- |
Transforms the Handler of a channel denoted by its name in the Context.
-}
changeHandler :: ChannelId -> (MessageHandler -> MessageHandler) -> JanusStateArrow a a
changeHandler channel transform =
    proc through -> do
        mvar_channel    <- getChannel channel               -<  ()
        Ch (msgs, hdl, lsts) <- arrIO $ takeMVar            -<  mvar_channel
        let hdl' = transform hdl
        let new_channel = Ch (msgs, hdl', lsts)
        arrIO $ putMVar mvar_channel                        -<< new_channel
        returnA                                             -<  through

{- |
Appends a Handler to a channel's existing Handler.
-}
addHandler :: ChannelId -> MessageHandler -> JanusStateArrow a a
addHandler channel handler =
    changeHandler channel (\current -> current >>> handler)

{- |
Replaces a channel's existing Handler by the defaultHandler.
-}
clearHandler :: ChannelId -> JanusStateArrow a a
clearHandler channel =
    changeHandler channel (\_ -> defaultHandler)



-- -- -- Message Operations

{- |
Sends a message denoted by a MessageArrow to a channel denoted by its name.
-}
(<-@) :: ChannelId -> MessageArrow Context -> JanusStateArrow a a
(<-@) channel msg =
    sendMsg channel msg

{- |
Sends a plain text to a channell
-}

sendPlain 		:: ChannelId -> String -> JanusStateArrow a a
sendPlain channel msg	= channel <-@ mkPlainMsg msg

globalMsg		:: String -> JanusStateArrow a a
globalMsg		= sendPlain chGlobal

localMsg		:: String -> JanusStateArrow a a
localMsg		= sendPlain chLocal

controlMsg		:: String -> JanusStateArrow a a
controlMsg		= sendPlain chControl

{- |
Sends a message denoted by a MessageArrow to a channel denoted by its name if the first argument Arrow fails.
However, in this case the whole Arrow still fails.
-}
(<*>) :: JanusStateArrow a b -> (ChannelId, MessageArrow Context) -> JanusStateArrow a b
(<*>) op (channel, msg) =
    op
    `orElse`
    (proc _ -> do
        channel <-@ msg                 -< ()
        zeroArrow                       -< ()
        )

{- |
Sends an error message denoted by a its components (source, code, text, state) to a channel denoted by its name if the
first argument Arrow fails. However, in this case the whole Arrow still fails.
-}
(<!>) :: JanusStateArrow a b -> (ChannelId, MessageSource, MessageCode, MessageValue, [(String, String)]) -> JanusStateArrow a b
(<!>) op (channel, source, code, text, state) =
    op <*> (channel, mkErr source code text state)

{- |
Equivalent to <-\@: Sends a message denoted by a MessageArrow to a channel denoted by its name.
-}
sendMsg :: ChannelId -> XmlConstSource Context -> JanusStateArrow a a
sendMsg channel msg =
    proc through -> do
        mvar_channel <- getChannel channel          -<  ()
        ch@(Ch (_, hdl, _)) <- arrIO $ takeMVar     -<  mvar_channel
        msgs <- listA msg                           -<  ()
        (ch' , _) <- hdl                            -<< (ch, msgs)
        arrIO $ putMVar mvar_channel                -<< ch'
        returnA                                     -<  through

{- |
Returns all messages buffered in a channel denoted by its name.
-}
getMsg :: ChannelId -> JanusStateArrow a Message
getMsg channel =
    proc _ -> do
        mvar_channel <- getChannel channel          -<  ()
        Ch (msgs, _, _) <- arrIO $ readMVar         -<  mvar_channel
        constL msgs                                 -<< ()

{- |
Applies a MessageFilter value to all messages buffered in a channel denoted by its name.
-}
filterMsg :: ChannelId -> MessageFilter Context -> JanusStateArrow a a
filterMsg channel transform =
    proc through -> do
        mvar_channel    <- getChannel channel       -<  ()
        Ch (msgs, hdl, lsts) <- arrIO $ takeMVar    -<  mvar_channel
        msgs' <- listA $ constL msgs >>> transform  -<< ()
        let new_channel = Ch (msgs', hdl, lsts)
        arrIO $ putMVar mvar_channel                -<< new_channel
        returnA                                     -<  through

{- |
Removes all messages buffered in a channel denoted by its name.
-}
clearMsg :: ChannelId -> JanusStateArrow a a
clearMsg channel =
    filterMsg channel zeroArrow



-- -- -- Predefined Message Handlers

{- |
The default Handler is equivalent to the identity Arrow. No side effect is caused, especially the messages are NOT forwarded
to the channel's message buffer, they are simply dropped.
-}
defaultHandler :: MessageHandler
defaultHandler =
    this

{- |
Stores incoming messages in the respective channel's message buffer. The messages are not filtered.
-}
storeHandler :: MessageHandler
storeHandler =
    arr (\(Ch (mstore, hdl, lsts), msgs) -> (Ch (msgs ++ mstore, hdl, lsts), msgs))

{- |
Displays messages on the console. The messages are not filtered.
-}
consoleHandler :: MessageHandler
consoleHandler =
    proc input -> do
        arrIO $ hSetBuffering stdout                    -<  NoBuffering
        logHandler stdout                               -<  input

{- |
Stores messages in file denoted by its filename. The messages are not filtered.
-}
logFileHandler :: String -> MessageHandler
logFileHandler filename =
    proc input -> do
        handle  <- exceptA
                            (openFile filename)
                            (proc exc -> do
                                arrIO $ putStrLn    -< "Error in storeHandler while opening file " ++ filename ++ ": " ++ (show exc)
                                returnA             -< stdout
                                )                       -<   AppendMode
        logHandler handle                               -<< input

{- |
Forwards messages in string representation to a given IO handle. The messages are not filtered.
-}
logHandler :: Handle -> MessageHandler
logHandler handle =
    proc input@(_, msgs) -> do
        writable <- arrIO $ hIsWritable  -<  handle
        open     <- arrIO $ hIsOpen      -<  handle
        (if open && writable
            then listA $ constL msgs >>> showMsg >>> (arrIO $ hPutStr handle)
            else this
            )                                           -<< []
        returnA                                         -<  input

{- |
Filters incoming messages based on a MessageFilter value.
-}
filterHandler :: MessageFilter Context -> MessageHandler
filterHandler flt =
    proc (ch, msgs) -> do
        msgs' <- listA $ constL msgs >>> flt -<< ()
        returnA -< (ch, msgs')

{- |
Invokes all listeners of the respective channel if messages arrive.
-}
invokeHandler :: MessageHandler
invokeHandler =
    proc (Ch (mstore, hdl, lsts), msgs) -> do
        arrIO $ invoke lsts                             -<< msgs
        returnA                                         -<  (Ch (mstore, hdl, []), msgs)
    where
        invoke (x:xs) msgs  = putMVar x msgs >> invoke xs msgs
        invoke [] _         = return ()


-- ------------------------------------------------------------
--
-- Message Buffer Operations

{- |
Stores a message denoted by an Arrow in the Context message buffer. This is used to temporarily store Transaction messages in the Context
before failing the current Shader. The mkCreator ShaderCreator wrapper then removes the buffered messages and adds them to the Transaction.
-}
bufferMsg :: XmlConstSource Context -> JanusStateArrow a a
bufferMsg msg =
    proc through -> do
        context     <- getContext                       -<  ()
        msgs        <- listA $ msg                      -<  ()
        let context' = context { c_msgbuf = msgs ++ (c_msgbuf context) }
        setContext context'                             -<< ()
        returnA                                         -<  through

{- |
Forwards all messages contained in the Context message buffer to a channel denoted by its name.
-}
forwardMsgBuf :: ChannelId -> JanusStateArrow a a
forwardMsgBuf channel =
    proc through -> do
        context     <- getContext                       -<  ()
        let msgs = c_msgbuf context
        clearMsgBuf                                     -<  ()
        listA $ sendMsg channel (constL msgs)           -<< ()
        returnA                                         -<  through

{- |
TODO
-}
forwardError :: ChannelId -> JanusStateArrow a b -> JanusStateArrow a b
forwardError channel op =
    op `orElse` (forwardMsgBuf channel >>> zeroArrow)

{- |
TODO
-}
maskError :: ChannelId -> b -> JanusStateArrow a b -> JanusStateArrow a b
maskError channel def op =
    op `orElse` (forwardMsgBuf channel >>> constA def)

{- |
Returns all messages contained in the Context message buffer.
-}
getMsgBuf :: JanusStateArrow a Message
getMsgBuf =
    proc _ -> do
        context     <- getContext                       -<  ()
        constL $ c_msgbuf context                       -<< ()

{- |
Clears the Context message buffer.
-}
clearMsgBuf :: JanusStateArrow a a
clearMsgBuf =
    proc through -> do
        context     <- getContext                       -<  ()
        let context' = context { c_msgbuf = [] }
        setContext context'                             -<< ()
        returnA                                         -<  through

{- |
Adds a message denoted by a MessageArrow to the Context message buffer if the first argument Arrow fails. In this case, the
whole Arrow still fails.
-}
(<+*>) :: JanusStateArrow a b -> MessageArrow Context -> JanusStateArrow a b
(<+*>) op msg =
    op
    `orElse`
    (proc _ -> do
        bufferMsg msg                                   -< ()
        zeroArrow                                       -< ()
        )

{- |
Adds an error message denoted by its components (source, code, value and state) to the Context message buffer if the first argument
Arrow fails. In this case, the whole Arrow still fails.
-}
(<+!>) :: JanusStateArrow a b -> (MessageSource, MessageCode, MessageValue, [(String, String)]) -> JanusStateArrow a b
(<+!>) op (source, code, text, state) =
    op <+*> (mkErr source code text state)



-- ------------------------------------------------------------

-- Handler

{- |
The null Handler writes a log message to the chGlobal scope on l_debug level and terminates immediately.
-}
nullHandler :: Handler
nullHandler =
   proc _ -> do
      chGlobal <-@ mkSimpleLog "Core.hs:nullHandler" ("NullHandler") l_debug -<< ()
      returnA  -< ()

{- |
TODO
-}
createThread :: String -> String -> JanusStateArrow a b -> JanusStateArrow a a
createThread node name thread =
    proc in_val -> do
        threadid    <- processA thread'                     -<  in_val
        globalMsg $ "started thread '" ++
                        name ++ "'...\n"                    -<  ()
        (node ++ "/" ++ name) <$! (ThreadVal threadid)      -<< ()
        arrIO $ putStrLn -< "created thread"
        returnA                                             -<  in_val
    where
    thread'
	= thread
	  >>> (globalMsg $ "stopped thread '" ++ name ++ "'...\n")
          >>> delStateTree (node ++ "/" ++ name)

-- ------------------------------------------------------------
