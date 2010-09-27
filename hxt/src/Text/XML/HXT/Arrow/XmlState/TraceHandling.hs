-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlState.TraceHandling
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   the trace arrows

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlState.TraceHandling
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowIO

import System.IO                        ( hPutStrLn
                                        , hFlush
                                        , stderr
                                        )

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.Arrow.XmlState.SystemConfig

import Text.XML.HXT.Arrow.Edit          ( addHeadlineToXmlDoc
                                        , treeRepOfXmlDoc
                                        , indentDoc
                                        )

-- ------------------------------------------------------------

-- | set the global trace level

setTraceLevel           :: Int -> IOStateArrow s b b
setTraceLevel l         = configSysVar $ withTrace l

-- | read the global trace level

getTraceLevel           :: IOStateArrow s b Int
getTraceLevel           = getSysVar theTraceLevel

-- | set the global trace command. This command does the trace output

setTraceCmd             :: (Int -> String -> IO ()) -> IOStateArrow s b b
setTraceCmd c           = configSysVar $ setS theTraceCmd c

-- | acces the command for trace output

getTraceCmd             :: IOStateArrow a b (Int -> String -> IO ())
getTraceCmd             = getSysVar theTraceCmd

-- | run an arrow with a given trace level, the old trace level is restored after the arrow execution

withTraceLevel          :: Int -> IOStateArrow s b c -> IOStateArrow s b c
withTraceLevel level f  = localSysEnv $ setTraceLevel level >>> f

-- | apply a trace arrow and issue message to stderr

trace                   :: Int -> IOStateArrow s b String -> IOStateArrow s b b
trace level trc         = perform ( trc
                                    >>>
                                    ( getTraceCmd &&& this )
                                    >>>
                                    arrIO (\ (cmd, msg) -> cmd level msg)
                                  )
                          `when` ( getTraceLevel
                                   >>>
                                   isA (>= level)
                                 )

-- | trace the current value transfered in a sequence of arrows.
--
-- The value is formated by a string conversion function. This is a substitute for
-- the old and less general traceString function

traceValue              :: Int -> (b -> String) -> IOStateArrow s b b
traceValue level trc    = trace level (arr $ (('-' : "- (" ++ show level ++ ") ") ++) . trc)

-- | an old alias for 'traceValue'

traceString             :: Int -> (b -> String) -> IOStateArrow s b b
traceString             = traceValue

-- | issue a string message as trace

traceMsg                :: Int -> String -> IOStateArrow s b b
traceMsg level msg      = traceValue level (const msg)

-- | issue the source representation of a document if trace level >= 3
--
-- for better readability the source is formated with indentDoc

traceSource             :: IOStateArrow s XmlTree XmlTree
traceSource             = trace 3 $
                          xshow $
                          choiceA [ isRoot :-> ( indentDoc
                                                 >>>
                                                 getChildren
                                               )
                                  , isElem :-> ( root [] [this]
                                                 >>> indentDoc
                                                 >>> getChildren
                                                 >>> isElem
                                               )
                                  , this   :-> this
                                  ]

-- | issue the tree representation of a document if trace level >= 4
traceTree               :: IOStateArrow s XmlTree XmlTree
traceTree               = trace 4 $
                          xshow $
                          treeRepOfXmlDoc
                          >>>
                          addHeadlineToXmlDoc
                          >>>
                          getChildren

-- | trace a main computation step
-- issue a message when trace level >= 1, issue document source if level >= 3, issue tree when level is >= 4

traceDoc                :: String -> IOStateArrow s XmlTree XmlTree
traceDoc msg            = traceMsg 1 msg
                          >>>
                          traceSource
                          >>>
                          traceTree

-- ----------------------------------------------------------

traceOutputToStderr     :: Int -> String -> IO ()
traceOutputToStderr _level msg
                        = do
                          hPutStrLn stderr msg
                          hFlush stderr

-- ----------------------------------------------------------

