-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.XmlHelper
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: XmlHelper.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus XML Helper functions

   A set of general helper functions operating on Arrows and HXT's XmlTree.

-}

-- ------------------------------------------------------------

module Network.Server.Janus.XmlHelper
    (
    -- data types
      JanusTimestamp
    , XmlTransform
    , XmlAccess
    , XmlSource
    , XmlConstSource

    -- utilities
    , getTS
    , getCurrentTS

    -- arrow operations
    , parseA
    , parseDefA
    , toDynA
    , fromDynA
    , fromDynDefA
    , finallyA
    , maybeA
    -- , catchA
    -- , catchA_
    , exceptA
    , exceptionA
    , exceptZeroA
    , exceptZeroA_
    , processA
    , liftA
    , liftConstSource
    , fileSource
    , staticSource
    , emptyConfig

    -- arrow evaluation
    , evalXmlList
    , evalXml
    , evalXmlDef
    , evalState

    -- XmlTree operations to support Shaders
    , getVal
    , getValDef
    , getValP
    , listVals
    , listValPairs
    , setVal
    , setValP
    , delVal
    , swapVal

    , getTree
    , insTree
    , insEmptyTree
    , addTree
    , delTree

    -- , get1Line
    )
where

import Control.Concurrent
import Control.Exception

import Data.Dynamic
import Data.Maybe

import System.Time

import Text.XML.HXT.Core
import Text.XML.HXT.XPath

import Network.Server.Janus.JanusPaths

-- ------------------------------------------------------------

-- just for read1Line

-- import Data.Maybe
{-
import System.Console.Readline
    ( readline
    , addHistory
    )
import Text.XML.HXT.DOM.Util(stringTrim)
-}

-- ------------------------------------------------------------

type XmlTransform s     = IOStateArrow s XmlTree XmlTree
type XmlAccess s a      = IOStateArrow s XmlTree a
type XmlSource s a      = IOStateArrow s a XmlTree
type XmlConstSource s   = XmlSource s ()

type JanusTimestamp     = Integer

-- ------------------------------------------------------------


{- |
Transforms a ClockTime value into a timestamp value (represented by an arbitrary precision integer).
-}
getTS :: ClockTime -> JanusTimestamp
getTS (TOD sec pico) =
    (sec * 1000)
    +
    (pico `div` 1000000000)

{- |
Returns the current time in timestamp representation.
-}
getCurrentTS :: IOStateArrow s a JanusTimestamp
getCurrentTS =
    (arrIO0 $ getClockTime)
    >>>
    (arr getTS)

{- |
Delivers an Arrow parsing its input string to a polymorphically bound target type (which has to be installed
in the Read type class). Fails if the parser function throws an exception.
-}
parseA :: Read b => IOStateArrow s String b
parseA =
    exceptA readIO zeroArrow

{- |
Delivers an Arrow parsing its input string to a polymorphically bound target type (which has to be installed
in the Read type class). Returns a default value if the parser function throws an exception.
-}
parseDefA :: Read a => a -> IOStateArrow s String a
parseDefA def =
    exceptA readIO (constA def)

{- |
TODO
-}
toDynA :: Typeable a => IOStateArrow s a Dynamic
toDynA =
    arr $ toDyn

{- |
TODO
-}
fromDynDefA :: Typeable a => a -> IOStateArrow s Dynamic a
fromDynDefA def =
    arr $ (\dyn -> fromDyn dyn def)

{- |
TODO
-}
fromDynA :: Typeable a => IOStateArrow s Dynamic a
fromDynA =
    maybeA (arr $ fromDynamic)

{- |
Defines an Arrow always evaluating the second argument, especially even if the first argument fails.
-}
finallyA :: ArrowIf a => a b c -> a b c -> a b c
finallyA op always =
    (proc x -> do
        op -< x
        always -< x
        )
    `orElse`
    always

{- |
Transforms an Arrow delivering a Maybe value into an Arrow delivering the unwrapped Just value and failing in the case
of the a Nothing value.
-}
maybeA :: (ArrowZero a, ArrowChoice a) => a b (Maybe c) -> a b c
maybeA op =
    op
    >>>
    proc val -> do
        (if isJust val
            then returnA    -<  fromJust val
            else zeroArrow  -<  ()
            )
{-
{- |
Transforms an IO Monad constructor into an Arrow. An exception thrown by the IO Monad value is catched and handled
by the second argument.
-}
catchA :: ArrowIO a => (b -> IO c) -> (SomeException -> IO c) -> a b c
catchA action handler =
    proc x -> do
        arrIO $ (\param -> Control.Exception.catch (action param) handler)  -< x

{- |
Like catchA, but operates on IO Monad values instead of constructor functions. Therefore the input value of the resulting
Arrow is ignored.
-}
catchA_ :: ArrowIO a => IO c -> (SomeException -> IO c) -> a b c
catchA_ action handler =
    catchA (\_ -> action) handler
-}
{- |
Like catchA, but utilizes an Arrow for exception handling.
-}
exceptA :: (a -> IO b) -> IOStateArrow s SomeException b -> IOStateArrow s a b
exceptA f ea =
    proc x -> do
        result <- arrIO $ (\param -> Control.Exception.catch (action param) handler)    -< x
        case result of
            Left  e   -> ea         -<  e
            Right val -> constA val -<< ()
    where
        handler e = return $ Left e
        action param = do
            original <- f param
            return $ Right original

{- |
Like exceptA, but defaulting to a failing Arrow (zeroArrow) for the exception case.
-}
exceptionA :: (a -> IO b) -> IOStateArrow s a b
exceptionA f =
    exceptA f zeroArrow

{- |
Like catchA, but without configurable exception handling - an exception is represented by the failing Arrow (zeroArrow).
-}
exceptZeroA :: (a -> IO b) -> IOStateArrow s a b
exceptZeroA f =
    exceptA f zeroArrow

{- |
Like exceptZeroA, but operates on IO Monad values instead of constructor functions. Therefore the input value of the resulting
Arrow is ignored.
-}
exceptZeroA_ :: IO b -> IOStateArrow s a b
exceptZeroA_ f =
    exceptionA (\_ -> f)

{- |
Creates a thread to process the argument Arrow, which is started with the input value and current state of the processA Arrow.
The result value of the multi-threaded Arrow is dropped, as processA immediately returns with the thread id of the newly forked thread.
-}
processA :: IOStateArrow s a b -> IOStateArrow s a ThreadId
processA arrow =
    proc x -> do
        state   <- getUserState         -< ()
        thread  <- arrIO $ forkIO       -< action x state
        returnA                         -< thread
    where
        action input state =
            runX (withOtherUserState state (constA input >>> arrow))
            >> return ()

{- |
An Arrow taking an XmlTree and delivering an Arrow independent of its input and returning this XmlTree.
-}
liftA :: XmlAccess s (XmlConstSource s)
liftA =
    arr constA

{- |
An Arrow transforming a constant XmlTree generator (() -> XmlTree) into an Arrow ignoring its input value of arbitrary type.
-}
liftConstSource :: XmlConstSource s -> XmlSource s a
liftConstSource source =
    proc _ -> do
        result  <- source   -< ()
        returnA             -< result

{- |
An Arrow reading the file denoted by the argument, parsing the content by means of HXT and returning the XmlTree computed.
-}
fileSource :: String -> XmlSource s a
fileSource filename
    = runInLocalURIContext $
      readDocument [ withRemoveWS yes
		   , withCanonicalize yes
		   , withIndent no
		   , withValidate no] filename

{- |
An Arrow returning an XmlTree value independently of the Arrow's input.
-}
staticSource :: XmlTree -> XmlSource s a
staticSource tree =
    constA tree

{- |
The empty configuration Arrow (currently a root element of name "config").
-}
emptyConfig :: XmlSource s a
emptyConfig =
    eelem "config"

{- |
Evaluates a given Arrow with an XmlTree as input type (first argument) by applying a given state (second argument).
Non-determinism of the HXT Arrows is handled by returning a list of result values.
-}
evalXmlList :: XmlAccess s a -> s -> IO [a]
evalXmlList op state =
    do
        result <- runX (withOtherUserState state
            (proc tree -> do
                val     <- op   -< tree
                returnA         -< val
            ))
        return result

{- |
Evaluates a given Arrow with an XmlTree as input type (first argument) by applying a given state (second argument).
Non-determinism of the HXT Arrows is handled by returning either the first result or Nothing for no result.
-}
evalXml :: XmlAccess s a -> s -> IO (Maybe a)
evalXml op state =
    do
        results <- evalXmlList op state
        return (if null results then Nothing else (Just $ head results))

{- |
Evaluates a given Arrow with an XmlTree as input type (first argument) by applying a given state (second argument).
Non-determinism of the HXT Arrows is handled by returning either the first result or a default value (second argument)
for no result.
-}
evalXmlDef :: XmlAccess s a -> s -> a -> IO a
evalXmlDef op state def =
    do
        results <- evalXmlList op state
        return (if null results then def else head results)

{- |
Evaluates a given Arrow (with input type ()) starting with an argument state and returning the resulting state.
-}
evalState :: XmlConstSource s -> s -> IO s
evalState transformer init_state =
    do
        final_state <- evalXml (liftConstSource transformer &&& getUserState >>> arr snd) init_state
        return $ fromJust final_state





{- |
Helper function to apply Arrows to an input XmlTree with regard to the last element's axis. The first argument defines an Arrow
to get applied to Attribute axis final elements, the second argument defines an Arrow to get applied to Child axis final elements.
The third argument represents the XPath expression in question.
-}

{-
xpOp :: (String -> String -> XmlAccess s a) -> (String -> String -> XmlAccess s a) -> String -> XmlAccess s a
xpOp attr_op child_op xpath =
    xpOp' attr_op child_op (jp xpath)
    proc in_xml -> do
        let parts = case (runParser parseXPath [] "" xpath) of
                Left _          -> Nothing
                Right xpExpr    -> local_xpath xpExpr
        out_xml <- (case parts of
            Just (axis, loc)    ->
                proc in_xml' -> do
                    let path = base xpath
                    case axis of
                        Attribute   -> attr_op loc path  -<< in_xml'
                        Child       -> child_op loc path -<< in_xml'
                        _           -> zeroArrow         -<  ()
            Nothing             -> zeroArrow) -<< in_xml
        returnA -< out_xml
    where
        local_xpath (PathExpr _ (Just (LocPath Abs path)))  = select_parts (reverse $ path)
        local_xpath _                                       = Nothing
        select_parts ((Step axis (NameTest name) []):_)     = Just (axis, qualifiedName name)
        select_parts _                                      = Just (Child, ".")
        base xpath'             = let xpath'' = removeLastSlash xpath' in
                                    case xpath'' of
                                        []   -> "/"
                                        ('/':[]) -> "/"
                                        (_:xs)   -> reverse xs
        removeLastSlash xpath'  = dropWhile (\c -> c /= '/') (reverse xpath')
-}

xpOp :: (String -> String -> XmlAccess s a) -> (String -> String -> XmlAccess s a) -> JanusPath -> XmlAccess s a
xpOp _       child_op (ChildPath loc path) = child_op loc path
xpOp attr_op _        (AttrPath  loc path) = attr_op  loc path
xpOp _       _         NoPath              = zeroArrow

{- |
Transforms an XPath string into a JanusPath
-}


{- |
Returns an XPath denoted element of an XmlTree value. For a child element, all subsequent text nodes are delivered. For an
attribute, the attribute value is delivered. For a local part * in case of the Attribute axis (e.g. \/test\/\@*), all attribute
values of the node in question are delivered. Hence, this Arrow is a non-deterministic one. The Arrow fails for non-existing
values.
-}
getVal :: JanusPath -> XmlAccess s String
getVal xpath =
    xpOp
        (\loc path  ->
            (if loc == "*"
                then listValPairs xpath >>> arr snd
                else getXPathTrees path >>> getAttrValue0 loc
                ) -- ifA (getXPathTrees xpath) (getXPathTrees path >>> getAttrValue loc) (none) ))
            )
        (\_ _       -> getXPathTrees (show xpath) >>> getChildren >>> getText)
        xpath

{- |
Returns an XPath denoted element of an XmlTree value. For a child element, all subsequent text nodes are delivered. For an
attribute, the attribute value is delivered. For a local part * in case of the attribute axis (e.g. \/test\/\@*), all attribute
values of the node in question are delivered. Hence, this Arrow is a non-deterministic one. The Arrow delivers a default string
value in case the requested value does not exist.
-}
getValDef :: JanusPath -> String -> XmlAccess s String
getValDef xpath def =
    getVal xpath
    `orElse`
    constA def

{- |
Like getVal, but delivers a polymorphically bound type based on the Read type class parser. Therefore the returned type is
required to be installed in the Read and Show type classes. The Arrow fails if the requested value does not exist or cannot
be parsed to the requested type.
-}
getValP :: (Read a, Show a) => JanusPath -> XmlAccess s a
getValP xpath =
    getVal xpath >>> parseA

{- |
Returns the names of all children respectively attributes of a given node. Hence, this Arrow is a non-deterministic one.
The Arrow fails if the father node does not exist.
-}
listVals :: JanusPath -> XmlAccess s String
listVals xpath =
    xpOp
        (\loc path  -> (if loc == "*"
                            then getXPathTrees path >>> getAttrl >>> getLocalPart
                            else zeroArrow))
        (\loc path  -> (if loc == "*"
                            then getXPathTrees path >>> processChildren (none `when` (neg isElem)) >>> getChildren >>> getName
                            else zeroArrow))
        xpath

{- |
Pairwise returns the names and values of all children respectively attributes of a given node. Hence, this Arrow is a non-deterministic one.
The Arrow fails if the father node does not exist.
-}
listValPairs :: JanusPath -> XmlAccess s (String, String)
listValPairs xpath =
    xpOp
        (\loc path -> (if loc == "*"
                then proc tree -> do
                    element <- getXPathTrees path               -<  tree
                    attrl   <- getAttrl >>> getLocalPart        -<  element
                    constA attrl &&& getAttrValue attrl         -<< element
                else zeroArrow))
        (\loc path -> (if loc == "*"
                then proc tree -> do
                    children <- (getXPathTrees path
                                    >>>
                                    getChildren)                -< tree
                    getLocalPart &&& (getChildren >>> getText)  -< children
                else zeroArrow))
        xpath

{- |
Sets an XPath denoted XmlTree value by inserting a text node into an existing element or by inserting an attribute. Existing values are
replaced. Missing intermediate nodes get created. In case of the * as local part, all children respectively attributes are changed.
-}

setVal :: JanusPath -> String -> XmlTransform s
setVal xpath val =
    xpOp
        (\loc path -> ( let path' = jp path in
			( insEmptyTree path' `when` neg (getTree path') )
                        >>>
			processXPathTrees
			( if loc == "*"
			  then processAttrl (replaceChildren (txt val))
			  else addAttr loc val
			) path
                      )
	)
        (\_ _       -> ( processXPathTrees (processChildren (none `when` isText)) (show xpath)	-- remove text node
			 `when`
			 getTree xpath                                                          -- if set
                       )
                       >>>
                       insTree xpath (txt val)                                                  -- insert new text
        )
        xpath

{- |
Like setVal, but storing an arbitrary polymorphically bound type installed in the Read and Show classes.
-}
setValP :: (Read a, Show a) => JanusPath -> a -> XmlTransform s
setValP xpath val =
    setVal xpath (show val)

{- |
Removes an attribute or text node value from an XmlTree. Using *, all children's text values or the node's attribute get deleted. If an element
remains empty (i.e. no subsequent elements), it gets removed from the tree.
-}

delVal :: JanusPath -> XmlTransform s
delVal xpath =
    xpOp
        ( \ loc path ->
          processXPathTrees ( if loc == "*"
			      then (processAttrl none)
			      else (removeAttr loc)
			    ) path
        )
        ( \ _ _ ->
	  processXPathTrees (processChildren (none `when` isText)) (show xpath)
	  >>>
          ( delTree xpath
	    `when`
            neg ( getTree xpath >>> getChildren )
	  )
        )
        xpath

{- |
Like setVal, but returns the previous value.
-}

swapVal :: JanusPath -> String -> XmlAccess s (XmlTree, String)
swapVal xpath val =
    proc tree -> do
        resultVal   <- getVal xpath         -<  tree
        resultTree  <- setVal xpath val     -<  tree
        returnA                             -<  (resultTree, resultVal)

{- |
Returns an XPath denoted subtree of an XmlTree value.
-}

getTree :: JanusPath -> XmlTransform s
getTree xpath =
    getXPathTrees (show xpath)

{- |
Inserts an XmlTree delivered by an Arrow (second argument) into an existing XPath denoted element. Non-existing
intermediate elements get created automatically.
-}

insTree :: JanusPath -> XmlTransform s -> XmlTransform s
insTree xpath tree =
    ( insEmptyTree xpath
      `when`
      neg (getTree xpath)
    )
    >>>
    processXPathTrees (replaceChildren (tree <+> getChildren)) (show xpath)
    -- (processXPathTrees (insertChildrenAt 0 tree) (show xpath))

{- |
Creates an empty element in an existing XPath denoted element. Non-existing intermediate elements get created automatically.
-}

insEmptyTree :: JanusPath -> XmlTransform s
insEmptyTree xpath =
    xpOp
        (\_ _      -> zeroArrow)
        (\loc path ->
                (if loc == "*"
                    then zeroArrow
                    else insTree (jp path) (eelem loc)
                    )
            )
        xpath

{- |
Like insTree, but the new subtree is inserted as the last child of the node in question.
-}

addTree :: JanusPath -> XmlTransform s -> XmlTransform s
addTree xpath tree =
        ( insEmptyTree xpath
          `when`
           neg (getTree xpath)
	)
        >>>
        processXPathTrees (replaceChildren (getChildren <+> tree)) (show xpath)

{- |
Removes a whole XPath denoted subtree.
-}

delTree :: JanusPath -> XmlTransform s
delTree xpath =
    processXPathTrees none (show xpath)

-- ------------------------------------------------------------
{-
-- this is a hack
--
-- readline is used in console shader, which is loaded dynamically
-- and can not handle to load readline.so
-- so the function is put here to be loaded into the janus core

get1Line :: String -> IO String
get1Line prompt
    = do
      line' <- readline prompt
      let line = stringTrim . fromMaybe "" $ line'
      if null line
	 then get1Line prompt
	 else do
	      if length line > 1
		 then addHistory line
		 else return ()
	      return line
-}
-- ------------------------------------------------------------
