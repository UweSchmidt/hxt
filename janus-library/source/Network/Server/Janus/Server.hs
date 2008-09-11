-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Server
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Janus Server Module

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Server
    ( serverArrow
    , normalizeConfig
    )
where

import Text.XML.HXT.Arrow

import Network.Server.Janus.Core as Shader
import Network.Server.Janus.Messaging
import Network.Server.Janus.Shader.SystemShader
import Network.Server.Janus.Shader.ControlShader
import Network.Server.Janus.Transaction as TA
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths
import Network.Server.Janus.ServerVersion	( build_version )

import System.Directory

-- ------------------------------------------------------------

{- |
The primary Arrow of Janus.
-}

serverArrow	:: String -> String -> JanusArrow Context () ()
serverArrow build_date
		= serverArrow' full_release
                where
		full_release
		    | null build_date	= build_version
		    | otherwise		= build_version ++ " built " ++ build_date

serverArrow' :: String -> String -> JanusArrow Context () ()
serverArrow' full_release conf_file =
    proc _ -> do

        defineChannelsAndStartupMessageHandlers                                                 -<  ()

        -- start-message
        globalMsg $ "Janus Server " ++ full_release ++ " starting up...\n"     -<  ()
        globalMsg $ "-----------------------------------------------\n"        -<  ()
        globalMsg $ ""                                                         -<  ()

        -- load server configuration
        globalMsg $ "loading and normalizing server config (" ++ conf_file ++ ")... " -< ()
        cfg         <-
                    fileSource conf_file
                    >>>
                    normalizeConfig
                    >>>
                    getTree _janus                                                              -<  ()
        globalMsg $ "done\n"                                                   -<  ()

        -- initialize Context
        globalMsg $ "updating context (adding global and local scopes)... "    -<  ()
        swapConfig                                                                              -<  cfg
        addScope "global"                                                                       -<  ()
        addScope "local"                                                                        -<  ()
        "/global/system/version"    <-! full_release                                            -<  ()
        currentdir  <- exceptZeroA_ $ getCurrentDirectory                                       -<  ()
        "/global/system/serverroot" <-! currentdir                                              -<< ()
        globalMsg $ "done\n"                                                   -<  ()

        -- registering Server-Context
        globalMsg $ "registering server context... "                           -<< ()
        context     <- getContext                                                               -<  ()
        "/global/consoles/server" <$! (ContextVal context)                                      -<< ()
        globalMsg $ "done\n"                                                   -<< ()

        loadCoreShaders                                                                          -<< ()

        -- load handler trees
        globalMsg $ "loading system shader from configuration file... "        -<  ()
        rootTree    <- single $ staticSource cfg >>> getTree _janus_shader                      -<< ()
        globalMsg $ "done\n"                                                   -<< ()
        globalMsg $ "constructing system shader...\n"                          -<  ()
        (_, shader) <- loadShader                                                               -<  rootTree
        globalMsg $ "done: constructing system shader\n"                       -<< ()
        globalMsg $ "executing system shader...\n"                             -<  ()
        executeShader shader                                                                    -<< ()
        globalMsg $ "done: executing system shader\n"                          -<< ()

        globalMsg $ "server running...\n"                                      -<  ()

        listenChannel chControl                                                                 -<  ()
        returnA                                                                                 -<  ()

defineChannelsAndStartupMessageHandlers	:: JanusStateArrow a a
defineChannelsAndStartupMessageHandlers
    = seqA $
      [ addChannel chGlobal
      , addChannel chLocal
      , addChannel chControl
      , changeHandler chGlobal  (\_ -> myhandler)
      , changeHandler chLocal   (\_ -> consoleHandler)
      , changeHandler chControl (\_ -> invokeHandler)
      ]
    where
    myhandler = filterHandler (getMsgLevelFilter l_error) >>> consoleHandler

loadCoreShaders			:: JanusStateArrow a a
loadCoreShaders
    = seqA $
      [ globalMsg "loading core shaders... "
      , addShaderCreator' "system.loadshadercreator"  loadShaderCreator
      , addShaderCreator' "system.loadhandlercreator" loadHandlerCreator
      , addShaderCreator' "system.loadhandler"        loadHandler
      , addShaderCreator' "system.changeroot"         changeServerRoot
      , addShaderCreator' "control.seq"               seqControl
      , addShaderCreator' "control.like"              likeControl
      , addShaderCreator' "control.select"            selectControl
      , addShaderCreator' "control.loopuntil"         loopUntilControl
      , addShaderCreator' "control.loopwhile"         loopWhileControl
      , addShaderCreator' "control.if"                ifThenElseControl
      , globalMsg "done\n"
      ]

addShaderCreator'		:: String -> ShaderCreator -> JanusStateArrow a a
addShaderCreator' n sc		= globalMsg n >>> addShaderCreator n sc

{-
TODO
Normalize server configuration. Fails with according errors in the context if syntax errors are found.
Unchecked: semantics, like existence and correctness of type attributes
translate control structure case
-}
normalizeConfig :: JanusStateArrow XmlTree XmlTree
normalizeConfig =
    computeJanusRoot
    >>>
    resolveHighLevelShortcuts
    >>>
    resolveLowLevelShortcuts
    >>>
    computeJanusShader
    >>>
    propagateApply
    >>>
    computeConfigNode
    >>>
    computeInitNode

{- |
Returns the child node \"janus\" as the root of a new XML tree. All other children of the root node are ignored.
If no \"janus\" node is present, an empty one is created and returned.
-}
computeJanusRoot :: JanusStateArrow XmlTree XmlTree
computeJanusRoot =
    (insertChildrenAt 0 (eelem "janus")) `when` (neg $ getChildren >>> hasName "janus")
    >>>
    getChildren
    >>>
    hasName "janus"
    -- >>>
    -- processXPathTrees ((insertChildrenAt 0 (eelem "shader")) `when` (neg $ getChildren >>> hasName "shader")) "/janus"

{- |
Resolves high level shortcuts to low level shortcuts. Descendants of \"config\" nodes are ignored.
1 loadhandlers -> n loadhandler, each attribute represents an reference=object pair, the module attribute represents the module file in question
and is removed before resolving.
1 loadshaders -> n loadshader, each attribute represents an reference=object pair, the module attribute represents the module file in question
and is removed before resolving.
-}
resolveHighLevelShortcuts :: JanusStateArrow XmlTree XmlTree
resolveHighLevelShortcuts =
    processXPathTrees (proc node -> do
        node'       <- replaceChildren zeroArrow                            -<  node
        mod_name    <- getAttrValue0 "module"                               -<  node'
        node''      <- removeAttr "module"                                  -<  node'
        (getAttrl >>> getName)
            >>>
            (proc name -> do
                val     <- getAttrValue name                        -<< node''
                aelem "loadhandler"
                        [attr "reference" (txt name), attr "object" (txt val), attr "module" (txt mod_name)] -<< ()
                )                                                           -<< node''
        )
        "/janus//loadhandlers[not(ancestor::config)]"
    >>>
    processXPathTrees (proc node -> do
        node'       <- replaceChildren zeroArrow                            -<  node
        mod_name    <- getAttrValue0 "module"                               -<  node'
        node''      <- removeAttr "module"                                  -<  node'
        (getAttrl >>> getName)
            >>>
            (proc name -> do
                val     <- getAttrValue name                        -<< node''
                aelem "loadshader"
                        [attr "reference" (txt name), attr "object" (txt val), attr "module" (txt mod_name)] -<< ()
                )                                                           -<< node''
        )
        "/janus//loadshaders[not(ancestor::config)]"
    >>>
    resolveLike
    >>>
    resolveSelect

{- |
<absolute name="..." value="..."/> -> <shader type="absolute" name="..." value="..."/> -> ...
<relative name="..." value="..."/> -> <shader type="absolute" name="..." value="..."/> -> ...

Resolves low level shortcuts to shader nodes. Descendants of \"config\" nodes are ignored.
loadshader -> shader type=\"system.loadshadercreator\".
loadhandler -> shader type=\"system.loadhandlercreator\".
handler -> shader type=\"system.loadhandler\".
block -> shader type=\"control.seq\".
casematch -> shader type=\"control.casematch\".
case -> shader type=\"control.case\".
loopuntil -> shader type=\"control.loopuntil\".
loopwhile -> shader type=\"control.loopwhile\".
if -> shader type=\"control.if\".
-}
resolveLowLevelShortcuts :: JanusStateArrow XmlTree XmlTree
resolveLowLevelShortcuts =
    processXPathTrees (setElemName (mkName "shader") >>> addAttr "type" "system.loadshadercreator") "/janus//loadshader[not(ancestor::config)]"
    >>>
    processXPathTrees (setElemName (mkName "shader") >>> addAttr "type" "system.loadhandlercreator") "/janus//loadhandler[not(ancestor::config)]"
    >>>
    processXPathTrees (setElemName (mkName "shader") >>> addAttr "type" "system.loadhandler") "/janus//handler[not(ancestor::config)]"
    >>>
    processXPathTrees (setElemName (mkName "shader") >>> addAttr "type" "control.seq") "/janus//block[not(ancestor::config)]"
    >>>
    processXPathTrees (setElemName (mkName "shader") >>> addAttr "type" "control.loopuntil") "/janus//until[not(ancestor::config)]"
    >>>
    processXPathTrees (setElemName (mkName "shader") >>> addAttr "type" "control.loopwhile") "/janus//while[not(ancestor::config)]"
    >>>
    processXPathTrees (setElemName (mkName "shader") >>> addAttr "type" "control.if") "/janus//if[not(ancestor::config)]"

{- |
If no \"shader\" child of the \"janus\" root is present, an empty one is created.
-}
computeJanusShader :: JanusStateArrow XmlTree XmlTree
computeJanusShader =
    processXPathTrees ((insertChildrenAt 0 (eelem "shader")) `when` (neg $ getChildren >>> hasName "shader")) "/janus"

{- |
Applies the immediate attributes of each \"apply\" node to all descendant \"shader\" nodes. The \"apply\" nodes are processed depth-first, therefore in
case of ambiguities the deeper nodes overrule the higher ones. Existing attributes of \"shader\" nodes don't get overwritten and therefore overrule
any \"apply\" node attribute. After applying an \"apply\" node, the node is replaced by its children.
Descendants of \"config\" nodes are ignored, so neither \"apply\" descendants of a \"config\" node are resolved nor \"apply\" attributes get forwarded to
\"shader\" descendants of \"config\" nodes.
-}
propagateApply :: JanusStateArrow XmlTree XmlTree
propagateApply =
    processXPathTrees (proc node -> do
        attribs     <- listA $ (getAttrl >>> getName)
                                >>>
                                (proc name -> do
                                    val     <- getAttrValue name    -<< node
                                    returnA                         -<  (name, val)
                                    )                                                   -<< node
        let addA = foldr (\(name,val) arrow -> arrow >>> ifA (hasAttr name) (this) (addAttr name val))
                            this
                            attribs
        node'       <- processXPathTrees addA "//shader[not(ancestor::config)]"         -<< node
        getChildren                                                                     -<  node'
        )
        "/janus//apply[not(ancestor::config)]"

{- |
For each \"shader\" node and the \"janus\" root node, a \"config\" child is created if not present already.
All attributes of the \"shader\" node are moved into the \"config\" child.
If no \"type\" attribute is present, the type \"control.seq\" is inserted (so this becomes the default type for shaders).
If no \"id\" attribute is present, a unique one (across all shaders without predefined id) is generated.
If no \"state\" attribute is present, it is set to /local/_<id>. In case of \"control.seq\" shaders, it is set to /local.
If no \"root_state\" attribute is present, it is set to the \"state\" attribute's value.
If no \"accepts\" attribute is present or if it cannot be parsed to a TransactionState list, it is set to [Processing].
\"shader\" descendants of \"config\" nodes are ignored.
-}
computeConfigNode :: JanusStateArrow XmlTree XmlTree
computeConfigNode =
    processXPathTrees ((insertChildrenAt 0 (eelem "config")) `when` (neg $ getChildren >>> hasName "config")) "/janus"
    >>>
    processXPathTrees ((insertChildrenAt 0 (eelem "config")) `when` (neg $ getChildren >>> hasName "config")) "/janus//shader[not(ancestor::config)]"
    >>>
    processXPathTrees
        (proc node -> do
            attribs         <- listA $ getAttrl                                                     -<  node
            node'           <- processChildren (addAttrl (constL attribs) `when` hasName "config")  -<< node
            node''          <- setAttrl none                                                        -<  node'
            stype           <- getVal _shader_config_type
                               `orElse`
                               (constA $ "control.seq")                                             -<  node''
            ident           <- getVal _shader_config_id
                               `orElse`
                               (getQualifiedUID "shaderid" 1
                                    >>>
                                    arr (\x -> stype ++ "_" ++ show x))                             -<< node''
            state           <- getVal _shader_config_state
                               `orElse`
                               (constA $ if stype == "control.seq"
                                            then "/local"
                                            else "/local/_" ++ ident)                               -<< node''
            root_state      <- getVal _shader_config_rootState
                               `orElse`
                               (constA $ state)                                                     -<< node''
            accepts         <- ( getVal _shader_config_accepts >>> parseA )
                                `orElse`
                                (constA [Processing])                                               -<< node''
            node'''         <- setVal _shader_config_type stype
                               >>>
                               setVal _shader_config_id ident
                               >>>
                               setVal _shader_config_state state
                               >>>
                               setVal _shader_config_rootState root_state
                               >>>
                               setVal _shader_config_accepts (show accepts)                         -<< node''
            returnA                                                                                 -<  node'''
            ) "/janus//shader[not(ancestor::config)]"

{- |
Checks the existence of an \"init\" child node for all \"shader\" nodes. If no one is present, an empty one gets inserted.
\"shader\" descendants of \"config\" nodes are ignored.
-}
computeInitNode :: JanusStateArrow XmlTree XmlTree
computeInitNode =
    processXPathTrees ((insertChildrenAt 0 (eelem "init")) `when` (neg $ getChildren >>> hasName "init")) "/janus//shader[not(ancestor::config)]"

{- |
TODO
-}
resolveLike :: JanusStateArrow XmlTree XmlTree
resolveLike =
    processXPathTrees (proc node -> do
        defaultnode <- (single $ (getChildren >>> hasName "default" >>> setElemName (mkName "block")))
                        `orElse`
                        (eelem "block")                                                     -<  node
        valuenode   <- (single $ (getChildren >>> hasName "value" >>> setElemName (mkName "block")))
                        `orElse`
                        (eelem "block")                                                     -<  node
        children    <- listA $ (getChildren >>> hasName "case")                             -<  node
        node'       <- setChildren []                                                       -<  node
        let refactorA = foldr (\child arrow -> arrow
                                >>>
                                (proc (tree, selects, counter) -> do
                                    let ident = "case_" ++ (show counter)
                                    select  <- getAttrValue "match"                     -<  child
                                    child'  <- removeAttr "match"                       -<  child
                                    child'' <- setElemName (mkName "block")
                                                >>>
                                                addAttr "id" ident                      -<< child'
                                    tree'   <-  insertChildrenAt 0 (constA child'')     -<< tree
                                    returnA -<  (tree', (ident, select):selects, counter+1)
                                    )
                                )
                            this
                            children
        (node'', selects', nextid)   <- refactorA                                           -<< (node', [], 0 :: Int)

        node''' <- setElemName (mkName "shader")
                    >>>
                    addAttr "type" "control.like"
                    >>>
                    addAttr "value" ("case_" ++ (show nextid))
                    >>>
                    addAttr "default" ("case_" ++ (show $ nextid+1))                        -<< node''


        let addA = foldr (\(ident, select) arrow -> arrow >>> insertChildrenAt 0 (selem ident [txt select]))
                             this
                             selects'
        node4       <- ifA (getChildren >>> hasName "config")
                        zeroArrow
                        (insertChildrenAt 0 (eelem "config"))                               -<  node'''

        node5       <- processXPathTrees addA "/shader/config"                              -<< node4
        valuenode'  <- addAttr "id" ("case_" ++ (show $ nextid))                            -<< valuenode
        defaultnode'    <- addAttr "id" ("case_" ++ (show $ nextid+1))                      -<< defaultnode
        node6       <- insertChildrenAt 0 (constA valuenode')                               -<< node5
        insertChildrenAt 0 (constA defaultnode')                                            -<< node6
        )
        "/janus//like[not(ancestor::config)]"

{- |
TODO
-}
resolveSelect :: JanusStateArrow XmlTree XmlTree
resolveSelect =
    processXPathTrees (proc node -> do
        defaultnode <- (single $ (getChildren >>> hasName "default" >>> setElemName (mkName "block")))
                        `orElse`
                        (eelem "block")                                                     -<  node
        valuenode   <- (single $ (getChildren >>> hasName "value" >>> setElemName (mkName "block")))
                        `orElse`
                        (eelem "block")                                                     -<  node
        children    <- listA $ (getChildren >>> hasName "case")                             -<  node
        node'       <- setChildren []                                                       -<  node
        let refactorA = foldr (\child arrow -> arrow
                                >>>
                                (proc (tree, selects, counter) -> do
                                    let ident = "case_" ++ (show counter)
                                    select  <- getAttrValue "match"                     -<  child
                                    child'  <- removeAttr "match"                       -<  child
                                    child'' <- setElemName (mkName "block")
                                                >>>
                                                addAttr "id" ident                      -<< child'
                                    tree'   <-  insertChildrenAt 0 (constA child'')     -<< tree
                                    returnA -<  (tree', (ident, select):selects, counter+1)
                                    )
                                )
                            this
                            children
        (node'', selects', nextid)   <- refactorA                                           -<< (node', [], 0 :: Int)

        node''' <- setElemName (mkName "shader")
                    >>>
                    addAttr "type" "control.select"
                    >>>
                    addAttr "value" ("case_" ++ (show nextid))
                    >>>
                    addAttr "default" ("case_" ++ (show $ nextid+1))                        -<< node''


        let addA = foldr (\(ident, select) arrow -> arrow >>> insertChildrenAt 0 (selem ident [txt select]))
                             this
                             selects'
        node4       <- ifA (getChildren >>> hasName "config")
                        zeroArrow
                        (insertChildrenAt 0 (eelem "config"))                               -<  node'''

        node5       <- processXPathTrees addA "/shader/config"                              -<< node4
        valuenode'  <- addAttr "id" ("case_" ++ (show $ nextid))                            -<< valuenode
        defaultnode'    <- addAttr "id" ("case_" ++ (show $ nextid+1))                      -<< defaultnode
        node6       <- insertChildrenAt 0 (constA valuenode')                               -<< node5
        insertChildrenAt 0 (constA defaultnode')                                            -<< node6
        )
        "/janus//select[not(ancestor::config)]"
