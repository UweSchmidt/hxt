-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.ConsoleShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ConsoleShader.hs, v1.1 2007/03/27 00:00:00 janus Exp $

   Janus Console Shader Definitions
   
   Definition of Shaders to be used as commands by a console application.
   
-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.ConsoleShader
    (

    -- console shaders
      testShader
    , ttyCommandShader
    , ttyConfigShader
    , ttyExecuteShader
    , ttyExitShader
    , ttyForkShader
    , ttyHelpShader
    , ttyListShader
    , ttyLoadShader
    , ttyShowShader
    , ttyStoreShader
    , ttySwitchShader
    , ttyThreadShader
    , ttyVersionShader    

    , ttySendShader
    )
where

import Control.Concurrent
import Text.XML.HXT.Arrow

import Network.Server.Janus.Core
import Network.Server.Janus.DynamicLoader
import Network.Server.Janus.Messaging
import Network.Server.Janus.Server (normalizeConfig)
import Network.Server.Janus.XmlHelper



testShader :: ShaderCreator
testShader = 
    mkDynamicCreator $ proc (_, _) -> do
        "test" <*! (0 :: Int)                                                   -< () 
        let shader = proc in_ta -> do
            (val :: Int) <- getSVP "test"                               -<  ()
            "test" <*! (val+1)                                          -<< ()             
            setVal "/transaction/response_fragment" (show $ val + 1)    -<< in_ta
        returnA                                                                 -<  shader

{- |
TODO
-}
ttyCommandShader :: ShaderCreator
ttyCommandShader = 
    mkStaticCreator $ 
    proc in_ta -> do
        request <- getVal "/transaction/request_fragment" 
                        <+!> ("requestShader", TAValueNotFound, "No request fragment found.", [("value", "/transaction/request_fragment")])  
                                                                            -<  in_ta
        let command = words request
        (case command of 
            cmd:xs  -> setVal "/transaction/console/@command" cmd
                            >>>
                            setVal "/transaction/console/@argcount" (show $ length command - 1)
                            >>>
                            add xs (1::Int)
            _       -> zeroArrow
            )                                           -<< in_ta
    where
        add (x:xs) num = setVal ("/transaction/console/args/_" ++ (show num)) x >>> add xs (num+1)
        add []     _   = returnA

{- |
TODO
-}
ttyConfigShader :: ShaderCreator
ttyConfigShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"             -<  in_ta
        context         <- getSVP ("ctx")                                       -<  ()        
        (case argc of
            0   ->
                proc in_ta' -> do
                    cfg     <- runInContext context getConfig               -<  ()
                    str     <- xshow (constA cfg)                           -<< ()
                    setVal "/transaction/response_fragment" str             -<< in_ta'
            _   -> 
                returnA
            )                                                                   -<< in_ta

{- |
TODO
Executes Shaders
-}
ttyExecuteShader :: ShaderCreator
ttyExecuteShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"             -<  in_ta
        context         <- getSVP ("ctx")                                       -<  ()
        (case argc of
            0   ->
                proc in_ta' -> do
                    sRep    <- runInContext context getShaderCreators       -<  ()
                    creators <- listA $ listComponents                      -<  sRep
                    let shdStr = foldl (\creator str -> str ++ "\n" ++ creator) "" creators
                    setVal "/transaction/response_fragment" shdStr          -<< in_ta'
            3   ->
                proc in_ta' -> do
                    creator <- getVal "/transaction/console/args/_1"        -<  in_ta'
                    ident   <- getVal "/transaction/console/args/_2"        -<  in_ta'
                    select  <- getVal "/transaction/console/args/_3"        -<  in_ta'
                    
                    (XmlVal xml)
                            <- runInContext context $ getSV ident           -<< ()
                    conf    <- single $ getTree ("//shader[config/@id='" ++ 
                                            select ++ "']")                 -<< xml
                    creator' <- runInContext context $ 
                                    getShaderCreator creator                -<< ()
                    shader  <- runInContext context $ creator'              -<< conf
                    runInContext context $ executeShader shader             -<< ()                  
                    setVal "/transaction/response_fragment" "Done."         -<  in_ta'
            _   -> 
                returnA
            )                                                                   -<< in_ta

{- |
TODO
-}
ttyExitShader :: ShaderCreator
ttyExitShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"             -<  in_ta
        (case argc of
            0   ->
                proc in_ta' -> do
                    "control"   <-@ mkControlMsg [] Terminate               -<  ()
                    returnA                                                 -< in_ta'
            _   -> 
                returnA
            )                                                                   -<< in_ta

{- |
TODO
-}
ttyForkShader :: ShaderCreator
ttyForkShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"             -<  in_ta
        context         <- getSVP ("ctx")                                       -<  ()        
        (case argc of
            0   ->
                proc in_ta' -> do
                    cfg     <- runInContext context getConfig               -<  ()
                    shaders <- listA $ getVal ("/janus//shader/config/@id") -<  cfg
                    let shStr = foldl (\shader str -> str ++ "\n" ++ shader) "" shaders
                    setVal "/transaction/response_fragment" shStr           -<< in_ta'            
            1   ->
                proc in_ta' -> do
                    ident   <- getVal "/transaction/console/args/_1"        -<  in_ta'
                    cfg     <- runInContext context getConfig               -<  ()
                    xml     <- single $ getTree ("/janus//shader[config/@id='" ++ 
                                    ident ++ "']")                  
                                    <+!> ("ttyForkShader", ShaderFailed, "Definition not found.", [("id", ident)])     
                                                                            -<< cfg 
                    (_, shader) <- runInContext context loadShader          -<  xml
                    runInContext context (executeShader shader)             -<< ()
                    setVal "/transaction/response_fragment" "Done."         -<  in_ta'
            _   -> 
                returnA
            )                                                                   -<< in_ta

{- |
TODO
-}
ttyHelpShader :: ShaderCreator
ttyHelpShader =
    mkStaticCreator $ 
    proc in_ta -> do
        setVal "/transaction/response_fragment" (concat helpString)             -<  in_ta
    where
        helpString = 
                    [ 
                          "\n"
                        , "Help\n"
                        , "----\n"
                        , " config                              (displays the configuration XML tree)\n"
                        , " execute                             (lists all registered ShaderCreators)\n"
                        , " execute <creator> <ident> <select>  (instantiates and executes a Shader)\n"
                        , " exit                                (quits the server)\n"
                        , " fork                                (lists all Shader definitions in the configuration)\n"
                        , " fork <select>                       (loads and executes a Shader definition in the configuration)\n"
                        , " help                                (this usage information)\n"                        
                        , " list                                (lists all scopes)\n"
                        , " list <ident>                        (lists all state children of <ident>)\n"
                        , " load config <ident>                 (copies the configuration into the state)\n"
                        , " load str <literal> <ident>          (loads a string literal into the state)\n"
                        , " load xml <filename> <ident>         (loads an XML file into the state)\n"
                        , " show <ident>                        (shows the state content at <ident>)\n"
                        , " store config <ident>                (recovers the configuration from the state)\n"
                        , " store xml <filename> <ident>        (stores an XML file from the state)\n"
                        , " switch server                       (switches to the server context)\n"
                        , " switch handler                      (lists all handlers)\n"
                        , " switch handler <handler>            (switches to <handler>'s context)\n"
                        , " thread list                         (lists all threads)\n"
                        , " thread terminate <thread>           (terminates <thread>)\n" 
                        , " version                             (displays the system release)\n"
                    ]

{- |
TODO
-}
ttyListShader :: ShaderCreator
ttyListShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"             -<  in_ta
        context         <- getSVP ("ctx")                                       -<  ()        
        (case argc of
            0   -> 
                proc in_ta' -> do
                    value   <- listA $ runInContext context listScopes      -<  ()
                    setVal "/transaction/response_fragment" (show value)    -<< in_ta'
            1   ->
                proc in_ta' -> do
                    ident   <- getVal "/transaction/console/args/_1"        -<  in_ta'
                    value   <- listA $ 
                                runInContext context (listStateTrees ident) -<< ()
                    setVal "/transaction/response_fragment" (show value)    -<< in_ta'
            _   -> 
                returnA
            )                                                               -<< in_ta

{- |
TODO
-}
ttyLoadShader :: ShaderCreator
ttyLoadShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"                     -<  in_ta
        context         <- getSVP ("ctx")                                               -<  ()        
        (case argc of
            2   ->
                proc in_ta' -> do
                    op      <- getVal "/transaction/console/args/_1"                -<  in_ta'
                    ident   <- getVal "/transaction/console/args/_2"                -<  in_ta'
                    (if op == "config"
                        then proc in_ta'' -> do
                            cfg     <- runInContext context getConfig           -<  ()
                            runInContext context $ ident <$! (XmlVal cfg)       -<< ()
                            setVal "/transaction/response_fragment" "Done."     -<< in_ta''
                        else
                            this
                        )                                                           -<< in_ta'
            3   ->
                proc in_ta' -> do
                    op      <- getVal "/transaction/console/args/_1"                -<  in_ta'
                    arg2    <- getVal "/transaction/console/args/_2"                -<  in_ta'
                    ident   <- getVal "/transaction/console/args/_3"                -<  in_ta'                    
                    (case op of
                        "str"   ->
                            (runInContext context $ ident <-! arg2)
                            >>>
                            setVal "/transaction/response_fragment" "Done."                            
                        "xml"   ->
                            proc in_ta'' -> do
                                cfg <- fileSource arg2 
                                        >>> 
                                        normalizeConfig
                                        >>>
                                        getTree "/janus"                        -<< ()
                                runInContext context $ ident <$! (XmlVal cfg)   -<< ()
                                setVal "/transaction/response_fragment" "Done." -<  in_ta''
                        _       ->
                            this
                        )                                                           -<< in_ta'            
            _   -> 
                this
            )                                                                           -<< in_ta

{- |
TODO
-}
ttyShowShader :: ShaderCreator
ttyShowShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"             -<  in_ta
        context         <- getSVP ("ctx")                                       -<  ()        
        (case argc of
            1   ->
                proc in_ta' -> do
                    ident   <- getVal "/transaction/console/args/_1"        -<  in_ta'
                    value   <- listA $ runInContext context (getSV ident)   -<< ()
                    setVal "/transaction/response_fragment" (show value)    -<< in_ta'
            _   -> 
                returnA
            )                                                                   -<< in_ta

{- |
TODO
-}
ttyStoreShader :: ShaderCreator
ttyStoreShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"                     -<  in_ta
        context         <- getSVP ("ctx")                                               -<  ()        
        (case argc of
            2   ->
                proc in_ta' -> do
                    op      <- getVal "/transaction/console/args/_1"                -<  in_ta'
                    ident   <- getVal "/transaction/console/args/_2"                -<  in_ta'
                    (if op == "config"
                        then proc in_ta'' -> do
                            (XmlVal cfg)    <- runInContext context $ 
                                                    getSV ident                 -<< ()
                            runInContext context $ swapConfig                   -<< cfg
                            setVal "/transaction/response_fragment" "Done."     -<  in_ta''
                        else
                            this
                        )                                                           -<< in_ta'
            3   ->
                proc in_ta' -> do
                    op      <- getVal "/transaction/console/args/_1"                -<  in_ta'
                    file    <- getVal "/transaction/console/args/_2"                -<  in_ta'
                    ident   <- getVal "/transaction/console/args/_3"                -<  in_ta'
                    
                    (if op == "xml"
                        then proc in_ta'' -> do
                            (XmlVal cfg)    <- runInContext context $ 
                                                    getSV ident                 -<< ()
                            
                            putXmlDocument file                                 -<< cfg
                            setVal "/transaction/response_fragment" "Done."     -<  in_ta''
                        else
                            this
                        )                                                           -<< in_ta'            
            _   -> 
                this
            )                                                                           -<< in_ta

{- |
TODO
-}
ttySwitchShader :: ShaderCreator
ttySwitchShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"                     -<  in_ta
        context         <- getSVP ("ctx")                                               -<  ()        
        (case argc of
            1   ->
                proc in_ta' -> do
                    op      <- getVal "/transaction/console/args/_1"                -<  in_ta'
                    (case op of
                        "server"   ->
                            proc in_ta'' -> do
                                (ContextVal ctx) <- 
                                    getSV ("/global/consoles/server")           -<  ()
                                "ctx" <*! ctx                                   -<< ()
                                "ctx_name" <-! "server"                         -<  ()
                                setVal "/transaction/response_fragment" "Done." -<  in_ta''
                        "handler"   ->
                            proc in_ta'' -> do
                                value   <- listA $ runInContext context 
                                            (listStateTrees "/global/consoles/handlers")    -<< ()
                                let hdlStr = foldl (\hdl str -> str ++ "\n" ++ hdl) "" value
                                setVal "/transaction/response_fragment" hdlStr  -<< in_ta''
                        _           ->
                            this
                        )                                                           -<< in_ta'            
            2   ->
                proc in_ta' -> do
                    op      <- getVal "/transaction/console/args/_1"                -<  in_ta'
                    handler <- getVal "/transaction/console/args/_2"                -<  in_ta'                    
                    (if op == "handler"
                        then proc in_ta'' -> do
                            (ContextVal ctx) <- 
                                getSV ("/global/consoles/handlers/" ++ handler) -<< ()
                            "ctx" <*! ctx                                       -<< ()
                            "ctx_name" <-! handler                              -<< ()
                            setVal "/transaction/response_fragment" "Done."     -<  in_ta''
                        else
                            this
                        )                                                           -<< in_ta'            
            _   -> 
                this
            )                                                                           -<< in_ta

{- |
TODO
-}
ttyThreadShader :: ShaderCreator
ttyThreadShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"                     -<  in_ta
        context         <- getSVP ("ctx")                                               -<  ()        
        (case argc of
            1   ->
                proc in_ta' -> do
                    op      <- getVal "/transaction/console/args/_1"                -<  in_ta'
                    (if op == "list"
                        then proc in_ta'' -> do
                            value   <- listA $ runInContext context 
                                            (listStateTrees "/global/threads")      -<< ()
                            setVal "/transaction/response_fragment" (show value)    -<< in_ta''
                        else
                            this
                        )                                                           -<< in_ta'            
            2   ->
                proc in_ta' -> do
                    op      <- getVal "/transaction/console/args/_1"                -<  in_ta'
                    thread  <- getVal "/transaction/console/args/_2"                -<  in_ta'                    
                    (if op == "terminate"
                        then proc in_ta'' -> do
                            (ThreadVal threadid)    <- runInContext context $ 
                                getSV ("/global/threads/" ++ thread)            -<< ()
                            arrIO $ killThread                                  -<  threadid
                            setVal "/transaction/response_fragment" "Done."     -<  in_ta''     
                        else
                            this
                        )                                                           -<< in_ta'            
            _   -> 
                this
            )                                                                           -<< in_ta

{- |
TODO
-}
ttyVersionShader :: ShaderCreator
ttyVersionShader =
    mkStaticCreator $ 
    proc in_ta -> do
        (argc :: Int)   <- getValP "/transaction/console/@argcount"             -<  in_ta
        (case argc of
            0   ->
                proc in_ta' -> do
                    version <- getSVS "/global/system/version"              -<  ()
                    setVal "/transaction/response_fragment" 
                            ("version: " ++ version)                        -<< in_ta'
            _   -> 
                returnA
            )                                                                   -<< in_ta


showId :: JanusArrow Context XmlTree (String, String)
showId = 
    proc tree -> do
        showId'     -<  (tree, "", "/janus")

showId' :: JanusArrow Context (XmlTree, String, String) (String, String)
showId' = 
    proc (tree, ident, xpath) -> do
        name        <- getChildren >>> hasName "config" >>> getAttrValue "id"       -<  tree
        let ident' = ident ++ "/" ++ name
        let xpath' = xpath ++ "/shader[config/@id='" ++ name ++ "']"
        children'    <- listA $ (proc _ -> do
            child   <- getChildren 
                        >>> 
                        hasName "shader"                                    -<  tree
            showId'                                                         -<  (child, ident', xpath')
            )                                                                       -<< ()
        constL $ (ident', xpath'):children'                                         -<< ()


-- ==================================================================

{- |
TODO
-}
ttySendShader :: ShaderCreator
ttySendShader =
    mkStaticCreator $ 
    proc in_ta -> do
        returnA -< in_ta
{- 
        (case command of
            "send":state:code:channel:[]    -> 
                proc _  -> do
                    msg_state   <- parseDefA []                             -< state
                    msg_code    <- parseDefA GenericMessage                 -< code
                    let msg     = mkControlMsg msg_state msg_code
                    arrIO $ putStr                                          -< "sending to channel '" ++ channel ++ "': "
                    msg >>> showMsg >>> (arrIO putStrLn)                    -<< ()
                    finallyA 
                        ((channel <-@ msg)
                            <!> ("global", "consoleLoop", GenericMessage, "Invalid channel.", [])
                            )
                        (consoleLoop)                                       -<<  ()
-}





