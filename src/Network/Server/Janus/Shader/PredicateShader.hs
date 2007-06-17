-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.PredicateShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: PredicateShader.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus Predicate Shaders
   
   These Shaders represent predicates over Transactions, i.e., Shaders letting a Transaction pass to denote True and failing to
   denote False. 

-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.PredicateShader
    (
    -- predicate shaders
      truePredicate
    , falsePredicate
    , negPredicate
    , taPredicate
    , taTreePredicate
    , statePredicate
    , errorPredicate
    , matchPredicate
    , logicPredicate
    , comparePredicate
    )
where

import Data.Maybe
import Text.Regex
import Text.XML.HXT.Arrow
        
import Network.Server.Janus.Core
import Network.Server.Janus.Messaging
import Network.Server.Janus.Transaction
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

{- |
A Shader simply forwarding its Transaction to always denote True.
-}
truePredicate :: ShaderCreator
truePredicate =
    mkFallibleCreator $ arr $ \(_, _) -> 
        this

{- |
A Shader failing independently of its Transaction to always denote False.
-}
falsePredicate :: ShaderCreator
falsePredicate =
    mkFallibleCreator $ arr $ \(_, _) -> 
        zeroArrow

{- |
A Shader negating a sub-Shader \"predicate\", i.e. delivering the Transaction if the sub-Shader fails and failing if
the sub-Shader returns the Transaction. If no sub-Shader is present, this Shader equals the truePredicate.
-}
negPredicate :: ShaderCreator
negPredicate =
    mkFallibleCreator $ proc (_, associations) -> do              
        let shader = proc in_ta -> do
            let predicate = lookupShader "predicate" idShader associations
            neg $ predicate                                     -<< in_ta
        returnA                                                         -<  shader    

{- |
A Shader checking the existence of a Transaction value denoted by \/config\/\@path. The Shader fails if no path is 
configured.
-}
taPredicate :: ShaderCreator
taPredicate = 
    mkFallibleCreator $ arr $ \(conf, _) -> 
    proc in_ta -> do
        path    <- getVal _shader_config_path                   -<  conf
        ifA (getVal $ jp path) (this) (zeroArrow)               -<< in_ta
        
{- |
A Shader checking the existence of a Transaction subtree denoted by \/config\/\@path. The Shader fails if no path is 
configured.
-}
taTreePredicate :: ShaderCreator
taTreePredicate = 
    mkFallibleCreator $ arr $ \(conf, _) -> 
    proc in_ta -> do
        path    <- getVal _shader_config_path                   -<  conf
        ifA (getTree $ jp path) (this) (zeroArrow)              -<< in_ta
        
{- |
A Shader checking the existence of a state value denoted by \/config\/\@path. The Shader fails if no path is configured.
-}
statePredicate :: ShaderCreator
statePredicate = 
    mkFallibleCreator $ arr $ \(conf, _) ->
    proc in_ta -> do
        path    <- getVal _shader_config_path                   -<  conf
        ifA (getSV path) (this) (zeroArrow)                 	-<< in_ta

{- |
A Shader succeeding if a Transaction contains error messages.
-}
errorPredicate :: ShaderCreator
errorPredicate = 
    mkFallibleCreator $ arr $ \(_, _) ->
    proc in_ta -> do
        msgs    <- listA $ getTAMsg 
                   >>> 
                   getMsgTypeFilter ErrorMsg                    -<  in_ta
        (case msgs of 
            []      -> zeroArrow
            _:_     -> this
            )                                                   -<< in_ta
        
{- |
A Shader succeeding if the string delivered by a sub-Expression Shader \"expr\" matches a regular expression
defined by \/config\/\@match. The regular expression defaults to \".*\". The Shader fails if no \"expr\" Shader
exists.
-}
matchPredicate :: ShaderCreator
matchPredicate = 
    mkFallibleCreator $ proc (conf, associations) -> do              
        let shader = proc in_ta -> do
            let expr = lookupShader "expr" zeroArrow associations
            val     <- expr >>> getVal _value                   -<< in_ta
            match   <- getValDef _shader_config_match ".*"      -<  conf
            (if txtMatch val match 
                then this
                else zeroArrow
                )                                               -<< in_ta
        returnA                                                         -<  shader    
        
{- |
A Shader comparing the integer values delivered by two sub-Expression Shaders \"left\" and \"right\" by a given 
operator. The operator is defined by \/config\/\@op and defaults to ==. If \"left\" or \"right\" do not exist,
the Shader fails. Supported operators are: ==, \/=, >, >=, <, <=.
-}
comparePredicate :: ShaderCreator
comparePredicate = 
    mkFallibleCreator $ proc (conf, associations) -> do              
        let shader = proc in_ta -> do
            op      <- getValDef _shader_config_op "=="         -<  conf
            let l_shader  = lookupShader "left"  zeroArrow associations
            let r_shader  = lookupShader "right" zeroArrow associations
            (left' :: Int)  <- l_shader >>> getValP _value      -<< in_ta
            (right' :: Int) <- r_shader >>> getValP _value      -<< in_ta

            let op' = case op of 
                        "=="    -> (==)
                        "/="    -> (/=)
                        ">"     -> (>)
                        ">="    -> (>=)
                        "<"     -> (<)
                        "<="    -> (<=)
                        _       -> (==)

            (if (op' left' right') 
                then (returnA) 
                else (zeroArrow)
                )                                               -<< in_ta
        returnA                                                         -<  shader    

{- |
A Shader combining two sub-Predicate Shaders \"left\" and \"right\" by a given operator. The operator is defined by \/config\/\@op and 
defaults to &&. If \"left\" or \"right\" do not exist, the Shader fails. Supported operators are: &&, ||.
-}
logicPredicate :: ShaderCreator
logicPredicate = 
    mkFallibleCreator $ proc (conf, associations) -> do            
        let shader = proc in_ta -> do
            op      <- getValDef _shader_config_op "&&"         -<  conf
            let l_shader  = lookupShader "left"  zeroArrow associations
            let r_shader  = lookupShader "right" zeroArrow associations
            left'   <- (l_shader >>> constA True) 
                        `orElse` 
                        (constA False)                          -<< in_ta
            right'  <- (r_shader >>> constA True) 
                        `orElse` 
                        (constA False)                          -<< in_ta

            let op' = case op of 
                        "&&"    -> (&&)
                        "||"    -> (||)
                        _       -> (&&)

            (if (op' left' right') 
                then (returnA) 
                else (zeroArrow)
                )                                               -<< in_ta
        returnA                                                         -<  shader    
        
{- |
Helper function to check a value (first argument) against a regular expression (second argument). If the regular expression matches the
value, True is returned, otherwise False.
-} 
txtMatch :: String -> String -> Bool
txtMatch val regex =
    isJust $ matchRegex (mkRegex regex) val


