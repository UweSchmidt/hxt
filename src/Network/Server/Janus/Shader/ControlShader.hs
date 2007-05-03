-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.ControlShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ControlShader.hs, v1.1 2007/04/28 00:00:00 janus Exp $

   Janus Control Shaders. 
   
   These Shaders are intended to compose their Subshaders to a new Shader, for example by means of sequence, selection or iteration.
-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.ControlShader
   (
   -- control shaders
     seqControl
   , likeControl
   , selectControl
   , loopUntilControl
   , loopWhileControl
   , ifThenElseControl
   )
where

import Data.Maybe
import Data.Map
import Text.Regex
import Text.XML.HXT.Arrow

import Network.Server.Janus.Core
import Network.Server.Janus.XmlHelper
      
{- |
Composes the sub-Shaders to a single Shader, delivering the effect of the original Shaders in sequence. The order of
execution is defined by the order in the Associations argument respectively the order of XML children in the
configuration file. 
-}
seqControl :: ShaderCreator
seqControl =
    mkFallibleCreator $ proc (_, associations) -> do 
        returnA                             -<  seqA (shaderList associations)
      
{- |
TODO REFRESH

Dynamically selects a sub-Shader based on a regular expression match against a value. The value is delivered by an Expression
Shader, which has id \"value\". A differing id may be defined by \/shader\/config\/\@value. The value Shader is applied to the Transaction first,
afterwards a sub-Shader is selected by interpreting their ids as regular expressions to match the value delivered. The first matching
sub-Shader is selected (therefore, order of sub-Shaders is significant). If no sub-Shader id matches, the sub-Shader with id \"default\"
is taken. If no sub-Shader matches and no default Shader exists, the whole Shader fails. A differing id may be defined for the default
Shader by \/shader\/config\/\@default.
-}
likeControl :: ShaderCreator
likeControl = 
    mkFallibleCreator $ proc (conf, associations) -> do 
        select  <- listA $ listValPairs "/shader/config/*"                      -<  conf
        value   <- getValDef "/shader/config/@value" "value"                    -<  conf
        def     <- getValDef "/shader/config/@default" "default"                -<  conf
        let select' = Prelude.map (\(name, regex) -> (regex, lookupShader name zeroArrow associations)) select
        let valueShader   = lookupShader value zeroArrow associations
        let defaultShader = lookupShader def   zeroArrow associations
        let shader = proc in_ta -> do
            compareTree     <- valueShader                              -<< in_ta
            comp            <- getVal "/value"                          -<  compareTree
            let result = foldl (\current (expr, hit) -> if valMatch comp expr then (Just hit) else current) 
                               (Nothing)
                               select'
            (if isNothing result
                then defaultShader
                else fromJust result)                                           -<< in_ta
        returnA                                                                 -<  shader

{- |
TODO REFRESH

Dynamically selects a sub-Shader based on a value. The value is delivered by an Expression Shader, which has id \"value\". A differing 
id may be defined by \/shader\/config\/\@value. The value Shader is applied to the Transaction first, afterwards a sub-Shader is selected by 
matching its id against the value delivered. In contrast to caseMatch this is no regular expression match but an exact match. Order is 
insignificant, behaviour of the Shader is undefined if sub-Shaders share an id. If no sub-Shader id matches, the sub-Shader with id \"default\"
is taken. If no sub-Shader matches and no default Shader exists, the whole Shader fails. A differing id may be defined for the default
Shader by \/shader\/config\/\@default.
-}
selectControl :: ShaderCreator
selectControl =
    mkFallibleCreator $ proc (conf, associations) -> do
        select  <- listA $ listValPairs "/shader/config/*"                      -<  conf
        value   <- getValDef "/shader/config/@value"   "value"                  -<  conf
        def     <- getValDef "/shader/config/@default" "default"                -<  conf
        let valueShader   = lookupShader value zeroArrow associations
        let defaultShader = lookupShader def   zeroArrow associations
        let select' = Prelude.map (\(name, regex) -> (regex, lookupShader name zeroArrow associations)) select
        let cases = fromList select'
        let shader = proc in_ta -> do
            compareTree <- valueShader                                  -<< in_ta
            comp        <- getVal "/value"                              -<  compareTree
            findWithDefault defaultShader comp cases                    -<< in_ta
        returnA                                                                 -<  shader

{- |
Repeatedly applies a Shader denoted by id \"body\" to the Transaction based on a Predicate Shader denoted by id \"predicate\". The body Shader
is applied at least one time. The loop continues as long as the predicate fails and terminates when the predicate holds.
-}
loopUntilControl :: ShaderCreator
loopUntilControl =
    mkFallibleCreator $ loopUntilControl'
    where
        loopUntilControl' = proc (conf, associations) -> do            
            let shader = proc in_ta -> do
                let predicate = lookupShader "predicate" (idShader) associations
                let operation = lookupShader "body"      (idShader) associations
                (operation >>> ifA predicate 
                                    (returnA)                            
                                    (proc in_ta' -> do
                                        shader'     <- loopUntilControl'    -<  (conf, associations)
                                        shader'                             -<< in_ta'
                                        )
                                    )                                               -<< in_ta
            returnA                                                                         -<  shader

{- |
Repeatedly applies a Shader denoted by id \"body\" to the Transaction based on a Predicate Shader denoted by id \"predicate\". The loop 
is repeated as long as the predicate holds. As the predicate is checked before the body is applied, the body may be applied zero
times.
-}
loopWhileControl :: ShaderCreator
loopWhileControl = 
    mkFallibleCreator $ loopWhileControl'
    where
        loopWhileControl' = proc (conf, associations) -> do            
            let shader = proc in_ta -> do
                let predicate = lookupShader "predicate" (idShader) associations
                let operation = lookupShader "body"      (idShader) associations
                ifA predicate 
                    (operation 
                        >>> 
                        proc in_ta' -> do
                            shader'     <- loopWhileControl'    -<  (conf, associations)
                            shader'                             -<< in_ta'
                        ) 
                    (returnA)                                           -<< in_ta
            returnA                                                             -<  shader
         
{- |
Dynamically selects a sub-Shader based on a predicate denoted by id \"predicate\". The predicate Shader is applied to the Transaction first. If
the predicate holds, the sub-Shader with id \"then\" is applied. Otherwise the sub-Shader with id "else" is applied. A non-existing Shader
of id \"then\" or \"else\" is replaced by the identity Shader (meaning that a selected and missing Shader leaves the Transaction unaffected and
continues execution).
-} 
ifThenElseControl :: ShaderCreator
ifThenElseControl = 
    mkFallibleCreator $ proc (_, associations) -> do        
        let shader = proc in_ta -> do
            let predicate = lookupShader "predicate" (idShader) associations
            let thenCase  = lookupShader "then"      (idShader) associations 
            let elseCase  = lookupShader "else"      (idShader) associations
            ifA predicate thenCase elseCase                             -<< in_ta
        returnA                                                                 -<  shader

{- |
Helper function to check a value (first argument) against a regular expression (second argument). If the regular expression matches the
value, True is returned, otherwise False.
-}    
valMatch :: String -> String -> Bool
valMatch val regex = 
   isJust match
   where
      match = matchRegex (mkRegex regex) val

