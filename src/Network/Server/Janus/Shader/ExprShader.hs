-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.Shader.ExprShader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: ExprShader.hs, v1.1 2007/03/26 00:00:00 janus Exp $

   Janus Expression Shaders
   
   These Shaders represent expressions, delivering string values. They are especially useful to compute values to be used 
   by Control Shaders. The string value denoted by an Expression Shader is delivered by means of an XML tree with a root node
   \"value\" and a text node child containing the value.
-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.Shader.ExprShader
    (
    -- expression shaders
      constExpr
    , binaryIntExpr
    , unaryIntExpr
    , binaryStrExpr
    , stateExpr
    , taExpr
    )
where

import Text.XML.HXT.Arrow
        
import Network.Server.Janus.Core as Shader
import Network.Server.Janus.XmlHelper
import Network.Server.Janus.JanusPaths

{- |
Ignores the input Transaction and deliveres the string value denoted by \/shader\/config\/\@value. If there is no value configured 
the empty string is returned.
-}
constExpr :: ShaderCreator
constExpr =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc _ -> do
        val     <- getValDef _shader_config_value ""            -<  conf 
        eelem "value" 
            += txt val                                          -<< ()

{- |
A sub-Shader \"left\" and a sub-Shader \"right\" are invoked with the input Transaction to deliver two string values. Order of applicaton
is undefined and as both sub-Shaders are invoked with the original input Transaction, they shall not rely on side effects of each
other. The two values are combined by means of a binary arithmetic operation (defaults to +). The operation can be overriden by 
\/shader\/config\/\@op, where +, -, div and * are valid operators. Both sub-Shaders are expected to return strings denoting integers. If this is not 
the case or a sub-Shader is actually missing, the whole Shader fails. An unknown operator is translated to the default operator +.
-}
binaryIntExpr :: ShaderCreator
binaryIntExpr =
    mkDynamicCreator $ proc (conf, associations) -> do         
        let shader = proc in_ta -> do
            op      <- getValDef _shader_config_op "+"              -<  conf
            let l_shader = lookupShader "left"  zeroArrow associations 
            let r_shader = lookupShader "right" zeroArrow associations
            (left' :: Int)  <- l_shader >>> getValP _value          -<< in_ta
            (right' :: Int) <- r_shader >>> getValP _value          -<< in_ta

            let op' = case op of 
                        "+" -> (+)
                        "-" -> (-)
                        "/" -> (div)
                        "*" -> (*)
                        _   -> (+)

            eelem "value" 
                += txt (show $ op' left' right')                    -<< ()
        returnA                                                             -<  shader    
            
{- |
A sub-Shader \"expr\" is invoked with the input Transaction to deliver a string value. This value is transformed by means of a unary 
arithmetic operation (defaults to -). The operation can be overriden by \/shader\/config\/\@op, however the negation is the only operator currently 
supported. The sub-Shader is expected to return a string denoting an integer. If this is not the case or the sub-Shader is actually 
missing, the whole Shader fails. An unknown operator is translated to the default operator -.
-}
unaryIntExpr :: ShaderCreator
unaryIntExpr =
    mkDynamicCreator $ proc (conf, associations) -> do            
        let shader = proc in_ta -> do
            op      <- getValDef _shader_config_op "-"              -<  conf
            let expr = lookupShader "expr" zeroArrow associations 
            (expr' :: Int) <- expr >>> getValP _value               -<< in_ta

            let op' = case op of 
                        "-" -> negate
                        _   -> negate

            eelem "value" 
                += txt (show $ op' expr')                           -<< ()
        returnA                                                             -<  shader    

{- |
A sub-Shader \"left\" and a sub-Shader \"right\" are invoked with the input Transaction to deliver two string values. Order of applicaton
is undefined and as both sub-Shaders are invoked with the original input Transaction, they shall not rely on side effects of each
other. The two values are combined by means of a binary string operation (defaults to ++). The operation can be overriden by \/shader\/config\/\@op, 
however the concatenation is the only operator currently supported. If a sub-Shader is actually missing, the whole Shader fails.
An unknown operator is translated to the default operator ++.
-}
binaryStrExpr :: ShaderCreator
binaryStrExpr =
    mkDynamicCreator $ proc (conf, associations) -> do           
        let shader = proc in_ta -> do
            op      <- getValDef _shader_config_op "++"             -<  conf
            let l_shader = lookupShader "left"  zeroArrow associations
            let r_shader = lookupShader "right" zeroArrow associations 
            left'   <- l_shader >>> getVal _value                   -<< in_ta
            right'  <- r_shader >>> getVal _value                   -<< in_ta

            let op' = case op of 
                        "++" -> (++)
                        _    -> (++)

            eelem "value" 
                += txt (op' left' right')                           -<< ()
        returnA                                                             -<  shader    
            
{- |
Delivers a string value from the state. A default value may be defined by \/shader\/config\/\@default, which defaults to the empty string. 
The node in question is defined by \/shader\/config\/\@path. The Shader fails if no path is specified.
-}
stateExpr :: ShaderCreator
stateExpr =
    mkDynamicCreator $ arr $ \(conf, _) ->
    proc _ -> do
        def     <- getValDef _shader_config_default ""          -<  conf 
        path    <- getVal    _shader_config_path                -<  conf 
        val     <- (getSVS path) 
                   `orElse` 
                   (constA def)                                 -<< ()
        eelem "value" 
            += txt val                                          -<< ()

{- |
Delivers a string value from the current Transaction. A default value may
be defined by \/shader\/config\/\@default, which defaults to the empty
string. The node in question may be defined by \/shader\/config\/\@path.
The Shader fails if no path is specified.
-}
taExpr :: ShaderCreator
taExpr = 
    mkDynamicCreator $ arr $ \(conf, _) -> 
    proc in_ta -> do
        def     <- getValDef _shader_config_default ""          -<  conf
        path    <- getVal    _shader_config_path                -<  conf
        val     <- getValDef (jp path) def                      -<< in_ta
        eelem "value" 
            += txt val                                          -<< ()

