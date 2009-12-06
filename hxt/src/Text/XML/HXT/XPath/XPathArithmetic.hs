-- |
-- The module contains arithmetic calculations according the IEEE 754 standard
-- for plus, minus, unary minus, multiplication, modulo and division.
--


module Text.XML.HXT.XPath.XPathArithmetic
    ( xPathMulti
    , xPathMod
    , xPathDiv
    , xPathAdd
    , xPathUnary
    )
where

import Text.XML.HXT.XPath.XPathDataTypes


-- |
-- Unary minus: the value 'NaN' is not calculatable and returned unchanged,
-- all other values can be denied.
--

xPathUnary :: XPathFilter
xPathUnary (XPVNumber (Float f)) =  XPVNumber (Float (-f))
xPathUnary (XPVError e)          = XPVError e
xPathUnary (XPVNumber NaN)       = XPVNumber NaN
xPathUnary (XPVNumber Pos0)      = XPVNumber Neg0
xPathUnary (XPVNumber Neg0)      = XPVNumber Pos0
xPathUnary (XPVNumber PosInf)    = XPVNumber NegInf
xPathUnary (XPVNumber NegInf)    = XPVNumber PosInf
xPathUnary _                     = XPVError "Call to unaryEval without a number"


-- |
-- Multiplication
--

xPathMulti :: Op -> XPathValue -> XPathFilter
xPathMulti _ (XPVNumber (Float a)) (XPVNumber (Float b))
    = XPVNumber (Float (a * b))
xPathMulti _ (XPVNumber NegInf) (XPVNumber (Float a))
    | a < 0     = XPVNumber PosInf
    | otherwise = XPVNumber NegInf
xPathMulti _ (XPVNumber PosInf) (XPVNumber (Float a))
    | a < 0     = XPVNumber NegInf
    | otherwise = XPVNumber PosInf
xPathMulti _ (XPVNumber (Float a)) (XPVNumber NegInf)
    | a < 0     = XPVNumber PosInf
    | otherwise = XPVNumber NegInf
xPathMulti _ (XPVNumber (Float a)) (XPVNumber PosInf)
    | a < 0     = XPVNumber NegInf
    | otherwise = XPVNumber PosInf
xPathMulti _ (XPVNumber Pos0) (XPVNumber (Float a))
    | a < 0     = XPVNumber Neg0
    | otherwise = XPVNumber Pos0
xPathMulti _ (XPVNumber Neg0) (XPVNumber (Float a))
    | a < 0     = XPVNumber Pos0
    | otherwise = XPVNumber Neg0
xPathMulti _ (XPVNumber (Float a)) (XPVNumber Pos0)
    | a < 0     = XPVNumber Neg0
    | otherwise = XPVNumber Pos0
xPathMulti _ (XPVNumber (Float a)) (XPVNumber Neg0)
    | a < 0     = XPVNumber Pos0
    | otherwise = XPVNumber Neg0
xPathMulti a b c = xPathSpez a b c


-- |
-- Modulo
--

xPathMod :: Op -> XPathValue -> XPathFilter
xPathMod _ (XPVNumber (Float a)) (XPVNumber (Float b))
    | floatMod a b == 0 = XPVNumber Pos0
    | otherwise = XPVNumber (Float (floatMod a b))
      where
      floatMod x y
        | x/y >= 0 = x - y * fromInteger(floor (x / y))
        | otherwise =x - y * fromInteger(ceiling (x / y))

xPathMod _ (XPVNumber (Float a)) (XPVNumber NegInf)
    = XPVNumber (Float a)
xPathMod _ (XPVNumber (Float a)) (XPVNumber PosInf)
    = XPVNumber (Float a)
xPathMod _ (XPVNumber Neg0) (XPVNumber Pos0)
    = XPVNumber Neg0
xPathMod a b c = xPathSpez a b c


-- |
-- Division: the divison-operator is not according the IEEE 754 standard,
-- it calculates the same as the % operator in Java and ECMAScript
--

xPathDiv :: Op -> XPathValue -> XPathFilter
xPathDiv _ (XPVNumber (Float a)) (XPVNumber (Float b))
    = XPVNumber (Float (a / b))
xPathDiv _ (XPVNumber NegInf) (XPVNumber (Float a))
    | a < 0     = XPVNumber PosInf
    | otherwise = XPVNumber NegInf
xPathDiv _ (XPVNumber PosInf) (XPVNumber (Float a))
    | a < 0     = XPVNumber NegInf
    | otherwise = XPVNumber PosInf
xPathDiv _ (XPVNumber (Float a)) (XPVNumber NegInf)
    | a < 0     = XPVNumber Pos0
    | otherwise = XPVNumber Neg0
xPathDiv _ (XPVNumber (Float a)) (XPVNumber PosInf)
    | a < 0     = XPVNumber Neg0
    | otherwise = XPVNumber Pos0
xPathDiv _ (XPVNumber Neg0) (XPVNumber (Float a))
    | a < 0     = XPVNumber Pos0
    | otherwise = XPVNumber Neg0
xPathDiv _ (XPVNumber (Float a)) (XPVNumber Neg0)
    | a < 0     = XPVNumber PosInf
    | otherwise = XPVNumber NegInf
xPathDiv _ (XPVNumber (Float a)) (XPVNumber Pos0)
    | a < 0     = XPVNumber NegInf
    | otherwise = XPVNumber PosInf
xPathDiv a b c  = xPathSpez a b c


-- |
-- Plus and minus
--
--    1.parameter op :  plus or minus operation
--

xPathAdd :: Op -> XPathValue -> XPathFilter
xPathAdd Plus (XPVNumber (Float a)) (XPVNumber (Float b))
    = if a + b == 0
        then XPVNumber Pos0
        else XPVNumber (Float (a+b))
xPathAdd Minus (XPVNumber (Float a)) (XPVNumber (Float b))
    = if a - b == 0
        then XPVNumber Pos0
        else XPVNumber (Float (a-b))
xPathAdd _ (XPVNumber PosInf) (XPVNumber NegInf)  = XPVNumber NaN
xPathAdd _ (XPVNumber NegInf) (XPVNumber PosInf)  = XPVNumber NaN
xPathAdd _ (XPVNumber PosInf) _                   = XPVNumber PosInf
xPathAdd _ (XPVNumber NegInf) _                   = XPVNumber NegInf
xPathAdd _ _ (XPVNumber PosInf)                   = XPVNumber PosInf
xPathAdd _ _ (XPVNumber NegInf)                   = XPVNumber NegInf
xPathAdd _ (XPVNumber (Float a)) (XPVNumber Pos0) = XPVNumber (Float a)
xPathAdd op (XPVNumber Pos0) (XPVNumber (Float a))
    | op == Minus = XPVNumber (Float (-a))
    | otherwise   = XPVNumber (Float a)
xPathAdd op (XPVNumber Neg0) (XPVNumber (Float a))
    | op == Minus = XPVNumber (Float (-a))
    | otherwise   = XPVNumber (Float a)
xPathAdd _ (XPVNumber (Float a)) (XPVNumber Neg0) = XPVNumber (Float a)
xPathAdd _ (XPVNumber Neg0) (XPVNumber Pos0)      = XPVNumber Neg0
xPathAdd _ (XPVNumber Pos0) (XPVNumber Neg0)      = XPVNumber Neg0
xPathAdd _ (XPVNumber Neg0) (XPVNumber Neg0)      = XPVNumber Neg0
xPathAdd _ (XPVNumber Pos0) (XPVNumber Pos0)      = XPVNumber Pos0
xPathAdd a b c = xPathSpez a b c


-- |
-- Identically results of the operators are combined to get
-- as few as possible combinations of the special IEEE values
--
xPathSpez :: Op -> XPathValue -> XPathFilter
xPathSpez _ (XPVError e) _ = XPVError e
xPathSpez _ _ (XPVError e) = XPVError e
xPathSpez _ _ _            = XPVNumber NaN
