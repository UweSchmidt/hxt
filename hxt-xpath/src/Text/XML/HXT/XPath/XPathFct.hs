-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XPath.XPathFct
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   The module contains the core-functions of the XPath function library.
   All functions are implemented as XFct. Each XFct contains the evaluation context,
   the variable environment and the function arguments.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XPath.XPathFct
    ( XFct
    , evalFct
    , toXValue
    , xnumber
    , xboolean
    , xstring
    , getConvFct
    , stringValue
--    , remDups
    , isNotInNodeList
{-
    , createDocumentOrder
    , createDocumentOrderReverse
-}
    , getVarTab
    , getKeyTab
    )
where

import Text.XML.HXT.XPath.XPathDataTypes
import Text.XML.HXT.XPath.XPathParser
    ( parseNumber
    )
import Text.XML.HXT.XPath.XPathArithmetic
    ( xPathAdd
    )

import Control.Arrow                            ( (>>>), (<+>) )
import Control.Arrow.ArrowList                  ( constA )
import Control.Arrow.ArrowIf                    ( ifA )
import Control.Arrow.ArrowTree                  ( deep )
import Control.Arrow.ListArrow                  ( LA, runLA )

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.ReadDocument          (readDocument)
import Text.XML.HXT.Arrow.XmlIOStateArrow       (runX)

import           Text.XML.HXT.DOM.Interface
import qualified Text.XML.HXT.DOM.XmlNode as XN

import System.IO.Unsafe                         ( unsafePerformIO
                                                )

import Data.Char                                ( isAscii
                                                , isUpper
                                                , isLower
                                                , isDigit
                                                , ord
                                                )

import Data.Maybe

-- -----------------------------------------------------------------------------

-- added by Tim Walkenhorst to fix Pos0 vs. Float 0.0 problems...

int2XPNumber :: Int -> XPNumber
int2XPNumber 0 = Pos0
int2XPNumber i = Float $ fromIntegral i

-- |
-- Type signature for all functions which can be used in the XPath module.

type XFct = (Context -> Env -> [XPathValue] -> XPathValue)

-- |
-- All functions are stored in a function table.

type FctTable = [(FctName, FctTableElem)]

-- |
-- Each table entry consists of the function and the expected function arguments.

type FctTableElem = (XFct, CheckArgCount)

-- |
-- Tests whether the number of current function arguments is correct

type CheckArgCount = ([XPathValue] -> Bool)

-- -----------------------------------------------------------------------------

zero
 , zeroOrOne
 , one
 , two
 , twoOrM
 , twoOrThree
 , three :: CheckArgCount

zero ex       = length ex == 0
zeroOrOne ex  = length ex == 0 || length ex == 1
one ex        = length ex == 1
two ex        = length ex == 2
twoOrM ex     = length ex >= 2
twoOrThree ex = length ex == 2 || length ex == 3
three ex      = length ex == 3


-- -----------------------------------------------------------------------------
-- |
-- The core-functions library

fctTable :: FctTable
fctTable = [
            ("last", (xlast, zero)), -- nodeset functions
            ("position",(xposition, zero)),
            ("count",(xcount, one)),
            ("id", (xid, one)),
            ("local-name", (xlocalName, zeroOrOne)),
            ("namespace-uri", (xnamespaceUri, zeroOrOne)),
            ("name", (xname, zeroOrOne)),

            ("string", (xstring, zeroOrOne)), -- string functions
            ("concat", (xconcat, twoOrM)),
            ("starts-with",(xstartsWith, two)),
            ("contains", (xcontains, two)),
            ("substring-before", (xsubstringBefore, two)),
            ("substring-after", (xsubstringAfter, two)),
            ("substring", (xsubstring, twoOrThree)),
            ("string-length", (xstringLength, zeroOrOne)),
            ("normalize-space", (xnormalizeSpace, zeroOrOne)),
            ("translate", (xtranslate, three)),

            ("boolean", (xboolean, one)), -- boolean functions
            ("not", (xnot, one)),
            ("true", (xtrue, zero)),
            ("false",(xfalse, zero)),
            ("lang", (xlang, one)),

            ("number",(xnumber, zeroOrOne)), -- number functions
            ("sum",(xsum, one)),
            ("floor",(xfloor, one)),
            ("ceiling",(xceiling, one)),
            ("round",(xround, one)),
            ("key",(xkey, two)),
            ("format-number",(xformatNumber, twoOrThree)),

            ("document", (xdocument, one)),-- extension functions for xslt 1.0
            ("generate-id", (xgenerateId, zeroOrOne))

           ]

-- -----------------------------------------------------------------------------

-- some helper functions

-- |
-- Returns the table of keys, needed by xslt, from the environment

getKeyTab :: Env -> KeyTab
getKeyTab (_, keyTab) = keyTab


-- -----------------------------------------------------------------------------
-- |
-- Returns the table of variables from the environment

getVarTab :: Env -> VarTab
getVarTab (varTab, _) = varTab


-- -----------------------------------------------------------------------------
-- |
-- Returns the conversion function for the XPath results: string, boolean and number
-- A nodeset can not be converted.

getConvFct :: XPathValue -> Maybe XFct
getConvFct (XPVNumber _) = Just xnumber
getConvFct (XPVString _) = Just xstring
getConvFct (XPVBool _)   = Just xboolean
getConvFct _             = Nothing

-- -----------------------------------------------------------------------------
-- |
-- Check whether a node is not a part of a node list. Needed to implement matching & testing in xslt.

isNotInNodeList :: NavXmlTree -> [NavXmlTree] -> Bool
isNotInNodeList n xs' = nodeID' n `notElem` map nodeID' xs'

-- -----------------------------------------------------------------------------
-- |
-- calculate an ID for a NODE
--
--    - returns : a list of numbers, one number for each level of the tree

-- Tim Walkenhorst:
--   - Attributes are identified by their QName (they do not have previous siblings)
--   - Elements are identified by their relative position (# of previous siblings)

data IdPathStep         = IdRoot String
                        | IdPos Int
                        | IdAttr QName
                          deriving (Show, Eq, Ord)

nodeID                  :: Maybe NavXmlTree -> [IdPathStep]
nodeID                  = maybe [] nodeID'

nodeID'                 :: NavXmlTree -> [IdPathStep]
nodeID' t@(NT (NTree (XAttr qn) _) _ix _ _ _)
                        = IdAttr qn : nodeID (upNT t)

nodeID' t@(NT node ix _ _ _)
   | XN.isRoot node     = return $ IdRoot (getRootId node)
   | otherwise          = IdPos ix : nodeID (upNT t)
   where
   getRootId            = concat . runLA (getAttrValue "rootId")

-- -----------------------------------------------------------------------------
-- |
-- Evaluates a function.
-- Calculation of the function value is done by looking up the function name in the function table,
-- check the number of arguments and calculate the funtion, if no
-- argument evaluation returns an error.
--
--    - returns : the function value as 'XPathValue'

evalFct :: FctName -> Env -> Context -> [XPathValue] -> XPathValue
evalFct name env cont args
    = case (lookup name fctTable) of
        Nothing -> XPVError ("Call to undefined function "++ name)
        Just (fct, checkArgCount) ->
          if not (checkArgCount args)
            then XPVError ("Call to function "++ name ++ " with wrong arguments")
            else case (checkArgErrors args) of
                   Just e  -> e
                   Nothing -> fct cont env args
      where
      checkArgErrors [] = Nothing
      checkArgErrors ((XPVError r):_) = Just (XPVError r)
      checkArgErrors (_:xs) = checkArgErrors xs


-- |
-- Converts a list of different 'XPathValue' types in a list of one 'XPathValue' type.
--
--    * 1.parameter fct :  the conversion function
--

toXValue                        :: XFct -> Context -> Env -> [XPathValue] -> [XPathValue]
toXValue fct c env args         = [fct c env [x] | x <- args]

-- -----------------------------------------------------------------------------
-- core-funktions library

-- nodeset functions

-- |
-- number last(): returns a number equal to the context size from the expression evaluation context

xlast                           :: XFct
xlast (_, len , _) _ _          = XPVNumber $ int2XPNumber len


-- -----------------------------------------------------------------------------
-- |
-- number position(): returns a number equal to the context position from the expression evaluation context

xposition                       :: XFct
xposition (pos, _ , _) _ _      = XPVNumber $ int2XPNumber pos


-- -----------------------------------------------------------------------------
-- |
-- number count(node-set): returns the number of nodes in the argument node-set

xcount                          :: XFct
xcount _ _ [XPVNode ns]         = XPVNumber . int2XPNumber . cardNodeSet $ ns
xcount _ _ _                    = XPVError "Call to function count with wrong arguments"


-- -----------------------------------------------------------------------------
-- |
-- node-set id(object): selects elements by their unique ID

xid                             :: XFct
xid (_, _, cn) env [XPVNode ns] = isInId  (getIds env) (strValues ns) [cn]
                                  where
                                  strValues = map ((\ (XPVString str) -> str) . stringValue) . fromNodeSet

xid c@(_, _, cn) env arg        = isInId (getIds env) ( (\(XPVString s) -> words s) (xstring c env arg)) [cn]



-- -----------------------------------------------------------------------------
-- |
-- returns all IDs from the variable environment as a list of strings.
-- the IDs are stored in the variable: idAttr

getIds                          :: Env -> [String]
getIds env                      = words $                       -- hier muss noch auf prefix getestet werden
                                  (\ (XPVString str) -> str) . fromJust $ lookup ("", "idAttr") $
                                  getVarTab env

-- -----------------------------------------------------------------------------

isInId                          :: [String] -> [String] -> NavXmlTrees -> XPathValue
isInId ids str                  = XPVNode . toNodeSet . concatMap (filterNS ids str . descendantOrSelfAxis)


-- -----------------------------------------------------------------------------

filterNS                        :: [String] -> [String] -> NavXmlTrees -> NavXmlTrees
filterNS ids str ns             = [ n | n@(NT a _ _ _ _) <- ns
                                      , or $ map (idInIdList a str) ids
                                  ]
      where
      idInIdList                :: XmlTree -> [String] -> String -> Bool
      idInIdList al str' b      = (getValue b al) `elem` str'


-- -----------------------------------------------------------------------------
-- |
-- string local-name(node-set?):
-- returns the local part of the expanded-name of the node in the argument node-set
-- that is first in document order.
-- If the argument node-set is empty or the first node has no expanded-name, an empty string is returned.
-- If the argument is omitted, it defaults to a node-set with the context node as its only member

--   Bugfix: name(\/) is "" not "\/"!

xlocalName                      :: XFct
xlocalName (_, _, cn) _ []      = XPVString (xpLocalPartOf . subtreeNT $ cn)
xlocalName _ _ [XPVNode ns]
    | nullNodeSet ns            = XPVString ""
    | otherwise                 = XPVString (xpLocalPartOf . subtreeNT . headNodeSet $ ns)
xlocalName _ _ _                = XPVError "Call to function local-name with wrong arguments"

-- -----------------------------------------------------------------------------
-- |
-- string namespace-uri(node-set?):
-- returns the namespace URI of the expanded-name of the node in the argument node-set
-- that is first in document order.
-- If the argument node-set is empty, the first node has no expanded-name,
-- or the namespace URI of the expanded-name
-- is null, an empty string is returned. If the argument is omitted,
-- it defaults to a node-set with the context node as its only member

xnamespaceUri                   :: XFct
xnamespaceUri (_, _, cn) _ []   = XPVString (xpNamespaceOf . subtreeNT $ cn)
xnamespaceUri _ _ [XPVNode ns]
    | nullNodeSet ns            = XPVString ""
    | otherwise                 = XPVString (xpNamespaceOf . subtreeNT . headNodeSet $ ns)
xnamespaceUri _ _ _             = XPVError "Call to function namespace-uri with wrong arguments"


-- -----------------------------------------------------------------------------
-- |
-- string name(node-set?):
-- returns a string containing a QName representing the expanded-name of the node
-- in the argument node-set
-- that is first in document order. If the argument node-set is empty or the first
-- node has no expanded-name,
-- an empty string is returned. If the argument it omitted, it defaults to a node-set
-- with the context node as its only member.
-- Tim Walkenhorst:

--   Bugfix: name(\/) is "" not "\/"!

xname                           :: XFct
xname (_, _, cn) _ []           = XPVString (xpNameOf . subtreeNT $ cn)
xname _ _ [XPVNode ns]
    | nullNodeSet ns            = XPVString ""
    | otherwise                 = XPVString (xpNameOf . subtreeNT . headNodeSet $ ns)
xname _ _ _                     = XPVError "Call to function name with wrong arguments"

-- ------------------------------------------------------------
-- string functions

-- |
-- some helper functions
getFirstPos                     :: String -> String -> Int
getFirstPos s sub               = if (getFirstPos' s sub) > length s
                                  then -1
                                  else getFirstPos' s sub

-- -----------------------------------------------------------------------------

getFirstPos'                    :: String -> String -> Int
getFirstPos' [] _               = 2
getFirstPos' (x:xs) sub         = if strStartsWith (x:xs) sub
                                  then 0
                                  else 1 + getFirstPos' xs sub

-- -----------------------------------------------------------------------------

strStartsWith                   :: String -> String -> Bool
strStartsWith a b               = take (length b) a == b

-- -----------------------------------------------------------------------------
-- |
-- Returns the string-value of a node,
-- the value of a namespace node is not supported

stringValue                     :: NavXmlTree -> XPathValue
stringValue                     = XPVString . xpTextOf . self

{-
      textFilter
        = getXCmt `orElse`
--        getXNamespace `orElse`
          multi isXText

--        = (isXTag `guards` multi isXText) `orElse`
--          (isXPi `guards` multi isXText) `orElse`
--          (isXAttr `guards` multi isXText) `orElse`
--          (isXText `guards` multi isXText) `orElse`
--          getXCmt
-}


-- -----------------------------------------------------------------------------
-- |
-- string string(object?): converts an object to a string

xstring                                 :: XFct
xstring _ _ [XPVNode ns]
    | nullNodeSet ns                    = XPVString ""
    | otherwise                         = stringValue . headNodeSet $ ns
xstring (_, _, cn) _ []                 = stringValue cn

xstring _ _ [XPVNumber (Float a)]
    | a == (fromInteger $ round a)      = XPVString (show ((round a)::Integer))
    | otherwise                         = XPVString (show a)
xstring _ _ [XPVNumber s]               = XPVString (show s)

xstring _ _ [XPVBool True]              = XPVString "true"
xstring _ _ [XPVBool False]             = XPVString "false"

xstring _ _ [XPVString s]               = XPVString s
xstring _ _ [XPVError e]                = XPVError e
xstring _ _ _                           = XPVError "Call to xstring with a wrong argument"

-- -----------------------------------------------------------------------------
-- |
-- string concat(string, string, string*): returns the concatenation of its arguments

xconcat                                 :: XFct
xconcat c env args                      = XPVString (foldr (\ (XPVString s) -> (s ++)) "" (toXValue xstring c env args))

-- -----------------------------------------------------------------------------
-- |
-- boolean starts-with(string, string):
-- returns true if the first argument string starts
-- with the second argument string, and otherwise returns false

xstartsWith                             :: XFct
xstartsWith c env args                  = XPVBool $
                                          (\ ((XPVString a):[XPVString b]) -> strStartsWith a b) $
                                          toXValue xstring c env args

-- -----------------------------------------------------------------------------
-- |
-- boolean contains(string, string):
-- returns true if the first argument string contains the second argument string,
-- and otherwise returns false

xcontains                               :: XFct
xcontains c env args                    = XPVBool $
                                          (\ ((XPVString s):[XPVString sub]) -> getFirstPos s sub /= -1) $
                                          toXValue xstring c env args


-- -----------------------------------------------------------------------------
-- |
-- string substring-before(string, string):
-- returns the substring of the first argument string that precedes the first occurrence of
-- the second argument string
-- in the first argument string, or the empty string if the first argument string does not
-- contain the second argument string

xsubstringBefore                                        :: XFct
xsubstringBefore c env args                             = xsubstringBefore' c env (toXValue xstring c env args)

xsubstringBefore'                                       :: XFct
xsubstringBefore' _ _ ((XPVString _):[XPVString []])    = XPVString ""
xsubstringBefore' _ _ ((XPVString s):[XPVString sub])   = XPVString (take (getFirstPos s sub) s)
xsubstringBefore' _ _ _                                 = XPVError  "Call to xsubstringBefore' with a wrong argument"

-- -----------------------------------------------------------------------------
-- |
-- string substring-after(string, string):
-- returns the substring of the first argument string that follows the first occurrence of
-- the second argument string
-- in the first argument string, or the empty string if the first argument string does not
-- contain the second argument string

xsubstringAfter                                         :: XFct
xsubstringAfter c env args                              = xsubstringAfter' c env (toXValue xstring c env args)

xsubstringAfter'                                        :: XFct
xsubstringAfter' _ _ ((XPVString s):[XPVString []])     = XPVString s
xsubstringAfter' _ _ ((XPVString s):[XPVString sub])    = if getFirstPos s sub == -1
                                                          then (XPVString "")
                                                          else XPVString (drop ((getFirstPos s sub)+length sub) s)
xsubstringAfter' _ _ _                                  = XPVError "Call to xsubstringAfter' with a wrong argument"


-- -----------------------------------------------------------------------------
-- |
-- string substring(string, number, number?):
-- returns the substring of the first argument starting at the position specified
-- in the second argument
-- with length specified in the third argument. If the third argument is not specified,
-- it returns the substring
-- starting at the position specified in the second argument and continuing to the end of the string.

xsubstring                              :: XFct
xsubstring c env (x:xs)                 = xsubstring' c env ((toXValue xstring c env [x])++(toXValue xnumber c env xs))
xsubstring _ _ _                        = XPVError "Call to xsubstring with a wrong argument"

xsubstring'                             :: XFct
xsubstring' c env ((XPVString s):start:[])
                                        = case xround c env [start] of
                                          XPVNumber NaN       -> XPVString ""
                                          XPVNumber PosInf    -> XPVString ""
                                          XPVNumber (Float f) -> XPVString (drop ((round f)-1) s)
                                          XPVNumber _         -> XPVString s
                                          _                   -> XPVError "Call to xsubstring' with a wrong argument"

xsubstring' c env ((XPVString s):start:[end])
                                        = case xPathAdd Plus (xround c env [start]) (xround c env [end]) of
                                          XPVNumber (Float f) -> xsubstring' c env ( (XPVString (take ((round f) -1) s)):[start])
                                          XPVNumber PosInf    -> xsubstring' c env ( (XPVString s):[start])
                                          XPVNumber _         -> XPVString ""
                                          _                   -> XPVError "Call to xsubstring' with a wrong argument"
xsubstring' _ _ _                       = XPVError "Call to xsubstring' with a wrong argument"


-- -----------------------------------------------------------------------------
-- |
-- number string-length(string?):
-- returns the number of characters in the string. If the argument is omitted,
-- it defaults to the context node
-- converted to a string, in other words the string-value of the context node.

xstringLength                           :: XFct
xstringLength c@(_, _, cn) env []       = XPVNumber (Float (fromIntegral $ length s))
                                          where
                                          XPVString s = xstring c env [XPVNode $ singletonNodeSet cn]

xstringLength c env args                = XPVNumber $
                                          (\[XPVString s] -> int2XPNumber $ length s) $
                                          toXValue xstring c env args


-- -----------------------------------------------------------------------------
-- |
-- string normalize-space(string?):
-- returns the argument string with whitespace normalized by stripping leading
-- and trailing whitespace and replacing sequences
-- of whitespace characters by a single space. If the argument is omitted,
-- it defaults to the context node converted to a string,
-- in other words the string-value of the context node.
-- The string is parsed by a function parseStr from XPathParser module. <-- No longer! Tim Walkenhorst

xnormalizeSpace                         :: XFct
xnormalizeSpace c@(_, _, cn) env []     = (\ (XPVString s) -> XPVString $ normStr s) $
                                          xstring c env [XPVNode $ singletonNodeSet cn]
xnormalizeSpace c env args              = (\ [XPVString s] -> XPVString $ normStr s) $
                                          toXValue xstring c env args

-- Tim Walkenhorst normStr replaces the use of parseStr...
normStr                                 :: String -> String
normStr                                 = unwords . words

-- -----------------------------------------------------------------------------
-- |
-- string translate(string, string, string):
-- returns the first argument string with occurrences of characters in the second argument string replaced by the character at
-- the corresponding position in the third argument string

xtranslate                              :: XFct
xtranslate c env args                   = xtranslate' c env (toXValue xstring c env args)

xtranslate'                             :: XFct
xtranslate' _ _ ((XPVString a):(XPVString b):[XPVString c])
                                        = XPVString (replace a b c)
xtranslate' _ _ _                       = XPVError "Call to xtranslate' with a wrong argument"

replace                                 :: String -> String -> String -> String
replace str [] _                        = str

-- remove all characters, if there is no corresponding character in the third argument
replace str (x:xs) []                   = replace [ s | s <- str, x /= s] xs []

replace str (x:xs) (y:ys)               = replace (rep x y str) xs ys
                                          where -- replace all characters in the first argument
                                          rep :: Char -> Char -> String -> String
                                          rep a b = foldr (\c -> if c == a then (b:) else (c:)) ""

-- ------------------------------------------------------------
-- boolean functions

-- |
-- boolean boolean(object): converts its argument to a boolean value

xboolean                                :: XFct
xboolean _ _ [XPVNumber a]              = XPVBool (a/= NaN && a/= Neg0 && a/= Pos0)
xboolean _ _ [XPVString s]              = XPVBool (length s /= 0)
xboolean _ _ [XPVBool b]                = XPVBool b
xboolean _ _ [XPVNode ns]               = XPVBool (not . nullNodeSet $ ns)
xboolean _ _ [XPVError e]               = XPVError e
xboolean _ _ _                          = XPVError "Call to xboolean with a wrong argument"


-- -----------------------------------------------------------------------------
-- |
-- boolean not(boolean): returns true if its argument is false, and false otherwise
xnot                                    :: XFct
xnot c env args                         = XPVBool ( (\ (XPVBool b) -> not b) (xboolean c env args) )

-- -----------------------------------------------------------------------------
-- |
-- boolean true(): returns true
xtrue                                   :: XFct
xtrue _ _ _                             = XPVBool True

-- -----------------------------------------------------------------------------
-- |
-- boolean false(): returns false
xfalse                                  :: XFct
xfalse _ _ _                            = XPVBool False

-- -----------------------------------------------------------------------------
-- |
-- boolean lang(string):
-- returns true or false depending on whether the language of the context node as specified by xml:lang attributes
-- is the same as or is a sublanguage of the language specified by the argument string

-- -----------------------------------------------------------------------------
--
-- function needs namespaces which are not supported by the toolbox (???)

xlang                                   :: XFct
xlang _ _ _                             = XPVError "namespaces are not supported"

-- xlang c env args
--    = (\ (_, _, cn) [XPVString s] -> ...) c (toXValue xstring c env args)

-- ------------------------------------------------------------
-- number functions

-- |
-- number number(object?): converts its argument to a number
xnumber                                 :: XFct
xnumber c@(_, _, cn) env []             = (\ (XPVString s) -> parseNumber s) (xstring c env [XPVNode $ singletonNodeSet cn])
xnumber c env [n@(XPVNode _)]           = (\ (XPVString s) -> parseNumber s) (xstring c env [n])

xnumber _ _ [XPVBool b]
    | b                                 = XPVNumber (Float 1)
    | otherwise                         = XPVNumber Pos0

xnumber _ _ [XPVString s]               = parseNumber s
xnumber _ _ [XPVNumber a]               = XPVNumber a
xnumber _ _ [XPVError e]                = XPVError e
xnumber _ _ _                           = XPVError "Call to xnumber with a wrong argument"

-- -----------------------------------------------------------------------------
-- |
-- number sum(node-set):
-- returns the sum, for each node in the argument node-set, of the result of
-- converting the string-values of the node to a number

xsum                                    :: XFct
xsum c env [XPVNode ns]
    | nullNodeSet ns                    = XPVNumber NaN
    | otherwise                         = foldr1 (\ a b -> (xPathAdd Plus a b)) (getValues ns)
      where
      getValues                         :: NodeSet -> [XPathValue]
      getValues                         = foldr (\ n -> ([xnumber c env $ [stringValue n] ] ++) ) [] . fromNodeSet

xsum _ _ _                              = XPVError "The value of the function sum is not a nodeset"

-- -----------------------------------------------------------------------------
-- |
-- number floor(number): returns the largest (closest to positive infinity) number that is not greater
-- than the argument and that is an integer

xfloor                                  :: XFct
xfloor c env args                       = xfloor' (toXValue xnumber c env args)
    where
    xfloor' [XPVNumber (Float f)]
        | f > 0 && f < 1                = XPVNumber Pos0
        | otherwise                     = XPVNumber (Float (fromInteger $ floor f))
    xfloor' [XPVNumber a]               = XPVNumber a
    xfloor' _                           = XPVError "Call to xfloor' without a number"

-- -----------------------------------------------------------------------------
-- |
-- number ceiling(number): returns the smallest (closest to negative infinity) number that is not less
-- than the argument and that is an integer

xceiling                                :: XFct
xceiling c env args                     = xceiling' (toXValue xnumber c env args)
    where
    xceiling' [XPVNumber (Float f)]
        | f < 0 && f > -1               = XPVNumber Pos0
        | otherwise                     = XPVNumber (Float (fromInteger $ ceiling f))
    xceiling' [XPVNumber a]             = XPVNumber a
    xceiling' _                         = XPVError "Call to xceiling' without a number"

-- -----------------------------------------------------------------------------
-- |
-- number round(number):
-- returns the number that is closest to the argument and that is an integer.
-- If there are two such numbers, then the one that is closest to positive infinity is returned.

xround                                  :: XFct
xround c env args                       = xround' c env (toXValue xnumber c env args)

xround'                                 :: XFct
xround' _ _ [XPVNumber (Float f)]
    | f < 0 && f >= -0.5                = XPVNumber Neg0
    | f >= 0 && f < 0.5                 = XPVNumber Pos0
    | otherwise                         = XPVNumber (Float (fromInteger $ xPathRound f))
    where
    xPathRound a                        = if a - (fromInteger $ floor a) < 0.5
                                          then floor a
                                          else floor (a+1)

xround' _ _ [XPVNumber a]               = XPVNumber a
xround' _ _ _                           = XPVError "Call to xround' without a number"

-- -----------------------------------------------------------------------------
-- |
-- node-set key(string, object):
-- does for keys what the id function does for IDs
-- The first argument specifies the name of the key.
-- When the second argument is of type node-set, then the result is the
-- union of the result of applying the key function to the string value
-- of each of the nodes in the argument node-set.
-- When the second argument is of any other type, the argument is
-- converted to a string
xkey                                    :: XFct
xkey _ env ((XPVString s) : [XPVNode ns])
                                        = isInKey (getKeyTab env) s (strValues . fromNodeSet $ ns)
    where
    strValues                           = map ((\ (XPVString str) -> str) . stringValue)

xkey c env ((XPVString s) : arg)        -- = isInKey (getKeyTab env) s  ( (\(XPVString s) -> words s) (xstring c env arg))
                                        = isInKey (getKeyTab env) s [str]
                                          where
                                          XPVString str = xstring c env arg

xkey _ _ _                              = XPVError "Call to xkey with a wrong argument"


isInKey                                 :: KeyTab -> String -> [String] -> XPathValue
isInKey kt kn kv                        = XPVNode . toNodeSet $ ts
    where
    (_, _, ts)                          = unzip3 $ concat $ map (isKeyVal (isKeyName kt kn)) kv

isKeyName                               :: KeyTab -> String -> KeyTab
isKeyName kt kn                         = filter (isOfKeyName kn) kt

isKeyVal                                :: KeyTab -> String -> KeyTab
isKeyVal kt kv                          = filter (isOfKeyValue kv) kt

isOfKeyName                             :: String -> (QName, String, NavXmlTree) -> Bool
isOfKeyName kn (qn, _, _)               = localPart qn == kn


isOfKeyValue                            :: String -> (QName, String, NavXmlTree) -> Bool
isOfKeyValue kv (_, v, _)               = v == kv


-- -----------------------------------------------------------------------------
-- |
-- string format-number(number, string, string?):
-- converts its first argument to a string using the format pattern string
-- specified by the second argument and the decimal-format named by the
-- third argument, or the default decimal-format, if there is no third argument

xformatNumber                           :: XFct
xformatNumber c env (x:xs)              = xsubstring' c env (toXValue xstring c env [x] ++ toXValue xnumber c env xs)
xformatNumber _ _ _                     = XPVError "Call to xformatNumber with a wrong argument"


-- -----------------------------------------------------------------------------

-- Poor man's document(...) function. Opens exactly one document.
-- Does not support "fragment identifiers". "Base-URI" is always current directory.
-- Should still be good enough for home use.

xdocument                               :: XFct
xdocument c e val                       = XPVNode . toNodeSet . (\ (XPVString s) -> xdocument' s) . xstring c e $ val

xdocument'                              :: String -> [NavXmlTree]
xdocument' uri                          = map ntree $
                                          unsafePerformIO $
                                          runX ( readDocument [(a_validate, v_0)] uri
                                                 >>>
                                                 addAttr "rootId" ("doc " ++ uri)
                                               )

-- -----------------------------------------------------------------------------

-- generate-id, should be fully compliant with XSLT specification.

xgenerateId                             :: XFct
xgenerateId _            _ [XPVNode ns]
    | not (nullNodeSet ns)              = xgenerateId' . headNodeSet $ ns

xgenerateId (_, _, node) _ []           = xgenerateId' node
xgenerateId _            _ _            = error "illegal arguments in xgenerateId"

xgenerateId'                            :: NavXmlTree -> XPathValue
xgenerateId'                            = XPVString . ("id_"++) . str2XmlId . show . nodeID . Just

str2XmlId                               :: String -> String
str2XmlId                               = concatMap convert
    where
    convert c                           = if isAscii c && (isUpper c || isLower c || isDigit c)
                                          then [c]
                                          else "_" ++ (show $ ord c) ++ "_"

-- ------------------------------------------------------------

xpNamePart :: LA XmlTree String -> XmlTree -> String
xpNamePart getNp
    = concat
      .
      runLA ( ifA isRoot
              (constA "")
              getNp
            )

xpLocalPartOf   :: XmlTree -> String
xpLocalPartOf   = xpNamePart getLocalPart

xpNamespaceOf   :: XmlTree -> String
xpNamespaceOf   = xpNamePart getNamespaceUri

xpNameOf        :: XmlTree -> String
xpNameOf        = xpNamePart getName

getValue        :: String -> XmlTree -> String
getValue n      = concat . runLA (getAttrValue n)

xpTextOf        :: XmlTree -> String
xpTextOf        = concat . runLA (xshow ((getCmt >>> mkText) <+> deep isText))

-- ------------------------------------------------------------
