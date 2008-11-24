-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XPath.XPathEval
   Copyright  : Copyright (C) 2006 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: XPathEval.hs,v 1.8 2006/10/12 11:51:29 hxml Exp $

   The core functions for evaluating the different types of XPath expressions.
   Each 'Expr'-constructor is mapped to an evaluation function.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XPath.XPathEval
    ( getXPath
    , getXPathWithNsEnv
    , getXPathSubTrees
    , getXPathSubTreesWithNsEnv
    , getXPathNodeSet
    , getXPathNodeSetWithNsEnv
    , evalExpr
    )
where

import Text.XML.HXT.XPath.XPathFct
import Text.XML.HXT.XPath.XPathDataTypes
import Text.XML.HXT.XPath.XPathArithmetic
    ( xPathAdd
    , xPathDiv
    , xPathMod
    , xPathMulti
    , xPathUnary
    )
import Text.XML.HXT.XPath.XPathParser
    ( parseXPath )

import Text.XML.HXT.XPath.XPathToString
    ( xPValue2XmlTrees )

import Text.XML.HXT.XPath.XPathToNodeSet
    ( xPValue2NodeSet
    , emptyNodeSet
    )

import Text.ParserCombinators.Parsec
    ( runParser )

import Data.Maybe
    ( fromJust )

-- ----------------------------------------

-- the DOM functions

import           Text.XML.HXT.DOM.Interface
import qualified Text.XML.HXT.DOM.XmlNode as XN

-- ----------------------------------------

-- the list arrow functions

import Control.Arrow			( (>>>), (>>^) )
import Control.Arrow.ArrowList		( arrL, isA )
import Control.Arrow.ArrowIf    	( filterA )
import Control.Arrow.ListArrow		( runLA )
import qualified
       Control.Arrow.ArrowTree		as AT
import Text.XML.HXT.Arrow.XmlArrow	( ArrowDTD, isDTD, getDTDAttrl )
import Text.XML.HXT.Arrow.Edit		( canonicalizeForXPath )

-- -----------------------------------------------------------------------------

-- |
-- Select parts of a document by an XPath expression.
--
-- The main filter for selecting parts of a document via XPath.
-- The string argument must be a XPath expression with an absolute location path,
-- the argument tree must be a complete document tree.
-- Result is a possibly empty list of XmlTrees forming the set of selected XPath values.
-- XPath values other than XmlTrees (numbers, attributes, tagnames, ...)
-- are convertet to text nodes.

getXPath		:: String -> XmlTree -> XmlTrees
getXPath		= getXPathWithNsEnv []

-- |
-- Select parts of a document by a namespace aware XPath expression.
--
-- Works like 'getXPath' but the prefix:localpart names in the XPath expression
-- are interpreted with respect to the given namespace environment

getXPathWithNsEnv	:: Attributes -> String -> XmlTree -> XmlTrees
getXPathWithNsEnv env s	= runLA ( canonicalizeForXPath
				  >>>
				  arrL (getXPathValues xPValue2XmlTrees xPathErr (toNsEnv env) s)
				)

-- |
-- Select parts of an XML tree by a XPath expression.
--
-- The main filter for selecting parts of an arbitrary XML tree via XPath.
-- The string argument must be a XPath expression with an absolute location path,
-- There are no restrictions on the arument tree.
--
-- No canonicalization is performed before evaluating the query
--
-- Result is a possibly empty list of XmlTrees forming the set of selected XPath values.
-- XPath values other than XmlTrees (numbers, attributes, tagnames, ...)
-- are convertet to text nodes.

getXPathSubTrees	:: String -> XmlTree -> XmlTrees
getXPathSubTrees	= getXPathSubTreesWithNsEnv []

-- | Same as 'getXPathSubTrees' but with namespace aware XPath expression

getXPathSubTreesWithNsEnv	:: Attributes -> String -> XmlTree -> XmlTrees
getXPathSubTreesWithNsEnv nsEnv xpStr
    = getXPathValues xPValue2XmlTrees xPathErr (toNsEnv nsEnv) xpStr

-- | compute the node set of an XPath query

getXPathNodeSet		:: String -> XmlTree -> XmlNodeSet
getXPathNodeSet		= getXPathNodeSetWithNsEnv []

-- | compute the node set of a namespace aware XPath query

getXPathNodeSetWithNsEnv	:: Attributes -> String -> XmlTree -> XmlNodeSet
getXPathNodeSetWithNsEnv nsEnv xpStr
    = getXPathValues xPValue2NodeSet (const (const emptyNodeSet)) (toNsEnv nsEnv) xpStr

-- | parse xpath, evaluate xpath expr and prepare results

getXPathValues	:: (XPathValue -> a) -> (String -> String -> a) -> NsEnv -> String -> XmlTree -> a
getXPathValues cvRes cvErr nsEnv xpStr t
    = case (runParser parseXPath nsEnv "" xpStr) of
      Left parseError
	  -> cvErr xpStr (show parseError)
      Right xpExpr
	  -> evalXP xpExpr
    where
    evalXP xpe
	= cvRes xpRes
	where
	t'      = addRoot t				-- we need a root node for starting xpath eval
	idAttr	= ( ("", "idAttr")			-- id attributes from DTD (if there)
		  , idAttributesToXPathValue . getIdAttributes $ t'
		  )
	navTD	= ntree t'
	xpRes	= evalExpr (idAttr:(getVarTab varEnv),[]) (1, 1, navTD) xpe (XPVNode [navTD])

addRoot	:: XmlTree -> XmlTree
addRoot t
    | XN.isRoot t
	= t
    | otherwise
	= XN.mkRoot [] [t]

xPathErr	:: String -> String -> [XmlTree]
xPathErr xpStr parseError
    = [XN.mkError c_err ("Syntax error in XPath expression " ++ show xpStr ++ ": " ++ show parseError)]

-- |
-- The main evaluation entry point. 
-- Each XPath-'Expr' is mapped to an evaluation function. The 'Env'-parameter contains the set of global variables
-- for the evaluator, the 'Context'-parameter the root of the tree in which the expression is evaluated.
-- 

evalExpr :: Env -> Context -> Expr -> XPathFilter
evalExpr env cont (GenExpr Or ex)
    = boolEval env cont Or ex
evalExpr env cont (GenExpr And ex)
    = boolEval env cont And ex
    
evalExpr env cont (GenExpr Eq ex)
    = relEqEval env cont Eq . evalExprL env cont ex
evalExpr env cont (GenExpr NEq ex)
    = relEqEval env cont NEq . evalExprL env cont ex
evalExpr env cont (GenExpr Less ex)
    = relEqEval env cont Less . evalExprL env cont ex
evalExpr env cont (GenExpr LessEq ex)
    = relEqEval env cont LessEq . evalExprL env cont ex
evalExpr env cont (GenExpr Greater ex)
    = relEqEval env cont Greater . evalExprL env cont ex
evalExpr env cont (GenExpr GreaterEq ex)
    = relEqEval env cont GreaterEq . evalExprL env cont ex

evalExpr env cont (GenExpr Plus ex)
    = numEval xPathAdd Plus . toXValue xnumber cont env . evalExprL env cont ex
evalExpr env cont (GenExpr Minus ex)
    = numEval xPathAdd Minus . toXValue xnumber cont env . evalExprL env cont ex
evalExpr env cont (GenExpr Div ex)
    = numEval xPathDiv Div . toXValue xnumber cont env . evalExprL env cont ex
evalExpr env cont (GenExpr Mod ex)
    = numEval xPathMod Mod . toXValue xnumber cont env . evalExprL env cont ex
evalExpr env cont (GenExpr Mult ex)
    = numEval xPathMulti Mult . toXValue xnumber cont env . evalExprL env cont ex

evalExpr env cont (GenExpr Unary ex)
    = xPathUnary . xnumber cont env . evalExprL env cont ex

evalExpr env cont (GenExpr Union ex)
    = unionEval . evalExprL env cont ex

evalExpr env cont (FctExpr name args)
    = fctEval env cont name args

evalExpr env _ (PathExpr Nothing (Just lp))
    = locPathEval env lp
evalExpr env cont (PathExpr (Just fe) (Just lp))
    = locPathEval env lp . evalExpr env cont fe

evalExpr env cont (FilterExpr ex)
    = filterEval env cont ex
evalExpr env _ ex
    = evalSpezExpr env ex



evalExprL :: Env -> Context -> [Expr] -> XPathValue -> [XPathValue]
evalExprL env cont ex ns
    = map (\e -> evalExpr env cont e ns) ex

evalSpezExpr :: Env -> Expr -> XPathFilter
evalSpezExpr _ (NumberExpr (Float 0)) _
    = XPVNumber Pos0
evalSpezExpr _ (NumberExpr (Float f)) _
    = XPVNumber (Float f)
evalSpezExpr _ (LiteralExpr s) _
    = XPVString s
evalSpezExpr env (VarExpr name) v
    = getVariable env name v
evalSpezExpr _ _ _
    = XPVError "Call to evalExpr with a wrong argument"


-- -----------------------------------------------------------------------------

-- |
-- filter for evaluating a filter-expression
filterEval :: Env -> Context -> [Expr] -> XPathFilter
filterEval env cont (prim:predicates) ns
    = case evalExpr env cont prim ns of
        new_ns@(XPVNode _) -> evalPredL env predicates new_ns
        _                  -> XPVError "Return of a filterexpression is not a nodeset"
filterEval _ _ _ _
    = XPVError "Call to filterEval without an expression"



-- |
-- returns the union of its arguments, the arguments have to be node-sets.
unionEval :: [XPathValue] -> XPathValue
unionEval
    = createDocumentOrder . remDups . unionEval'
      where
      unionEval' (e@(XPVError _):_)           = e
      unionEval' (_:e@(XPVError _):_)         = e
      unionEval' [n@(XPVNode _)]              = n
      unionEval' ((XPVNode n):(XPVNode m):xs) = unionEval ( (XPVNode (n ++ m)):xs)
      unionEval' _                            = XPVError "The value of a union ( | ) is not a nodeset"



-- |
-- Equality or relational test for node-sets, numbers, boolean values or strings,
-- each computation of two operands is done by relEqEv'
relEqEval :: Env -> Context -> Op -> [XPathValue] -> XPathValue
relEqEval env cont op
    = foldl1 (relEqEv' env cont op)

    

relEqEv' :: Env -> Context -> Op -> XPathValue -> XPathFilter
relEqEv' _ _ _ e@(XPVError _) _ = e
relEqEv' _ _ _ _ e@(XPVError _) = e

-- two node-sets
relEqEv' env cont op a@(XPVNode _) b@(XPVNode _)
    = relEqTwoNodes env cont op a b

-- one node-set
relEqEv' env cont op a b@(XPVNode _)
    = relEqOneNode env cont (fromJust $ getOpFct op) a b
relEqEv' env cont op a@(XPVNode _) b
    = relEqOneNode env cont (flip $ fromJust $ getOpFct op) b a


--  test without a node-set and equality or not-equality operator
relEqEv' env cont Eq a b  = eqEv env cont (==) a b
relEqEv' env cont NEq a b = eqEv env cont (/=) a b

-- test without a node-set and less, less-equal, greater or greater-equal operator
relEqEv' env cont op a b
    = XPVBool ((fromJust $ getOpFct op) (toXNumber a) (toXNumber b))
      where
      toXNumber x = xnumber cont env [x]



-- |
-- Equality or relational test for two node-sets.
-- The comparison will be true if and only if there is a node in the first node-set
-- and a node in the second node-set such that the result of performing the
-- comparison on the string-values of the two nodes is true
relEqTwoNodes :: Env -> Context -> Op -> XPathValue -> XPathFilter
relEqTwoNodes _ _ op (XPVNode ns) (XPVNode ms)
    = XPVBool (foldr  (\n -> (any (fct op n) (getStrValues ms) ||)) False ns)
      where
      fct op' n'   = (fromJust $ getOpFct op') (stringValue n')
      getStrValues = map stringValue
relEqTwoNodes _ _ _ _ _
    = XPVError "Call to relEqTwoNodes without a nodeset"


-- |
-- Comparison between a node-set and different type.
-- The node-set is converted in a boolean value if the second argument is of type boolean.
-- If the argument is of type number, the node-set is converted in a number, otherwise it is converted
-- in a string value.
relEqOneNode :: Env -> Context -> (XPathValue -> XPathValue -> Bool) -> XPathValue -> XPathFilter
relEqOneNode env cont fct arg (XPVNode ns)
    = XPVBool (any  (fct arg) (getStrValues arg ns))
      where
      getStrValues arg' = map ((fromJust $ getConvFct arg') cont env . wrap) . map stringValue
      wrap x = [x]
relEqOneNode _ _ _ _ _
    = XPVError "Call to relEqOneNode without a nodeset"



-- |
-- No node-set is involved and the operator is equality or not-equality.
-- The arguments are converted in a common type. If one argument is a boolean value
-- then it is the boolean type. If a number is involved, the arguments have to converted in numbers,
-- else the string type is the common type.
eqEv :: Env -> Context -> (XPathValue -> XPathValue -> Bool) -> XPathValue -> XPathFilter
eqEv env cont fct f@(XPVBool _) g
    = XPVBool (f `fct` xboolean cont env [g])
eqEv env cont fct f g@(XPVBool _)
    = XPVBool (xboolean cont env [f] `fct` g)

eqEv env cont fct f@(XPVNumber _) g
    = XPVBool (f `fct` xnumber cont env [g])
eqEv env cont fct f g@(XPVNumber _)
    = XPVBool (xnumber cont env [f] `fct` g)

eqEv env cont fct f g
    = XPVBool (xstring cont env [f] `fct` xstring cont env [g])


getOpFct :: Op -> Maybe (XPathValue -> XPathValue -> Bool)
getOpFct Eq        = Just (==)
getOpFct NEq       = Just (/=)
getOpFct Less      = Just (<)
getOpFct LessEq    = Just (<=)
getOpFct Greater   = Just (>)
getOpFct GreaterEq = Just (>=)
getOpFct _         = Nothing



-- |
-- Filter for accessing the root element of a document tree
getRoot :: XPathFilter
getRoot (XPVNode (n:_))
    = XPVNode [getRoot' n]
      where
      getRoot' tree
        = case upNT tree of
            Nothing -> tree
            Just t -> getRoot' t
getRoot _
    = XPVError "Call to getRoot without a nodeset"



-- |
-- Filter for accessing all nodes of a XPath-axis
--
--    * 1.parameter as :  axis specifier
--
getAxisNodes :: AxisSpec ->  XPathFilter
getAxisNodes as (XPVNode ns)
    = XPVNode (concat $ map (fromJust $ lookup as axisFctL) ns)
getAxisNodes _ _
    = XPVError "Call to getAxis without a nodeset"



-- |
-- Axis-Function-Table.
-- Each XPath axis-specifier is mapped to the corresponding axis-function
axisFctL :: [(AxisSpec, (NavXmlTree -> NavXmlTrees))]
axisFctL = [ (Ancestor, ancestorAxis)
           , (AncestorOrSelf, ancestorOrSelfAxis)
           , (Attribute, attributeAxis)
           , (Child, childAxis)
           , (Descendant, descendantAxis)
           , (DescendantOrSelf, descendantOrSelfAxis)
           , (Following, followingAxis)
           , (FollowingSibling, followingSiblingAxis)
           , (Parent, parentAxis)
           , (Preceding, precedingAxis)
           , (PrecedingSibling, precedingSiblingAxis)
           , (Self, selfAxis)
           ]


-- |
-- evaluates a location path,
-- evaluation of an absolute path starts at the document root, 
-- the relative path at the context node
locPathEval :: Env -> LocationPath -> XPathFilter
locPathEval env (LocPath Rel steps)
    = evalSteps env steps
locPathEval env (LocPath Abs steps)
    = evalSteps env steps . getRoot




evalSteps :: Env -> [XStep] -> XPathFilter
evalSteps env steps ns
    = foldl (evalStep env) ns steps


-- |
-- evaluate a single XPath step
-- namespace-axis is not supported
evalStep :: Env -> XPathValue -> XStep -> XPathValue
evalStep _   _  (Step Namespace _ _)  = XPVError "namespace-axis not supported"
evalStep _   ns (Step Attribute nt _) = evalAttr nt (getAxisNodes Attribute ns)
evalStep env ns (Step axisSpec nt pr) = evalStep' env pr nt (getAxisNodes axisSpec ns)



evalAttr :: NodeTest -> XPathFilter
evalAttr nt (XPVNode ns)
    = XPVNode (foldr (\n -> (evalAttrNodeTest nt n ++)) [] ns)
evalAttr _ _
    = XPVError "Call to evalAttr without a nodeset"


evalAttrNodeTest :: NodeTest -> NavXmlTree -> NavXmlTrees
evalAttrNodeTest (NameTest qn) ns@(NT (NTree (XAttr qn1) _) _ _ _)
    = if ( ( uri == uri1               && lp == lp1)
	   || 
           ((uri == "" || uri == uri1) && lp == "*") 
         )
      then [ns]
      else []
      where
      uri  = namespaceUri qn
      uri1 = namespaceUri qn1
      lp   = localPart    qn
      lp1  = localPart    qn1

evalAttrNodeTest (TypeTest XPNode) ns@(NT (NTree (XAttr _) _) _ _ _)
    = [ns]

evalAttrNodeTest _ _
    = []

evalStep' :: Env -> [Expr] -> NodeTest -> XPathFilter
evalStep' env pr nt
    = evalPredL env pr . nodeTest nt


evalPredL :: Env -> [Expr] -> XPathFilter
evalPredL env pr n@(XPVNode ns)
    = remDups $ foldl (evalPred env 1 (length ns)) n pr
evalPredL _ _ _
    = XPVError "Call to evalPredL without a nodeset"


evalPred :: Env -> Int -> Int -> XPathValue -> Expr -> XPathValue
evalPred _ _ _ ns@(XPVNode []) _ = ns
evalPred env pos len (XPVNode (x:xs)) ex
    = case testPredicate env (pos, len, x) ex (XPVNode [x]) of
        e@(XPVError _) -> e
        XPVBool True   -> XPVNode (x : n)
        XPVBool False  -> nextNode
        _              -> XPVError "Value of testPredicate is not a boolean"
      where nextNode@(XPVNode n) = evalPred env (pos+1) len (XPVNode xs) ex
evalPred _ _ _ _ _
    = XPVError "Call to evalPred without a nodeset"



testPredicate :: Env -> Context -> Expr -> XPathFilter
testPredicate env context@(pos, _, _) ex ns
    = case evalExpr env context ex ns of
        XPVNumber (Float f) -> XPVBool (f == fromIntegral pos)
        XPVNumber _         -> XPVBool False
        _                   -> xboolean context env [evalExpr env context ex ns]


-- |
-- filter for selecting a special type of nodes from the current fragment tree
--
-- the filter works with namespace activated and without namespaces.
-- If namespaces occur in XPath names, the uris are used for matching,
-- else the name prefix
--
--    Bugfix : "*" (or any other name-test) must not match the root node

nodeTest :: NodeTest -> XPathFilter

-- nodeTest (NameTest (QN "" "*" ""))      = filterNodes (\n -> isXTagNode n && not (isRootNode n))
-- nodeTest (NameTest (QN _ "*" uri))      = filterNodes (filterd uri)

nodeTest (NameTest q)
    | isWildcardTest
	= filterNodes (wildcardTest q)
    | otherwise
        = filterNodes (nameTest q)		-- old: (isOfTagNode1 q)
      where
      isWildcardTest = localPart q == "*"

nodeTest (PI n)                         = filterNodes isPiNode
					  where
					  isPiNode = maybe False ((== n) . qualifiedName) . XN.getPiName

nodeTest (TypeTest t)                   = typeTest t

nameTest	:: QName -> XNode -> Bool
nameTest xpName (XTag elemName _)
    | namespaceAware
	= localPart xpName == localPart elemName
	  &&
	  namespaceUri xpName == namespaceUri elemName
    | otherwise
	= qualifiedName xpName == qualifiedName elemName
    where
    namespaceAware = not . null . namespaceUri $ xpName

nameTest _ _ = False

wildcardTest	:: QName -> XNode -> Bool
wildcardTest xpName (XTag elemName _)
    | namespaceAware
	= namespaceUri xpName == namespaceUri elemName
    | prefixMatch
	= namePrefix xpName == namePrefix elemName
    | otherwise
	= localPart elemName /= t_root			-- all names except the root name "/"
    where
    namespaceAware = not . null . namespaceUri $ xpName
    prefixMatch    = not . null . namePrefix   $ xpName

wildcardTest _ _ = False

-- |
-- tests whether a node is of a special type
--
typeTest :: XPathNode -> XPathFilter
typeTest XPNode         = id
typeTest XPCommentNode  = filterNodes XN.isCmt
typeTest XPPINode       = filterNodes XN.isPi
typeTest XPTextNode     = filterNodes XN.isText

-- |
-- the filter selects the NTree part of a navigable tree and
-- tests whether the node is of the necessary type
--
--    * 1.parameter fct :  filter function from the XmlTreeFilter module which tests the type of a node
--
filterNodes :: (XNode -> Bool) -> XPathFilter
filterNodes fct (XPVNode ns)
    = XPVNode ([n | n@(NT (NTree node _) _ _ _) <- ns , fct node])
filterNodes _ _
    = XPVError "Call to filterNodes without a nodeset"

-- |
-- evaluates a boolean expression, the evaluation is non-strict
boolEval :: Env -> Context -> Op -> [Expr] -> XPathFilter
boolEval _ _ op [] _
    = XPVBool (op==And)
boolEval env cont Or (x:xs) ns
    = case xboolean cont env [evalExpr env cont x ns] of
        e@(XPVError _) -> e
        XPVBool True   -> XPVBool True
        _              -> boolEval env cont Or xs ns

boolEval env cont And (x:xs) ns
    = case xboolean cont env [evalExpr env cont x ns] of
        e@(XPVError _) -> e
        XPVBool True   -> boolEval env cont And xs ns
        _              -> XPVBool False
boolEval _ _ _ _ _
    = XPVError "Call to boolEval with a wrong argument"



-- |
-- returns the value of a variable
getVariable :: Env -> VarName -> XPathFilter
getVariable env name _
    = case lookup name (getVarTab env) of
        Nothing -> XPVError ("Variable: " ++ show name ++ " not found")
        Just v  -> v


-- |
-- evaluates a function, 
-- computation is done by 'XPathFct.evalFct' which is defined in "XPathFct".
fctEval :: Env -> Context -> FctName -> [Expr] -> XPathFilter
fctEval env cont name args
    = evalFct name env cont . evalExprL env cont args



-- |
-- evaluates an arithmetic operation.
--
--   1.parameter f :  arithmetic function from "XPathArithmetic"
--
numEval :: (Op -> XPathValue -> XPathValue -> XPathValue) -> Op -> [XPathValue] -> XPathValue
numEval f op = foldl1 (f op)



-- |
-- Convert list of ID attributes from DTD into a space separated 'XPVString'
--

idAttributesToXPathValue	:: XmlTrees -> XPathValue
idAttributesToXPathValue ts
    = XPVString (foldr (\ n -> ( (valueOfDTD a_value n ++ " ") ++)) [] ts)

-- |
-- Extracts all ID-attributes from the document type definition (DTD).
--

getIdAttributes	:: XmlTree -> XmlTrees
getIdAttributes
    = runLA $
      AT.getChildren
      >>>
      isDTD
      >>>
      AT.deep (isIdAttrType)

-- ----------------------------------------

isIdAttrType		:: ArrowDTD a => a XmlTree XmlTree
isIdAttrType		= hasDTDAttrValue a_type (== k_id)

valueOfDTD		:: String -> XmlTree -> String
valueOfDTD n		= concat . runLA ( getDTDAttrl >>^ lookup1 n )

hasDTDAttrValue		:: ArrowDTD a => String -> (String -> Bool) -> a XmlTree XmlTree
hasDTDAttrValue	an p	= filterA $
			  getDTDAttrl >>> isA (p . lookup1 an)

-- ------------------------------------------------------------
