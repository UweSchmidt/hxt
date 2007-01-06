-- |
-- Format an expression or value in tree- or string-representation
--


module Text.XML.HXT.XPath.XPathToString
    ( expr2XPathTree
    , xPValue2String
    , xPValue2XmlTrees
    , nt2XPathTree
    , pred2XPathTree
    , toXPathTree
    )
where

import Text.XML.HXT.DOM.XmlTree
import Text.XML.HXT.DOM.FormatXmlTree
    ( formatXmlTree )

import Text.XML.HXT.XPath.XPathDataTypes

import Data.Char
    ( toLower )

-- ------------------------------------------------------------

type XPathTree = NTree String


-- -----------------------------------------------------------------------------
-- |
-- Convert an navigable tree in a xmltree
--
toXPathTree			:: [NavTree a] -> [NTree a]
toXPathTree xs			= foldr (\ x -> (subtreeNT x :)) [] xs



-- -----------------------------------------------------------------------------
-- |
-- Format a XPath-value in string representation.
-- Text output is done by 'formatXmlTree' for node-sets (trees),
-- all other values are represented as strings.
--

xPValue2String			:: XPathValue -> String
xPValue2String (XPVNode ns)
    = foldr (\t -> ((formatXmlTree t ++ "\n") ++)) "" (toXPathTree ns)
xPValue2String (XPVBool b) 	= map toLower (show b)
xPValue2String (XPVNumber (Float f)) = show f
xPValue2String (XPVNumber s) 	= show s
xPValue2String (XPVString s) 	= s
xPValue2String (XPVError s) 	= "Error: " ++ s

-- -----------------------------------------------------------------------------
-- |
-- Convert a a XPath-value into XmlTrees.
--

xPValue2XmlTrees			:: XPathValue -> XmlTrees
xPValue2XmlTrees (XPVNode ns)		= toXPathTree ns
xPValue2XmlTrees (XPVBool b)		= xtext (show b)
xPValue2XmlTrees (XPVNumber (Float f))	= xtext (show f)
xPValue2XmlTrees (XPVNumber s)		= xtext (show s)
xPValue2XmlTrees (XPVString s)		= xtext s
xPValue2XmlTrees (XPVError  s)		= xerr s



-- -----------------------------------------------------------------------------
-- |
-- Format a parsed XPath-expression in tree representation.
-- Text output is done by 'formatXmlTree'
--
expr2XPathTree			:: Expr -> XPathTree
expr2XPathTree (GenExpr op ex)	= NTree (show op) (map expr2XPathTree ex)
expr2XPathTree (NumberExpr f) 	= NTree (show f) []
expr2XPathTree (LiteralExpr s) 	= NTree s []
expr2XPathTree (VarExpr name) 	= NTree ("Var: " ++ show name) []
expr2XPathTree (FctExpr n arg)	= NTree ("Fct: " ++ n) (map expr2XPathTree arg)

expr2XPathTree (FilterExpr [])	= NTree "" []
expr2XPathTree (FilterExpr (primary:predicate))
    = NTree "FilterExpr" [expr2XPathTree primary, pred2XPathTree predicate]

expr2XPathTree (PathExpr Nothing (Just lp)) = locpath2XPathTree lp
expr2XPathTree (PathExpr (Just fe) (Just lp))
    = NTree "PathExpr" [expr2XPathTree fe, locpath2XPathTree lp]
expr2XPathTree (PathExpr _ _) 	= NTree "" []

locpath2XPathTree		:: LocationPath -> XPathTree
locpath2XPathTree (LocPath rel steps)
    = NTree (show rel ++ "LocationPath") (map step2XPathTree steps)


step2XPathTree 			:: XStep -> XPathTree
step2XPathTree (Step axis nt [])
    = NTree (show axis) [nt2XPathTree nt]
step2XPathTree (Step axis nt expr)
    = NTree (show axis) [nt2XPathTree nt, pred2XPathTree expr]

nt2XPathTree 			:: NodeTest -> XPathTree
nt2XPathTree (TypeTest s) 	= NTree ("TypeTest: "++ typeTest2String s) []
nt2XPathTree (PI s) 		= NTree ("TypeTest: processing-instruction("++ show s ++ ")") []
nt2XPathTree (NameTest s) 	= NTree ("NameTest: "++ show s) []


pred2XPathTree 			:: [Expr] -> XPathTree
pred2XPathTree exprL 		= NTree "Predicates" (map expr2XPathTree exprL)

typeTest2String 		:: XPathNode -> String
typeTest2String XPNode 		= "node()"
typeTest2String XPCommentNode	= "comment()"
typeTest2String XPPINode 	= "processing-instruction()"
typeTest2String XPTextNode 	= "text()"