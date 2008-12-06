-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XPath.GetSimpleXPath
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   XPath selection for simple XPath expressions with list arrows
   instead of navigatable trees.

   It is recomended, that this module is imported qualified,
   e.g like @Text.XML.HXT.Arrow.XPathSimple as XS@.

   The arrows defined in this module have the same
   functionality as the functions in 'Text.XML.HXT.Arrow.XPath'.

   The computation modell in XPath is a navigatable tree,
   that means a tree wicht can be traveresed in arbitrary directions,
   not only from the root to the leafs. Sometimes this modell
   leads to inefficent XPath processing for simple queries, which
   only need a top down tree traversal.

   When evaluating an XPath expression with these functions, first an attempt is made
   to map the XPath expression to a pure arrow. If this is possible
   due to the siplicity of the XPath expressions, the result is computed
   directly, else the query is processed by the corresponding
   function in 'Text.XML.HXT.Arrow.XPath'.

   The simple evaluation is possible, when in the XPath expression
   only the top down axis (self, child, descendant, descendant or self are used,
   when no builtin functions concerning the position of a node are used,
   and no comparison of nodes e.g. in node set union is required.

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XPathSimple
where

-- import qualified Debug.Trace as T

import Control.Monad
import Control.Arrow.ListArrows

import Data.Maybe

import Text.ParserCombinators.Parsec		( runParser )

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow

import qualified Text.XML.HXT.Arrow.XPath as XP	( getXPathTreesWithNsEnv
						)
import Text.XML.HXT.Arrow.Edit			( canonicalizeForXPath
						)

import Text.XML.HXT.XPath.XPathDataTypes	( XPNumber (..)
						, Expr (..)
						, Op (..)
						, XPathNode (..)
						, LocationPath (..)
						, Path (..)
						, XStep (..)
						, AxisSpec (..)
						, NodeTest (..)
						, XPathValue (..)
						)
import Text.XML.HXT.XPath.XPathParser		( parseXPath
						, parseNumber
						)

-- ----------------------------------------

-- |
-- Same Functionality as 'Text.XML.HXT.Arrow.XPath.getXPathTreesInDoc'

getXPathTreesInDoc			:: ArrowXml a => String -> a XmlTree XmlTree
getXPathTreesInDoc			= getXPathTreesInDocWithNsEnv []

-- |
-- Same Functionality as 'Text.XML.HXT.Arrow.XPath.getXPathTreesInDocWithNsEnv'

getXPathTreesInDocWithNsEnv		:: ArrowXml a => Attributes -> String -> a XmlTree XmlTree
getXPathTreesInDocWithNsEnv env query	= canonicalizeForXPath
					  >>>
					  tryGetXPath env query
-- |
-- Same Functionality as 'Text.XML.HXT.Arrow.XPath.getXPathTrees'

getXPathTrees				:: ArrowXml a => String -> a XmlTree XmlTree
getXPathTrees				= getXPathTreesWithNsEnv []

-- |
-- Same Functionality as 'Text.XML.HXT.Arrow.XPath.getXPathTreesWithNsEnv'

getXPathTreesWithNsEnv			:: ArrowXml a => Attributes -> String -> a XmlTree XmlTree
getXPathTreesWithNsEnv			= tryGetXPath


tryGetXPath				:: ArrowXml a => Attributes -> String -> a XmlTree XmlTree
tryGetXPath env query			= ( listA (getXPathTreesWithNsEnvSimple env query)
					    &&&
					    listA (   XP.getXPathTreesWithNsEnv env query)
					  )
                                          >>>
					  ifA (arr fst >>> (unlistA >>. take 1) >>> isError)
					      (arr snd >>> unlistA)
					      (arr fst >>> unlistA)

-- |
-- The xpath interpreter for simple xpath expressions.
--
-- In case of a too complicated or illegal xpath expression an error
-- node is returned, else the list of selected XML trees

getXPathTreesWithNsEnvSimple		:: ArrowXml a => Attributes -> String -> a XmlTree XmlTree
getXPathTreesWithNsEnvSimple env s	= fromLA $ getXP (toNsEnv env) s

-- ----------------------------------------

getXP				:: NsEnv -> String -> LA XmlTree XmlTree
getXP env s			= either ( err
					   .
					   (("Syntax error in XPath expression " ++ show s ++ ": ") ++)
					   .
					   show . show
					 ) (fromMaybe (err ( "XPath expression " ++ show s ++
							     " too complicated for simple arrow evaluation"
							   )
						      ) . compXPath
					   )
				  -- . ( \ e -> T.trace (("getXP: xp = "++) . show $ e) e)
				  .
				  runParser parseXPath env ""
				  $ s

-- ----------------------------------------

type XPArrow b c		= Maybe (LA b c)

mk				:: LA b c -> XPArrow b c
mk				= Just

unwrap				:: XPArrow b b -> LA b b
unwrap				= fromJust . toThis

(>>>>)				:: XPArrow b b -> XPArrow b b -> XPArrow b b
Nothing   >>>> a2		= a2
a1        >>>> Nothing		= a1
(Just f1) >>>> (Just f2)	= return $ f1 >>> f2

(&&&&)				:: XPArrow b b -> XPArrow b b -> XPArrow b (b, b)
Nothing   &&&& a2		= this'' &&&& a2
a1        &&&& Nothing		= a1     &&&& this''
(Just f1) &&&& (Just f2)	= return $ f1 &&& f2

(<+>>)				:: XPArrow b b -> XPArrow b b -> XPArrow b b
Nothing   <+>> _a2		= Nothing
_a1       <+>> Nothing		= Nothing
(Just f1) <+>> (Just f2)	= return $ f1 <+> f2

guards'				:: XPArrow b b -> XPArrow b b -> XPArrow b b
Nothing   `guards'` a2		= a2
a1        `guards'` Nothing	= a1 `guards'` this''
(Just f1) `guards'` (Just f2)	= return $ f1 `guards` f2

this'				:: XPArrow b b
this'				= Nothing

this''				:: XPArrow b b
this''				= mk this

toThis				:: XPArrow b b -> XPArrow b b
toThis Nothing			= this''
toThis a			= a

getChildren'			:: XPArrow XmlTree XmlTree -> XPArrow XmlTree XmlTree
getChildren' a			= mk getChildren >>>> a

getAttrl'			:: XPArrow XmlTree XmlTree -> XPArrow XmlTree XmlTree
getAttrl' a			= mk getAttrl >>>> a

multi'				:: XPArrow XmlTree XmlTree -> XPArrow XmlTree XmlTree
multi' a			= mk $ multi (unwrap a)

deep'				:: XPArrow XmlTree XmlTree -> XPArrow XmlTree XmlTree
deep' a				= mk $ deep (unwrap a)

xIndex				:: Int -> LA [b] b
xIndex i
    | i <= 0			= none
    | otherwise			= arrL (take 1 . drop (i-1))

xString				:: XPArrow XmlTree XmlTree -> LA XmlTree String
xString a			= unwrap a >>> xshow (deep isText)

xNumber'			:: XPArrow XmlTree XmlTree -> LA XmlTree XPNumber
xNumber' a			=  xString a >>> arr toNumber

-- ------------------------------

deadEndStreet			:: Monad m => m a
deadEndStreet			= fail "XPath expression too complicated for XmlArrows"

compXPath			:: MonadPlus m => Expr -> m (LA XmlTree XmlTree)
compXPath e			= do
				  r <- compXP e
				  return $ unwrap r

compXP				:: MonadPlus m => Expr -> m (XPArrow XmlTree XmlTree)
compXP (PathExpr Nothing (Just (LocPath Abs lp)))
				= compLP lp this'
compXP (FilterExpr (e1:el))	= do
				  r <- compXP e1
				  compFP el r
compXP _			= deadEndStreet

compFP				:: MonadPlus m => [Expr] -> XPArrow XmlTree XmlTree -> m (XPArrow XmlTree XmlTree)
compFP []      r		= return r
compFP (e1:es) r		= do
				  r1 <- compPred [e1] r
				  compFP es r1

compLP				:: MonadPlus m => [XStep] -> XPArrow XmlTree XmlTree -> m (XPArrow XmlTree XmlTree)
compLP []     r			= return r
compLP (x:xs) r			= do
				  a1 <- compXS x r
				  as <- compLP xs a1
				  return as

compXS				:: MonadPlus m => XStep -> XPArrow XmlTree XmlTree -> m (XPArrow XmlTree XmlTree)
compXS (Step Child nt ps) s	= do
				  an <- compNTE nt
				  compPred ps (s >>>> mk getChildren >>>> an)

compXS (Step DescendantOrSelf nt ps) s
				= do
				  an <- compNTE nt
				  compPred ps (s >>>> multi' an)

compXS (Step Descendant nt ps) s
				= do
				  an <- compNTE nt
				  compPred ps (s >>>> mk getChildren >>>> multi' an)

compXS (Step Self nt ps) s
				= do
				  an <- compNTE nt
				  compPred ps (s >>>> an)
compXS (Step Attribute nt ps) s
				= do
				  an <- compNTA nt
				  compPred ps (s >>>> getAttrl' an)

compXS _ _			= deadEndStreet

compNTE				:: (Monad m) => NodeTest -> m (XPArrow XmlTree XmlTree)
compNTE (NameTest qn)		= compNameT isElem qn
compNTE nt			= compNT nt

compNTA				:: (Monad m) => NodeTest -> m (XPArrow XmlTree XmlTree)
compNTA (NameTest qn)		= compNameT isAttr qn
compNTA nt			= compNT nt

compNameT			:: Monad m => LA XmlTree XmlTree -> QName -> m (XPArrow XmlTree XmlTree)
compNameT ist qn
    | null (namespaceUri qn)	= return $ mk
				  ( if qualifiedName qn == "*"
				    then ist
				    else ist >>> hasName (qualifiedName qn)
				  )
    | otherwise			= return $ mk
				  ( if localPart qn == "*"
				    then ist >>> hasNamespaceUri (namespaceUri qn)
				    else ist >>> hasQName qn

				  )

compNT				:: Monad m => NodeTest -> m (XPArrow XmlTree XmlTree)
compNT (TypeTest XPNode)       	= return this'
compNT (TypeTest XPCommentNode)	= return $ mk isCmt
compNT (TypeTest XPPINode)      = return $ mk isPi
compNT (TypeTest XPTextNode)    = return $ mk isText

compNT _			= deadEndStreet

compPred			:: MonadPlus m => [Expr] -> XPArrow XmlTree XmlTree -> m (XPArrow XmlTree XmlTree)
compPred []     r		= return r
compPred (e:es)	r		= do
				  r1 <- compPred1 e r
				  compPred es r1

compPred1			:: MonadPlus m => Expr -> XPArrow XmlTree XmlTree -> m (XPArrow XmlTree XmlTree)
compPred1 e r			= ( do
				    ix <- compIntExpr e
				    return . mk $ listA (unwrap r) >>> xIndex ix
				  )
				  `mplus`
				  ( do
				    a1 <- compRelPathExpr e
				    return $ r >>>> (a1 `guards'` this')
				  )
				  `mplus`
				  ( do
				    a1 <- compGenExpr e
				    return $ r >>>> (a1 `guards'` this')
				  )
				  `mplus`
				  ( do
				    b1 <- compBoolExpr e
				    return $ if b1 then r else mk none
				  )

compRelPathExpr			:: MonadPlus m => Expr -> m (XPArrow XmlTree XmlTree)
compRelPathExpr (PathExpr Nothing (Just (LocPath Rel lp)))
				= compLP lp this'
compRelPathExpr _		= deadEndStreet

compStringExpr			:: MonadPlus m => Expr -> m String
compStringExpr (LiteralExpr s)	= return s
compStringExpr _		= deadEndStreet

compNumberExpr			:: MonadPlus m => Expr -> m XPNumber
compNumberExpr (NumberExpr n)	= return n
compNumberExpr (FctExpr "number" [f1])
				= ( do
				    b <- compBoolExpr f1
				    return $ if b then (Float 1) else Pos0
				  )
				  `mplus`
				  ( do
				    s <- compStringExpr f1
				    return $ toNumber s
				  )
compNumberExpr _		= deadEndStreet

compIntExpr			:: MonadPlus m => Expr -> m Int
compIntExpr e			= ( do
				    (Float f) <- compNumberExpr e
				    return (round f)
				  )
				  `mplus`
				  deadEndStreet


compBoolExpr			:: MonadPlus m => Expr -> m Bool
compBoolExpr (FctExpr f [])
    | f `elem` ["true", "false"]
				= return $ f == "true"
compBoolExpr (FctExpr "not" [f1])
				= do
				  v1 <- compBoolExpr f1
				  return $ not v1
compBoolExpr (LiteralExpr s)	= return $ not (null s)
compBoolExpr (NumberExpr n)	= return $ nz n
				  where
				  nz (Float f) = f /= 0
				  nz NegInf    = True
				  nz PosInf    = True
				  nz _         = False
compBoolExpr _			= deadEndStreet

compGenExpr			:: MonadPlus m => Expr -> m (XPArrow XmlTree XmlTree)
compGenExpr (GenExpr op [e1,e2])
				= compString op e1 e2		-- on arg is a string expr
				  `mplus`
				  compNumber op e1 e2		-- one arg is a number expr
				  `mplus`
				  compBool   op e1 e2		-- and/or
				  `mplus`
				  compPath   op e1 e2		-- nodeset equality

compGenExpr (GenExpr op (e1:el))
    | op `elem` [And, Or]	= compGenExpr (GenExpr op [e1, GenExpr op el])

compGenExpr _			= deadEndStreet

compString			:: MonadPlus m => Op -> Expr -> Expr -> m (XPArrow XmlTree XmlTree)
compString op e1 e2
    | op `elem` [Eq, NEq]	= ( do
				    s <- compStringExpr  e2
				    a <- compRelPathExpr e1
				    return $ mkEq' a s
				  )
				  `mplus`
				  ( do
				    s <- compStringExpr  e1
				    a <- compRelPathExpr e2
				    return $ mkEq' a s
				  )
				  where
				  mkEq' a' s' = mk ( ( xString a'
							>>>
							isA ( if op == Eq
							      then (== s')
							      else (/= s')
							    )
						      )
						      `guards` this	-- just for type match
						    )
compString _ _ _		= deadEndStreet

compNumber			:: MonadPlus m => Op -> Expr -> Expr -> m (XPArrow XmlTree XmlTree)
compNumber op e1 e2
    | op `elem` [Eq, NEq, Less, Greater, LessEq, GreaterEq]
				= ( do
				    n <- compNumberExpr  e2
				    a <- compRelPathExpr e1
				    return $ mkEq' a n
				  )
				  `mplus`
				  ( do
				    n <- compNumberExpr  e1
				    a <- compRelPathExpr e2
				    return $ mkEq' a n
				  )
				  where
				  mkEq' a' n' = mk ( ( xNumber' a'
							>>>
							isA (flip ( case op of
								    Eq        -> (==)
								    NEq       -> (/=)
								    Less      -> (<)
								    Greater   -> (>)
								    LessEq    -> (<=)
								    GreaterEq -> (>=)
								    _         -> error "compNumber: wrong arg"
								  ) n'
							    )
						      )
						      `guards` this
						    )
compNumber _ _ _		= deadEndStreet

compBool			:: MonadPlus m => Op -> Expr -> Expr -> m (XPArrow XmlTree XmlTree)
compBool And e1 e2		= ( do
				    b <- compBoolExpr e1
				    if b
				       then compGenExpr e2
				       else return $ mk none
				  )
				  `mplus`
				  ( do
				    b <- compBoolExpr e2
				    if b
				       then compGenExpr e1
				       else return $ mk none
				  )
				  `mplus`
				  ( do
				    a1 <- compGenExpr e1
				    a2 <- compGenExpr e2
				    return $ a1 `guards'` a2
				  )
compBool Or e1 e2		= ( do
				    b <- compBoolExpr e1
				    if b
                                       then return this'
				       else compGenExpr e2
				  )
				  `mplus`
				  ( do
				    b <- compBoolExpr e2
				    if b
                                       then return this'
				       else compGenExpr e1
				  )
				  `mplus`
				  ( do
				    a1 <- compGenExpr e1
				    a2 <- compGenExpr e2
				    return $ a1 <+>> a2
				  )
compBool _ _ _			= deadEndStreet

compPath			:: MonadPlus m => Op -> Expr -> Expr -> m (XPArrow XmlTree XmlTree)
compPath op e1 e2
    | op `elem` [Eq, NEq]	= ( do
				    a1 <- compRelPathExpr e2
				    a2 <- compRelPathExpr e1
				    return $ mk . cmp op $ ( ( listA (xString a1) &&& listA (xString a2))
							      >>>
							      eqs
							    )
				  )
				  where
				  eqs		= arr2L equalNodeSet
				  cmp Eq  a	= a `guards` this
				  cmp NEq a	= ifA a none this
				  cmp _ _	= error "compPath: wrong agruments"

compPath _ _ _			= deadEndStreet

-- ----------------------------------------

toNumber		:: String -> XPNumber
toNumber s		= let ( XPVNumber v) = parseNumber s in v

equalNodeSet		:: Eq a => [a] -> [a] -> [a]
equalNodeSet s1 s2	= [ x | x <- s1, y <- s2, x == y]

-- ----------------------------------------
