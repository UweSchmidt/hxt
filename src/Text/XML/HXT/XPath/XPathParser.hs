-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XPath.XPathParser
   Copyright  : Copyright (C) 2006-2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   the XPath Parser

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XPath.XPathParser
    ( parseNumber
    , parseXPath
    )
where

import Data.Maybe

import Text.ParserCombinators.Parsec

import Text.XML.HXT.DOM.TypeDefs

import Text.XML.HXT.XPath.XPathKeywords
import Text.XML.HXT.XPath.XPathDataTypes

import Text.XML.HXT.Parser.XmlTokenParser
    ( separator
    , systemLiteral
    , skipS0
    , ncName
    , qName
    )

lookupNs				:: NsEnv -> XName -> XName
lookupNs uris prefix
					-- downwards compatibility: if namespace env is not supported
					-- no error is raised, but the uri remains empty
					-- not conformant to XPath spec:
					-- If namespaces are used, a complete env must be supported,
					-- but we don't care about this
					= fromMaybe nullXName $ lookup prefix uris

enhanceAttrQName			:: NsEnv -> QName -> QName
enhanceAttrQName uris qn
    | isNullXName (namePrefix' qn)	= qn
    | otherwise				= enhanceQName uris qn

enhanceQName 				:: NsEnv -> QName -> QName
enhanceQName uris qn			= setNamespaceUri' ( lookupNs uris (namePrefix' qn) ) qn

enhanceQN				::  AxisSpec -> NsEnv -> QName -> QName
enhanceQN Attribute			= enhanceAttrQName
enhanceQN _				= enhanceQName

type XParser a = GenParser Char NsEnv a

-- ------------------------------------------------------------
-- parse functions which are used in the XPathFct module

-- |
-- parsing a number, parseNumber is used in "XPathFct"
-- by the number function
--
--    - returns : the parsed number as 'XPNumber' float
--                or 'XPVNumber' 'NaN' in case of error
parseNumber :: String -> XPathValue
parseNumber s
    = case (runParser parseNumber' [] {- Map.empty -} "" s) of
        Left _ -> XPVNumber NaN
        Right x  -> if (read x :: Float) == 0
                      then (XPVNumber Pos0)
                      else XPVNumber (Float (read x))

parseNumber' :: XParser String
parseNumber'
    = do
      skipS0
      m <- option "" (string "-")
      n <- number
      skipS0
      eof
      return (m ++ n)

-- ------------------------------------------------------------


-- |
-- the main entry point:
-- parsing a XPath expression

parseXPath :: XParser Expr
parseXPath
    = do
      skipS0
      xPathExpr <- expr
      skipS0
      eof
      return xPathExpr


-- some useful token and symbol parser
lpar, rpar, lbra, rbra, slash, dslash	:: XParser ()

lpar   = tokenParser (symbol "(")
rpar   = tokenParser (symbol ")")
lbra   = tokenParser (symbol "[")
rbra   = tokenParser (symbol "]")
slash  = tokenParser (symbol "/")
dslash = tokenParser (symbol "//")


tokenParser :: XParser String -> XParser ()
tokenParser p
    = try ( do
            skipS0
            p
            skipS0
           )


symbolParser :: (String, a) -> XParser a
symbolParser (s,a)
    = do
      tokenParser (symbol s)
      return a


symbol :: String -> XParser String
symbol s = try (string s)



--  operation parser
orOp, andOp, eqOp, relOp, addOp, multiOp, unionOp :: XParser Op

orOp  = symbolParser ("or", Or)
andOp =	symbolParser ("and", And)

eqOp
    = symbolParser ("=", Eq)
      <|> 
      symbolParser ("!=", NEq)

relOp
    = choice [ symbolParser ("<=", LessEq)
             , symbolParser (">=", GreaterEq)
             , symbolParser ("<", Less)
             , symbolParser (">", Greater)
             ]

addOp
    = symbolParser ("+", Plus)
      <|> 
      symbolParser ("-", Minus)


multiOp
    = choice [ symbolParser ("*", Mult)
             , symbolParser ("mod", Mod)
             , symbolParser ("div", Div)
             ]


unionOp	= symbolParser ("|", Union)

-- ------------------------------------------------------------

mkExprNode :: Expr -> [(Op, Expr)] -> Expr
mkExprNode e1 [] = e1
mkExprNode e1 l@((op, _): _) = 
    if null rest
      then GenExpr op (e1:(map snd l))
      else GenExpr op $ (e1:(map snd $ init same)) ++ [mkExprNode (snd $ last same) rest]
  where 
    (same, rest) = span ((==op) . fst) l
    
-- Tim Walkenhorst, original expr. below:
-- It seems mkExprNode is called only with operators of the same precedence, that should make it fixable
-- FIXED, see above!
--mkExprNode e1 l@((op, _): _) = GenExpr op (e1:(map snd l))  -- Less than ideal: 1+1-1 = 3 ???


--GenExpr op (e1:(map snd l))


exprRest :: XParser Op -> XParser Expr -> XParser (Op, Expr)
exprRest parserOp parserExpr
    = do
      op <- parserOp
      e2 <- parserExpr
      return (op, e2)


-- ------------------------------------------------------------

-- abbreviation of "//"
descOrSelfStep :: XStep 
descOrSelfStep = (Step DescendantOrSelf (TypeTest XPNode) [])

-- ------------------------------------------------------------
-- Location Paths (2)


-- [1] LocationPath 
locPath :: XParser LocationPath
locPath
    = absLocPath
      <|> 
      relLocPath'


-- [2] AbsoluteLocationPath
absLocPath :: XParser LocationPath
absLocPath
    = do -- [10]
      dslash
      s <- relLocPath
      return (LocPath Abs ([descOrSelfStep] ++ s))
      <|> 
      do
      slash
      s <- option [] relLocPath
      return (LocPath Abs s)
      <?> "absLocPath"


-- [3] RelativeLocationPath
relLocPath' :: XParser LocationPath
relLocPath'
    = do
      rel <- relLocPath
      return (LocPath Rel rel)

relLocPath :: XParser [XStep]
relLocPath
    = do
      s1 <- step
      s2 <- many (step')
      return ([s1] ++ (concat s2))
      <?> "relLocPath"


-- Location Steps (2.1)
--
-- [4] Step
step' :: XParser [XStep]
step'
    = do -- [11]
      dslash
      s <- step
      return [descOrSelfStep,s]
      <|>
      do
      slash
      s <- step
      return [s]
      <?> "step'"

step :: XParser XStep
step
    = abbrStep
      <|>
      do
      as <- axisSpecifier'
      nt <- nodeTest as
      pr <- many predicate
      return (Step as nt pr)
      <?> "step"


-- [5] AxisSpecifier
axisSpecifier' :: XParser AxisSpec
axisSpecifier'
    = do  -- [13]
      tokenParser (symbol "@")
      return Attribute
      <|>
      do
      as <- option Child ( try ( do -- child-axis is default-axis
                                 a <- axisSpecifier
                                 tokenParser (symbol "::")
                                 return a
                               )
                          )
      return as
      <?> "axisSpecifier'"


-- Axes (2.2)
--
-- [6] AxisName
axisSpecifier :: XParser AxisSpec
axisSpecifier
    = choice [ symbolParser (a_ancestor_or_self, AncestorOrSelf)
             , symbolParser (a_ancestor, Ancestor)
             , symbolParser (a_attribute, Attribute)
             , symbolParser (a_child, Child)
             , symbolParser (a_descendant_or_self, DescendantOrSelf)
             , symbolParser (a_descendant, Descendant)
             , symbolParser (a_following_sibling, FollowingSibling)
             , symbolParser (a_following, Following)
             , symbolParser (a_namespace, Namespace)
             , symbolParser (a_parent, Parent)
             , symbolParser (a_preceding_sibling, PrecedingSibling)
             , symbolParser (a_preceding, Preceding)
             , symbolParser (a_self, Self)
             ]
      <?> "axisSpecifier"


-- Node Tests (2.3)
--
-- [7] NodeTest
nodeTest :: AxisSpec -> XParser NodeTest
nodeTest as
    = do
      nt <- try nodeType'
      return (TypeTest nt)
      <|>
      do
      processInst <- pI
      return (PI processInst)
      <|>
      do
      nt <- nameTest as
      return (NameTest nt)
      <?> "nodeTest"

pI :: XParser String
pI
    = do
      tokenParser (symbol n_processing_instruction)
      li <- between lpar rpar literal
      return li
      <?> "Processing-Instruction"


-- Predicates (2.4)
--
-- [8] Predicate
-- [9] PredicateExpr
predicate :: XParser Expr
predicate 
    = do
      ex <- between lbra rbra expr
      return ex


-- Abbreviated Syntax (2.5)
--
-- [10] AbbreviatedAbsoluteLocationPath: q.v. [2]
-- [11] AbbreviatedRelativeLocationPath: q.v. [4]

-- [12] AbbreviatedStep
abbrStep :: XParser XStep
abbrStep 
    = do
      tokenParser (symbol "..")
      return (Step Parent (TypeTest XPNode) [])
      <|>
      do
      tokenParser (symbol ".")
      return (Step Self (TypeTest XPNode) [])
      <?> "abbrStep"

-- [13] AbbreviatedAxisSpecifier: q.v. [5]


-- ------------------------------------------------------------
-- Expressions (3)


-- Basics (3.1)
--
-- [14] Expr
expr :: XParser Expr
expr = orExpr


-- [15] PrimaryExpr
primaryExpr ::  XParser Expr
primaryExpr
    = do
      vr <- variableReference
      return (VarExpr vr)
      <|>
      do
      ex <- between lpar rpar expr
      return ex
      <|>
      do
      li <- literal
      return (LiteralExpr li)
      <|> 
      do
      num <- number
      return (NumberExpr (Float $ read num))
      <|>
      do
      fc <- functionCall
      return (fc)
      <?> "primaryExpr"


-- Function Calls (3.2)
--
-- [16] FunctionCall
-- [17] Argument
functionCall :: XParser Expr
functionCall
    = do
      fn <- functionName
      arg <- between lpar rpar ( sepBy expr (separator ',') )
      return (FctExpr fn arg)
      <?> "functionCall"


-- Node-sets (3.3)
--
-- [18] UnionExpr
unionExpr :: XParser Expr
unionExpr
    = do
      e1 <- pathExpr
      eRest <- many (exprRest unionOp pathExpr)
      return (mkExprNode e1 eRest)


-- [19] PathExpr
pathExpr :: XParser Expr
pathExpr
    = do
      fe <- try filterExpr
      path <- do
              dslash
              LocPath t1 t2 <- relLocPath'
              return (PathExpr (Just fe) (Just (LocPath t1 ([descOrSelfStep] ++ t2))))
              <|>
              do
              slash
              relPath <- relLocPath'
              return (PathExpr (Just fe) (Just relPath))
              <|>
              return fe
      return path
      <|>
      do
      lp <- locPath
      return (PathExpr Nothing (Just lp))
      <?> "pathExpr"


-- [20] FilterExpr
filterExpr :: XParser Expr
filterExpr
    = do
      prim <- primaryExpr
      predicates <- many predicate
      if length predicates > 0
        then return (FilterExpr (prim : predicates))
        else return prim
      <?> "filterExpr"


-- Booleans (3.4)
--
-- [21] OrExpr
orExpr :: XParser Expr
orExpr
    = do
      e1 <- andExpr
      eRest <- many (exprRest orOp andExpr)
      return (mkExprNode e1 eRest)
      <?> "orExpr"


-- [22] AndExpr
andExpr :: XParser Expr
andExpr
    = do
      e1 <- equalityExpr
      eRest <- many (exprRest andOp equalityExpr)
      return (mkExprNode e1 eRest)
      <?> "andExpr"


-- [23] EqualityExpr
equalityExpr :: XParser Expr
equalityExpr
    = do
      e1 <- relationalExpr
      eRest <- many (exprRest eqOp relationalExpr)
      return (mkExprNode e1 eRest)
      <?> "equalityExpr"


-- [24] RelationalExpr
relationalExpr :: XParser Expr
relationalExpr
    = do
      e1 <- additiveExpr
      eRest <- many (exprRest relOp additiveExpr)
      return (mkExprNode e1 eRest)
      <?> "relationalExpr"


-- Numbers (3.5)
--
-- [25] AdditiveExpr
additiveExpr :: XParser Expr
additiveExpr
    = do
      e1 <- multiplicativeExpr
      eRest <- many (exprRest addOp multiplicativeExpr)
      return (mkExprNode e1 eRest)
      <?> "additiveExpr"


-- [26] MultiplicativeExpr
multiplicativeExpr :: XParser Expr
multiplicativeExpr
    = do
      e1 <- unaryExpr
      eRest <- many (exprRest multiOp unaryExpr)
      return (mkExprNode e1 eRest)
      <?> "multiplicativeExpr"


-- [27] UnaryExpr
unaryExpr :: XParser Expr
unaryExpr
    = do
      tokenParser (symbol "-")
      u <- unaryExpr
      return (GenExpr Unary [u])
      <|>
      do
      u <- unionExpr
      return u
      <?> "unaryExpr"


-- Lexical Structure (3.7)
--
-- [29] Literal
-- systemLiteral from XmlParser is used
literal :: XParser String
literal = systemLiteral


-- [30] Number
number :: XParser String
number
    = do
      tokenParser (symbol ".")
      d <- many1 digit
      return ("0." ++ d)
      <|>
      do
      d <- many1 digit
      d1 <- option "" ( do
                        tokenParser (symbol ".")
                        d2 <- option "0" (many1 digit)
                        return ("." ++ d2)
                      )
      return (d ++ d1)
      <?> "number"


-- [35] FunctionName
-- no nodetype name is allowed as a function name
-- Tim Walkenhorst:
--   Change in String encoding for function name
--
--         previoulsy:      new:
--           
--         name             name
--         pref:name        {http://uri-for-pref}name

functionName :: XParser String
functionName
    = try ( do           
            (p, n) <- qName
            uris   <- getState
            u      <- return $ if null p
	                       then ""
                               else '{' : show (lookupNs uris (newXName p)) ++ "}"
            let fn = (u ++ n) in
              if fn `elem` ["processing-instruction", "comment", "text", "node"]
                then fail ("function name: " ++ fn ++ "not allowed")
                else return fn
          )
      <?> "functionName"


-- [36] VariableReference
variableReference :: XParser (String, String)
variableReference
    = do
      tokenParser (symbol "$")
      (p,n) <- qName
      uris   <- getState
      return (show (lookupNs uris (newXName p)), n)
      <?> "variableReference"


-- [37] NameTest
nameTest :: AxisSpec -> XParser QName
nameTest as
    = do tokenParser (symbol "*")
	 uris <- getState
	 return (enhanceQN as uris $ mkPrefixLocalPart "" "*")
      <|>
      try ( do pre <- ncName
               symbol ":*"
               uris <- getState
               return (enhanceQN as uris $ mkPrefixLocalPart pre "*")
	  )
      <|>
      do (pre,local) <- qName
	 uris <- getState
	 return (enhanceQN as uris $ mkPrefixLocalPart pre local)
      <?> "nameTest"

	
-- [38] NodeType
nodeType' :: XParser XPathNode
nodeType' 
    = do
      nt <- nodeType
      lpar
      rpar
      return nt
      <?> "nodeType'"
	
nodeType :: XParser XPathNode
nodeType 
    = choice [ symbolParser (n_comment, XPCommentNode)
             , symbolParser (n_text, XPTextNode)
             , symbolParser (n_processing_instruction, XPPINode)
             , symbolParser (n_node, XPNode)
             ]
      <?> "nodeType"

-- ------------------------------------------------------------
