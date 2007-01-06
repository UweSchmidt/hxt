-- |
-- The core data types of XPath.
-- The Type NodeSet is based on the module "NavTree" which was adapted from
-- HXML (<http://www.flightlab.com/~joe/hxml/>)
--


module Text.XML.HXT.XPath.XPathDataTypes
    ( module Text.XML.HXT.XPath.XPathDataTypes
    , module Text.XML.HXT.XPath.NavTree
    )
where

import Text.XML.HXT.XPath.NavTree
import Text.XML.HXT.DOM.XmlTree

-- -----------------------------------------------------------------------------
--
-- Expr

-- | Represents expression
-- 

data Expr     = GenExpr Op [Expr]             -- ^ generic expression with an operator and one or more operands
              | PathExpr (Maybe Expr) (Maybe LocationPath)
	                                      -- ^ a path expression contains an optional filter-expression
					      -- or an optional locationpath. one expression is urgently
					      -- necessary, both are possible
              | FilterExpr [Expr]             -- ^ filter-expression with zero or more predicates
              | VarExpr VarName               -- ^ variable
              | LiteralExpr Literal           -- ^ string
              | NumberExpr XPNumber           -- ^ number
              | FctExpr FctName FctArguments  -- ^ a function with a name and an optional list of arguments
              deriving (Show, Eq)


-- -----------------------------------------------------------------------------
--
-- Op

-- | Represents XPath operators

data Op       = Or | And | Eq | NEq | Less | Greater | LessEq
              | GreaterEq |Plus | Minus | Div | Mod | Mult| Unary | Union
              deriving (Show, Eq)


-- -----------------------------------------------------------------------------
--
-- |
-- Represents a floating-point number according the IEEE 754 standard
--
-- The standard includes a special Not-a-Number (NaN) value,
-- positive and negative infinity, positive and negative zero.

data XPNumber = Float Float  -- ^ floating-point number
              | NaN          -- ^ not-a-number
              | NegInf       -- ^ negative infinity
              | Neg0         -- ^ negative zero
              | Pos0         -- ^ positive zero
              | PosInf       -- ^ positive infinity


instance Show XPNumber
    where
    show NaN           = "NaN"
    show NegInf        = "-Infinity"
    show Neg0          = "0"
    show Pos0          = "0"
    show (Float f)     = show f
    show PosInf        = "Infinity"



-- Negative zero is equal to positive zero,
-- equality test with NaN-value is always false
instance Eq XPNumber
    where
    NegInf  == NegInf  = True
    Pos0    == Neg0    = True
    Neg0    == Pos0    = True
    Pos0    == Pos0    = True
    Neg0    == Neg0    = True
    Float f == Float g = f == g
    PosInf  == PosInf  = True
    _       == _       = False


instance Ord XPNumber
    where
    a       <= b       = (a < b) || (a == b)
    a       >= b       = (a > b) || (a == b)
    a       >  b       =  b < a

    NaN     < _        = False
    _       < NaN      = False

    _       < NegInf   = False
    NegInf  < _	       = True

    Neg0    < Neg0     = False
    Pos0    < Pos0     = False
    Pos0    < Neg0     = False
    Neg0    < Pos0     = False

    Neg0    < Float f  = 0 < f
    Pos0    < Float f  = 0 < f
    Float f < Neg0     = f < 0
    Float f < Pos0     = f < 0

    Float f < Float g  = f < g

    PosInf  < _        = False
    _       < PosInf   = True


-- -----------------------------------------------------------------------------
-- | Represents location path
--
-- A location path consists of a sequence of one or more location steps.

data LocationPath = LocPath Path [XStep]
                  deriving (Show, Eq)


-- -----------------------------------------------------------------------------                  
-- |
-- A location path is either a relative or an absolute path.

data Path         = Rel | Abs
                  deriving (Show, Eq)
					

-- | Represents location step
-- 
-- A location step consists of an axis, a node-test and zero or more predicates.

data XStep        = Step AxisSpec NodeTest [Expr]
                  deriving (Show, Eq)

			
-- -----------------------------------------------------------------------------
--
-- AxisSpec

-- | Represents XPath axis

data AxisSpec     = Ancestor | AncestorOrSelf | Attribute | Child | Descendant  
                  | DescendantOrSelf | Following | FollowingSibling
                  | Namespace | Parent | Preceding | PrecedingSibling | Self
                  deriving (Show, Eq)


-- -----------------------------------------------------------------------------
--
-- NodeTest

-- | Represents XPath node-tests

--data NodeTest     = NameTest Name     -- ^ name-test
data NodeTest     = NameTest QName     -- ^ name-test
                  | PI String           -- ^ processing-instruction-test with a literal argument
                  | TypeTest XPathNode  -- ^ all nodetype-tests
                  deriving (Show, Eq)





-- -----------------------------------------------------------------------------
--
-- XPathNode

-- | Represents nodetype-tests

data XPathNode    = XPNode            -- ^ all 7 nodetypes
                                      --  (root, element, attribute, namespace, pi, comment, text)
                  | XPCommentNode     -- ^ comment-nodes
                  | XPPINode          -- ^ processing-instruction-nodes
                  | XPTextNode        -- ^ text-nodes: cdata, character data
                  deriving (Show, Eq)


-- -----------------------------------------------------------------------------
--
-- useful type definitions

type Name = (NamePrefix, LocalName)
type NamePrefix = String
type LocalName = String

-- | Variable name
type VarName      = Name

-- | a string
type Literal      = String					

-- | Function name
type FctName      = String

-- | Function arguments
type FctArguments = [Expr]

-- | Evaluation context
type Context      = (ConPos ,ConLen, ConNode)

-- | Context position
type ConPos       = Int

-- | Context length
type ConLen       = Int

-- | Context node
type ConNode      = NavXmlTree



-- -----------------------------------------------------------------------------
--
-- XPathValue

-- | Represents XPath results

data XPathValue   = XPVNode NodeSet      -- ^ node-set
                  | XPVBool Bool         -- ^ boolean value
                  | XPVNumber XPNumber   -- ^ number according the IEEE 754 standard
                  | XPVString String     -- ^ string value
                  | XPVError String      -- ^ error message with text
                  deriving (Show, Eq, Ord)


-- -----------------------------------------------------------------------------
--
-- Basic types for navigable tree and filters

-- | Node of navigable tree representation

type NavXmlTree   = NavTree XNode

-- | List of nodes of navigable tree representation

type NavXmlTrees  = [NavXmlTree]

-- | Type synonym for a list of navigable tree representation

type NodeSet      = NavXmlTrees

-- | A functions that takes a XPath result and returns a XPath result

type XPathFilter  = XPathValue -> XPathValue



-- -----------------------------------------------------------------------------
--
-- Env

-- | XPath environment
--
-- All variables are stored in the environment,
-- each variable name is bound to a value.

type VarTab       = [(VarName, XPathValue)]
type KeyTab       = [(QName, String, NavXmlTree)]

type Env          = (VarTab, KeyTab)

varEnv :: Env
varEnv = ( [ (("", "name"), XPVNumber NaN) ]
	 , []
         )

-- -----------------------------------------------------------------------------
