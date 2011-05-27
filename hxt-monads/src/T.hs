module Main where

import Control.Monad
import Control.Monad.ReaderStateIOError

type Env = [(String, Int)]
type State = [(String, Int)]
type Err = String

type Result r = ReaderStateIOError Env State Err r

-- ----------------------------------------
-- syntactic domains
 
data Expr  = Const  Int
           | Var    Id
           | Let    Id    Expr Expr
	   | Assign Id    Expr
           | Binary BinOp Expr Expr
	   | Try    Expr  Expr
             deriving (Show)
 
data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)
 
type Id    = String

-- ----------------------------------------
-- the meaning of an expression
 
eval :: Expr -> Result Int
eval (Const i)
  = return i
 
eval (Binary op l r)
  = do
    mf <- lookupMft op
    mf (eval l) (eval r)
 
eval (Var id)
  = do
    env <- ask
    case lookup id env of			-- 1. lookup env
      Nothing -> do
                 state <- get
		 case lookup id state of 	-- 2. lookup state
		   Nothing -> throwError
			      $ "free variable "
				++ show id
			        ++ " found"
		   Just v -> return v
      Just v  -> return v
 
eval (Let id e1 e2)
  = do
    v1 <- eval e1
    local (addEnv id v1)
          (eval e2)
    where
    addEnv i v = ((i, v) :)

eval (Assign id e1)
  = do
    v1 <- eval e1
    state <- get
    put (store id v1 state)
    return v1
    where
    store i v s = (i, v) : filter ((/= i) . fst) s

eval (Try e1 e2)
    = eval e1 `mplus` eval e2
 
-- ----------------------------------------
-- the meaning of binary operators
 
type MF = Result Int -> Result Int ->
          Result Int
 
lookupMft :: BinOp -> Result MF
lookupMft op
  = case lookup op mft of
    Nothing -> throwError
               "operation not yet implemented"
    Just mf -> return mf
 
mft :: [(BinOp, MF)]
mft
  = [ (Add, liftM2 (+))
    , (Sub, liftM2 (-))
    , (Mul, liftM2 (*))
    , (Div, \ x -> join . liftM2 div' x)
    ]
 
div' :: Int -> Int -> Result Int
div' x y
  | y == 0    = throwError
                "division by zero"
  | otherwise = return (x `div` y)

-- ----------------------------------------
-- evaluate an expression within a given env

evalExpr expr env state
    = runReaderStateIOError (eval expr) env state

eval' e = evalExpr e [] []
evalEnv e env = evalExpr e env []

-- ----------------------------------------
-- sample expressions
 
e1 = Binary Mul (Assign "b" (Binary Add (Const 2)
                             (Const 4)
			    )
		)
                (Assign "a" (Const 7))
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)
e4 = (Assign "c" (Var "x"))
e5 = Binary Mul (Binary Add e4
                            (Const 1)
                ) e4
e6 = (Try e1 (Const 1))
e7 = (Try e1 e2)
e4' = Let "x" (Const 42) e4
e5' = Let "x" (Const 6)  e5 
 
v1 = eval' e1
v2 = eval' e2
v3 = eval' e3
v6 = eval' e6
 
v4  = evalEnv e4 [("x", 42)]
v5  = evalEnv e5 [("x",  6)]
v4' = eval'   e4'
v5' = eval'   e5'
