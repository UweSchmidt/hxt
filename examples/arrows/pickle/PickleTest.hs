module Main
where

import Data.Maybe
import System.Exit
import Test.HUnit

import Text.XML.HXT.Arrow

-- ------------------------------------------------------------
--
-- a somewhat complex data structure
-- for representing programs of a simple
-- imperative language

type Program	= Stmt

type StmtList	= [Stmt]

data Stmt
    = Assign  Ident  Expr
    | Stmts   StmtList 
    | If      Expr  Stmt (Maybe Stmt)
    | While   Expr  Stmt
      deriving (Eq, Show)

type Ident	= String

data Expr
    = IntConst	Int
    | BoolConst Bool
    | Var       Ident
    | UnExpr	UnOp  Expr
    | BinExpr	Op    Expr  Expr
      deriving (Eq, Show)

data Op
    = Add | Sub | Mul | Div | Mod | Eq | Neq
      deriving (Eq, Ord, Enum, Show)

data UnOp
    = UPlus | UMinus | Neg
      deriving (Eq, Ord, Read, Show)

-- ------------------------------------------------------------
--
-- the pickler definition for the data types

-- the main pickler

xpProgram :: PU Program
xpProgram = xpickle

instance XmlPickler UnOp where
    xpickle = xpPrim

instance XmlPickler Op where
    xpickle = xpWrap (toEnum, fromEnum) xpPrim

instance XmlPickler Expr where
    xpickle = xpAlt tag ps
	where
	tag (IntConst _    ) = 0
	tag (BoolConst _   ) = 1
	tag (Var _         ) = 2
	tag (UnExpr _ _    ) = 3
	tag (BinExpr _ _ _ ) = 4
	ps = [ xpWrap ( IntConst
		      , \ (IntConst i ) -> i ) ( xpElem "int"  $
					         xpAttr "value" $
					         xpickle )

	     , xpWrap ( BoolConst
		      , \ (BoolConst b) -> b)  ( xpElem "bool" $
						 xpAttr "value" $
						 xpWrap (toEnum, fromEnum) xpickle )

	     , xpWrap ( Var
		      , \ (Var n)       -> n)  ( xpElem "var"  $
						 xpAttr "name"  $
						 xpText )

	     , xpWrap ( uncurry UnExpr
		      , \ (UnExpr op e) -> (op, e))
                                               ( xpElem "unex" $
						 xpPair (xpAttr "op" xpickle) xpickle )

	     , xpWrap ( uncurry3 $ BinExpr
		      , \ (BinExpr op e1 e2) -> (op, e1, e2))
                                               ( xpElem "binex" $
						 xpTriple (xpAttr "op" xpickle) xpickle xpickle )
	     ]

instance XmlPickler Stmt where
    xpickle = xpAlt tag ps
	where
	tag ( Assign _ _ ) = 0
	tag ( Stmts _ )    = 1
	tag ( If _ _ _ )   = 2
	tag ( While _ _ )  = 3
	ps = [ xpWrap ( uncurry Assign
		      , \ (Assign n v) -> (n, v))
                                               ( xpElem "assign" $
						 xpPair (xpAttr "name" xpText) xpickle )
	     , xpWrap ( Stmts
		      , \ (Stmts sl) -> sl)    ( xpElem "block" $
						 xpList xpickle )
	     , xpWrap ( uncurry3 If
		      , \ (If c t e) -> (c, t, e))
                                               ( xpElem "if" $
						 xpTriple xpickle xpickle xpickle )
	     , xpWrap ( uncurry While
		      , \ (While c b) -> (c, b))
                                               ( xpElem "while" $
						 xpPair xpickle xpickle )
	     ]

-- ------------------------------------------------------------
--
-- example programs

progs	:: [Program]
progs	= [p0, p1, p2]

p0, p1, p2 :: Program

p0 = Stmts []		-- the empty program

p1 = Stmts		
     [ Assign i ( UnExpr UMinus ( IntConst (-22) ) )
     , Assign j ( IntConst 20 )
     , While
       ( BinExpr Neq ( Var i ) ( IntConst 0 ) )
       ( Stmts
	 [ Assign i ( BinExpr Sub ( Var i ) ( IntConst 1 ) )
	 , Assign j ( BinExpr Add ( Var j ) ( IntConst 1 ) )
	 , If ( IntConst 0 ) (Stmts []) Nothing
	 ]
       )
     ]
    where
    i = "i"
    j = "j"

p2 = Stmts		
     [ Assign x (IntConst 6)
     , Assign y (IntConst 7)
     , Assign p (IntConst 0)
     , While
       ( BinExpr Neq (Var x) (IntConst 0) )
       ( If ( BinExpr Neq ( BinExpr Mod (Var x) (IntConst 2) ) (IntConst 0) )
	    ( Stmts
	      [ Assign x ( BinExpr Sub (Var x) (IntConst 1) )
	      , Assign p ( BinExpr Add (Var p) (Var y) )
	      ]
	    )
	    ( Just ( Stmts
		     [ Assign x ( BinExpr Div (Var x) (IntConst 2) )
		     , Assign y ( BinExpr Mul (Var y) (IntConst 2) )
		     ]
		   )
	    )
       )
     ]
    where
    x = "x"
    y = "y"
    p = "p"

-- ------------------------------------------------------------

-- |
-- the complete set of test cases

pickleUnpickleTests	:: Test
pickleUnpickleTests
    = TestLabel "pickle/unpickle tests with example programs" $
      TestList $
      map mkTests progs
    where
    mkTests p
	= TestList $
	  [ TestCase $
	    assertEqual "pickleDoc/unpickleDoc without XML serialisation: " [p] res1

	  , TestCase $
	    assertEqual "pickleDoc/unpickleDoc with xshow/xread: " [p] res2

	  , TestCase $
	    do
	    res <- res4
	    assertEqual "pickle/unpickle with readFromString: " [p] res

	  , TestCase $
	    res5 >>= 
	    assertEqual "pickle/unpickle with writeDocument/readDocument: " [p]

	  , TestCase $
	    res6 >>= 
	    assertEqual "pickle/unpickle with xpickleDocument/xunpickleDocument: " [p]
	  ]
	where
	res1	:: [Program]
	res1 = maybeToList . unpickleDoc xpProgram . pickleDoc xpProgram $ p

	res2	:: [Program]
	res2 = runLA
	       ( xshow ( arr (pickleDoc xpProgram)
			 >>>
			 getChildren
		       )
		 >>>
		 root [] [xread]
		 >>>
		 arrL (maybeToList . unpickleDoc xpProgram)
	       ) p

	res4	:: IO [Program]
	res4 = runX
	       ( constA p
		 >>>
		 arr (pickleDoc xpProgram)			-- Program => XmlTree
		 >>>
		 writeDocumentToString []			-- XmlTree => String
		 >>>
		 readFromString [(a_validate, v_0)]		-- String => XmlTree
		 >>>
		 arrL (maybeToList . unpickleDoc xpProgram)	-- XmlTree => Program
	       )

	res5	:: IO [Program]					-- the most important case
								-- for persistent data storage
								-- and message passing
	res5 = runX
	       ( constA p					-- take the Program value
		 >>>
		 arr (pickleDoc xpProgram)			-- Program => XmlTree
		 >>>
		 writeDocument [ (a_indent, v_1)		-- XmlTree => formated external XML document
			       ] "pickle.xml"
		 >>>
		 readDocument  [ (a_remove_whitespace, v_1)	-- formated external XML document => XmlTree
			       , (a_validate, v_0)
			       ] "pickle.xml"
		 >>>
		 arrL (maybeToList . unpickleDoc xpProgram)	-- XmlTree => Program
	       )

	res6	:: IO [Program]					-- the most important case
								-- for persistent data storage
								-- and message passing
								-- same as res5, but the convenient way
	res6 = runX
	       ( constA p					-- take the Program value
		 >>>
		 xpickleDocument   xpProgram
                                 [ (a_indent, v_1)		-- Program => formated external XML document
				 ] "pickle.xml"
		 >>>
		 xunpickleDocument xpProgram
                                   [ (a_remove_whitespace, v_1)	-- formated external XML document => Program
				   , (a_validate, v_0)
				   ] "pickle.xml"
	       )

allTests	:: Test
allTests
    = TestList
      [ pickleUnpickleTests
      -- , pickleXshowTests
      ]

main	:: IO ()
main
    = do
      c <- runTestTT allTests
      putStrLn $ show c
      let errs = errors c
	  fails = failures c
      exitWith (codeGet errs fails)

codeGet	:: Int -> Int -> ExitCode
codeGet errs fails
    | fails > 0       = ExitFailure 2
    | errs > 0        = ExitFailure 1
    | otherwise       = ExitSuccess

-- ----------------------------------------------------------
