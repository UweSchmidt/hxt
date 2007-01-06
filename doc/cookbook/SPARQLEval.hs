{- |
   Module     : SPARQLEval 
   Author     : Manuel Ohlendorf  

   Maintainer : Manuel Ohlendorf
   Stability  : experimental
   Portability: portable
   Version    :

The core evaluation function.
-}
module SPARQLEval
    (evalQuery)

where

import Text.XML.HXT.DOM.Util (noDoubles)
import SPARQLDataTypes
import SPARQLFunctions
import RDFDataTypes
import List

-- | a helper data type, only for internal use
data Binding = BT Var RDFTerm | BU Var URI deriving (Show,Eq,Ord)

mkTBind :: Var -> RDFTerm -> Binding
mkTBind v t = BT v t

mkUBind :: Var -> URI -> Binding
mkUBind v u = BU v u

-- | generates all possible bindings, also returns the triple with the containing binding. This is not needed yet, but
-- can neccesary later.
substituteOne :: Triple -> TriplePattern -> [(Triple,[Binding])]
substituteOne tr@(Triple (Subject a) (Predicate b) (Object c)) (TriplePatN (Subject x) (Predicate y) (Object z)) 
 = if (a == x &&  b == y && c == z) then [(tr,[])] else []

substituteOne tr@(Triple (Subject a) (Predicate b) (Object c)) (TriplePatN (SubjectVar x) (Predicate y) (Object z)) 
 = if (b == y && c == z) then [(tr,[mkTBind x a])] else []

substituteOne tr@(Triple (Subject a) (Predicate b) (Object c)) (TriplePatN (Subject x) (PredicateVar y) (Object z)) 
 = if (a == x && c == z) then [(tr,[mkUBind y b])] else []

substituteOne tr@(Triple (Subject a) (Predicate b) (Object c)) (TriplePatN (Subject x) (Predicate y) (ObjectVar z)) 
 = if (a == x &&  b == y) then [(tr,[mkTBind z c])] else []

substituteOne tr@(Triple (Subject a) (Predicate b) (Object c)) (TriplePatN (SubjectVar x) (PredicateVar y) (Object z)) 
 | x == y =  if (equalRDFTermURI a b && c == z) then [(tr,[(mkTBind x a),(mkUBind y b)])] else []
 | otherwise = if (c == z) then [(tr,[(mkTBind x a),(mkUBind y b)])] else []

substituteOne tr@(Triple (Subject a) (Predicate b) (Object c)) (TriplePatN (SubjectVar x) (Predicate y) (ObjectVar z)) 
 | x == z =  if (a == c && b == y) then [(tr,[(mkTBind x a),(mkTBind z c)])] else []
 | otherwise = if (b == y) then [(tr,[(mkTBind x a),(mkTBind z c)])] else []

substituteOne tr@(Triple (Subject a) (Predicate b) (Object c)) (TriplePatN (Subject x) (PredicateVar y) (ObjectVar z)) 
 | y == z =  if (equalRDFTermURI c b && a == x) then [(tr,[(mkUBind y b),(mkTBind z c)])]  else []
 | otherwise = if (a == x) then [(tr,[(mkUBind y b),(mkTBind z c)])] else []

substituteOne tr@(Triple (Subject a) (Predicate b) (Object c)) (TriplePatN (SubjectVar x) (PredicateVar y) (ObjectVar z))
 | x == y && y == z =  if (equalRDFTermURI a b && equalRDFTermURI c b) 
                       then [(tr,[(mkTBind x a), (mkUBind y b),(mkTBind z c)])] else []
 | x == y =  if (equalRDFTermURI a b) 
             then [(tr,[(mkTBind x a), (mkUBind y b),(mkTBind z c)])] else []
 | y == z =  if (equalRDFTermURI c b) 
             then [(tr,[(mkTBind x a), (mkUBind y b),(mkTBind z c)])] else []
 | x == z =  if (a == c) 
             then [(tr,[(mkTBind x a), (mkUBind y b),(mkTBind z c)])] else []
 | x/=y && y/=z && x/=z  = [(tr,[(mkTBind x a), (mkUBind y b),(mkTBind z c)])]
 | otherwise = []
substituteOne (Triple _ _ _) _ = [] 

-- different selector functions.

getBindings :: [(Triple,[Binding])] -> [[Binding]]
getBindings [] = []
getBindings (x:xs) = [snd x] ++ getBindings xs

getVarBinding :: Var -> Binding -> [(Either RDFTerm URI)]
getVarBinding v (BT a b) = if v==a then [ (Left b)] else []
getVarBinding v (BU a b) = if v==a then [ (Right b)] else []

getVarBindings :: Var -> [Binding] -> [(Either RDFTerm URI)]
getVarBindings _ [] = []
getVarBindings v (x:xs) = getVarBinding v x ++ getVarBindings v xs

getAllVarBindings :: [Var] -> [Binding] -> [(Var,[(Either RDFTerm URI)])]
getAllVarBindings [] _ = []
getAllVarBindings (x:xs) ys = (x,(getVarBindings x ys)) : getAllVarBindings xs ys


substitute :: [Triple] -> [TriplePattern] ->  [(Triple,[Binding])]
substitute [] [] = []
substitute _ []  = []
substitute [] _ = []
substitute (x:xs) ys = concat (map (substituteOne x) ys) ++ substitute xs ys



-- | The evaluation function. 
-- Takes a 'Query' and a 'RDFStore' and generates a list of possible bindings between variables and concrete values.
-- If the 'Query' is not in the normalised form, it is normalised before evaluation starts.
evalQuery :: Query -> RDFStore -> [(Var,[(Either RDFTerm URI)])] 
evalQuery Empty _ = []
evalQuery _ [] = []
evalQuery (Query a b ) store = evalQuery (normalizeQuery (Query a b)) store 
evalQuery (QueryN var tr) store = getAllVarBindings var (concat(getBindings(noDoubles (substitute store tr))))



