{- |
   Module     : SPARQLFunctions 
   Author     : Manuel Ohlendorf  

   Maintainer : Manuel Ohlendorf
   Stability  : experimental
   Portability: portable
   Version    :

Several functions used for query processing.
-}
module SPARQLFunctions
    (
    normalizeQuery
    ,showResult
    ,getSPARQLQuery
    )
where

import SPARQLDataTypes
import RDFDataTypes
import SPARQLParser
import Text.ParserCombinators.Parsec (parse)
import Text.XML.HXT.Arrow

-- | normalises the Triple Patterns
normalizeTripleP :: TriplePattern -> [TriplePattern]
normalizeTripleP (TriplePatN s p o ) = [TriplePatN s p o]
normalizeTripleP (TriplePat _ [])          = []
normalizeTripleP (TriplePat _ ((_,[]):_)) = []
normalizeTripleP (TriplePat s ((p,[o]):xs)) =  (TriplePatN s p o) : normalizeTripleP(TriplePat s xs)
normalizeTripleP (TriplePat s ((p,(x:xs)):ys)) = (TriplePatN s p x) : normalizeTripleP (TriplePat s [(p,xs)]) 
                                                ++ normalizeTripleP (TriplePat s ys)

-- | normalises the Pattern elements
normalizePatternElement :: PatternElement -> [TriplePattern]
normalizePatternElement (PET t) = normalizeTripleP t
normalizePatternElement (PEG []) = []
normalizePatternElement (PEG (x:xs)) = normalizePatternElement x ++ normalizePatternElement' xs
                                        
normalizePatternElement' :: [PatternElement] -> [TriplePattern]
normalizePatternElement' [] = []
normalizePatternElement' (x:xs) = normalizePatternElement x  ++ normalizePatternElement' xs

-- | normalises the graph patterns
normalizeGraphPattern :: GraphPattern -> [TriplePattern]
normalizeGraphPattern (GraphPattern []) = []
normalizeGraphPattern (GraphPattern (x:xs)) = normalizePatternElement x ++ normalizePatternElement' xs 

-- | normalises a 'Query' by removing all abbreviations. 
-- The result is a 'Query' containing a list of simple triple patterns.
-- This function has to be called be for query evaluation.
normalizeQuery :: Query -> Query
normalizeQuery Empty = Empty
normalizeQuery (QueryN _ [])  = Empty
normalizeQuery (QueryN v (x:xs)) = QueryN v (x:xs)
normalizeQuery (Query v gp) = QueryN v (normalizeGraphPattern gp)

showIt :: [(Either RDFTerm URI)] -> String
showIt [] = ""
showIt ((Right u) :xs) = '|' : "<" ++ u ++">|\n" ++ showIt xs
showIt ((Left r) :xs)  = '|' : show r ++"|\n" ++ showIt xs

-- | The show function for printing out the result of the query evaluation
showResult :: [(Var,[(Either RDFTerm URI)])] -> String
showResult [] = ""
showResult ((var, xs):ys) 
    = if (xs /=[]) 
      then '\t' : '?':var ++ "\n" ++ (showIt xs) ++"\n"++ (showResult ys) 
      else ""


-- | The main entry point, when combining the SPARQL parser with a arrow based programme.
-- Executes the SPARQL parser and returns a 'Query'.
--
-- Raises an error if the parsing fails and returns a empty 'Query'.
--
-- The extra parameter is the query string which will be parsed.
-- The arrow input is ignored, the output is the parsed query.
getSPARQLQuery :: String -> IOSArrow a Query 
getSPARQLQuery queryStr
    = case (parse parseSPARQL "" queryStr) of
      Left parseError
       -> (issueFatal ("Syntax error in SPARQL query " 
                        ++ show queryStr 
                        ++ ": " ++ show parseError) >>> constA (Empty))
      Right theQuery
       -> constA (theQuery)
