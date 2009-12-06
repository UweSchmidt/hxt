{- |
   Module     : SPARQLDataTypes 
   Author     : Manuel Ohlendorf  

   Maintainer : Manuel Ohlendorf
   Stability  : experimental
   Portability: portable
   Version    :

The core SPARQL query language data types. Every data type has two constructors: 

One for the parser result and the other for the normalised version of this data type.
-}
module SPARQLDataTypes
    (Query (..)
    ,GraphPattern (..)
    ,PatternElement (..)
    ,TriplePattern (..)
    )
where

import RDFDataTypes

-- | Represents a SPARQL query
data Query = Query [Var] GraphPattern       -- ^ the parse result: a list of variables from the SELECT-Clause 
                                            --  and the graph pattern from the WHERE-clause.
            | QueryN [Var] [TriplePattern]  -- ^ the normalised query: a list of variables and a list of triple patterns 
            | Empty                         -- ^ the empty query: only returned when parser fails
            deriving (Show,Ord,Eq)

{- two versions of triple patterns. the first one is the unnomalized version with predicate-object and object list abbreviations. the second is normalized. only this can be processed by the query evaluator -}
-- | Represents a triple pattern
data TriplePattern = TriplePat Subject [(Predicate, [Object])] -- ^ triple pattern, 
                                                               -- containing predicate-object and object list abbreviations
                    | TriplePatN Subject Predicate Object      -- ^ normalised triple pattern with simple triples
                    deriving (Show,Ord,Eq)

-- | The graph pattern, only used by the SPARQL parser.
data GraphPattern = GraphPattern [PatternElement]
                    deriving (Show,Ord,Eq)

-- | The pattern element 
data PatternElement = PET TriplePattern     -- ^ represents a query with a single triple pattern
                    | PEG [PatternElement]  -- ^ represents a query with a list of triple patterns
                    deriving (Show,Ord,Eq)








