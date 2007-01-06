{- |
   Module     : RDFDataTypes 
   Author     : Manuel Ohlendorf

   Maintainer : Manuel Ohlendorf
   Stability  : experimental
   Portability: portable
   Version    : 

The core data types for RDF processing. 

The constructor functions for the data types are also listed hier. 

Some of the data types are also used by the SPARQL parser.

-}
module RDFDataTypes 
    (RDFStore 
    ,showRDFStore
    ,Triple (..) 
    ,Object (..)
    ,Predicate (..)
    ,Subject (..)
    ,RDFTerm (..)
    ,URI
    ,Var
    ,mkPredicate
    ,mkTriple
    ,mkResource
    ,mkLangLiteral
    ,mkLiteral
    ,mkTypedLiteral
    ,mkSubjectBlankNode
    ,mkURIRef
    ,mkObjectBlankNode
    ,equalRDFTermURI)
where

import Char (toLower)

 
{- | 
    Represents an RDF triple. The Show-implementation returns it in N-Triples syntax.
    Subject, Predicate and Object are shared by the SPARQParser and RDFParser.
    That's why every data type can hold a 'Var' which is needed by the SPARQL Parser
-}
data Triple = Triple Subject Predicate Object deriving (Ord,Eq)


-- | returns the triples in N-Triples syntax
instance Show Triple
  where
  show (Triple s p o)   = show s ++ " "++ show p ++" " ++ show o++" ."


-- | Represents a Subject or a Subject variable 
data Subject = Subject RDFTerm -- ^ used by RDF parser and SPARQL parser
                | SubjectVar Var -- ^ used only by SPARQL parser, represents the variable of a query
                deriving (Ord,Eq)

instance Show Subject
    where
    show (Subject r)    = show r
    show (SubjectVar v) = show v

-- | Represents a Predicate or a Predicate variable  
data Predicate = Predicate URI -- ^ used by RDF parser and SPARQL parser
                | PredicateVar Var -- ^ used only by SPARQL parser, represents the variable of a query
                deriving (Ord,Eq)

instance Show Predicate
    where
    show (Predicate u)    = '<' : u ++ ">"
    show (PredicateVar v) = show v

-- | Represents a Object or a Object variable
data Object = Object RDFTerm -- ^ used only by SPARQL parser, represents the variable of a query
              | ObjectVar Var -- ^ used only by SPARQL parser, represents the variable of a query
              deriving (Ord,Eq)

instance Show Object 
    where
    show (Object r)    = show r
    show (ObjectVar v) = show v

-- | The variable in SPARQL query
type Var = String


-- | Represents all logical units of a Subject or Object 
data RDFTerm = URIRef URI                       -- ^ a URI reference
               | RDFLiteral String              -- ^ a plain literal
               | RDFLangLiteral String String   -- ^ a literal with language extension (literal, language)
               | RDFTypedLiteral String URI     -- ^ a typed literal (literal, type uri)
               | BlankNode String               -- ^ a blank node with blank node identifier
                 deriving (Ord,Eq)

-- | A Uniform Resource Identifier
type URI = String

instance Show RDFTerm
  where
  show (URIRef uri)              = '<' : uri ++ ">"
  show (BlankNode i)             = '_' : ':' : i
  show (RDFLiteral lit)          = '"' : lit ++ "\"" 
  show (RDFLangLiteral lit lang) = '"' : lit ++ "\"" ++ "@" ++ lang
  show (RDFTypedLiteral l u)     = '\"' : l ++ "\"^^" ++"<"++ u ++">"


-- the different construtor functions

-- | 'Subject' constructor with URIref
mkURIRef :: URI -> Subject
mkURIRef u = Subject (URIRef u)

-- | 'Subject' constructor with blank node identifier
mkSubjectBlankNode :: String -> Subject
mkSubjectBlankNode i = Subject (BlankNode i)

-- | 'Predicate' constructor with URIref
mkPredicate :: URI -> Predicate
mkPredicate u = Predicate u 

-- | 'Object' constructor with blank node identifier
mkObjectBlankNode :: String -> Object
mkObjectBlankNode i = Object (BlankNode i)

-- | 'Object' constructor with URIref (a resource)
mkResource :: String -> Object
mkResource r  = Object (URIRef r)

-- | 'Object' constructor with plain literal
mkLiteral :: String -> Object
mkLiteral l  = Object (RDFLiteral l) 

-- | 'Object' constructor with literal and language information
mkLangLiteral :: String -> String -> Object
mkLangLiteral lang lit = Object (RDFLangLiteral  lit ((map $ toLower) $ lang)) 

-- | 'Object' constructor with typed literal
mkTypedLiteral :: URI -> String -> Object
mkTypedLiteral t l  = Object (RDFTypedLiteral l t) 

-- | Equal function for 'RDFTerm' and 'URI'
equalRDFTermURI :: RDFTerm -> URI -> Bool
equalRDFTermURI (URIRef u) uri = u == uri
equalRDFTermURI _ _ = False

-- | A list of triples
type RDFStore = [Triple]

-- | show function for 'RDFStore'
showRDFStore ::  RDFStore ->  String
showRDFStore []  = ""
showRDFStore (x:xs) =  show x ++ "\n" ++ showRDFStore xs

--printResult :: RDFStore -> String
--printResult xs = show (length xs) ++" Triples found\n"

-- | constructor function for 'Triple'
mkTriple :: Subject -> Predicate -> Object -> Triple
mkTriple s p o = Triple s p o

