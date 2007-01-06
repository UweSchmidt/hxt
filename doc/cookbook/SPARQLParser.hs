{- |
   Module     : SPARQLParser 
   Author     : Manuel Ohlendorf  

   Maintainer : Manuel Ohlendorf
   Stability  : experimental
   Portability: portable
   Version    :

The SPARQL query language Parsec parser. Exports the main parser. The grammar of the query language can be found here: <file:../grammar.txt>
-}


--Some of the parsers cover more than one production. Some abbreviations have been commented out.

module SPARQLParser 
   (parseSPARQL)
where

import Char
    (toLower
    ,toUpper
    )

import Text.ParserCombinators.Parsec
import SPARQLDataTypes
import RDFDataTypes
import Monad
import Text.XML.HXT.Parser.XmlParser
    (skipS0
    )


-- some useful token and symbol parser
xsd, rdf, rdfType, booleanD, integerD, doubleD :: String
xsd         = "http://www.w3.org/2001/XMLSchema#"
rdf         = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfType     = rdf ++"type"
booleanD    = xsd ++ "boolean"
doubleD     = xsd ++ "double"
integerD    = xsd ++ "integer"


lbrace, rbrace, dot, com, semcol :: Parser ()
lbrace      = tokenParser (symbol "{")
rbrace      = tokenParser (symbol "}")
dot         = tokenParser (symbol ".")
com         = tokenParser (symbol ",")
semcol      = tokenParser (symbol ";")

caseChar        :: Char -> CharParser st Char
caseChar c       = satisfy (\x -> toUpper x == toUpper c) <?> ""

caseString      :: String -> CharParser st () 
caseString cs    = mapM_ caseChar cs <?> "" 

tokenParser :: Parser String -> Parser ()
tokenParser p
    = try ( do
            skipS0
            p
            skipS0
           )

symbol :: String -> Parser String
symbol s = try (string s)

-- | The main entry point to the parser.
-- Creates a 'Query' as result, which contains all SPARQL abbreviations.
-- In order to evaluate the result, it has to be normalised before.
-- see "SPARQLFunctions" for normalisation function
parseSPARQL :: Parser Query
parseSPARQL
    = do
        skipS0
        qr <- query
        skipS0
        eof
        return qr

-- [1] in the case of 'SELECT *' a query is returned, which matches every triple
query :: Parser Query
query 
    = do
        caseString "SELECT"  
        skipS0
        a <- do string "*"
                return (["s","p","o"])
             <|> selectClause 
        skipS0
        b <- option [] whereClause
        return (if b == [] then (QueryN a [triple]) 
                else (Query a (GraphPattern b)))
    where triple = TriplePatN (SubjectVar "s") (PredicateVar "p") (ObjectVar "o")

-- [2]
selectClause :: Parser [Var]
selectClause 
    = do
        a <- varPlus
        return a

-- not part of the gramma 
varPlus :: Parser [Var]
varPlus = do
            a <- var
            skipS0
            b <- many var
            return (a :b)
-- [3]
whereClause :: Parser [PatternElement] 
whereClause 
    = do
        caseString "WHERE"
        skipS0
        a <- graphPattern
        skipS0
        eof
        return a
--[4]
graphPattern :: Parser [PatternElement]
graphPattern
    = do
        lbrace
        a <- patternElementsList
        rbrace <?> "end of graph pattern"
        return (a)
    
-- [5]
patternElementsList :: Parser [PatternElement] 
patternElementsList 
    = do
        skipS0
        a <- patternElement
        b <- option [] patternElementsListTail
        return (a : b)
--[6]
patternElementsListTail :: Parser [PatternElement]
patternElementsListTail 
    = do
        dot
        a <- option [] patternElementsList                 
        return a
-- [7]
patternElement:: Parser PatternElement
patternElement = do 
                    skipS0 
                    pe <- do tr <- triples
                             return (PET tr)
 
                          <|>
                          do gr <- graphPattern
                             return (PEG gr)
                    return pe 



--[8]
triples :: Parser TriplePattern
triples = do 
            subj <- do 
                        v <- var 
                        return (SubjectVar v)
                    <|>
                    do
                        g <- graphTerm
                        return (Subject g)

            predObL <- propertyListNotEmpty
            return (TriplePat subj predObL)
          

-- [9]
propertyList:: Parser [(Predicate,[Object])] 
propertyList = do 
                a <- option [] propertyListNotEmpty
                return a
-- [10]
propertyListNotEmpty:: Parser [(Predicate,[Object])] 
propertyListNotEmpty = do
                        skipS0
                        p <- (do  
                                 v <- var
                                 return (PredicateVar v)
                                 <|>
                                 do  
                                 u <- uri
                                 return (Predicate u)
                                 <|>
                                 do
                                 string "a"
                                 return (Predicate rdfType)
                                )
                        skipS0
                        obl <- objectList
                        skipS0
                        t <- option [] propertyListTail
                        return ((p,obl):t)
-- [11]
propertyListTail:: Parser [(Predicate,[Object])] 
propertyListTail = do
                    semcol
                    a <- option []  propertyList
                    return a

-- [12]
objectList :: Parser [Object] 
objectList = do
                a <- object
                b <- option [] objectTail      
                return (a:b)
-- [13]
objectTail :: Parser [Object]
objectTail = do
                com
                a <- option [] (objectList)
                return (a)

-- [14]
--verb :: Parser String
--verb = varOrURI -- <|> string "a"

-- [15]
object :: Parser Object 
object = do v <- var 
            return (ObjectVar v)
         <|>
         do g <- graphTerm
            return (Object g)
--original:
-- varOrTerm  <|> triplesNode <?> "object"

-- [16]
{-varOrURI :: Parser String
varOrURI = do v <- var 
              return (show v)
           <|> 
           do u <- uri
              return (show u)

-- [17]
varOrTerm :: Parser String
varOrTerm = do v <- var 
               return (show v)
            <|> 
            do g <- graphTerm
               return (show g)
-}
-- [18][32]
var :: Parser Var 
var = do {
         ; char '?' <|> char '$'
         ; variable <- (ncname2 <|> ncname1)
         ; skipS0
		 ; return (variable)
         }
        <?> "Variable"

-- [19] [26]
graphTerm :: Parser RDFTerm
graphTerm = do
               a <-rdfTerm
               return (a) 
          --  <|> 
          --  (string "()" <?> "")

-- [20]
rdfTerm :: Parser RDFTerm 
rdfTerm = do u <- uri
             return (URIRef u)
          <|> rdfLiteral 
          <|> blankNode 
          <|> numericLiteral 
          <|> booleanLiteral

-- [21] 
numericLiteral :: Parser RDFTerm
numericLiteral = do i <- try floatingPoint
                    return (RDFTypedLiteral i doubleD)
                 <|> 
                 do f <- integerP
                    return (RDFTypedLiteral f integerD)
                    

-- [22]
rdfLiteral :: Parser RDFTerm 
rdfLiteral = do
                a <- stringP
                (b,c) <- option ("","") (do l <-langtag
                                            return (l,"")
                                         <|>
                                         do string "\^\^" 
                                            u <- uri
                                            return ("",u)
                                        )
                return (if (c =="") 
                        then if (b == "")
                             then (RDFLiteral a)
                             else (RDFLangLiteral a b)                              
                        else (RDFTypedLiteral a c)
                        )          
                
            <?> "RDF Literal"

-- [23]
booleanLiteral :: Parser RDFTerm
booleanLiteral = do caseString "TRUE"
                    return (RDFTypedLiteral "TRUE" booleanD) 
                 <|> 
                 do caseString "FALSE" 
                    return (RDFTypedLiteral "FALSE" booleanD)
                 <?> "Boolean Literal"

-- [25] [27] [30]
uri :: Parser URI 
uri = do {char '<' 
		 ;theUri <-many (noneOf ">") 
		 ;char '>' <?> "end of URI"				  
		 ;return (theUri)
		 }
        <?> "URI"

-- [26]
blankNode :: Parser RDFTerm 
blankNode
        = do
            lab <- bnode_label 
            return (BlankNode lab)
        <?> "BlankNode"-- <|> string "[]"

-- [28] Integer [37] <INTEGER_10> und [42] <DIGITS>
integerP :: Parser String
integerP = many1 digit <?> "Integer"

--[29] [38]
floatingPoint :: Parser String
floatingPoint 
    = do
        a <- many1 digit
        b <- try (do 
                    char '.'
                    c <- many digit
                    d <- option "" exponentP
                    return ('.':c++d)
                  <|>
                  exponentP
                )
        return (a++b)
      <|>
      do
        char '.'
        a <- many1 digit
        b <- option "" exponentP
        return ("0."++a++b)
      <?> "floating point"

-- [39]
exponentP :: Parser String
exponentP = do
             a <- oneOf "eE"
             b <- option '+' (oneOf "+-")
             c <- many1 digit
             return (a:b:c) 
             <?> "exponent"                     
--[31]
bnode_label :: Parser String
bnode_label = do
                string "_:"
                lab <- (ncname2 <|> ncname1) 
                return lab

-- [33][34][35][36]
langtag :: Parser String
langtag 
	= do {char '@'
		 ;s1 <- many1 letter 
		 ;s2 <- many (do 
                         char '-' 
                         s3 <- (many1 alphaNum)
                         return ('-' : s3)
                      )
		 ;return ((map toLower) $ (s1 ++ (concat s2)))
		 }
        <?> "Language Tag"

-- [24]    
stringP :: Parser String
stringP = string_literal1 <|> string_literal2

--[40]
string_literal1 :: Parser String
string_literal1 
    = do
        char '\'' 
        str <- (many (noneOf "'\\\n\r")) 
        char '\''
        return str

--[41]
string_literal2 :: Parser String
string_literal2 
	= do
		char '"' 
		str <- (many (noneOf "\"\\\n\r")) 
		char '"'
		return str 

-- [43]
ncchar1 :: Parser Char 
ncchar1 =   oneOf ['A' .. 'Z'] 
        <|> oneOf['a'..'z'] 
        <|> oneOf ['\x00c0' .. '\x00D6']  
        <|> oneOf ['\x00D8' .. '\x00F6']
        <|> oneOf ['\x00F8' .. '\x02FF']
        <|> oneOf ['\x0370' .. '\x037D']
        <|> oneOf ['\x037F' .. '\x1FFF']
        <|> oneOf ['\x200C' .. '\x200D']
        <|> oneOf ['\x2070' .. '\x218F']
        <|> oneOf ['\x2C00' .. '\x2FEF']
        <|> oneOf ['\x3001' .. '\xD7FF']
        <|> oneOf ['\xF900' .. '\xFFFF']
       
-- [44] [45]
ncchar_end :: Parser Char 
ncchar_end = ncchar1 <|> char '_' <|> char '-' <|> digit <|> char '\x00B7'

-- [45]
--ncchar_full :: Parser Char
--ncchar_full = ncchar_end <|>  char '.'

--[46]
ncname1 :: Parser String 
ncname1 = do
         c   <- ncchar1
         rs1 <- many ncchar_end
         rs2 <- many ( do
                       xs1 <- many1 (char '.')
                       xs2 <- many1 ncchar_end
                       return (xs1 ++ xs2)
                     )
         return (c : rs1 ++ concat rs2)

--[47]
ncname2 :: Parser String 
ncname2 = do
         c   <- char '_'
         rs1 <- many ncchar_end
         rs2 <- many ( do
                       xs1 <- many1 (char '.')
                       xs2 <- many1 ncchar_end
                       return (xs1 ++ xs2)
                     )
         return (c : rs1 ++ concat rs2)

