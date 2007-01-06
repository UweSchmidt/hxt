{- |
   Module     : RDFParser 
   Author     : Manuel Ohlendorf  

   Maintainer : Manuel Ohlendorf
   Stability  : experimental
   Portability: portable
   Version    :

Arrows to parse an RDF\/XML document. Two versions of processing arrows are provided. 

parseRDF normalises the document before it parses the document and creates the list of triples. 

processRDF expects a normalised document for parsing.
-}

module RDFParser (processRDF,parseRDF) 
where

import Text.XML.HXT.Arrow
import RDFDataTypes
import RDFFunctions
import RDFNormalize

-- -------------------------------------------------------------
-- ***** PARSEN ****
-- -------------------------------------------------------------

-- | normalises the document and then parses it.
--
-- > normalizeRDF >>> processRDF
--
-- see 'normalizeRDF'
parseRDF :: IOSArrow XmlTree RDFStore
parseRDF = normalizeRDF >>> processRDF

-- | just processes the document and does not do any normalisation.
processRDF :: (ArrowXml a) => a XmlTree RDFStore
processRDF = getChildren >>> isRDF `guards` arr triples  

-- | stores the resulting triples in a list
triples :: XmlTree -> RDFStore 
triples = runLA  ( processSubject)

-- | processes the node elements
processSubject :: (ArrowXml a) => a XmlTree Triple 
processSubject 
    = getChildren >>> (isNodeElem `guards` (arr getTriple $< blankOrNot))
    where
    blankOrNot 
        = ifA hasNodeID
        ((getQAttrValue rdf_nodeID >>> arr mkSubjectBlankNode) &&& getAttrValue "xml:lang") 
        ((getQAttrValue rdf_about >>> arr mkURIRef) &&& getAttrValue "xml:lang")

-- | processes the property nodes
processPredicate :: (ArrowXml a) => a XmlTree Predicate  
processPredicate 
    = choiceA [ hasResourceParseType :-> (getUniversalUri >>> arr mkPredicate),
               this :-> (getUniversalUri >>> arr mkPredicate)
              ]
-- | generates the object elements
processObject :: (ArrowXml a) => String -> a XmlTree Object 
processObject lang
    = choiceA [isResource                    :-> (getQAttrValue rdf_resource >>> arr mkResource)  
              ,(isEmptyElem >>> hasNodeID)   :-> (getQAttrValue rdf_nodeID >>> arr mkObjectBlankNode)
              ,hasDatatype                   :-> op1
              ,hasLiteralParseType           :-> op2
              ,hasAttr "xml:lang"            :-> ((getAttrValue "xml:lang"  &&& xshow getChildren) >>> arr2 mkLangLiteral)
              ,isA(const (length lang /= 0)) :-> (xshow getChildren >>> arr (mkLangLiteral lang))
              ,this                          :-> (xshow getChildren >>> arr mkLiteral)  
              ]
                where
                op1 = getQAttrValue rdf_datatype &&& (xshow getChildren) >>> arr2 mkTypedLiteral
                op2 = xshow getChildren >>> arr (mkTypedLiteral xmlliteral)

-- generates the whole triple
getTriple :: (ArrowXml a) => (Subject, String) -> a XmlTree Triple 
getTriple (subject,lang)
        = getChildren >>> (processPredicate &&& processObject lang) >>> arr2 (mkTriple subject) 




