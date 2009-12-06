{- |
   Module     : RDFFunctions 
   Author     : Manuel Ohlendorf  

   Maintainer : Manuel Ohlendorf
   Stability  : experimental
   Portability: portable
   Version    :

All RDF related functions, predicates and syntax term definitions
-}
module RDFFunctions
where

import Text.XML.HXT.Arrow

-- namespace definition for RDF and others

-- | Namespace definition for RDF and XML
namespaceXML, namespaceRDF :: String
namespaceRDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
namespaceXML = "http://www.w3.org/XML/1998/namespace"

-- | XML terms
xml_base, xml_lang :: QName
xml_base = mkNsName "xml:base" namespaceXML
xml_lang = mkNsName "xml:lang" namespaceXML

-- | RDF Core Syntax Terms
rdf_RDF,rdf_about,rdf_ID,rdf_resource,rdf_nodeID,rdf_datatype,rdf_parseType :: QName
rdf_RDF           = mkNsName "rdf:RDF" namespaceRDF
rdf_about         = mkNsName "rdf:about" namespaceRDF
rdf_ID            = mkNsName "rdf:ID" namespaceRDF  
rdf_resource      = mkNsName "rdf:resource" namespaceRDF
rdf_nodeID        = mkNsName "rdf:nodeID" namespaceRDF
rdf_datatype      = mkNsName "rdf:datatype" namespaceRDF
rdf_parseType     = mkNsName "rdf:parseType" namespaceRDF --TODO ParseType  Resource => keine BlankNode, kommt in Properties vor

-- | Other RDF Syntax Terms
rdf_Description,rdf_li ::QName
rdf_Description   = mkNsName "rdf:Description" namespaceRDF
rdf_li            = mkNsName "rdf:li" namespaceRDF

-- | RDF Container Terms
rdf_Seq , rdf_Bag , rdf_Alt :: QName
rdf_Seq = mkNsName "rdf:Seq" namespaceRDF
rdf_Bag = mkNsName "rdf:Bag" namespaceRDF
rdf_Alt = mkNsName "rdf:Alt" namespaceRDF

-- | Allowed Property Names
rdf_Statement,rdf_subject,rdf_predicate,rdf_object,rdf_type,rdf_value,rdf_first,rdf_rest :: QName
rdf_Statement = mkNsName "rdf:Statement"namespaceRDF
rdf_subject   = mkNsName "rdf:subject" namespaceRDF
rdf_predicate = mkNsName "rdf:predicate" namespaceRDF
rdf_object    = mkNsName "rdf:object" namespaceRDF
rdf_type  	  = mkNsName "rdf:type" namespaceRDF
rdf_value 	  =	mkNsName "rdf:value" namespaceRDF
rdf_first  	  = mkNsName "rdf:first" namespaceRDF
rdf_rest  	  = mkNsName "rdf:rest" namespaceRDF
 --und: _n where n is a decimal integer greater than zero with no leading zeros.

-- | URI of XML Literals
xmlliteral :: String
xmlliteral = "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"


{- Functions for RDF Parsing and Normalizing -}

-- | rdf:RDF test
isRDF :: (ArrowXml a) => a XmlTree XmlTree
isRDF = isElem >>>  hasQName rdf_RDF  

-- | Tests, wether the element is a node element
isNodeElem :: (ArrowXml a) => a XmlTree XmlTree
isNodeElem = isElem >>> (hasQName rdf_Description >>> (hasQAttr rdf_nodeID `orElse` hasQAttr rdf_about)) 

-- | Tests, wether the element is a resource element
isResource :: (ArrowXml a) => a XmlTree XmlTree
isResource = isEmptyElem >>> hasQAttr rdf_resource 

-- | Checks wether an element is empty or not
isEmptyElem :: (ArrowXml a) => a XmlTree XmlTree 
isEmptyElem = (isElem >>> neg getChildren) `guards` this

-- | Tests, wether an element has a datatype definition
hasDatatype :: (ArrowXml a) => a XmlTree XmlTree
hasDatatype = hasQAttr rdf_datatype

-- | Tests, wether an element as rdf:nodeID attribute
hasNodeID :: (ArrowXml a) => a XmlTree XmlTree
hasNodeID = hasQAttr rdf_nodeID

-- | has parse type \"Literal\" or not
hasLiteralParseType :: (ArrowXml a) => a XmlTree XmlTree
hasLiteralParseType = hasQAttrValue rdf_parseType (== "Literal")

-- | has parse type \"Resource\" or not
hasResourceParseType :: (ArrowXml a) => a XmlTree XmlTree
hasResourceParseType = hasQAttrValue rdf_parseType (== "Resource") -- for omitting blank nodes 

