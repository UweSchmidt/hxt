-- |
-- Validation of a XML document with respect to a valid Relax NG schema in simple form.
-- Copied and modified from \"An algorithm for RELAX NG validation\" by James Clark
-- (<http://www.thaiopensource.com/relaxng/derivative.html>).

module Text.XML.HXT.RelaxNG.Validation
    ( validateWithRelaxAndHandleErrors
    , validateDocWithRelax
    , validateRelax
    , validateXMLDoc
    , readForRelax
    , normalizeForRelaxValidation
    , contains
    )
where

import Control.Arrow.ListArrows

import Text.XML.HXT.Arrow.DOMInterface
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.Edit
    ( canonicalizeAllNodes
    , collapseAllXText
    )

import Text.XML.HXT.Arrow.ProcessDocument
    ( propagateAndValidateNamespaces
    , getDocumentContents
    , parseXmlDocument
    )

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.CreatePattern
import Text.XML.HXT.RelaxNG.PatternToString
import Text.XML.HXT.RelaxNG.DataTypeLibraries
import Text.XML.HXT.RelaxNG.Utils
    ( qn2String
    , formatStringList
    , compareURI
    )

import qualified Text.XML.HXT.Arrow.XmlNode as XN
    ( getText
    )

import Data.Tree.NTree.TypeDefs	( NTree (..) )

import qualified Text.XML.HXT.DOM.XmlTreeFunctions as TF ( xshow )

import Data.Char
import Data.Maybe

-- ------------------------------------------------------------

validateWithRelaxAndHandleErrors	:: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
validateWithRelaxAndHandleErrors theSchema
    = validateWithRelax theSchema
      >>>
      handleErrors

validateWithRelax	:: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
validateWithRelax theSchema
    = traceMsg 2 "validate with Relax NG schema"
      >>>
      ( ( normalizeForRelaxValidation		-- prepare the document for validation
	  >>>
	  getChildren
	  >>>
	  isElem				-- and select the root element
	)
	&&&
	theSchema
      )
      >>>
      arr2A validateRelax			-- compute vaidation errors as a document

handleErrors	:: IOSArrow XmlTree XmlTree
handleErrors
    = traceDoc "error found when validating with Relax NG schema"
      >>>
      ( getChildren				-- prepare error format
	>>>
	getText
	>>>
	arr ("Relax NG validation: " ++)
	>>>
	mkError c_err
      )
      >>>
      filterErrorMsg				-- issue errors and set system status


{- |
   normalize a document for validation with Relax NG: remove all namespace declaraion attributes,
   remove all processing instructions and merge all sequences of text nodes into a single text node
-}

normalizeForRelaxValidation :: ArrowXml a => a XmlTree XmlTree
normalizeForRelaxValidation
  = processTopDownWithAttrl
    (
     ( none `when`			-- remove all namespace attributes
       ( isAttr
         >>> 
         getNamespaceUri
         >>>
         isA (compareURI xmlnsNamespace)
       )
     )
     >>>
     (none `when` isPi)			-- processing instructions
    )
    >>>
    collapseAllXText			-- all text node sequences are merged into a single text node

-- ------------------------------------------------------------

{- | Validates a xml document with respect to a Relax NG schema

   * 1.parameter  :  the arrow for computing the Relax NG schema

   - 2.parameter  :  list of options for reading and validating
   
   - 3.parameter  :  XML document URI

   - arrow-input  :  ignored
   
   - arrow-output :  list of errors or 'none'
-}  

validateDocWithRelax :: IOSArrow XmlTree XmlTree -> Attributes -> String -> IOSArrow XmlTree XmlTree
validateDocWithRelax theSchema al doc
  = ( if null doc
      then root [] []
      else readForRelax al doc
    )
    >>>
    validateWithRelax theSchema
    >>>
    perform handleErrors


{- | Validates a xml document with respect to a Relax NG schema

   * 1.parameter  :  XML document

   - arrow-input  :  Relax NG schema
   
   - arrow-output :  list of errors or 'none'
-}

validateRelax :: XmlTree -> IOSArrow XmlTree XmlTree
validateRelax xmlDoc
  = fromLA
    ( createPatternFromXmlTree
      >>>
      arr (\p -> childDeriv ("",[]) p xmlDoc)
      >>>
      ( (not . nullable)
        `guardsP`
        root [] [ (take 1024 . show) ^>> mkText ]	-- pattern may be recursive, so the string representation
	                                                -- is truncated to 1024 chars to assure termination
      )
    )

-- ------------------------------------------------------------

readForRelax	:: Attributes -> String -> IOSArrow b XmlTree
readForRelax options schema
    = getDocumentContents options schema
      >>>
      parseXmlDocument False
      >>>
      canonicalizeAllNodes
      >>>
      propagateAndValidateNamespaces

-- ------------------------------------------------------------

{- old stuff -}

validateXMLDoc :: Attributes -> String -> IOSArrow XmlTree XmlTree
validateXMLDoc al xmlDoc
  = validateRelax
    $<
    ( readForRelax al xmlDoc
      >>>
      normalizeForRelaxValidation
      >>>
      getChildren
    )


-- ------------------------------------------------------------


-- | tests whether a 'NameClass' contains a particular 'QName'
contains :: NameClass -> QName -> Bool
contains AnyName _			= True
contains (AnyNameExcept nc) n		= not (contains nc n)
contains (NsName ns1) (QN _ _ ns2)	= ns1 == ns2
contains (NsNameExcept ns1 nc) qn@(QN _ _ ns2)
					= ns1 == ns2 && not (contains nc qn)
contains (Name ns1 ln1) (QN _ ln2 ns2)	= (ns1 == ns2) && (ln1 == ln2)
contains (NameClassChoice nc1 nc2) n 	= (contains nc1 n) || (contains nc2 n)
contains (NCError _) _ 			= False


-- | tests whether a pattern matches the empty sequence
nullable:: Pattern -> Bool
nullable (Group p1 p2)		= nullable p1 && nullable p2
nullable (Interleave p1 p2)	= nullable p1 && nullable p2
nullable (Choice p1 p2)		= nullable p1 || nullable p2
nullable (OneOrMore p)		= nullable p
nullable (Element _ _)		= False
nullable (Attribute _ _)	= False
nullable (List _)		= False
nullable (Value _ _ _)		= False
nullable (Data _ _)		= False
nullable (DataExcept _ _ _)	= False
nullable (NotAllowed _)		= False
nullable Empty			= True
nullable Text			= True
nullable (After _ _)		= False


-- | computes the derivative of a pattern with respect to a XML-Child and a 'Context'
childDeriv :: Context -> Pattern -> XmlTree -> Pattern

childDeriv cx p (NTree (XText s) _)
    = textDeriv cx p s

childDeriv _ p (NTree (XTag qn atts) children)
    = let
      cx = ("",[])
      p1 = startTagOpenDeriv p qn
      p2 = attsDeriv cx p1 atts
      p3 = startTagCloseDeriv p2
      p4 = childrenDeriv cx p3 children
      in
      endTagDeriv p4

childDeriv _ _ _
    = NotAllowed "Call to childDeriv with wrong arguments"


-- | computes the derivative of a pattern with respect to a text node
textDeriv :: Context -> Pattern -> String -> Pattern

textDeriv cx (Choice p1 p2) s
    = choice (textDeriv cx p1 s) (textDeriv cx p2 s)

textDeriv cx (Interleave p1 p2) s
    = choice
      (interleave (textDeriv cx p1 s) p2)
      (interleave p1 (textDeriv cx p2 s))

textDeriv cx (Group p1 p2) s
    = let
      p = group (textDeriv cx p1 s) p2
      in
      if nullable p1
      then choice p (textDeriv cx p2 s)
      else p

textDeriv cx (After p1 p2) s
    = after (textDeriv cx p1 s) p2

textDeriv cx (OneOrMore p) s
    = group (textDeriv cx p s) (choice (OneOrMore p) Empty)

textDeriv _ Text _
    = Text

textDeriv cx1 (Value (uri, s) value cx2) s1
    = case datatypeEqual uri s value cx2 s1 cx1
      of
      Nothing     -> Empty 
      Just errStr -> NotAllowed errStr

textDeriv cx (Data (uri, s) params) s1
    = case datatypeAllows uri s params s1 cx
      of
      Nothing     -> Empty 
      Just errStr -> NotAllowed errStr

textDeriv cx (DataExcept (uri, s) params p) s1
    = case (datatypeAllows uri s params s1 cx)
      of
      Nothing     -> if not $ nullable $ textDeriv cx p s1 
                     then Empty 
		     else NotAllowed ( "Any value except '" ++
				       show p ++ 
				       "' expected, but value '" ++
				       s1 ++
				       "' found"
				     )
      Just errStr -> NotAllowed errStr

textDeriv cx (List p) s
    = if nullable (listDeriv cx p (words s)) 
      then Empty
      else NotAllowed ( "List with value(s) " ++
			show p ++ 
			" expected, but value(s) " ++ 
			formatStringList ", " (words s) ++
			" found"
		      )

textDeriv _ n@(NotAllowed _) _
    = n

textDeriv _ p s
    = NotAllowed ( "Pattern " ++ getPatternName p ++
		   " expected, but text " ++ s ++ " found"
		 )


-- | To compute the derivative of a pattern with respect to a list of strings, 
-- simply compute the derivative with respect to each member of the list in turn.

listDeriv :: Context -> Pattern -> [String] -> Pattern

listDeriv _ p []
    = p

listDeriv cx p (x:xs)
    = listDeriv cx (textDeriv cx p x) xs
    

-- | computes the derivative of a pattern with respect to a start tag open

startTagOpenDeriv :: Pattern -> QName -> Pattern

startTagOpenDeriv (Choice p1 p2) qn
    = choice (startTagOpenDeriv p1 qn) (startTagOpenDeriv p2 qn)

startTagOpenDeriv (Element nc p) qn@(QN _ local uri)
    = if contains nc qn 
      then after p Empty
	   else NotAllowed ( "Element with name " ++ nameClassToString nc ++ 
			     " expected, but {" ++ uri ++ "}" ++ local ++ " found"
			   )

startTagOpenDeriv (Interleave p1 p2) qn
    = choice
      (applyAfter (flip interleave p2) (startTagOpenDeriv p1 qn))
      (applyAfter (interleave p1) (startTagOpenDeriv p2 qn))

startTagOpenDeriv (OneOrMore p) qn
    = applyAfter
      (flip group (choice (OneOrMore p) Empty))
      (startTagOpenDeriv p qn)

startTagOpenDeriv (Group p1 p2) qn
    = let
      x = applyAfter (flip group p2) (startTagOpenDeriv p1 qn)
      in
      if nullable p1 
      then choice x (startTagOpenDeriv p2 qn)
      else x

startTagOpenDeriv (After p1 p2) qn
    = applyAfter (flip after p2) (startTagOpenDeriv p1 qn)

startTagOpenDeriv n@(NotAllowed _) _
    = n

startTagOpenDeriv p (QN _ local uri)
    = NotAllowed ( show p ++ " expected, but Element {" ++
		   uri ++ "}" ++ local ++ " found"
		 )

-- | To compute the derivative of a pattern with respect to a sequence of attributes, 
-- simply compute the derivative with respect to each attribute in turn.

attsDeriv :: Context -> Pattern -> XmlTrees -> Pattern

attsDeriv _ p []
    = p
attsDeriv cx p (attribute@(NTree (XAttr _) _):xs)
    = attsDeriv cx (attDeriv cx p attribute) xs

attsDeriv _ _ _
    = NotAllowed "Call to attsDeriv with wrong arguments"

attDeriv :: Context -> Pattern -> XmlTree -> Pattern

attDeriv cx (After p1 p2) att
    = after (attDeriv cx p1 att) p2

attDeriv cx (Choice p1 p2) att
    = choice (attDeriv cx p1 att) (attDeriv cx p2 att)

attDeriv cx (Group p1 p2) att
    = choice
      (group (attDeriv cx p1 att) p2)
      (group p1 (attDeriv cx p2 att))

attDeriv cx (Interleave p1 p2) att
    = choice
      (interleave (attDeriv cx p1 att) p2)
      (interleave p1 (attDeriv cx p2 att))

attDeriv cx (OneOrMore p) att
    = group
      (attDeriv cx p att)
      (choice (OneOrMore p) Empty)

attDeriv cx (Attribute nc p) (NTree (XAttr qn) attrValue)
    = attDeriv' (TF.xshow attrValue)
    where
    attDeriv' val
	= if contains nc qn && valueMatch cx p val
	  then Empty
	  else ( if (not $ contains nc qn)
		 then ( NotAllowed $ "Attribut with name " ++ 
			nameClassToString nc ++ " expected, but " ++ 
			qn2String qn ++ " found")
		 else ( NotAllowed $ "Attributvalue " ++ val ++ 
			" expected, but " ++ show p ++ " found")
               )

attDeriv _ n@(NotAllowed _) _
    = n

attDeriv _ p att
    = NotAllowed ( "Attribute-Pattern for Attribute " ++  show att ++ 
		   " expected, but Pattern " ++ show p ++ " found"
		 )

-- | tests, whether an attribute value matches a pattern
valueMatch :: Context -> Pattern -> String -> Bool
valueMatch cx p s = 
    ( nullable p
      &&
      whitespace s
    )
    ||
    nullable (textDeriv cx p s)
  
  
-- | computes the derivative of a pattern with respect to a start tag close

startTagCloseDeriv :: Pattern -> Pattern

startTagCloseDeriv (After p1 p2)
    = after (startTagCloseDeriv p1) p2

startTagCloseDeriv (Choice p1 p2)
    = choice
      (startTagCloseDeriv p1)
      (startTagCloseDeriv p2)

startTagCloseDeriv (Group p1 p2)
    = group
      (startTagCloseDeriv p1)
      (startTagCloseDeriv p2)

startTagCloseDeriv (Interleave p1 p2)
    = interleave
      (startTagCloseDeriv p1)
      (startTagCloseDeriv p2)

startTagCloseDeriv (OneOrMore p)
    = oneOrMore (startTagCloseDeriv p)

startTagCloseDeriv (Attribute nc _)
    = NotAllowed ( "Attribut with name, " ++ show nc ++ 
                   " expected, but no more attributes found"
		 )

startTagCloseDeriv p
    = p


-- | Computing the derivative of a pattern with respect to a list of children involves 
-- computing the derivative with respect to each pattern in turn, except
-- that whitespace requires special treatment.

childrenDeriv :: Context -> Pattern -> XmlTrees -> Pattern
childrenDeriv cx p []
    = childrenDeriv cx p [(NTree (XText "") [])]

childrenDeriv cx p [(NTree (XText s) children)]
    = let
      p1 = childDeriv cx p (NTree (XText s) children)
      in
      if whitespace s
      then choice p p1
      else p1

childrenDeriv cx p children
    = stripChildrenDeriv cx p children    

stripChildrenDeriv :: Context -> Pattern -> XmlTrees -> Pattern
stripChildrenDeriv _ p []
    = p

stripChildrenDeriv cx p (h:t)
    = stripChildrenDeriv cx
      ( if strip h
	then p
	else (childDeriv cx p h)
      ) t


-- | computes the derivative of a pattern with respect to a end tag

endTagDeriv :: Pattern -> Pattern
endTagDeriv (Choice p1 p2)
    = choice (endTagDeriv p1) (endTagDeriv p2)

endTagDeriv (After p1 p2)
    = if nullable p1 
      then p2 
      else NotAllowed $ show p1 ++ " expected"

endTagDeriv n@(NotAllowed _)
    = n

endTagDeriv _
    = NotAllowed "Call to endTagDeriv with wrong arguments"


-- ------------------------------------------------------------
-- some helper function

choice :: Pattern -> Pattern -> Pattern
choice p              (NotAllowed _) = p
choice (NotAllowed _) p              = p
choice p1             p2             = Choice p1 p2

group :: Pattern -> Pattern -> Pattern
group _                n@(NotAllowed _) = n
group n@(NotAllowed _) _                = n
group p                Empty            = p
group Empty            p                = p
group p1               p2               = Group p1 p2

oneOrMore :: Pattern -> Pattern
oneOrMore n@(NotAllowed _) = n
oneOrMore p                = OneOrMore p

interleave :: Pattern -> Pattern -> Pattern
interleave _                n@(NotAllowed _) = n
interleave n@(NotAllowed _) _                = n
interleave p                Empty            = p
interleave Empty            p                = p
interleave p1               p2               = Interleave p1 p2

after :: Pattern -> Pattern -> Pattern
after _                n@(NotAllowed _) = n
after n@(NotAllowed _) _                = n
after p1               p2               = After p1 p2

-- --------------------
-- | applies a function (first parameter) to the second part of a after pattern

applyAfter :: (Pattern -> Pattern) -> Pattern -> Pattern

applyAfter f (After p1 p2)	= after p1 (f p2)
applyAfter f (Choice p1 p2)	= choice (applyAfter f p1) (applyAfter f p2)
applyAfter _ n@(NotAllowed _)	= n
applyAfter _ _			= NotAllowed "Call to applyAfter with wrong arguments"

-- --------------------

strip	:: XmlTree -> Bool
strip
    = maybe False whitespace . XN.getText
{-
strip (NTree (XText s) _) = whitespace s
strip _ = False
-}

whitespace :: String -> Bool
whitespace
    = all isSpace

-- ------------------------------------------------------------
