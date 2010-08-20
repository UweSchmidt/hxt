-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.RelaxNG.Validation
   Copyright  : Copyright (C) 2008 Torben Kuseler, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Validation of a XML document with respect to a valid Relax NG schema in simple form.
   Copied and modified from \"An algorithm for RELAX NG validation\" by James Clark
   (<http://www.thaiopensource.com/relaxng/derivative.html>).

-}

-- ------------------------------------------------------------

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

import           Control.Arrow.ListArrows

import           Data.Char.Properties.XMLCharProps      ( isXmlSpaceChar )
import           Data.Maybe                             ( fromJust )

import           Text.XML.HXT.DOM.Interface
import qualified Text.XML.HXT.DOM.XmlNode as XN

import           Text.XML.HXT.Arrow.XmlArrow
import           Text.XML.HXT.Arrow.XmlIOStateArrow

import           Text.XML.HXT.Arrow.Edit                ( canonicalizeAllNodes
                                                        , collapseAllXText
                                                        )

import           Text.XML.HXT.Arrow.ProcessDocument     ( propagateAndValidateNamespaces
                                                        , getDocumentContents
                                                        , parseXmlDocument
                                                        )

import           Text.XML.HXT.RelaxNG.DataTypes
import           Text.XML.HXT.RelaxNG.CreatePattern
import           Text.XML.HXT.RelaxNG.PatternToString
import           Text.XML.HXT.RelaxNG.DataTypeLibraries
import           Text.XML.HXT.RelaxNG.Utils             ( formatStringListQuot
                                                        , compareURI
                                                        )

{-
import qualified Debug.Trace as T
-}

-- ------------------------------------------------------------

validateWithRelaxAndHandleErrors        :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
validateWithRelaxAndHandleErrors theSchema
    = validateWithRelax theSchema
      >>>
      handleErrors

validateWithRelax       :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
validateWithRelax theSchema
    = traceMsg 2 "validate with Relax NG schema"
      >>>
      ( ( normalizeForRelaxValidation           -- prepare the document for validation
          >>>
          getChildren
          >>>
          isElem                                -- and select the root element
        )
        &&&
        theSchema
      )
      >>>
      arr2A validateRelax                       -- compute vaidation errors as a document

handleErrors    :: IOSArrow XmlTree XmlTree
handleErrors
    = traceDoc "error found when validating with Relax NG schema"
      >>>
      ( getChildren                             -- prepare error format
        >>>
        getText
        >>>
        arr ("Relax NG validation: " ++)
        >>>
        mkError c_err
      )
      >>>
      filterErrorMsg                            -- issue errors and set system status


{- |
   normalize a document for validation with Relax NG: remove all namespace declaration attributes,
   remove all processing instructions and merge all sequences of text nodes into a single text node
-}

normalizeForRelaxValidation :: ArrowXml a => a XmlTree XmlTree
normalizeForRelaxValidation
  = processTopDownWithAttrl
    (
     ( none `when`                      -- remove all namespace attributes
       ( isAttr
         >>>
         getNamespaceUri
         >>>
         isA (compareURI xmlnsNamespace)
       )
     )
     >>>
     (none `when` isPi)                 -- processing instructions
    )
    >>>
    collapseAllXText                    -- all text node sequences are merged into a single text node

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
        root [] [ (take 1024 . show) ^>> mkText ]       -- pattern may be recursive, so the string representation
                                                        -- is truncated to 1024 chars to assure termination
      )
    )

-- ------------------------------------------------------------

readForRelax    :: Attributes -> String -> IOSArrow b XmlTree
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
--
-- | tests whether a 'NameClass' contains a particular 'QName'

contains :: NameClass -> QName -> Bool
contains AnyName _                      = True
contains (AnyNameExcept nc)    n        = not (contains nc n)
contains (NsName ns1)          qn       = ns1 == namespaceUri qn
contains (NsNameExcept ns1 nc) qn       = ns1 == namespaceUri qn && not (contains nc qn)
contains (Name ns1 ln1)        qn       = (ns1 == namespaceUri qn) && (ln1 == localPart qn)
contains (NameClassChoice nc1 nc2) n    = (contains nc1 n) || (contains nc2 n)
contains (NCError _) _                  = False


-- ------------------------------------------------------------
--
-- | tests whether a pattern matches the empty sequence
nullable:: Pattern -> Bool
nullable (Group p1 p2)          = nullable p1 && nullable p2
nullable (Interleave p1 p2)     = nullable p1 && nullable p2
nullable (Choice p1 p2)         = nullable p1 || nullable p2
nullable (OneOrMore p)          = nullable p
nullable (Element _ _)          = False
nullable (Attribute _ _)        = False
nullable (List _)               = False
nullable (Value _ _ _)          = False
nullable (Data _ _)             = False
nullable (DataExcept _ _ _)     = False
nullable (NotAllowed _)         = False
nullable Empty                  = True
nullable Text                   = True
nullable (After _ _)            = False


-- ------------------------------------------------------------
--
-- | computes the derivative of a pattern with respect to a XML-Child and a 'Context'

childDeriv :: Context -> Pattern -> XmlTree -> Pattern

childDeriv cx p t
    | XN.isText t       = textDeriv cx p . fromJust . XN.getText $ t
    | XN.isElem t       = endTagDeriv p4
    | otherwise         = notAllowed "Call to childDeriv with wrong arguments"
    where
    children    =            XN.getChildren $ t
    qn          = fromJust . XN.getElemName $ t
    atts        = fromJust . XN.getAttrl    $ t
    cx1         = ("",[])
    p1          = startTagOpenDeriv p qn
    p2          = attsDeriv cx1 p1 atts
    p3          = startTagCloseDeriv p2
    p4          = childrenDeriv cx1 p3 children

-- ------------------------------------------------------------
--
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
      Just errStr -> notAllowed errStr

textDeriv cx (Data (uri, s) params) s1
    = case datatypeAllows uri s params s1 cx
      of
      Nothing     -> Empty
      Just errStr -> notAllowed2 errStr

textDeriv cx (DataExcept (uri, s) params p) s1
    = case (datatypeAllows uri s params s1 cx)
      of
      Nothing     -> if not $ nullable $ textDeriv cx p s1
                     then Empty
                     else notAllowed
                              ( "Any value except " ++
                                show (show p) ++
                                " expected, but value " ++
                                show (show s1) ++
                                " found"
                              )
      Just errStr -> notAllowed errStr

textDeriv cx (List p) s
    = if nullable (listDeriv cx p (words s))
      then Empty
      else notAllowed
               ( "List with value(s) " ++
                 show p ++
                 " expected, but value(s) " ++
                 formatStringListQuot (words s) ++
                 " found"
               )

textDeriv _ n@(NotAllowed _) _
    = n

textDeriv _ p s
    = notAllowed
      ( "Pattern " ++ show (getPatternName p) ++
        " expected, but text " ++ show s ++ " found"
      )


-- ------------------------------------------------------------
--
-- | To compute the derivative of a pattern with respect to a list of strings,
-- simply compute the derivative with respect to each member of the list in turn.

listDeriv :: Context -> Pattern -> [String] -> Pattern

listDeriv _ p []
    = p

listDeriv cx p (x:xs)
    = listDeriv cx (textDeriv cx p x) xs


-- ------------------------------------------------------------
--
-- | computes the derivative of a pattern with respect to a start tag open

startTagOpenDeriv :: Pattern -> QName -> Pattern

startTagOpenDeriv (Choice p1 p2) qn
    = choice (startTagOpenDeriv p1 qn) (startTagOpenDeriv p2 qn)

startTagOpenDeriv (Element nc p) qn
    | contains nc qn
        = after p Empty
    | otherwise
        = notAllowed $
          "Element with name " ++ nameClassToString nc ++
            " expected, but " ++ universalName qn ++ " found"

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

startTagOpenDeriv p qn
    = notAllowed ( show p ++ " expected, but Element " ++ universalName qn ++ " found" )

-- ------------------------------------------------------------

-- auxiliary functions for tracing
{-
attsDeriv' cx p ts
    = T.trace ("attsDeriv: p=" ++ (take 1000 . show) p ++ ", t=" ++ showXts ts) $
      T.trace ("res= " ++ (take 1000 . show) res) res
    where
    res = attsDeriv cx p ts

attDeriv' cx p t
    = T.trace ("attDeriv: p=" ++ (take 1000 . show) p ++ ", t=" ++ showXts [t]) $
      T.trace ("res= " ++ (take 1000 . show) res) res
    where
    res = attDeriv cx p t
-}

-- | To compute the derivative of a pattern with respect to a sequence of attributes,
-- simply compute the derivative with respect to each attribute in turn.

attsDeriv :: Context -> Pattern -> XmlTrees -> Pattern

attsDeriv _ p []
    = p
attsDeriv cx p (t : ts)
    | XN.isAttr t
        = attsDeriv cx (attDeriv cx p t) ts
    | otherwise
        = notAllowed "Call to attsDeriv with wrong arguments"

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

attDeriv cx (Attribute nc p) att
    | isa
      &&
      not (contains nc qn)
        = notAllowed1 $
          "Attribute with name " ++ nameClassToString nc
          ++ " expected, but " ++ universalName qn ++ " found"
    | isa
      &&
      ( ( nullable p
          &&
          whitespace val
        )
        || nullable p'
      )
        = Empty
    | isa
        = err' p'
    where
    isa =            XN.isAttr      $ att
    qn  = fromJust . XN.getAttrName $ att
    av  =            XN.getChildren $ att
    val = showXts av
    p'  = textDeriv cx p val

    err' (NotAllowed (ErrMsg _l es))
        = err'' (": " ++ head es)
    err' _
        = err'' ""
    err'' e
        = notAllowed2 $
          "Attribute value \"" ++ val ++
          "\" does not match datatype spec " ++ show p ++ e

attDeriv _ n@(NotAllowed _) _
    = n

attDeriv _ _p att
    = notAllowed $
      "No matching pattern for attribute '" ++  showXts [att] ++ "' found"

-- ------------------------------------------------------------
--
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
    = notAllowed1 $
      "Attribut with name, " ++ show nc ++
      " expected, but no more attributes found"

startTagCloseDeriv p
    = p


-- ------------------------------------------------------------
--
-- | Computing the derivative of a pattern with respect to a list of children involves
-- computing the derivative with respect to each pattern in turn, except
-- that whitespace requires special treatment.

childrenDeriv :: Context -> Pattern -> XmlTrees -> Pattern
childrenDeriv _cx p@(NotAllowed _) _
    = p

childrenDeriv cx p []
    = childrenDeriv cx p [XN.mkText ""]

childrenDeriv cx p [tt]
    | ist
      &&
      whitespace s
        = choice p p1
    | ist
        = p1
    where
    ist =            XN.isText    tt
    s   = fromJust . XN.getText $ tt
    p1  = childDeriv cx p tt

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


-- ------------------------------------------------------------
--
-- | computes the derivative of a pattern with respect to a end tag

endTagDeriv :: Pattern -> Pattern
endTagDeriv (Choice p1 p2)
    = choice (endTagDeriv p1) (endTagDeriv p2)

endTagDeriv (After p1 p2)
    | nullable p1
        = p2
    | otherwise
        = notAllowed $
          show p1 ++ " expected"

endTagDeriv n@(NotAllowed _)
    = n

endTagDeriv _
    = notAllowed "Call to endTagDeriv with wrong arguments"

-- ------------------------------------------------------------
--
-- | applies a function (first parameter) to the second part of a after pattern

applyAfter :: (Pattern -> Pattern) -> Pattern -> Pattern

applyAfter f (After p1 p2)      = after p1 (f p2)
applyAfter f (Choice p1 p2)     = choice (applyAfter f p1) (applyAfter f p2)
applyAfter _ n@(NotAllowed _)   = n
applyAfter _ _                  = notAllowed "Call to applyAfter with wrong arguments"

-- --------------------

-- mothers little helpers

strip           :: XmlTree -> Bool
strip           = maybe False whitespace . XN.getText

whitespace      :: String -> Bool
whitespace      = all isXmlSpaceChar

showXts         :: XmlTrees -> String
showXts         = concat . runLA (xshow $ arrL id)

-- ------------------------------------------------------------
