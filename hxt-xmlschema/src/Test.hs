module Test

where

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

-- ----------------------------------------

-- | read and propagate namespaces in XML schema doc

main :: IO ()
main
    = do _ <- runX $
              xunpickleXmlSchema "tests/complexTypesWithTargetNS.xsd"
         return ()

xunpickleXmlSchema :: String -> IOSArrow a XmlTree
xunpickleXmlSchema src
    = readDocument
        [ withValidate yes        -- validate source
        , withTrace 0             -- trace processing steps?
        , withRemoveWS yes        -- remove redundant whitespace
        , withPreserveComment no  -- keep comments
        , withCheckNamespaces yes -- check namespaces
        , withCurl []             -- use libCurl for http access
        ] src
      >>>
      fromLA propagateTargetNamespace
      >>>
      fromLA propagateXmlSchemaNamespaces
      >>>
      traceMsg 1 ("xunpickleVal for " ++ show src ++ " started")
      >>>
      xunpickleVal xpXmlSchema'
      >>>
      traceMsg 1 ("xunpickleVal for " ++ show src ++ " finished")
    where
      xpXmlSchema' = ""
      xunpickleVal _xp = writeDocument [withIndent yes] ""		-- dummy unpickler

propagateTargetNamespace :: LA XmlTree XmlTree
propagateTargetNamespace
    = propagate $< getTargetNS
    where
      getTargetNS
          = getChildren
            >>> isSchema
            >>> getAttrValue "targetNamespace"
      propagate tns
          = processTopDown $
            addTns `when` (isElement <+> isAttribute) -- !!! oder nur isElement ???
          where
            addUri
                | null tns  = id
                | otherwise = (("{" ++ tns ++ "}") ++)
            addTns
                = processAttrl $
                  changeAttrValue addUri `when` hasName "name"


isSchema :: LA XmlTree XmlTree
isSchema = isXSElem "schema"

isElement :: LA XmlTree XmlTree
isElement = isXSElem "element"

isAttribute :: LA XmlTree XmlTree
isAttribute = isXSElem "attribute"

isXSElem :: String -> LA XmlTree XmlTree
isXSElem name
    = isElem >>> hasXSName name

hasXSName :: String -> LA XmlTree XmlTree
hasXSName lp
    = hasNameWith $
      \ qn -> namespaceUri qn == "http://www.w3.org/2001/XMLSchema"
              &&
              localPart qn == lp

propagateXmlSchemaNamespaces :: LA XmlTree XmlTree
propagateXmlSchemaNamespaces
    = processWithNsEnvWithoutAttrl propagate
      [ (xmlXName,   xmlNamespaceXName)
      , (xmlnsXName, xmlnsNamespaceXName)
      ]
    where
      hasPropAttr
          = isElement <+> isAttribute
      isPropAttr
          = hasName "type" <+> hasName "ref"
      propagate env
          = addXns `when` hasPropAttr
          where
            addXns
                = processAttrl $
                  changeAttrValue addUri `when` isPropAttr
            addUri n
                = maybe n (\ u -> "{" ++ unXN u ++ "}" ++ n) $ lookup (newXName px) env
                where
                  (px', lp') = span (/= ':') n
                  px
                      | null lp'  = ""
                      | otherwise = px'