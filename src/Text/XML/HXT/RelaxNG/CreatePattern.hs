-- |
-- 
-- Creates the 'Pattern' datastructure from a simplified Relax NG schema. 
-- The created datastructure is used in the validation algorithm
-- (see also: "Text.XML.HXT.RelaxNG.Validation")

module Text.XML.HXT.RelaxNG.CreatePattern
  ( createPatternFromXmlTree
  , createNameClass
  , module Text.XML.HXT.RelaxNG.PatternFunctions
  )
where

import Control.Arrow.ListArrows

import Text.XML.HXT.Arrow.DOMInterface

import Text.XML.HXT.Arrow.XmlArrow hiding
    ( hasName )

import qualified Text.XML.HXT.Arrow.XmlArrow as A
    ( hasName )

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.PatternFunctions

import Data.Maybe
    ( fromMaybe )

import Data.List
    ( isPrefixOf )

-- ------------------------------------------------------------

hasName :: String -> LA XmlTree XmlTree
hasName s = A.hasName s 
            `orElse`
            (hasLocalPart s >>> hasNamespaceUri relaxNamespace)


-- | Creates the 'Pattern' datastructure from a simplified Relax NG schema.
createPatternFromXmlTree :: LA XmlTree Pattern
createPatternFromXmlTree = createPatternFromXml $< createEnv
 where
 -- | Selects all define-pattern and creates an environment list.
 -- Each list entry maps the define name to the children of the define-pattern.
 -- The map is used to replace a ref-pattern with the referenced define-pattern.
 createEnv :: LA XmlTree Env
 createEnv = listA $ deep (isElem >>> hasName "define") 
                     >>> 
                     (getAttrValue "name" &&& getChildren)


-- | Transforms each XML-element to the corresponding pattern
createPatternFromXml :: Env -> LA XmlTree Pattern
createPatternFromXml env
 = choiceA [
     isRoot                            :-> processRoot env,
     (isElem >>> hasName "empty")      :-> constA Empty,
     (isElem >>> hasName "notAllowed") :-> mkNotAllowed,
     (isElem >>> hasName "text")       :-> constA Text,
     (isElem >>> hasName "choice")     :-> mkRelaxChoice env,
     (isElem >>> hasName "interleave") :-> mkRelaxInterleave env,
     (isElem >>> hasName "group")      :-> mkRelaxGroup env,
     (isElem >>> hasName "oneOrMore")  :-> mkRelaxOneOrMore env,
     (isElem >>> hasName "list")       :-> mkRelaxList env,
     (isElem >>> hasName "data")       :-> mkRelaxData env,
     (isElem >>> hasName "value")      :-> mkRelaxValue,
     (isElem >>> hasName "attribute")  :-> mkRelaxAttribute env,
     (isElem >>> hasName "element")    :-> mkRelaxElement env,
     (isElem >>> hasName "ref")        :-> mkRelaxRef env,
     this                              :-> mkRelaxError ""
   ]

              
processRoot :: Env -> LA XmlTree Pattern
processRoot env
  = getChildren
    >>> 
    choiceA [
      (isElem >>> hasName "relaxError") :-> (mkRelaxError $< getAttrValue "desc"),
      (isElem >>> hasName "grammar")    :-> (processGrammar env),
      this                              :-> (mkRelaxError "no grammar-pattern in schema")
    ]


processGrammar :: Env -> LA XmlTree Pattern
processGrammar env
  = getChildren
    >>> 
    choiceA [
      (isElem >>> hasName "define")     :-> none,
      (isElem >>> hasName "relaxError") :-> (mkRelaxError $< getAttrValue "desc"),
      (isElem >>> hasName "start")      :-> (getChildren >>> createPatternFromXml env),
      this                              :-> (mkRelaxError "no start-pattern in schema")
    ]


{- |
  Transforms a ref-element.
  The value of the name-attribute is looked up in the environment list
  to find the corresponding define-pattern. 
  Haskells lazy-evaluation is used to transform circular structures.
-}
mkRelaxRef :: Env -> LA XmlTree Pattern
mkRelaxRef e
 = getAttrValue "name"
   >>>
   arr (\n -> fromMaybe (NotAllowed $ "define-pattern with name " ++ n ++ " not found")
              . lookup n $ transformEnv e
       )
 where
 transformEnv :: [(String, XmlTree)] -> [(String, Pattern)]
 transformEnv env = [ (treeName, (transformEnvElem tree env)) | (treeName, tree) <- env]
 transformEnvElem :: XmlTree -> [(String, XmlTree)] -> Pattern
 transformEnvElem tree env = head $ runLA (createPatternFromXml env) tree 


-- | Transforms a notAllowed-element.
mkNotAllowed :: LA XmlTree Pattern
mkNotAllowed = constA $ NotAllowed "notAllowed-pattern in Relax NG schema definition"


-- | Creates an error message.
mkRelaxError :: String -> LA XmlTree Pattern
mkRelaxError errStr
 = choiceA [
     (isElem >>> hasName "relaxError") :-> (getAttrValue "desc" >>> arr NotAllowed),
     isElem  :-> ( getName
                   >>>
                   arr (\n -> NotAllowed $ "Pattern " ++ n ++ 
                                           " is not allowed in Relax NG schema"
                       )
                 ),
     isAttr  :-> ( getName
                   >>>
                   arr (\n -> NotAllowed $ "Attribute " ++ n ++ 
                                           " is not allowed in Relax NG schema"
                       )
                 ),
     isError :-> (getErrorMsg >>> arr NotAllowed),                          
     this    :-> (arr (\e -> NotAllowed $ if errStr /= ""
                                          then errStr
                                          else "Can't create pattern from " ++ show e)
                 )
   ]


-- | Transforms a choice-element.
mkRelaxChoice :: Env -> LA XmlTree Pattern
mkRelaxChoice env = listA getChildren
                    >>> 
                    ifP (\l -> length l == 1)
                      (arrL id >>> createPatternFromXml env)
                      (getTwoChildrenPattern env >>> arr2 Choice)

               
-- | Transforms a interleave-element.
mkRelaxInterleave :: Env -> LA XmlTree Pattern
mkRelaxInterleave env = listA getChildren
                        >>> 
                        getTwoChildrenPattern env
                        >>> 
                        arr2 Interleave


-- | Transforms a group-element.
mkRelaxGroup :: Env -> LA XmlTree Pattern
mkRelaxGroup env = listA getChildren
                   >>> 
                   getTwoChildrenPattern env
                   >>>
                   arr2 Group


-- | Transforms a oneOrMore-element.
mkRelaxOneOrMore :: Env -> LA XmlTree Pattern
mkRelaxOneOrMore env = getOneChildPattern env
                       >>> 
                       arr OneOrMore


-- | Transforms a list-element.
mkRelaxList :: Env -> LA XmlTree Pattern
mkRelaxList env = getOneChildPattern env >>> arr List


-- | Transforms a data- or dataExcept-element.
mkRelaxData :: Env -> LA XmlTree Pattern
mkRelaxData env 
  = ifA (getChildren >>> hasName "except")
     (processDataExcept >>> arr3 DataExcept)
     (processData >>> arr2 Data)
  where
  processDataExcept :: LA XmlTree (Datatype, (ParamList, Pattern))
  processDataExcept = getDatatype &&& getParamList &&& 
                      ( getChildren
                        >>> 
                        isElem >>> hasName "except"
                        >>> 
                        getChildren
                        >>>
                        createPatternFromXml env
                      )
  processData :: LA XmlTree (Datatype, ParamList)
  processData = getDatatype &&& getParamList
  getParamList :: LA XmlTree ParamList
  getParamList = listA $ getChildren
                         >>>
                         isElem >>> hasName "param"
                         >>> 
                         (getAttrValue "name" &&& (getChildren >>> getText))
         

-- | Transforms a value-element.
mkRelaxValue :: LA XmlTree Pattern         
mkRelaxValue = getDatatype &&& getValue &&& getContext
               >>>
               arr3 Value 
  where
  getContext :: LA XmlTree Context
  getContext = getAttrValue contextBaseAttr &&& getMapping
  getMapping :: LA XmlTree [(Prefix, Uri)]
  getMapping = listA $ getAttrl >>> 
                       ( (getName >>> isA (contextAttributes `isPrefixOf`))
                         `guards`
                         ( (getName >>> arr (drop $ length contextAttributes)) 
                           &&&
                           (getChildren >>> getText)
                         )
                       )
  getValue :: LA XmlTree String
  getValue = (getChildren >>> getText) `orElse` (constA "")


getDatatype :: LA XmlTree Datatype
getDatatype = getAttrValue "datatypeLibrary"
              &&&
              getAttrValue "type"


-- | Transforms a attribute-element.
-- The first child is a 'NameClass', the second one a pattern.
mkRelaxAttribute :: Env -> LA XmlTree Pattern
mkRelaxAttribute env
 = listA getChildren
   >>> 
   ((arr head >>> createNameClass) &&& secondPattern env)
   >>>
   arr2 Attribute


-- | Transforms a element-element.
-- The first child is a 'NameClass', the second one a pattern.
mkRelaxElement :: Env -> LA XmlTree Pattern
mkRelaxElement env
 = listA getChildren
   >>> 
   ((arr head >>> createNameClass) &&& secondPattern env)
   >>>
   arr2 Element


-- | Creates a 'NameClass' from an \"anyName\"-, \"nsName\"- or  \"name\"-Pattern, 
createNameClass :: LA XmlTree NameClass
createNameClass
 = choiceA [   
     (isElem >>> hasName "anyName") :-> processAnyName,
     (isElem >>> hasName "nsName")  :-> processNsName,
     (isElem >>> hasName "name")    :-> processName,
     (isElem >>> hasName "choice")  :-> processChoice,
     this                           :-> mkNameClassError
   ]
 where
 processAnyName :: LA XmlTree NameClass
 processAnyName = ifA (getChildren >>> hasName "except")
                    ( getChildren >>> getChildren >>>
                      createNameClass >>> arr AnyNameExcept
                    )
                    (constA AnyName)
 processNsName :: LA XmlTree NameClass
 processNsName = ifA (getChildren >>> hasName "except")
                   ( ( getAttrValue "ns" 
                       &&&
                       (getChildren >>> getChildren >>> createNameClass)
                     )
                     >>> 
                     arr2 NsNameExcept
                   )
                   (getAttrValue "ns" >>> arr NsName) 
 processName :: LA XmlTree NameClass
 processName = (getAttrValue "ns" &&& (getChildren >>> getText)) >>> arr2 Name
 processChoice :: LA XmlTree NameClass
 processChoice = listA getChildren
                 >>>
                 (arr head >>> createNameClass) &&& (arr last >>> createNameClass)
                 >>>
                 arr2 NameClassChoice
                        

mkNameClassError :: LA XmlTree NameClass
mkNameClassError 
 = choiceA [
     (isElem >>> hasName "relaxError") :-> (getAttrValue "desc" >>> arr NCError), 
     isElem  :-> ( getName
                   >>>
                   arr (\n -> NCError ("Can't create name class from element " ++ n))
                 ),
     isAttr  :-> ( getName
                   >>>
                   arr (\n -> NCError ("Can't create name class from attribute: " ++ n))
                 ),
     isError :-> (getErrorMsg >>> arr NCError),                          
     this    :-> (arr (\e ->  NCError $ "Can't create name class from " ++ show e))      
   ]


getOneChildPattern :: Env -> LA XmlTree Pattern
getOneChildPattern env = getChildren >>> createPatternFromXml env


getTwoChildrenPattern :: Env -> LA XmlTrees (Pattern, Pattern)
getTwoChildrenPattern env = firstPattern env &&& secondPattern env


firstPattern :: Env -> LA XmlTrees Pattern
firstPattern env = arr head >>> createPatternFromXml env


-- | After simplification, each choice, group, etc. pattern has exactly two children,
-- so @last@ selects the second one
secondPattern :: Env -> LA XmlTrees Pattern
secondPattern env = arr last >>> createPatternFromXml env
