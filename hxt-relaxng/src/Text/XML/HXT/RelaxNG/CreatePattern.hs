-- |
--
-- Creates the 'Pattern' datastructure from a simplified Relax NG schema.
-- The created datastructure is used in the validation algorithm
-- (see also: "Text.XML.HXT.RelaxNG.Validation")

module Text.XML.HXT.RelaxNG.CreatePattern
  ( createPatternFromXmlTree
  , createNameClass
  , firstChild
  , lastChild
  , module Text.XML.HXT.RelaxNG.PatternFunctions
  )
where

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface

import Text.XML.HXT.Arrow.XmlArrow

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.BasicArrows
import Text.XML.HXT.RelaxNG.PatternFunctions

import Data.Maybe
    ( fromMaybe )

import Data.List
    ( isPrefixOf )
{-
import qualified Debug.Trace as T
-}
-- ------------------------------------------------------------

-- | Creates the 'Pattern' datastructure from a simplified Relax NG schema.

createPatternFromXmlTree :: LA XmlTree Pattern
createPatternFromXmlTree = arr patternFromXmlTree

patternFromXmlTree :: XmlTree -> Pattern
patternFromXmlTree t = patternFromXml env t
   where env = map (fmap $ patternFromXml env) definitions
         definitions = runLA createDefinitionList t
         createDefinitionList :: LA XmlTree (String, XmlTree)
         createDefinitionList = deep isRngDefine
                                >>>
                                (getRngAttrName &&& getChildren)

patternFromXml :: PatternEnv -> XmlTree -> Pattern
patternFromXml env = head . runLA (createPatternFromXml env)

-- | Transforms each XML-element to the corresponding pattern

createPatternFromXml :: PatternEnv -> LA XmlTree Pattern
createPatternFromXml env
 = choiceA
   [ isRoot          :-> processRoot env
   , isRngEmpty      :-> constA Empty
   , isRngNotAllowed :-> mkNotAllowed
   , isRngText       :-> constA Text
   , isRngChoice     :-> mkRelaxChoice env
   , isRngInterleave :-> mkRelaxInterleave env
   , isRngGroup      :-> mkRelaxGroup env
   , isRngOneOrMore  :-> mkRelaxOneOrMore env
   , isRngList       :-> mkRelaxList env
   , isRngData       :-> mkRelaxData env
   , isRngValue      :-> mkRelaxValue
   , isRngAttribute  :-> mkRelaxAttribute env
   , isRngElement    :-> mkRelaxElement env
   , isRngRef        :-> mkRelaxRef env
   , this            :-> mkRelaxError "internal HXT RelaxNG error"
   ]

processRoot :: PatternEnv -> LA XmlTree Pattern
processRoot env
  = getChildren
    >>>
    choiceA [
      isRngRelaxError :-> (mkRelaxError $< getRngAttrDescr),
      isRngGrammar    :-> (processGrammar env),
      this            :-> (mkRelaxError "no grammar-pattern in schema")
    ]


processGrammar :: PatternEnv -> LA XmlTree Pattern
processGrammar env
  = getChildren
    >>>
    choiceA
    [ isRngDefine     :-> none
    , isRngRelaxError :-> (mkRelaxError $< getAttrValue "desc")
    , isRngStart      :-> (getChildren >>> createPatternFromXml env)
    , this            :-> (mkRelaxError "no start-pattern in schema")
    ]


{- |
  Transforms a ref-element.
  The value of the name-attribute is looked up in the environment list
  to find the corresponding define-pattern.
  Haskells lazy-evaluation is used to transform circular structures.
-}
mkRelaxRef :: PatternEnv -> LA XmlTree Pattern
mkRelaxRef e
 = getRngAttrName
   >>>
   arr (\n -> fromMaybe (notAllowed $ "define-pattern with name " ++ n ++ " not found")
              $ lookup n e
       )
{-
   where
 transformEnv :: [(String, XmlTree)] -> [(String, Pattern)]
 transformEnv env = [ (treeName, (transformEnvElem tree env)) | (treeName, tree) <- env]
 transformEnvElem :: XmlTree -> [(String, XmlTree)] -> Pattern
 transformEnvElem tree env = head $ runLA (createPatternFromXml env) tree
-}

-- | Transforms a notAllowed-element.
mkNotAllowed :: LA XmlTree Pattern
mkNotAllowed = constA $ notAllowed "notAllowed-pattern in Relax NG schema definition"


-- | Creates an error message.
mkRelaxError :: String -> LA XmlTree Pattern
mkRelaxError errStr
 = choiceA
   [ isRngRelaxError :-> (getRngAttrDescr >>> arr notAllowed)
   , isElem  :-> ( getName
                   >>>
                   arr (\n -> notAllowed $ "Pattern " ++ n ++
                                           " is not allowed in Relax NG schema" ++
                                           " (" ++ errStr ++ ")"
                       )
                 )
   , isAttr  :-> ( getName
                   >>>
                   arr (\n -> notAllowed $ "Attribute " ++ n ++
                                           " is not allowed in Relax NG schema"
                       )
                 )
   , isError :-> ( getErrorMsg >>> arr notAllowed )

   , this    :-> arr ( \e -> notAllowed $ if errStr /= ""
                                          then errStr
                                          else "Can't create pattern from " ++ show e)
   ]


-- | Transforms a choice-element.
mkRelaxChoice :: PatternEnv -> LA XmlTree Pattern
mkRelaxChoice env
    = ifA ( getChildren >>.
            ( \ l -> if length l == 1 then l else [] )
          )
      ( createPatternFromXml env )
      ( getTwoChildrenPattern env >>> arr2 Choice )

-- | Transforms a interleave-element.
mkRelaxInterleave :: PatternEnv -> LA XmlTree Pattern
mkRelaxInterleave env
    = getTwoChildrenPattern env
      >>>
      arr2 Interleave


-- | Transforms a group-element.
mkRelaxGroup :: PatternEnv -> LA XmlTree Pattern
mkRelaxGroup env
    = getTwoChildrenPattern env
      >>>
      arr2 Group


-- | Transforms a oneOrMore-element.
mkRelaxOneOrMore :: PatternEnv -> LA XmlTree Pattern
mkRelaxOneOrMore env
    = getOneChildPattern env
      >>>
      arr OneOrMore


-- | Transforms a list-element.
mkRelaxList :: PatternEnv -> LA XmlTree Pattern
mkRelaxList env
    = getOneChildPattern env
      >>>
      arr List


-- | Transforms a data- or dataExcept-element.
mkRelaxData :: PatternEnv -> LA XmlTree Pattern
mkRelaxData env
  = ifA (getChildren >>> isRngExcept)
     (processDataExcept >>> arr3 DataExcept)
     (processData >>> arr2 Data)
  where
  processDataExcept :: LA XmlTree (Datatype, (ParamList, Pattern))
  processDataExcept = getDatatype &&& getParamList &&&
                      ( getChildren
                        >>>
                        isRngExcept
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
                         isRngParam
                         >>>
                         (getRngAttrName &&& (getChildren >>> getText))


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
getDatatype = getRngAttrDatatypeLibrary
              &&&
              getRngAttrType


-- | Transforms a attribute-element.
-- The first child is a 'NameClass', the second (the last) one a pattern.

mkRelaxAttribute :: PatternEnv -> LA XmlTree Pattern
mkRelaxAttribute env
    = ( ( firstChild >>> createNameClass )
        &&&
        ( lastChild >>> createPatternFromXml env )
      )
      >>>
      arr2 Attribute

-- | Transforms a element-element.
-- The first child is a 'NameClass', the second (the last) one a pattern.
mkRelaxElement :: PatternEnv -> LA XmlTree Pattern
mkRelaxElement env
    = ( ( firstChild >>> createNameClass )
        &&&
        ( lastChild >>> createPatternFromXml env )
      )
      >>>
      arr2 Element


-- | Creates a 'NameClass' from an \"anyName\"-, \"nsName\"- or  \"name\"-Pattern,
createNameClass :: LA XmlTree NameClass
createNameClass
    = choiceA
      [ isRngAnyName :-> processAnyName
      , isRngNsName  :-> processNsName
      , isRngName    :-> processName
      , isRngChoice  :-> processChoice
      , this         :-> mkNameClassError
      ]
    where
    processAnyName :: LA XmlTree NameClass
    processAnyName
        = ifA (getChildren >>> isRngExcept)
          ( getChildren
            >>> getChildren
            >>> createNameClass
            >>> arr AnyNameExcept
          )
         ( constA AnyName )

    processNsName :: LA XmlTree NameClass
    processNsName
        = ifA (getChildren >>> isRngExcept)
          ( ( getRngAttrNs
              &&&
              ( getChildren >>> getChildren >>> createNameClass )
            )
            >>>
            arr2 NsNameExcept
          )
          ( getRngAttrNs >>> arr NsName )

    processName :: LA XmlTree NameClass
    processName
        = (getRngAttrNs &&& (getChildren >>> getText)) >>> arr2 Name

    processChoice :: LA XmlTree NameClass
    processChoice
        = ( ( firstChild >>> createNameClass )
            &&&
            ( lastChild  >>> createNameClass )
          )
          >>>
          arr2 NameClassChoice

mkNameClassError :: LA XmlTree NameClass
mkNameClassError
    = choiceA [ isRngRelaxError
                        :-> ( getRngAttrDescr
                              >>>
                              arr NCError
                         )
              , isElem  :-> ( getName
                              >>>
                              arr (\n -> NCError ("Can't create name class from element " ++ n))
                            )
              , isAttr  :-> ( getName
                              >>>
                              arr (\n -> NCError ("Can't create name class from attribute: " ++ n))
                            )
              , isError :-> ( getErrorMsg
                              >>>
                              arr NCError
                            )
              , this    :-> ( arr (\e ->  NCError $ "Can't create name class from " ++ show e) )
              ]


getOneChildPattern :: PatternEnv -> LA XmlTree Pattern
getOneChildPattern env
    = firstChild >>> createPatternFromXml env


getTwoChildrenPattern :: PatternEnv -> LA XmlTree (Pattern, Pattern)
getTwoChildrenPattern env
    = ( getOneChildPattern env )
        &&&
        ( lastChild  >>> createPatternFromXml env )

-- | Simple access arrows

firstChild      :: (ArrowTree a, Tree t) => a (t b) (t b)
firstChild      = single getChildren

lastChild       :: (ArrowTree a, Tree t) => a (t b) (t b)
lastChild       = getChildren >>. (take 1 . reverse)
