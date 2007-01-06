module Text.XML.HXT.RelaxNG.PatternToString
  ( patternToStringTree
  , patternToFormatedString
  , xmlTreeToPatternStringTree
  , xmlTreeToPatternFormatedString
  , xmlTreeToPatternString
  , nameClassToString
  )
where

import Control.Arrow.ListArrows

import Data.Tree.NTree.TypeDefs

import Text.XML.HXT.Arrow.DOMInterface

import Text.XML.HXT.RelaxNG.DataTypes
import Text.XML.HXT.RelaxNG.CreatePattern
import Text.XML.HXT.RelaxNG.Utils

-- ------------------------------------------------------------

type PatternTree = NTree String


{- |
Returns a string representation of the pattern structure.
(see also: 'createPatternFromXmlTree')

Example:

> Element {}foo (Choice (Choice (Value ("","token") "abc"
> ("foo","www.bar.baz")]))(Data ("http://www.mysql.com","VARCHAR")
> [("length","2"),("maxLength","5")])) (Element {}bar (Group (Element {}baz

The function can @not@ be used to display circular ref-pattern structures.
-}

xmlTreeToPatternString :: LA XmlTree String
xmlTreeToPatternString
    = createPatternFromXmlTree
      >>^
      show


-- | Returns a string representation of a nameclass.

nameClassToString :: NameClass -> String
nameClassToString AnyName
    = "AnyName"

nameClassToString (AnyNameExcept nc) 
    = "AnyNameExcept " ++ nameClassToString nc

nameClassToString (Name uri local) 
    = "{" ++ uri ++ "}" ++ local

nameClassToString (NsName uri)
    = "{" ++ uri ++ "}"

nameClassToString (NsNameExcept uri nc)
    = uri ++ "except (NsName) " ++ nameClassToString nc

nameClassToString (NameClassChoice nc1 nc2)
    = nameClassToString nc1 ++ " " ++ nameClassToString nc2

nameClassToString (NCError e)
    = "NameClass Error: " ++ e


-- ------------------------------------------------------------

{- |
Returns a tree representation of the pattern structure.
The hard work is done by 'formatTree'.

Example:

> +---element {}bar
>     |
>     +---group
>         |
>         +---oneOrMore
>         |   |
>         |   +---attribute AnyName
>         |       |
>         |       +---text
>         |
>         +---text
               
The function can be used to display circular ref-pattern structures.

Example:

> <define name="baz">
>   <element name="baz">
>     ... <ref name="baz"/> ...
>   </element>
> </define>

-}

patternToStringTree :: LA Pattern String
patternToStringTree
    = fromSLA [] pattern2PatternTree
      >>^
      (\p -> formatTree id p ++ "\n")


-- | Returns a tree representation of the pattern structure.
-- (see also: 'createPatternFromXmlTree' and 'patternToStringTree')

xmlTreeToPatternStringTree :: LA XmlTree String
xmlTreeToPatternStringTree
    = createPatternFromXmlTree 
      >>>
      patternToStringTree


pattern2PatternTree :: SLA [NameClass] Pattern PatternTree
pattern2PatternTree
    = choiceA
      [ isA isRelaxEmpty      :-> (constA $ NTree "empty" [])
      , isA isRelaxNotAllowed :-> notAllowed2PatternTree
      , isA isRelaxText       :-> (constA $ NTree "text" [])
      , isA isRelaxChoice     :-> choice2PatternTree          
      , isA isRelaxInterleave :-> children2PatternTree "interleave"
      , isA isRelaxGroup      :-> children2PatternTree "group"
      , isA isRelaxOneOrMore  :-> children2PatternTree "oneOrMore"
      , isA isRelaxList       :-> children2PatternTree "list"
      , isA isRelaxData       :-> data2PatternTree
      , isA isRelaxDataExcept :-> dataExcept2PatternTree
      , isA isRelaxValue      :-> value2PatternTree
      , isA isRelaxAttribute  :-> createPatternTreeFromElement "attribute"          
      , isA isRelaxElement    :-> element2PatternTree
      , isA isRelaxAfter      :-> children2PatternTree "after"
      ]

notAllowed2PatternTree :: SLA [NameClass] Pattern PatternTree
notAllowed2PatternTree
    = arr $ \(NotAllowed s) -> NTree "notAllowed" [NTree s []]


data2PatternTree :: SLA [NameClass] Pattern PatternTree
data2PatternTree 
    = arr $ \ (Data d p) -> NTree "data" [ datatype2PatternTree d
					 , mapping2PatternTree "parameter" p
					 ]
 
dataExcept2PatternTree :: SLA [NameClass] Pattern PatternTree
dataExcept2PatternTree 
    = this &&& (listA $ arrL getChildrenPattern >>> pattern2PatternTree)
      >>>
      arr2 ( \ (DataExcept d param _) pattern ->
             NTree "dataExcept" ([ datatype2PatternTree d
				 , mapping2PatternTree "parameter" param
				 ] ++ pattern)
           )

value2PatternTree :: SLA [NameClass] Pattern PatternTree 
value2PatternTree
    = arr $ \ (Value d v c) -> NTree ("value = " ++ v) [ datatype2PatternTree d
                                                       , context2PatternTree c
                                                       ]


createPatternTreeFromElement :: String -> SLA [NameClass] Pattern PatternTree
createPatternTreeFromElement name
    = ( arr getNameClassFromPattern
	&&&
	listA (arrL getChildrenPattern >>> pattern2PatternTree)
      )
      >>>
      arr2 (\nc rl -> NTree (name ++ " " ++ show nc) rl)

children2PatternTree :: String -> SLA [NameClass] Pattern PatternTree
children2PatternTree name 
    = listA (arrL getChildrenPattern >>> pattern2PatternTree) 
      >>^
      (NTree name)

choice2PatternTree :: SLA [NameClass] Pattern PatternTree
choice2PatternTree 
    = ifA ( -- wenn das zweite kind ein noch nicht ausgegebenes element ist, 
            -- muss dieses anders behandelt werden
            -- nur fuer bessere formatierung des outputs
            arr (last . getChildrenPattern) >>> isA (isRelaxElement) >>>
            (arr getNameClassFromPattern &&& getState) >>> 
            isA(\ (nc, liste) -> not $ elem nc liste)
          )
      ( -- element in status aufnehmen, wird dann nicht mehr vom erste kind ausgegeben
        arr getChildrenPattern
	>>>
        changeState (\s p -> (getNameClassFromPattern (last p)) : s)
	>>>
        ( ( head ^>> pattern2PatternTree )		-- erstes kind normal verarbeiten
          &&&						-- zweites kind, das element, verarbeiten
          ( last ^>> createPatternTreeFromElement "element" )
        )
        >>>
        arr2 ( \ l1 l2 -> NTree "choice" [l1, l2] )
      )
      ( children2PatternTree "choice" )

                            
element2PatternTree :: SLA [NameClass] Pattern PatternTree
element2PatternTree 
    = ifA ( (arr getNameClassFromPattern &&& getState)
            >>> 
            isA (\ (nc, liste) -> elem nc liste)
          )
      ( arr getNameClassFromPattern
        >>> 
        arr (\nc -> NTree ("reference to element " ++ show nc) [])
      )
      ( changeState (\ s p -> (getNameClassFromPattern p) : s)
        >>>
        createPatternTreeFromElement "element"
      )


mapping2PatternTree :: String -> [(Prefix, Uri)] -> PatternTree
mapping2PatternTree name mapping
    = NTree name (map (\(a, b) -> NTree (a ++ " = " ++ b) []) mapping)


datatype2PatternTree :: Datatype -> PatternTree
datatype2PatternTree dt
    = NTree (datatype2String dt) []


context2PatternTree :: Context -> PatternTree
context2PatternTree (base, mapping)
    = NTree "context" [ NTree ("base-uri = " ++ base) []
                      , mapping2PatternTree "namespace environment" mapping
                      ]

-- ------------------------------------------------------------

-- | Returns a formated string representation of the pattern structure.
-- (see also: 'createPatternFromXmlTree' and 'patternToFormatedString')
xmlTreeToPatternFormatedString :: LA XmlTree String
xmlTreeToPatternFormatedString
    = createPatternFromXmlTree 
      >>>
      fromSLA [] patternToFormatedString


{- |
Returns a formated string representation of the pattern structure.

Example:

> Element {}foo (Choice (Choice ( Value = abc, 
> datatypelibrary = http://relaxng.org/ns/structure/1.0, type = token, 
> context (base-uri =file://test.rng, 
> parameter: xml = http://www.w3.org/XML/1998/namespaces, foo = www.bar.baz), 

The function can be used to display circular ref-pattern structures.
-}

patternToFormatedString :: SLA [NameClass] Pattern String
patternToFormatedString
    = choiceA
      [ isA isRelaxEmpty      :-> (constA " empty ")
      , isA isRelaxNotAllowed :-> (arr $ \ (NotAllowed errorEnv) -> errorEnv)
      , isA isRelaxText       :-> (constA " text ")
      , isA isRelaxChoice     :-> children2FormatedString "choice"
      , isA isRelaxInterleave :-> children2FormatedString "interleave"
      , isA isRelaxGroup      :-> children2FormatedString "group"
      , isA isRelaxOneOrMore  :-> children2FormatedString "oneOrMore"
      , isA isRelaxList       :-> children2FormatedString "list"
      , isA isRelaxData       :-> data2FormatedString
      , isA isRelaxDataExcept :-> dataExcept2FormatedString
      , isA isRelaxValue      :-> value2FormatedString
      , isA isRelaxAttribute  :-> createFormatedStringFromElement "attribute"
      , isA isRelaxElement    :-> element2FormatedString
      , isA isRelaxAfter      :-> children2FormatedString "after"
      ]

children2FormatedString :: String -> SLA [NameClass] Pattern String
children2FormatedString name
    = listA (arrL getChildrenPattern >>> patternToFormatedString) 
      >>^
      (\ l -> name ++ " (" ++ formatStringList ", " l ++ ") " )

data2FormatedString :: SLA [NameClass] Pattern String
data2FormatedString
    = arr ( \ (Data datatype paramList) ->
	    "Data " ++ datatype2String datatype ++ "\n " ++ 
            mapping2String "parameter" paramList ++ "\n"
	  )

dataExcept2FormatedString :: SLA [NameClass] Pattern String
dataExcept2FormatedString 
 = arr ( \ (DataExcept datatype paramList _) ->
         "DataExcept " ++ show datatype ++ "\n " ++ 
         mapping2String "parameter" paramList ++ "\n "
       )
   &&&
   ( arr (\ (DataExcept _ _ p) -> p) >>> patternToFormatedString )
   >>>
   arr2 (++)


value2FormatedString :: SLA [NameClass] Pattern String
value2FormatedString 
 = arr $ \(Value datatype val context) ->
           "Value = " ++ val ++ ", " ++ datatype2String datatype ++ 
           "\n " ++ context2String context ++ "\n"


element2FormatedString :: SLA [NameClass] Pattern String
element2FormatedString
    = ifA ( (arr getNameClassFromPattern &&& getState)
            >>>
            isA (\ (nc, liste) -> elem nc liste)
	  )
      ( arr getNameClassFromPattern
	>>^ 
	( \nc -> "reference to element " ++ nameClassToString nc ++ " " )
      )
      ( changeState (\ s p -> (getNameClassFromPattern p) : s)
	>>>
	createFormatedStringFromElement "element"
      )


createFormatedStringFromElement :: String -> SLA [NameClass] Pattern String
createFormatedStringFromElement name
    = ( arr getNameClassFromPattern
	&&&
	( listA (arrL getChildrenPattern >>> patternToFormatedString)
	  >>^
	  formatStringList ", "
	)
      )
      >>>
      arr2 (\ nc rl -> name ++ " " ++ nameClassToString nc ++ " (" ++ rl ++ ")")


-- ------------------------------------------------------------

mapping2String :: String -> [(Prefix, Uri)] -> String
mapping2String name mapping
    = name ++ ": " ++ 
      formatStringList ", " (map (\(a, b) -> a ++ " = " ++ b) mapping)

datatype2String :: Datatype -> String
datatype2String (lib, localName)
    = "datatypelibrary = " ++ getLib ++ ", type = " ++ localName
    where
    getLib = if lib == "" then relaxNamespace else lib
  
context2String :: Context -> String
context2String (base, mapping)
    = "context (base-uri = " ++ base ++ ", " ++
      mapping2String "namespace environment" mapping ++ ")"
  
-- ------------------------------------------------------------
