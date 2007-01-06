-- | basic 'Pattern' functions
--

module Text.XML.HXT.RelaxNG.PatternFunctions

where

import Text.XML.HXT.RelaxNG.DataTypes


-- ------------------------------------------------------------

isRelaxEmpty :: Pattern -> Bool
isRelaxEmpty Empty = True          
isRelaxEmpty _     = False

isRelaxNotAllowed :: Pattern -> Bool
isRelaxNotAllowed (NotAllowed _) = True          
isRelaxNotAllowed _              = False

isRelaxText :: Pattern -> Bool
isRelaxText Text = True          
isRelaxText _    = False

isRelaxChoice :: Pattern -> Bool
isRelaxChoice (Choice _ _) = True          
isRelaxChoice _            = False

isRelaxInterleave :: Pattern -> Bool
isRelaxInterleave (Interleave _ _) = True          
isRelaxInterleave _                = False

isRelaxGroup :: Pattern -> Bool
isRelaxGroup (Group _ _) = True          
isRelaxGroup _           = False

isRelaxOneOrMore :: Pattern -> Bool
isRelaxOneOrMore (OneOrMore _) = True          
isRelaxOneOrMore _             = False

isRelaxList :: Pattern -> Bool
isRelaxList (List _) = True          
isRelaxList _        = False

isRelaxData :: Pattern -> Bool
isRelaxData (Data _ _) = True          
isRelaxData _          = False

isRelaxDataExcept :: Pattern -> Bool
isRelaxDataExcept (DataExcept _ _ _) = True          
isRelaxDataExcept _                  = False

isRelaxValue :: Pattern -> Bool
isRelaxValue (Value _ _ _) = True          
isRelaxValue _             = False

isRelaxAttribute :: Pattern -> Bool
isRelaxAttribute (Attribute _ _) = True          
isRelaxAttribute _               = False

isRelaxElement :: Pattern -> Bool
isRelaxElement (Element _ _) = True          
isRelaxElement _             = False
             
isRelaxAfter :: Pattern -> Bool
isRelaxAfter (After _ _) = True          
isRelaxAfter _           = False


-- | Returns a list of children pattern for each pattern, 
-- e.g. (Choice p1 p2) = [p1, p2]
getChildrenPattern :: Pattern -> [Pattern]
getChildrenPattern (Choice p1 p2) = [p1, p2]
getChildrenPattern (Interleave p1 p2) = [p1, p2]
getChildrenPattern (Group p1 p2) = [p1, p2]
getChildrenPattern (OneOrMore p) = [p]
getChildrenPattern (List p) = [p]
getChildrenPattern (Element _ p) = [p]
getChildrenPattern (DataExcept _ _ p) = [p]
getChildrenPattern (Attribute _ p) = [p]
getChildrenPattern (After p1 p2) = [p1, p2]
getChildrenPattern _ = []


-- | Returns the nameclass of a element- or attribute pattern.
-- Otherwise 'NCError' is returned.
getNameClassFromPattern :: Pattern -> NameClass
getNameClassFromPattern (Element nc _) = nc
getNameClassFromPattern (Attribute nc _) = nc
getNameClassFromPattern _ = NCError "Pattern without a NameClass"


-- | Returns a string representation of the pattern name
getPatternName :: Pattern -> String
getPatternName (Empty) = "Empty"
getPatternName (NotAllowed _) = "NotAllowed"
getPatternName (Text) = "Text"
getPatternName (Choice _ _) = "Choice"
getPatternName (Interleave _ _) = "Interleave"
getPatternName (Group _ _) = "Group"
getPatternName (OneOrMore _) = "OneOrMore"
getPatternName (List _) = "List"
getPatternName (Data _ _ ) = "Data"
getPatternName (DataExcept _ _ _) = "DataExcept"
getPatternName (Value _ _ _) = "Value"
getPatternName (Attribute _ _) = "Attribute"
getPatternName (Element _ _) = "Element"
getPatternName (After _ _) = "After"
