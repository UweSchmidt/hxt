module Text.XML.HXT.RelaxNG.DataTypes
where

import Text.XML.HXT.DOM.TypeDefs

-- ------------------------------------------------------------

relaxSchemaFile :: String
relaxSchemaFile = "Text/XML/HXT/RelaxNG/SpecificationSchema.rng"


relaxSchemaGrammarFile :: String
relaxSchemaGrammarFile = "Text/XML/HXT/RelaxNG/SpecificationSchemaGrammar.rng"

-- ------------------------------------------------------------
-- datatypes for the simplification process

a_numberOfErrors,
 a_relaxSimplificationChanges,
 a_output_changes,
 defineOrigName :: String

a_numberOfErrors             = "numberOfErrors"
a_relaxSimplificationChanges = "relaxSimplificationChanges"
a_output_changes             = "output-pattern-transformations"
defineOrigName               = "RelaxDefineOriginalName"


type Env = [(String, XmlTree)]

type PatternEnv = [(String, Pattern)]

-- | Start of a context attribute value
-- (see also: 'Text.XML.HXT.RelaxNG.Simplification.simplificationStep1')
--
-- The value is always followed by the original attribute name and value

contextAttributes               :: String
contextAttributes               = "RelaxContext-"

contextAttributesDefault        :: String
contextAttributesDefault        = "RelaxContextDefault"

-- | Start of base uri attribute value
-- (see also: 'simplificationStep1' in "Text.XML.HXT.RelaxNG.Simplification")

contextBaseAttr :: String
contextBaseAttr = "RelaxContextBaseURI"


-- see simplificationStep5 in Text.XML.HXT.RelaxNG.Simplification

type OldName = String
type NewName = String
type NamePair = (OldName, NewName)
type RefList = [NamePair]


-- ------------------------------------------------------------
-- datatype library handling

-- | Type of all datatype libraries functions that tests whether
-- a XML instance value matches a value-pattern.
--
-- Returns Just \"errorMessage\" in case of an error else Nothing.

type DatatypeEqual  = DatatypeName -> String -> Context -> String -> Context -> Maybe String


-- | Type of all datatype libraries functions that tests whether
-- a XML instance value matches a data-pattern.
--
-- Returns Just \"errorMessage\" in case of an error else Nothing.

type DatatypeAllows = DatatypeName -> ParamList -> String -> Context -> Maybe String


-- | List of all supported datatype libraries

type DatatypeLibraries = [DatatypeLibrary]


-- | Each datatype library is identified by a URI.

type DatatypeLibrary   = (Uri, DatatypeCheck)

type DatatypeName      = String

type ParamName         = String


-- | List of all supported params for a datatype

type AllowedParams     = [ParamName]


-- | List of all supported datatypes and there allowed params

type AllowedDatatypes  = [(DatatypeName, AllowedParams)]


-- | The Constructor exports the list of supported datatypes for a library.
-- It also exports the specialized datatype library functions to validate
-- a XML instance value with respect to a datatype.

data DatatypeCheck
  = DTC { dtAllowsFct    :: DatatypeAllows -- ^ function to test whether a value matches a data-pattern
        , dtEqualFct     :: DatatypeEqual -- ^ function to test whether a value matches a value-pattern
        , dtAllowedTypes :: AllowedDatatypes -- ^ list of all supported params for a datatype
        }

-- ------------------------------------------------------------
-- datatypes for the validation process

type Uri = String

type LocalName = String


-- | List of parameters; each parameter is a pair consisting of a local name and a value.

type ParamList = [(LocalName, String)]

type Prefix = String


-- | A Context represents the context of an XML element.
-- It consists of a base URI and a mapping from prefixes to namespace URIs.

type Context = (Uri, [(Prefix, Uri)])

-- | A Datatype identifies a datatype by a datatype library name and a local name.

type Datatype = (Uri, LocalName)

showDatatype    :: Datatype -> String
showDatatype (u, ln)
         | null u       = ln
         | otherwise    = "{" ++ u ++ "}" ++ ln

-- | Represents a name class

data NameClass = AnyName
               | AnyNameExcept NameClass
               | Name Uri LocalName
               | NsName Uri
               | NsNameExcept Uri NameClass
               | NameClassChoice NameClass NameClass
               | NCError String
               deriving Eq

instance Show NameClass
    where
    show AnyName        = "AnyName"
    show (AnyNameExcept nameClass)
                        = "AnyNameExcept: " ++ show nameClass
    show (Name uri localName)
        | null uri      = localName
        | otherwise     = "{" ++ uri ++ "}" ++ localName
    show (NsName uri)   = "{" ++ uri ++ "}AnyName"
    show (NsNameExcept uri nameClass)
                        = "NsNameExcept: {" ++ uri ++ "}" ++ show nameClass
    show (NameClassChoice nameClass1 nameClass2)
                        = "NameClassChoice: " ++ show nameClass1 ++ "|" ++ show nameClass2
    show (NCError string)
                         = "NCError: " ++ string


-- | Represents a pattern after simplification

data Pattern = Empty
             | NotAllowed ErrMessage
             | Text
             | Choice Pattern Pattern
             | Interleave Pattern Pattern
             | Group Pattern Pattern
             | OneOrMore Pattern
             | List Pattern
             | Data Datatype ParamList
             | DataExcept Datatype ParamList Pattern
             | Value Datatype String Context
             | Attribute NameClass Pattern
             | Element NameClass Pattern
             | After Pattern Pattern

instance Show Pattern where
    show Empty                  = "empty"
    show (NotAllowed e)         = show e
    show Text                   = "text"
    show (Choice p1 p2)         = "( " ++ show p1 ++ " | " ++ show p2 ++ " )"
    show (Interleave p1 p2)     = "( " ++ show p1 ++ " & " ++ show p2 ++ " )"
    show (Group p1 p2)          = "( " ++ show p1 ++ " , " ++ show p2 ++ " )"
    show (OneOrMore p)          = show p ++ "+"
    show (List p)               = "list { " ++ show p ++ " }"
    show (Data dt pl)           = showDatatype dt ++ showPL pl
                                  where
                                  showPL []     = ""
                                  showPL l      = " {" ++ concatMap showP l ++ " }"
                                  showP (ln, v) = " " ++ ln ++ " = " ++ show v
    show (DataExcept dt pl p)   = show (Data dt pl) ++ " - (" ++ show p ++ " )"
    show (Value dt v _cx)       = showDatatype dt ++ " " ++ show v
    show (Attribute nc p)       = "attribute " ++ show nc ++ " { " ++ show p ++ " }"
    show (Element nc p)         = "element "   ++ show nc ++ " { " ++ show p ++ " }"
    show (After p1 p2)          =  "( " ++ show p1 ++ " ; " ++ show p2 ++ " )"

data ErrMessage = ErrMsg ErrLevel [String]
                  -- deriving Show

instance Show ErrMessage where
    show (ErrMsg _lev es) = foldr1 (\ x y -> x ++ "\n" ++ y) es

type ErrLevel   = Int

-- ------------------------------------------------------------

-- smart constructor funtions for Pattern

-- | smart constructor for NotAllowed

notAllowed      :: String -> Pattern
notAllowed      = notAllowedN 0

notAllowed1     :: String -> Pattern
notAllowed1     = notAllowedN 1

notAllowed2     :: String -> Pattern
notAllowed2     = notAllowedN 2

notAllowedN     :: ErrLevel -> String -> Pattern
notAllowedN l s = NotAllowed (ErrMsg l [s])

-- | merge error messages
--
-- If error levels are different, the more important is taken,
-- if level is 2 (max level) both error messages are taken
-- else the 1. error mesage is taken

mergeNotAllowed :: Pattern -> Pattern -> Pattern
mergeNotAllowed p1@(NotAllowed (ErrMsg l1 s1)) p2@(NotAllowed (ErrMsg l2 s2))
    | l1 < l2   = p2
    | l1 > l2   = p1
    | l1 == 2   = NotAllowed $ ErrMsg 2 (s1 ++ s2)
    | otherwise = p1

-- TODO : weird error when collecting error messages errors are duplicated

mergeNotAllowed _p1 _p2
    = notAllowed2 "mergeNotAllowed with wrong patterns"

-- | smart constructor for Choice

choice :: Pattern -> Pattern -> Pattern
choice p1@(NotAllowed _) p2@(NotAllowed _)      = mergeNotAllowed p1 p2
choice p1                   (NotAllowed _)      = p1
choice (NotAllowed _)    p2                     = p2
choice p1                p2                     = Choice p1 p2

-- | smart constructor for Group

group :: Pattern -> Pattern -> Pattern
group p1@(NotAllowed _)  p2@(NotAllowed _)      = mergeNotAllowed p1 p2
group _                   n@(NotAllowed _)      = n
group   n@(NotAllowed _)  _                     = n
group p                  Empty                  = p
group Empty              p                      = p
group p1                 p2                     = Group p1 p2

-- | smart constructor for OneOrMore

oneOrMore :: Pattern -> Pattern
oneOrMore n@(NotAllowed _) = n
oneOrMore p                = OneOrMore p

-- | smart constructor for Interleave

interleave :: Pattern -> Pattern -> Pattern
interleave p1@(NotAllowed _) p2@(NotAllowed _)  = mergeNotAllowed p1 p2
interleave _                 p2@(NotAllowed _)  = p2
interleave p1@(NotAllowed _) _                  = p1
interleave p1                Empty              = p1
interleave Empty             p2                 = p2
interleave p1                p2                 = Interleave p1 p2

-- | smart constructor for After

after :: Pattern -> Pattern -> Pattern
after p1@(NotAllowed _) p2@(NotAllowed _)       = mergeNotAllowed p1 p2
after _                 p2@(NotAllowed _)       = p2
after p1@(NotAllowed _) _                       = p1
after p1                p2                      = After p1 p2


-- | Possible content types of a Relax NG pattern.
-- (see also chapter 7.2 in Relax NG specification)

data ContentType = CTEmpty
                 | CTComplex
                 | CTSimple
                 | CTNone
     deriving (Show, Eq, Ord)

-- ------------------------------------------------------------
