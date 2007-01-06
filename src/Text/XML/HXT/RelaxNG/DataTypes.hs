module Text.XML.HXT.RelaxNG.DataTypes
where 

import Text.XML.HXT.DOM.TypeDefs

-- ------------------------------------------------------------

relaxSchemaFile	:: String
relaxSchemaFile = "Text/XML/HXT/RelaxNG/SpecificationSchema.rng"


relaxSchemaGrammarFile :: String
relaxSchemaGrammarFile = "Text/XML/HXT/RelaxNG/SpecificationSchemaGrammar.rng"

-- ------------------------------------------------------------
-- datatypes for the simplification process

a_numberOfErrors,
 a_relaxSimplificationChanges,
 defineOrigName :: String

a_numberOfErrors             = "numberOfErrors"
a_relaxSimplificationChanges = "relaxSimplificationChanges"
defineOrigName               = "RelaxDefineOriginalName"


type Env = [(String, XmlTree)]

-- | Start of a context attribute value 
-- (see also: 'Text.XML.HXT.RelaxNG.Simplification.simplificationStep1')
--
-- The value is always followed by the original attribute name and value

contextAttributes :: String
contextAttributes = "RelaxContext:"


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
    show AnyName = "AnyName"
    show (AnyNameExcept nameClass) 
         = "AnyNameExcept: " ++ show nameClass
    show (Name uri localName) = "{" ++ uri ++ "}" ++ localName
    show (NsName uri) = "{" ++ uri ++ "}AnyName"
    show (NsNameExcept uri nameClass) 
          = "NsNameExcept: {" ++ uri ++ "}" ++ show nameClass
    show (NameClassChoice nameClass1 nameClass2)
         = "NameClassChoice: " ++ show nameClass1 ++ "|" ++ show nameClass2
    show (NCError string) = "NCError: " ++ string


-- | Represents a pattern after simplification

data Pattern = Empty
             | NotAllowed String -- ^ String represents the error message
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
    deriving Show

-- | Possible content types of a Relax NG pattern.
-- (see also chapter 7.2 in Relax NG specification)

data ContentType = CTEmpty
                 | CTComplex
                 | CTSimple
                 | CTNone
     deriving (Show, Eq, Ord)

-- ------------------------------------------------------------
