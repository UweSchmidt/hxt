-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XSLT.Application
   Copyright  : Copyright (C) 2006 Tim Walkenhorst, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Types for compiled stylesheets

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XSLT.CompiledStylesheet
where

import           Text.XML.HXT.XSLT.Common
import           Text.XML.HXT.XSLT.Names

import           Data.Maybe

import           Data.Map               (Map)
import qualified Data.Map as Map hiding (Map)

-- -------------------

-- compiled-Stylesheet:

data CompiledStylesheet =
  CompStylesheet
    [MatchRule]
    (Map ExName NamedRule)
    (Map ExName Variable)
    (Map ExName [AttributeSet])
    [Strips]
    NSAliasing
  deriving Show

getMatchRules :: CompiledStylesheet -> [MatchRule]
getMatchRules (CompStylesheet matchRules _ _ _ _ _) = matchRules

getNamedRules :: CompiledStylesheet -> (Map ExName NamedRule)
getNamedRules (CompStylesheet _ namedRules _ _ _ _) = namedRules

getVariables :: CompiledStylesheet -> (Map ExName Variable)
getVariables (CompStylesheet _ _ variables _ _ _) = variables

getAttributeSets :: CompiledStylesheet -> Map ExName [AttributeSet]
getAttributeSets (CompStylesheet _ _ _ attrSets _ _) = attrSets

getStrips :: CompiledStylesheet -> [Strips]
getStrips (CompStylesheet _ _ _ _ strips _) = strips

getAliases :: CompiledStylesheet -> NSAliasing
getAliases (CompStylesheet _ _ _ _ _ aliases) = aliases

-- -------------------
-- Match-Rules:

data MatchRule =
  MatRule MatchExpr
          Float           -- priority
          (Maybe ExName)  -- mode
          [MatchRule]     -- Imported rules only for xsl:apply-imports
          [Variable]      -- xsl:param list
          Template        -- content
  --deriving Show         -- output of imported Rules makes it unreadable

instance Show MatchRule where
    show (MatRule expr prio mode imprules params content)
        = "MkRule expr: " ++ show expr ++ "\n  prio: " ++ show prio ++ "\n  mode: "++ show mode
          ++ "\n  no. imported rules: " ++ show (length imprules) ++ "\n  xsl-params: " ++ show params
          ++ "\n  content: " ++ show content ++"\n"

getRulePrio :: MatchRule -> Float
getRulePrio (MatRule _ prio _ _ _ _) = prio

getRuleMode :: MatchRule -> Maybe ExName
getRuleMode (MatRule _ _ mode _ _ _) = mode

getRuleImports :: MatchRule -> [MatchRule]
getRuleImports (MatRule _ _ _ imports _ _) = imports

-- -------------------
-- Named-Rules:

data NamedRule = NamRule ExName [Variable] Template
  deriving Show

getRuleName :: NamedRule -> ExName
getRuleName (NamRule name _ _)  = name

-- -------------------
-- Variables

data Variable = MkVar
                  Bool                   -- modus: False => xsl:variable, True => xsl:param
                  ExName                 -- name
                  (Either Expr Template) -- select-expression or result tree fragment
                deriving Show

getVarName :: Variable -> ExName
getVarName (MkVar _ name _) = name

isParam :: Variable -> Bool
isParam (MkVar isP _ _) = isP

-- -------------------
-- Attribute sets:

newtype UsedAttribSets = UsedAttribSets [ExName]
  deriving Show

data AttributeSet = AttribSet ExName UsedAttribSets Template
  deriving Show

-- -------------------
-- Whitespace-stripping

type NTest = ExName

parseNTest :: UriMapping -> String -> NTest
parseNTest = parseExName

type Strips = Map NTest Bool

-- Lookup whether an element of the source document must be stripped.
-- Strip descriptions are ordered by descending import precedence:

lookupStrip :: ExName -> [Strips] -> Bool
lookupStrip name
    = head . (++ [False]) . mapMaybe (lookupStrip1 name)

-- Try to match a qualified name with a set of strip- and preserve-space attributes of the same import precedence:

lookupStrip1 :: ExName -> Strips -> Maybe Bool
lookupStrip1 name spec =
    if      isJust nameMatch then nameMatch
    else if isJust prefMatch then prefMatch
    else if isJust globMatch then globMatch
    else Nothing
  where
    nameMatch = Map.lookup (       name             ) spec
    prefMatch = Map.lookup (ExName "*"  $ exUri name) spec
    globMatch = Map.lookup (ExName "*"  ""          ) spec

feedSpaces :: Bool -> [NTest] -> Strips -> Strips
feedSpaces strip tests =
    Map.unionWithKey feedErr $ Map.fromListWithKey feedErr $ zip tests $ repeat strip
  where
    feedErr k = error $ "Ambiguous strip- or preserve-space rules for " ++ show k

feedStrips, feedPreserves :: [NTest] -> Strips -> Strips
feedStrips    = feedSpaces True
feedPreserves = feedSpaces False

stripDocument :: [Strips] -> XmlTree -> XmlTree
stripDocument strips
    = stripSpaces (\_ n -> lookupStrip (mkExName $ fromJust $ getElemName n) strips) False

stripStylesheet :: XmlTree -> XmlTree
stripStylesheet
    = stripSpaces isStrip True
    where
    isStrip strip' node
        = not (isElemType xsltText node)
          &&
          ( maybe strip' (=="default") $ tryFetchAttribute node xmlSpace )

stripSpaces :: (Bool -> XNode -> Bool) -> Bool -> XmlTree -> XmlTree
stripSpaces f def =
    fromJustErr "stripSpaces (internal error)" . filterTreeCtx step def
  where
    step strip node
     | isElem node           = (f strip node, True)
     | isWhitespaceNode node = (strip       , not strip)
     | otherwise             = (strip       , True)

-- -------------------
-- Namespace aliases and exclusion

-- Map a namespace URI to a new URI tuple,

type NSAliasing = Map String String

addAlias :: UriMapping -> String -> String -> NSAliasing -> NSAliasing
addAlias uris oldPr newPr
    = Map.insertWith (error $ "duplicate mapping for " ++ old) old new
    where
    old = lookupPrefix uris oldPr
    new = lookupPrefix uris newPr

-- lookup an alias in a namespace-mapping.
-- returns the original name, if there is no alias for that name.

lookupAlias :: NSAliasing -> QName -> QName
lookupAlias nsm qn
    = mkQName (namePrefix qn) (localPart qn)
      $ maybe (namespaceUri qn) id
      $ Map.lookup (namespaceUri qn) nsm

aliasUriMapping :: NSAliasing -> UriMapping -> UriMapping
aliasUriMapping nsm = Map.map (\uri -> Map.findWithDefault uri uri nsm)

-- -------------------
-- Templates:

data Template
    = TemplComposite [Template]
    | TemplForEach SelectExpr [SortKey] Template
    | TemplChoose [When]    -- otherwise will be represented by <xsl:when test="true()"/> in the abstract Syntax
    | TemplMessage Bool     -- halt?
                   Template -- content
    | TemplElement ComputedQName
                   UriMapping              -- Namespaces which *must* be added
                   UsedAttribSets          --
                   Template                -- content
    | TemplAttribute ComputedQName
                     Template              -- content
    | TemplText      String
    | TemplValueOf StringExpr  -- select
    | TemplComment Template
    | TemplProcInstr StringExpr       -- name
                     Template               -- content
    | TemplApply (Maybe SelectExpr)
                 (Maybe ExName)  -- mode
                 (Map ExName Variable) -- passed arguments
                 [SortKey]
    | TemplApplyImports
    | TemplVariable Variable
    | TemplCall ExName                 -- name
               (Map ExName Variable) -- passed arguments
    | TemplCopy UsedAttribSets
                Template
    | TemplCopyOf Expr
    deriving Show

data SortKey
    = SortK StringExpr -- select
            StringExpr -- data-type: number or text(default)
            StringExpr -- order: ascending(default) or descending
    deriving Show

data When
    = WhenPart TestExpr Template
    deriving Show

data ComputedQName
    = LiteralQName QName
    | CompQName UriMapping -- namespace-env
                StringExpr -- name
                StringExpr -- namespace
    deriving Show

-- -------------------
-- different kinds of expressions

newtype SelectExpr = SelectExpr Expr   deriving Show
newtype TestExpr   = TestExpr   Expr   deriving Show
newtype StringExpr = StringExpr Expr   deriving Show
newtype MatchExpr  = MatchExpr  Expr   deriving Show
