-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XSLT.Application
   Copyright  : Copyright (C) 2006 Tim Walkenhorst, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Compilation.hs,v 1.6 2006/11/11 15:36:05 hxml Exp $

   The compilation functions for XSLT stylesheets

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XSLT.Compilation 
    ( prepareXSLTDocument        -- :: XmlTree -> XmlTree
    , assembleStylesheet         -- :: XmlTree -> [CompiledStylesheet] -> CompiledStylesheet
    )
where

import Control.Monad

import Data.Maybe
import Data.Either
import Data.List
import qualified Data.Map as Map hiding (Map)
import Data.Map(Map)

import Text.ParserCombinators.Parsec.Prim(runParser)

import Text.XML.HXT.XSLT.Common
import Text.XML.HXT.XSLT.Names
import Text.XML.HXT.XSLT.CompiledStylesheet


-- No deep meaning just a shortcut notation for a *very* common expression...

infixl 9 ><

(><)	:: XmlNode n => (UriMapping -> a ) -> n -> a
f >< node
    = f $ getUriMap node

-- --------------------------

parseExpr :: UriMapping -> String -> Expr
parseExpr uris selectStr
    = either (error.show) id parseResult                               
    where
    parseResult = runParser parseXPath (Map.toList uris) ("select-expr:"++selectStr) selectStr 

parseSelect :: UriMapping -> String -> SelectExpr
parseSelect uris
    = SelectExpr . parseExpr uris

parseTest :: UriMapping -> String -> TestExpr
parseTest uris
    = TestExpr . mkBoolExpr . parseExpr uris

parseStringExpr :: UriMapping -> String -> StringExpr
parseStringExpr uris
    = StringExpr . mkStringExpr . parseExpr uris

parseMatch :: UriMapping -> String -> MatchExpr
parseMatch uris str
    = if isMatchExpr expr
      then MatchExpr expr
      else error $ str ++ " is not a legal match-expression"
    where 
    expr = parseExpr uris str

-- --------------------------

parseAVT :: UriMapping -> String -> StringExpr
parseAVT uris str = 
    StringExpr $ concatExpr $ splitAVT str ""
  where

    splitAVT :: String -> String -> [Expr]
    splitAVT ""           acc = acc2lit acc
    splitAVT ('{':'{':xs) acc = splitAVT xs $ '{':acc
    splitAVT ('}':'}':xs) acc = splitAVT xs $ '}':acc
    splitAVT ('{':xs)     acc = let (body, rest) = span (`notElem` "{}") xs in
                                  if not (null rest) && head rest == '}'
                                    then acc2lit acc ++ parseExpr uris body : splitAVT (tail rest) ""
                                    else error $ "Unterminated expression " ++ xs ++ " in AVT."
    splitAVT ('}':_)      _   = error $ "deserted '}' in AVT."
    splitAVT (x:xs)       acc = splitAVT xs $ x:acc

    acc2lit :: String -> [Expr] 
    acc2lit ""  = []
    acc2lit acc = [mkLiteralExpr $ reverse acc]

-- --------------------------

-- extract ComputedQName from "name" and "namespace" AVTs of an xsl:element- or xsl-attribute-node
compileComputedQName :: XmlTree -> ComputedQName
compileComputedQName node =
    (CompQName><node) nameAVT nsAVT
  where 
    nameAVT  = parseAVT><node $ fetchAttribute node xsltName
    nsAVT    = parseAVT><node $ fetchAttributeWDefault node xsltNamespace ""

-- --------------------------

compileComposite :: [XmlTree] -> Template
compileComposite = TemplComposite . map (compileTemplate . return)

compileMessage :: XmlTree -> Template
compileMessage node = TemplMessage halt content
  where halt     = termAttr == "yes"
        termAttr = fetchAttributeWDefault node xsltTerminate "no"
        content  = compileTemplate (getChildren node)

compileForEach :: XmlTree -> Template
compileForEach node = TemplForEach expr sorting template
  where expr       = parseSelect><node $ fetchAttribute node xsltSelect
        sorting    = map compileSortKey srt
        template   = compileTemplate cnt
        (srt, cnt) = partition (isElemType xsltSort) $ getChildren node

compileChoose :: XmlTree -> Template
compileChoose node = TemplChoose whenParts
  where whenParts  = map compl children
        children   = filter isElem (getChildren node)
        compl node' = let elemName = fromJust $ getElemName node' in
                        if      equivQName elemName xsltWhen      then compileWhen node'
                        else if equivQName elemName xsltOtherwise then compileOtherwise node'
                        else error ("No elements of type " ++ show elemName ++ " allowed within xsl-choose template!")

compileWhen :: XmlTree -> When
compileWhen node = WhenPart expr $ compileTemplate $ getChildren node
  where expr     = parseTest><node $ fetchAttribute node xsltTest

-- Otherwise is treated as a when-Part with node-test "true()"
compileOtherwise :: XmlTree -> When
compileOtherwise node = WhenPart (TestExpr mkTrueExpr) $ compileTemplate $ getChildren node

-- "if" is treated as a convenience-form of choose with exactly one "when"-Part
compileIf :: XmlTree -> Template
compileIf = TemplChoose . return . compileWhen

-- -----------------------------------

parseExNames :: UriMapping -> String -> [ExName]
parseExNames urm = map (parseExName urm) . words

compileElement :: XmlTree -> Template
compileElement node = 
    TemplElement compQName Map.empty attribSets template
  where 
    compQName   = compileComputedQName node
    attribSets  = UsedAttribSets $ parseExNames><node
                  $ fetchAttributeWDefault node xsltUseAttributeSets ""
    template    = compileTemplate (getChildren node)

compileAttribute :: XmlTree -> Template
compileAttribute node = 
    TemplAttribute (compileComputedQName node) $ compileTemplate (getChildren node)

-- compiles xsl:text
compileText :: XmlTree -> Template
compileText = TemplText . collectTextnodes . getChildren

-- compiles textNode 
compileTextnode :: XmlTree -> Template
compileTextnode = TemplText . fromJust . getText

compileValueOf :: XmlTree -> Template
compileValueOf node = 
    TemplValueOf $ parseStringExpr><node $ fetchAttribute node xsltSelect

compileComment :: XmlTree -> Template
compileComment = TemplComment . compileTemplate . getChildren

compileProcInstr :: XmlTree -> Template
compileProcInstr node = 
   TemplProcInstr name content
  where
    name    = parseAVT><node  $ fetchAttribute node xsltName
    content = compileTemplate $ getChildren node

-- -----------------------------------

compileLiteralResultElement :: XmlTree -> Template
compileLiteralResultElement node =
    TemplElement compQName nsUris attribSets content
  where 
    nsUris             = extractAddUris node
    compQName          = LiteralQName   $ fromJust $ getElemName node
    attribSets         = UsedAttribSets $ parseExNames><node $ attrSetsStr
    attrSetsStr        = fetchAttributeWDefault node xsltUseAttributeSetsLRE ""
    content            = TemplComposite $ attributes ++ [template]
    attributes         = mapMaybe (compileLREAttribute><node) $ fromJust $ getAttrl node
    template           = compileTemplate (getChildren node)

compileLREAttribute :: UriMapping -> XmlTree -> Maybe Template
compileLREAttribute uris node = 
    if isSpecial 
      then Nothing
      else Just $ TemplAttribute (LiteralQName name) val  
  where 
    isSpecial = namespaceUri name `elem` [xsltUri, xmlnsNamespace]
    name      = fromJust $ getAttrName node
    val       = TemplValueOf $ parseAVT uris $ collectTextnodes $ getChildren node

-- -----------------------------------

compileApplyTempl :: XmlTree -> Template
compileApplyTempl node =
    TemplApply expr mode args sorting
  where
    expr    = liftM (parseSelect><node) $ tryFetchAttribute node xsltSelect
    mode    = liftM (parseExName><node) $ tryFetchAttribute node xsltMode
    args    = compileVariables          $ filter (isElemType xsltWithParam) $ par
    sorting =  map compileSortKey srt
    (srt,par) =  partition (isElemType xsltSort) $ getChildren node

compileApplyImports :: XmlTree -> Template
compileApplyImports _node
    = TemplApplyImports

compileCallTempl :: XmlTree -> Template
compileCallTempl node =
    TemplCall name args
  where
    name = parseExName><node $ fetchAttribute node xsltName
    args = compileVariables  $ filter (isElemType xsltWithParam) $ getChildren node

compileTemplVariable :: XmlTree -> Template
compileTemplVariable = TemplVariable . compileVariable

-- -----------------------------------

compileCopy :: XmlTree -> Template
compileCopy node =
    TemplCopy attribSets $ compileTemplate (getChildren node)
  where
    attribSets  = UsedAttribSets $ parseExNames><node $ fetchAttributeWDefault node xsltUseAttributeSets ""

compileCopyOf :: XmlTree -> Template
compileCopyOf node = TemplCopyOf $ parseExpr><node $ fetchAttribute node xsltSelect

-- -----------------------------------

compileTemplate :: [XmlTree] -> Template
compileTemplate [node]       = 
   if isElem node
   then let elemName = fromJust $ getElemName node in        
        if      equivQName elemName xsltMessage        then compileMessage       node
        else if equivQName elemName xsltForEach        then compileForEach       node
        else if equivQName elemName xsltChoose         then compileChoose        node   
        else if equivQName elemName xsltIf             then compileIf            node
        else if equivQName elemName xsltElement        then compileElement       node
        else if equivQName elemName xsltAttribute      then compileAttribute     node
        else if equivQName elemName xsltText           then compileText          node
        else if equivQName elemName xsltValueOf        then compileValueOf       node
        else if equivQName elemName xsltComment        then compileComment       node
        else if equivQName elemName xsltProcInstr      then compileProcInstr     node
        else if equivQName elemName xsltApplyTemplates then compileApplyTempl    node
        else if equivQName elemName xsltApplyImports   then compileApplyImports  node
        else if equivQName elemName xsltCallTemplate   then compileCallTempl     node
        else if equivQName elemName xsltVariable       then compileTemplVariable node
        else if equivQName elemName xsltCopy           then compileCopy          node
        else if equivQName elemName xsltCopyOf         then compileCopyOf        node

        -- no other xslt elements allowed here:
        else if namespaceUri elemName == xsltUri
        then error $ "xslt-element " ++ localPart elemName ++ " not allowed within this context."

        -- for now all other elements will be considered as Literal Result Elements
        else compileLiteralResultElement node

   else if isText node then compileTextnode node

   else
       error $ "Unsupported node-type in xslt sheet: " ++ show (getNode node)
compileTemplate list = compileComposite list


-- -----------------------------------
-- Assembling of the entire stylesheet

assembleStylesheet :: XmlTree -> [CompiledStylesheet] -> CompiledStylesheet
assembleStylesheet xslNode imports =
    CompStylesheet matchRules namedRules variables attsets strips aliases
  where 
    -- entire contents:
    (namedRules,    
     matchRules)          = assembleRules ruleElems importedMatchRules importedNamedRules
    variables             = assembleVariables varElems importedVariables
    attsets               = assembleAttrSets attsetElems importedAttribSets
    strips                = assembleStrips stripElems preserveElems importedStrips
    aliases               = assembleAliases nsAliasElems importedAliases

    -- element content:
    (nsAliasElems, _r5)   = partition (isElemType xsltNamespaceAlias) r4
    (ruleElems, r4)       = partition (isElemType xsltTemplate) r3
    (varElems, r3)        = partition (\node -> isElemType xsltVariable node || isElemType xsltParam node) r2
    (attsetElems, r2)     = partition (isElemType xsltAttributeSet) r1
    (preserveElems, r1)   = partition (isElemType xsltPreserveSpace) r0
    (stripElems, r0)      = partition (isElemType xsltStripSpace) $ getChildren xslNode

    -- imported stuff:
    importedAttribSets    = map getAttributeSets imports
    importedVariables     = map getVariables revImports
    importedNamedRules    = map getNamedRules revImports
    importedMatchRules    = concatMap getMatchRules revImports
    importedStrips        = concatMap getStrips revImports
    importedAliases       = map getAliases revImports
    revImports            = reverse imports

assembleRules :: [XmlTree] -> [MatchRule] -> [Map ExName NamedRule] -> (Map ExName NamedRule, [MatchRule])
assembleRules nodes importedMatches importedProcs =
    (resProcs, resMatches)
  where
  
  -- matches:  
    resMatches       = localMatches ++ importedMatches
    localMatches     = reverse $ sortBy cmp matches
    cmp rulA rulB    = compare (getRulePrio rulA) (getRulePrio rulB)

  -- procedures:
    resProcs         = Map.unions (localProcs:importedProcs)
    localProcs       = foldl ins Map.empty procs
    ins map' rule    = Map.insertWith (error $ "named-rule "++ show (getRuleName rule) ++" is already defined on this level")
                                     (getRuleName rule) rule map'

  -- compile all xsl:template's:
    (procs, matches) = catMaybes *** concat $ unzip $ map (compileRule importedMatches) nodes

assembleVariables :: [XmlTree] -> [(Map ExName Variable)] -> (Map ExName Variable)       
assembleVariables varElems = Map.unions . (compileVariables varElems:)

assembleAttrSets :: [XmlTree] -> [Map ExName [AttributeSet]] -> Map ExName [AttributeSet]
assembleAttrSets attsetElems =
    foldr (Map.unionWith (++)) localAttribSets
  where
    localAttribSets       = foldr insertAs Map.empty
                            $ map compileAttributeSet attsetElems
    insertAs as@(AttribSet name _ _) = Map.insertWith (++) name [as]

assembleStrips :: [XmlTree] -> [XmlTree]-> [Strips] -> [Strips]
assembleStrips stripElems preserveElems =
    (localStrips :)
  where
    localStrips = feedStrips (concatMap compileStrips stripElems)
                  $ feedPreserves (concatMap compilePreserves preserveElems)
                  $ Map.empty

assembleAliases :: [XmlTree] -> [NSAliasing] -> NSAliasing
assembleAliases nsAliasElems =
    Map.unions . (localAliases:)
  where
    localAliases          = foldr addAlias' Map.empty nsAliasElems
    addAlias' node        = uncurry (addAlias><node) $ compileAlias node

-- -----------------------------------
--

{- not longer in use, all functionality within the IO monad moved to XsltArrows:

isStylesheetElem :: XmlTree -> Bool
isStylesheetElem node = 
  (isElemType xsltTransform node || isElemType xsltStylesheet node) && hasAttribute node xsltVersion

isLREstylesheet :: XmlTree -> Bool
isLREstylesheet node  = hasAttribute node xsltVersionLRE

lre2template :: XmlTree -> XmlTree
lre2template = mkElement xsltTemplate [mkAttr xsltMatch [mkText "/"]] . return

lre2stylesheet :: XmlTree -> XmlTree
lre2stylesheet = mkElement xsltTransform [] . return . lre2template

-- -----------------------------------
-- Stylesheet compilation in the IO Monad:

compileStylesheetFromUri :: String -> IO CompiledStylesheet
compileStylesheetFromUri = compileStylesheetFromUriWIncStk []

compileStylesheetFromUriWIncStk :: [String] -> String -> IO CompiledStylesheet
compileStylesheetFromUriWIncStk incstack uri = readStylesheetWIncStk incstack uri >>= compileStylesheetWIncStk (uri:incstack)

readStylesheetWIncStk :: [String] -> String -> IO XmlTree
readStylesheetWIncStk incstack uri = 
  if uri `elem` incstack
  then error $ "Error: " ++ uri ++ " is recursively imported/included." 
            ++ concatMap ("\n  imported/included from: " ++) incstack
  else readDocumentIO [(a_preserve_comment, "0")] uri >>= return . prepareXSLTDocument

compileStylesheet ::  XmlTree -> IO CompiledStylesheet
compileStylesheet = compileStylesheetWIncStk [] . prepareXSLTDocument

compileStylesheetWIncStk :: [String] -> XmlTree -> IO CompiledStylesheet
compileStylesheetWIncStk incstack node = 
  
  -- ======= 1: simplified syntax
  if isLREstylesheet xslNode then
    return $ assembleStylesheet (lre2stylesheet xslNode) []

  -- ======= 2: regular syntax
  else if isStylesheetElem xslNode then 
    do
      -- ======= 2.1: gather included stylesheets
      expandedContent <- expandIncludes incstack content

      -- ======= 2.2: compile imported stylesheets
      (imps, rest) <- return $ partition (isElemType xsltImport) expandedContent
      imports      <- mapM (compileStylesheetFromUriWIncStk incstack . getHRef) $ imps

      -- ======= 2.3: compile content
      expandedStylesheet <- return $ setChildren rest xslNode
      return $ assembleStylesheet expandedStylesheet imports
     
  -- ======= 3: unknown document type: 
  else error "Expected: Either xsl:stylesheet/xsl:transform or simplified syntax"

  where 
    content     = getChildren xslNode
    (xslNode:_) = filter isElem $ getChildren $ node
    getHRef     = flip fetchAttribute xsltHRef


expandIncludes :: [String] -> [XmlTree] -> IO [XmlTree]
expandIncludes incstack = liftM concat . mapM (expandInclude incstack) . filter isElem

expandInclude :: [String] -> XmlTree -> IO [XmlTree]
expandInclude incstack node = 
  if isElemType xsltInclude node
    then 
    do
       -- ======= read include-stylesheet and extract stylesheet node
       href        <- return $ fetchAttribute node xsltHRef
       docNode     <- readStylesheetWIncStk incstack href
       (xslNode:_) <- return $ filter isElem $ getChildren docNode

       -- ======= check for simplified syntax
       if isLREstylesheet xslNode
         then return [lre2template xslNode]

       -- ======= check for xsl:stylesheet or xsl:transform
         else if isStylesheetElem xslNode 
                 then expandIncludes (href:incstack) $ getChildren xslNode

       -- ======= include file has unknown type
                 else error $ "Error: Included file " ++ href ++ " is not a stylesheet"
    else return [node]
xxx -}

-- -----------------------------------

-- compile a rule from an xsl:template element.
-- The resulting rule can either be a one or more match rule or a named rule, or both.
-- The first argument is needed for xsl:apply-imports and contains
-- a list of all subsequently imported rules
compileRule :: [MatchRule] -> XmlTree -> (Maybe NamedRule, [MatchRule])
compileRule imports node =

    if isNothing match && isNothing name 
    then error "Error: Bogus rule (xsl:template) with neither match nor name attribute is illegal"

    else if isJust mode && isNothing match 
    then error "Error: Bogus mode attribute on none-match rule is illegal"

    else if isJust priority && isNothing match 
    then error "Error: Bogus priority attribute on none-match rule is illegal"

    else 
      (
        liftM (\n -> NamRule n params template) name
      , concat $ maybeToList $ liftM (assembleMatchRule priority mode imports params template) match
      ) 

  where
    match      = liftM (parseMatch><node)  $ tryFetchAttribute node xsltMatch
    name       = liftM (parseExName><node) $ tryFetchAttribute node xsltName
    priority   = liftM read                $ tryFetchAttribute node xsltPriority
    mode       = liftM (parseExName><node) $ tryFetchAttribute node xsltMode
    template   = compileTemplate content
    params     = map compileVariable paramsXml
    (paramsXml, content) =
                  partition (isElemType xsltParam) $ getChildren node

assembleMatchRule :: Maybe Float -> Maybe ExName -> [MatchRule] -> [Variable] -> Template ->  MatchExpr -> [MatchRule]
assembleMatchRule pri m imp par tmpl mtch@(MatchExpr expr) =
    if isJust pri
    then return $ MatRule mtch (fromJust pri) m imp par tmpl
    else map expand $ splitMatchByPrio expr
  where 
    expand (pri', mtch') = MatRule (MatchExpr mtch') pri' m imp par tmpl

-- -----------------------------------

compileVariables :: [XmlTree] -> Map ExName Variable
compileVariables nodes =
    foldl insertVar Map.empty $ varList
  where
    varList               = map compileVariable $ nodes
    insertVar map' var    = Map.insertWith (error $ "parameter or variable "++ show (getVarName var) ++" is already defined on this level")
                                           (getVarName var) var map'

compileVariable :: XmlTree -> Variable
compileVariable node =
    MkVar modus name expr
  where 
    modus   = isElemType xsltParam node
    name    = parseExName><node $ fetchAttribute node xsltName
    expr    = parseExpr><node   $ fetchAttributeWDefault node xsltSelect "''"


-- -----------------------------------

compileAttributeSet :: XmlTree -> AttributeSet
compileAttributeSet node =
    AttribSet name usedsets template
  where
    name     = parseExName><node $ fetchAttribute node xsltName
    usedsets = UsedAttribSets    $ parseExNames><node $ fetchAttributeWDefault node xsltUseAttributeSets ""
    template = compileTemplate   $ filter (isElemType xsltAttribute) $ getChildren node

-- -----------------------------------

compileSortKey :: XmlTree -> SortKey
compileSortKey node =
    SortK expr dataType order
  where
    expr     = parseStringExpr><node $ fetchAttributeWDefault node xsltSelect "."
    dataType = parseAVT><node        $ fetchAttributeWDefault node xsltDataType "text"
    order    = parseAVT><node        $ fetchAttributeWDefault node xsltOrder "ascending"

-- -----------------------------------

parseNTests :: UriMapping -> String -> [NTest]
parseNTests uris = map (parseNTest uris) . words

compileStrips,compilePreserves :: XmlTree -> [NTest]
compileStrips node = parseNTests><node $ fetchAttribute node xsltElements
compilePreserves = compileStrips

-- -----------------------------------

compileAlias :: XmlTree -> (String, String)
compileAlias node = 
  (fetchAttribute node xsltStylesheetPrefix, fetchAttribute node xsltResultPrefix)

-- -----------------------------------
-- Document level preprocessing

prepareXSLTDocument	:: XmlTree -> XmlTree
prepareXSLTDocument	= expandExEx . expandNSDecls . stripStylesheet . removePiCmt

removePiCmt :: XmlTree -> XmlTree
removePiCmt = fromJustErr "XSLT: No root element" . filterTree (\n -> not (isPi n) && not (isCmt n))

-- Expand exclude-result-prefixes AND extension-element-prefixes

expandExEx :: XmlTree -> XmlTree
expandExEx = mapTreeCtx expandExExElem ([xsltUri,xmlNamespace,xmlnsNamespace],[])

expandExExElem :: ([String], [String]) -> XNode -> (([String], [String]), XNode)
expandExExElem c@(excl, ext) node
  | isElem node = ((exclAcc, extAcc), nodeNew)
  | otherwise   = (c, node)
  where
    nodeNew    = setAttribute nameExcl (unwords exclAcc) $ setAttribute nameExt (unwords extAcc) node
    exclAcc    = exclNew ++ excl
    extAcc     = extNew  ++ ext
    exclNew    = extNew  ++ (parsePreList><node $ fetchAttributeWDefault node nameExcl "")
    extNew     =             parsePreList><node $ fetchAttributeWDefault node nameExt  ""
    (nameExcl,
     nameExt)  = if (namespaceUri $ fromJust $ getElemName node) == xsltUri
                   then (xsltExlcudeResultPrefixes   , xsltExtensionElementPrefixes   )
                   else (xsltExlcudeResultPrefixesLRE, xsltExtensionElementPrefixesLRE)

-- parse a prefix list, create a list of uris: 
-- "pre1 pre2 pre3" -> ["pre1.uri","pre2.uri","pre3.uri"]

parsePreList :: UriMapping -> String -> [String]
parsePreList uris = map (lookupPrefix uris) . words

-- -----------------------------------
-- Extraction of contextual Information from an XML-Node

extractAddUris :: XmlTree -> UriMapping
extractAddUris node = 
    (Map.filter (`notElem` exclUris))><node
  where
    exclUris = words $ fetchAttributeWDefault node xsltExlcudeResultPrefixesLRE ""

-- -----------------------------------
