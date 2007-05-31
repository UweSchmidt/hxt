-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XSLT.Application
   Copyright  : Copyright (C) 2006 Tim Walkenhorst, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: Application.hs,v 1.7 2007/05/02 06:41:05 hxml Exp $

   The transformation functions for XSLT transformation of XML documents

   Exports only two pure functions 'applyStylesheet' and 'applyStylesheetWParams'

-}

-- ------------------------------------------------------------

module Text.XML.HXT.XSLT.Application 
    ( applyStylesheet        -- CompiledStylesheet -> XmlTree -> [XmlTree]
    , applyStylesheetWParams -- Map ExName Expr -> CompiledStylesheet -> XmlTree -> [XmlTree]
    , XPathParams
    )
where

import Text.XML.HXT.XSLT.Common
import Text.XML.HXT.XSLT.CompiledStylesheet

import Data.Maybe
import Data.Either
import Data.List

import Data.Map(Map)
import qualified Data.Map as Map hiding (Map)

import Data.Char

-- ------------------------------------------------------------

type XPathParams = Map ExName Expr

type VariableSet = Map ExName XPathValue

type ParamSet = VariableSet

data Context = Ctx NavXmlTree               -- current node
                   [NavXmlTree]             -- current node list 
                   Int                      -- pos. of curr-node 1..length
                   Int                      -- length of node list
                   VariableSet              -- glob. Var
                   VariableSet              -- loc. Var
                   CompiledStylesheet       -- The stylesheet which is being applied
                   (Maybe MatchRule)        -- Just the last applied match rule, Nothing within xsl:for-each
                   Int                      -- recursion depth, needed for the creation of rtf-ids
               | CtxEmpty        -- The empty-context, indicates that a branch of a transformation has been finished

ctxGetNode :: Context -> NavXmlTree
ctxGetNode CtxEmpty = error "ctxGetNode: Internal error attempt to access the empty context"
ctxGetNode (Ctx node _ _ _ _ _ _ _ _) = node

ctxGetStylesheet :: Context -> CompiledStylesheet
ctxGetStylesheet CtxEmpty = error "ctxGetStylesheet: Internal error attempt to access the empty context"
ctxGetStylesheet (Ctx _ _ _ _ _ _ stylesheet _ _) = stylesheet

ctxGetRule :: Context -> Maybe MatchRule
ctxGetRule CtxEmpty = Nothing
ctxGetRule (Ctx _ _ _ _ _ _ _ rule _) = rule

ctxSetNodes :: [NavXmlTree] -> Context -> Context
ctxSetNodes _ CtxEmpty = error "ctxSetNodes: Internal error attempt to access the empty context"
ctxSetNodes [] _       = CtxEmpty
ctxSetNodes nodes (Ctx _ _ _ _ globVars locVars cs rl rd) =
  Ctx (head nodes) nodes 1 (length nodes) globVars locVars cs rl rd

ctxSetRule :: Maybe MatchRule -> Context -> Context
ctxSetRule _ CtxEmpty = error "ctxSetRule: Internal error attempt to access the empty context"
ctxSetRule rule (Ctx node nodes pos len globVars locVars cs _ rd) =
  Ctx node nodes pos len globVars locVars cs rule rd

addVariableBinding :: ExName -> XPathValue -> Context -> Context
addVariableBinding name val (Ctx node nodes pos len globVars locVars cs rl rd)
    = Ctx node nodes pos len globVars locVarsNew cs rl rd
    where
    locVarsNew = Map.insertWith (errF) name val locVars
    errF       = error $ "Local variable or parameter " ++ show name ++ " is already bound in this context"

addVariableBinding _ _ CtxEmpty = CtxEmpty

clearLocalVariables :: Context -> Context
clearLocalVariables CtxEmpty = CtxEmpty
clearLocalVariables (Ctx node nodes pos len globVars _ cs rl rd)
    = (Ctx node nodes pos len globVars Map.empty cs rl rd)

processContext :: Context -> (Context->[XmlTree]) -> [XmlTree]
processContext CtxEmpty _f = []
processContext ctx@(Ctx _node nodeList pos len globVar locVar cs rl rd) f
    | pos > len
	= []
    | otherwise
	= f ctx ++ processContext (Ctx (nodeList!!pos) nodeList (pos+1) len globVar locVar cs rl rd) f
                 
incRecDepth :: Context -> Context
incRecDepth CtxEmpty = CtxEmpty
incRecDepth (Ctx n nl p l gl lc cs rl rd) = Ctx n nl p l gl lc cs rl (rd+1)

recDepth :: Context -> Int
recDepth (Ctx _ _ _ _ _ _ _ _ rd) = rd
recDepth CtxEmpty = 0
                 
-- ----------------

evalXPathExpr :: Expr -> Context -> XPathValue
evalXPathExpr expr (Ctx node _ pos len globVars locVars _ _ _)
    = filterXPath $ evalExpr (vars,[]) (pos, len, node) expr (XPVNode [node])
    where 
    filterXPath (XPVError err)    = error err
    filterXPath (XPVNode nodes)   = XPVNode $ (\x -> fst x ++ snd x) $ partition (isAttr.subtreeNT) nodes
    -- line above: complicated issue: consider: <lre><xsl:copy-of select="a/@*|a/*|b/@*"/></lre>
    -- assume a is in document order before b. Shall b's attributes be added to lre or be ignored?!
    filterXPath xpv               = xpv
    vars                          = map (\(name, val) -> ((exUri name, exLocal name), val)) varList
    varList                       = Map.toAscList $ locVars `Map.union` globVars

evalXPathExpr _ CtxEmpty
    = error "internal error in evalXPathExpr in XSLT module"

evalRtf :: Template -> String -> Context -> XPathValue
evalRtf template rtfId ctx = XPVNode [ntree rtfRoot]
  where
    rtfRoot = setAttribute rootIdName ("rtf " ++ rtfId) $ mkRootTree [] $ applyTemplate template ctx
    rootIdName = mkQName "" "rootId" ""

applySelect :: SelectExpr -> Context -> [NavXmlTree]
applySelect (SelectExpr expr) ctx = 
    extractNodes xpathResult
  where 
    xpathResult                  = evalXPathExpr expr ctx
    extractNodes (XPVNode nodes) = nodes
    extractNodes r               = error $ "XPATH-Expression in select or match attribute returned a value of the wrong type ("              
                                           ++ take 15 (show r)  ++ "...)"

applyTest :: TestExpr -> Context -> Bool
applyTest (TestExpr expr) ctx = bool
  where (XPVBool bool) = evalXPathExpr expr ctx

applyStringExpr :: StringExpr -> Context -> String
applyStringExpr (StringExpr expr) ctx = string
  where (XPVString string) = evalXPathExpr expr ctx

applyMatch :: MatchExpr -> Context -> Bool
applyMatch (MatchExpr expr) ctx
    = matchBySelect (SelectExpr expr) (ctxGetNode ctx) ctx
    where
    matchBySelect :: SelectExpr -> NavXmlTree -> Context -> Bool
    matchBySelect _ _ CtxEmpty = False
    matchBySelect expr' matchNode ctx'  
	| matchNode `isNotInNodeList` applySelect expr' ctx'
	    = matchBySelect expr' matchNode $ ctxSetNodes (maybeToList $ upNT $ ctxGetNode ctx') ctx'
        | otherwise
	    = True   

-- ------------------------------------

applyComputedQName :: ComputedQName -> Context -> QName

applyComputedQName (LiteralQName qName) ctx = 
    lookupAlias (getAliases $ ctxGetStylesheet ctx) qName

applyComputedQName (CompQName uris nameATV nsATV) ctx =
    if null nsuri && not (null pref)
    then mkQName pref loc $ lookupPrefix uris pref
    else mkQName pref loc nsuri
  where       
    nsuri         = applyStringExpr nsATV ctx   
    (pref, loc)   = if null loc' then ("", pref') 
                                 else (pref', tail loc')
    (pref', loc') = span (/=':') $ applyStringExpr nameATV ctx

-- ------------------------------------
 
applyComposite :: Template -> Context -> [XmlTree]
applyComposite (TemplComposite templates) ctx
    = concat $ reverse $ fst $ foldl applyElem ([], ctx) templates
    where 
    applyElem :: ([[XmlTree]], Context) -> Template -> ([[XmlTree]], Context)
    applyElem (nodes, ctx') (TemplVariable v) = (nodes, processLocalVariable v Map.empty ctx')
    applyElem (nodes, ctx') t                 = (applyTemplate t ctx' : nodes, ctx')

applyComposite _ _ = []


applyForEach :: Template -> Context -> [XmlTree]
applyForEach (TemplForEach expr sorting template) ctx
    = processContext sortedCtx $ applyTemplate template
    where 
    sortedCtx = applySorting sorting ctxWOrule nodes
    ctxWOrule = ctxSetRule Nothing $ ctx
    nodes     = applySelect expr ctx

applyForEach _ _ = []

         
applyChoose :: Template -> Context -> [XmlTree]
applyChoose (TemplChoose whenList) ctx
    = applyWhenList whenList ctx

applyChoose _ _ = []


applyWhenList :: [When] -> Context -> [XmlTree]
applyWhenList []  _ = []
applyWhenList ((WhenPart expr template):xs) ctx
    | applyTest expr ctx
	= applyTemplate template ctx
    | otherwise
	= applyWhenList xs ctx

applyMessage :: Template -> Context -> [XmlTree]
applyMessage (TemplMessage halt template) ctx
    | halt
	= error $ "Message(fatal): " ++ msg
    | otherwise
	= []	-- trace ("Message(trace): " ++ msg) []
    where
    msg     = xshow content
    content = applyTemplate template ctx

applyMessage _ _ = []

-- ------------------------------------

applyElement :: Template -> Context -> [XmlTree]
applyElement (TemplElement compQName uris attribSets template) ctx =
    return $ createElement name uris aliases fullcontent
  where 
    aliases     = getAliases $ ctxGetStylesheet ctx
    name        = applyComputedQName compQName ctx
    fullcontent = applyAttribSets [] attribSets ctx ++ applyTemplate template ctx

applyElement _ _ = []


-- create an element from a list of attributes followed by content

createElement :: QName -> UriMapping -> NSAliasing -> [XmlTree] -> XmlTree
createElement name uris aliases fullcontent =
    mkElement name (nsAttrs ++ distinctAttribs) content
  where 
    nsAttrs            = uriMap2Attrs $ aliasUriMapping aliases uris
    distinctAttribs    = nubBy eqAttr $ reverse attribs 
    (attribs, content) = span (isAttr) fullcontent
    eqAttr node1 node2 = equivQName (fromJust $ getAttrName node1) (fromJust $ getAttrName node2)  


applyAttribute :: Template -> Context -> [XmlTree]
applyAttribute (TemplAttribute compQName template) ctx =
    return $ mkAttr qName content
  where 
    qName   = applyComputedQName compQName ctx
    content = applyTemplate template ctx

applyAttribute _ _ = []

  
applyText :: Template -> Context -> [XmlTree]
applyText (TemplText s) _
    = [mkText s]

applyText _ _ = []


applyValueOf :: Template -> Context -> [XmlTree]
applyValueOf (TemplValueOf expr) ctx
    = [mkText $ applyStringExpr expr ctx]

applyValueOf _ _ = []


applyComment :: Template -> Context -> [XmlTree]
applyComment (TemplComment content) ctx = 
    return $ mkCmt $ format $ collectTextnodes $ applyTemplate content ctx
  where
    format ""           = ""               -- could probably move to hxt...?
    format "-"          = "- "
    format ('-':'-':xs) = '-':' ':format ('-':xs)
    format (x:xs)       = x:format xs

applyComment _ _ = []

applyProcInstr :: Template -> Context -> [XmlTree]
applyProcInstr (TemplProcInstr nameExpr template) ctx =
    return $ mkXPiTree name $ format $ collectTextnodes $ applyTemplate template ctx
  where
    name      = applyStringExpr nameExpr ctx      
    format ""           = ""                       -- In a better Haskell: format = replaceAll "?>" "? >"
    format ('?':'>':xs) = '?':' ':'>':format xs    -- could probably move to hxt...?
    format (x:xs)       = x:format xs

applyProcInstr _ _ = []

-- ------------------------------------

applyApplTempl :: Template -> Context -> [XmlTree]
applyApplTempl (TemplApply expr mode args sorting) ctx =
    applyMatchRulesToEntireContext params rules mode sortedCtx
  where 
    params      = createParamSet args ctx
    sortedCtx   = applySorting sorting ctx nodes
    nodes       = maybe (getChildrenNT $ ctxGetNode ctx)
                        (flip applySelect ctx)
                        expr
    rules       = getMatchRules $ ctxGetStylesheet ctx

applyApplTempl _ _ = []


applyImports :: Template -> Context -> [XmlTree]
applyImports (TemplApplyImports) ctx= 
    applyMatchRules Map.empty rules mode ctx
  where
    rules    = getRuleImports currRule
    mode     = getRuleMode currRule
    currRule = maybe (error "apply-imports must not be called during xsl:for-each") id $ ctxGetRule ctx

applyImports _ _ = []


applyCallTempl  :: Template -> Context -> [XmlTree]
applyCallTempl (TemplCall name args) ctx =
    instantiateNamedRule params rule ctx
  where
    params      = createParamSet args ctx
    rule        = maybe errNoRule id $ Map.lookup name rules
    rules       = getNamedRules $ ctxGetStylesheet ctx
    errNoRule   = error $ "No rule with qualified name: " ++ show name

applyCallTempl _ _ = []

-- ------------------------------------

applyCopy :: Template -> Context -> [XmlTree]
applyCopy (TemplCopy attrsets template) ctx
    | isRoot currNode			-- Case 1: Root node => just use the content template
	= applyTemplate template ctx
    | isElem currNode			-- Case 2: Any other element-node
	= return $ createElement name (getUriMap currNode) Map.empty fullcontent
    | otherwise				-- Just return the current node as result
	= return currNode        
  where
    currNode    = subtreeNT $ ctxGetNode ctx
    name        = fromJust $ getElemName currNode
    fullcontent = applyAttribSets [] attrsets ctx ++ applyTemplate template ctx

applyCopy _ _ = []

applyCopyOf :: Template -> Context -> [XmlTree]
applyCopyOf (TemplCopyOf expr)
    = concatMap expandRoot . xPValue2XmlTrees . evalXPathExpr expr
    where
    expandRoot node
	| isRoot node	= getChildren node
        | otherwise	= return node

applyCopyOf _ = const []

-- ------------------------------------

applyTemplate :: Template -> Context -> [XmlTree]
applyTemplate t@(TemplComposite _)     = applyComposite t
applyTemplate t@(TemplMessage _ _)     = applyMessage t
applyTemplate t@(TemplForEach _ _ _)   = applyForEach t
applyTemplate t@(TemplChoose _)        = applyChoose t
applyTemplate t@(TemplElement _ _ _ _) = applyElement t
applyTemplate t@(TemplAttribute _ _)   = applyAttribute t
applyTemplate t@(TemplText _)          = applyText t
applyTemplate t@(TemplValueOf _)       = applyValueOf t
applyTemplate t@(TemplComment _)       = applyComment t
applyTemplate t@(TemplProcInstr _ _)   = applyProcInstr t
applyTemplate t@(TemplApply _ _ _ _)   = applyApplTempl t
applyTemplate t@(TemplApplyImports)    = applyImports t
applyTemplate t@(TemplCall _ _)        = applyCallTempl t
applyTemplate t@(TemplCopy _ _)        = applyCopy t
applyTemplate t@(TemplCopyOf _)        = applyCopyOf t
applyTemplate   (TemplVariable _)      = const []	-- trace ("Warning: Unreacheable variable: " ++ show (getVarName v)) const []

-- ------------------------------------
-- "Main" :

applyStylesheetWParams :: XPathParams -> CompiledStylesheet -> XmlTree -> [XmlTree]
applyStylesheetWParams inputParams cs@(CompStylesheet matchRules _ vars _ strips _) rawDoc = 
    map fixupNS $ applyMatchRules Map.empty matchRules Nothing ctxRoot
  where
    ctxRoot   = Ctx docNode [docNode] 1 1 gloVars Map.empty cs Nothing 0
    gloVars   = Map.map (evalVariableWParamSet extParams ctxRoot) vars
    extParams = Map.map (flip evalXPathExpr ctxRoot) inputParams
    docNode   = ntree $ expandNSDecls $ stripDocument strips rawDoc

applyStylesheet :: CompiledStylesheet -> XmlTree -> [XmlTree]
applyStylesheet  = applyStylesheetWParams Map.empty

-- ------------------------------------
-- calling named- and applying match-rules

applyMatchRulesToChildren :: ParamSet -> [MatchRule] -> (Maybe ExName) -> Context -> [XmlTree]
applyMatchRulesToChildren args rules mode ctx = 
    applyMatchRulesToEntireContext args rules mode childCtx
  where
    childCtx = ctxSetNodes (getChildrenNT $ ctxGetNode ctx) ctx

applyMatchRulesToEntireContext :: ParamSet -> [MatchRule] -> Maybe ExName -> Context -> [XmlTree]
applyMatchRulesToEntireContext args rules mode ctx = processContext ctx (applyMatchRules args rules mode)

applyMatchRules :: ParamSet -> [MatchRule] -> (Maybe ExName) -> Context -> [XmlTree]
applyMatchRules _    []           mode ctx = matchDefaultRules mode ctx
applyMatchRules args (rule:rules) mode ctx = 
    maybe (applyMatchRules args rules mode ctx) 
          id
          (applyMatchRule args rule mode ctx)

applyMatchRule :: ParamSet -> MatchRule -> Maybe ExName -> Context -> Maybe [XmlTree]
applyMatchRule args rule@(MatRule expr _ ruleMode _ _ _) mode ctx =
  if mode==ruleMode && applyMatch expr ctx
    then Just $ instantiateMatchRule args rule $ ctxSetRule (Just rule) ctx
    else Nothing


instantiateMatchRule :: ParamSet -> MatchRule -> Context -> [XmlTree]
instantiateMatchRule args (MatRule _ _ _ _ params content) ctx =
    applyTemplate content ctxNew
  where 
    ctxNew = incRecDepth $ processParameters params args $ clearLocalVariables ctx

instantiateNamedRule :: ParamSet -> NamedRule -> Context -> [XmlTree]
instantiateNamedRule args (NamRule _ params content) ctx =
    applyTemplate content ctxNew
  where 
    ctxNew = incRecDepth $ processParameters params args $ clearLocalVariables ctx

-- ------------------------------------
    
matchDefaultRules :: (Maybe ExName) -> Context -> [XmlTree]
matchDefaultRules mode ctx@(Ctx ctxNavNode _ _ _ _ _ stylesheet _ _)
    | isElem ctxNode				-- rules for match="*|/"
	= applyMatchRulesToChildren Map.empty rules mode ctx 
    | isText ctxNode				-- rule for match="text()"
	= [ctxNode]
    | isAttr ctxNode				-- rule for match="@*"
	= [mkText $ collectTextnodes $ getChildren ctxNode]
    | otherwise					-- the glorious rest (PIs and comments):
	= []
    where 
    rules   = getMatchRules stylesheet
    ctxNode = subtreeNT ctxNavNode

matchDefaultRules _ _ = []
  
-- ------------------------------------

-- Variables and Parameters

-- Evaluate a xsl:variable or xsl:param element and add the newly
-- created local variable to the context

processLocalVariable :: Variable -> ParamSet -> Context -> Context
processLocalVariable var@(MkVar _ name _) arguments ctx =    
    addVariableBinding name val ctx
  where
    val = evalVariableWParamSet arguments ctx var

processParameters :: [Variable] -> ParamSet -> Context -> Context
processParameters params arguments ctx
    = foldl (\c v -> processLocalVariable v arguments c) ctx params

evalVariableWParamSet :: ParamSet -> Context -> Variable -> XPathValue
evalVariableWParamSet ps ctx (MkVar isPar name exprOrRtf)
    | isPar
	= maybe (resultFromVar exprOrRtf) id $ Map.lookup name ps 
    | otherwise
	= resultFromVar exprOrRtf
  where
    resultFromVar (Left expr) = evalXPathExpr expr ctx
    resultFromVar (Right rtf) = evalRtf rtf (show (recDepth ctx) ++ " " ++ show name) ctx

-- create a set of parameters (Names refering to XPath-values) from a set of Variable-placeholders (unevaluated expressions)
createParamSet :: Map ExName Variable -> Context -> ParamSet
createParamSet wParamList ctx = Map.map (evalVariableWParamSet Map.empty ctx) wParamList

-- ------------------------------------
-- handling of imported attributes

applyAttribSets :: [ExName] -> UsedAttribSets -> Context -> [XmlTree]
applyAttribSets callstack (UsedAttribSets sets) ctx = concatMap (\name -> applyAllAttrSetForName callstack name ctx) sets

applyAllAttrSetForName :: [ExName] -> ExName -> Context -> [XmlTree]
applyAllAttrSetForName callstack name ctx =

    if name `elem` callstack
    then error $ "Attribute-Set " ++ show name ++ " is recursively used." ++
                 concatMap (("\n  used in "++) . show) callstack
    
    else if isNothing attrset
    then error $ "No attribute set with name: " ++ show name
    
    else concatMap (flip (applyAttribSet (name:callstack)) ctx) $ fromJust attrset

  where
    attrset = Map.lookup name $ getAttributeSets $ ctxGetStylesheet ctx

applyAttribSet :: [ExName] -> AttributeSet -> Context -> [XmlTree]
applyAttribSet callstack (AttribSet _ usedSets content) ctx = 
    applyAttribSets callstack usedSets ctx ++ applyTemplate content ctx
    
-- ------------------------------------
-- Sorting

applySorting :: [SortKey] -> Context -> [NavXmlTree] -> Context
applySorting [] ctx nodes = ctxSetNodes nodes ctx
applySorting sortKeys ctx nodes = 
    ctxSetNodes resultOrder ctx
  where
    resultOrder          = snd $ unzip sortedKVs
    sortedKVs            = sortBy compKV keysWVals
    keysWVals            = zip keys nodes
    keys                 = map extract nodes
    (extrFs, cmpFs)      = unzip $ map (flip applySortKey ctx) sortKeys

    -- helper functions:
    extract node         = map ($ ctxSetNodes [node] ctx) extrFs
    compKV (k1,_) (k2,_) = compressOrds $ compares k1 k2
    compares             = zipWith3 (($) $) cmpFs
    compressOrds         = maybe EQ id . find (/=EQ)

type SortVal = Either Float String

applySortKey :: SortKey -> Context -> ( Context -> SortVal
                                      , SortVal -> SortVal -> Ordering)
applySortKey (SortK expr typeATV orderATV) ctx
    | typ /= "number"
      &&
      typ /= "text" 
	  = error $ "unsupported type in xsl:sort: " ++ typ
    | ordering /="ascending"
      &&
      ordering /="descending"
	  = error $ "order in xsl:sort element must be ascending or descending. Found: " ++ ordering
    | otherwise
	= (extractFct, cmpFct)
    where
    isNum          = typ == "number"
    isDesc         = ordering == "descending"
    ordering       = applyStringExpr orderATV ctx
    typ            = applyStringExpr typeATV ctx

    extractFct ctx'
	= let
	  val = applyStringExpr expr ctx'
	  in
          if isNum
          then Left $ readWDefault (-1.0 / 0.0) val
          else Right val

    cmpFct a
	= ( if isDesc then invertOrd else id ) 
          .
          ( if isNum then cmpNum a else cmpString a )

    cmpNum (Left n1)  (Left n2)
	= compare n1 n2
    cmpNum _ _
	= error "internal error in cmpNum in applySortKey"

    cmpString (Right s1) (Right s2)
	= compare (map toLower s1) (map toLower s2)      -- The text comparison still needs to be improved...
    cmpString _ _
	= error "internal error in cmpString in applySortKey"

invertOrd :: Ordering -> Ordering
invertOrd EQ = EQ
invertOrd LT = GT
invertOrd GT = LT

-- ------------------------------------
-- Namespace FIXUP

fixupNS :: XmlTree -> XmlTree
fixupNS = compressNS . disambigNS

compressNS :: XmlTree -> XmlTree
compressNS = 
    mapTreeCtx compressElem $ Map.fromAscList [("xml", xmlNamespace), ("xmlns", xmlnsNamespace)]
  
compressElem :: UriMapping -> XNode -> (UriMapping, XNode)
compressElem uris node
  | isElem node = (newUris, changeAttrl (filter $ isImportant) node)
  | otherwise   = (uris, node)
  where
    newUris       = uris `Map.union` getUriMap node
    isImportant n = not (isNsAttr n) 
                         || not ((localPart $ fromJust $ getAttrName n) `Map.member` uris)

disambigNS :: XmlTree -> XmlTree
disambigNS = 
    mapTreeCtx step $ Map.fromAscList [("xml", xmlNamespace), ("xmlns", xmlnsNamespace)]
  where
    step uris node
      | isElem node = let uris'               = uris `Map.union` getUriMap node
                          (newUris, newNode') = disambigElem uris' node in
                        (newUris, setUriMap newUris newNode')
      | otherwise   = (uris, node)

disambigElem :: UriMapping -> XNode -> (UriMapping, XNode)
disambigElem nsMap element =    
    (newNsMap, XTag                  
                 (remapNsName newNsMap $ fromJust $ getElemName element)
                 $ map (changeName $ remapNsName newNsMap) $ fromJust $ getAttrl element )
  where
    newNsMap   = nsMap `Map.union` Map.fromAscList newTuples
    newTuples  = zip newPrefs $ nub newUris
    newUris    = filter (`notElem` oldUris) $ filter (not . null) $ map namespaceUri $ mapMaybe getName
                   (element : map getNode (fromJust $ getAttrl element))
    newPrefs   = filter (`notElem` oldPrefs) ["ns" ++ show i | i <- [(1::Int)..]]
    oldPrefs   = Map.keys nsMap
    oldUris    = Map.elems nsMap  

remapNsName :: UriMapping -> QName -> QName
remapNsName nsMap name = 

    if maybe (nsUri=="") (== nsUri) luUri
    then name

    else mkQName newPref (localPart name) nsUri

  where
    luUri   = Map.lookup (namePrefix name) nsMap
    newPref = head $ (++ (error $ "int. error: No prefix for " ++ nsUri)) 
                $ Map.keys $ Map.filter (==namespaceUri name) nsMap
    nsUri   = namespaceUri name

