-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.DTDProcessing
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: DTDProcessing.hs,v 1.15 2006/11/12 14:53:00 hxml Exp $

   DTD processing function for
   including external parts of a DTD
   parameter entity substitution and general entity substitution

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.DTDProcessing
    ( getWellformedDoc
    , checkWellformedDoc
    , processDTD
    , processGeneralEntities
    )
where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.Parser.XmlOutput
    ( traceTree
    , traceSource
    , traceMsg
    )

import Text.XML.HXT.Parser.XmlParser
    ( parseXmlDoc
    , parseXmlDTDPart
    , parseXmlAttrValue
    , parseXmlGeneralEntityValue
    )

import Text.XML.HXT.Parser.XmlDTDParser
    ( parseXmlDTDdecl
    , parseXmlDTDdeclPart
    , parseXmlDTDEntityValue
    )

import Text.XML.HXT.Parser.XmlInput
    ( getXmlContents
    , getXmlEntityContents
    , runInLocalURIContext
    , runInNewURIContext
    , getBaseURI
    , setBaseURI
    , getAbsolutURI
    , isStandaloneDocument
    )

import Text.XML.HXT.DOM.EditFilters
    ( transfCharRef
    , transfAllCharRef
    )

import Text.XML.HXT.DOM.XmlState

import Data.Maybe

import qualified Data.Map as M
    ( Map
    , empty
    , lookup
    , insert
    -- , foldWithKey
    )

-- ------------------------------------------------------------
--
-- the "main" building blocks

-- |
-- monadic filter for reading, parsing and checking a wellformed document.
-- the input tree must consist of a root node with a source attribute
-- in its attribute list.
--
-- All attributes from the document root are copied into the system state,
-- and may be queried by the monadic filters, e.g. trace options.
--
-- Result is the single element list containing the well-formed document tree
-- or, in case of errors, the document root with an empty list as children
-- and attributes 'a_status' and 'a_module' for error level and the module,
-- where the error occured.
--
-- this is a shortcut for 'getXmlContents' .\>\> 'checkWellformedDoc'
--
-- example for a main program:
--
-- @
-- main =
--   run\' \$
--   do
--   res  \<- getWellformedDoc \$ newDocument \"myfile.xml\"
--   ...
-- @

getWellformedDoc	:: XmlStateFilter state
getWellformedDoc
    = setSystemParams
      .>>
      getXmlContents
      .>>
      checkWellformedDoc

-- |
-- parses a text node with 'parseXmlDoc', processes the DTD and general entities
-- and transforms all char references into characters

checkWellformedDoc		:: XmlStateFilter state
checkWellformedDoc
    = parseXmlDoc
      .>>
      processDTD
      .>>
      processGeneralEntities
      .>>
      liftMf transfAllCharRef

-- ------------------------------------------------------------
--

type RecList		= [String]

type DTDState res	= XState PeEnv res

type DTDStateFilter	= XmlTree -> DTDState XmlTrees

data DTDPart		= Internal
			| External
			  deriving (Eq)

-- ------------------------------------------------------------

type PeEnv		= M.Map String XmlTree

emptyPeEnv	:: PeEnv
emptyPeEnv	= M.empty

lookupPeEnv	:: String -> PeEnv -> Maybe XmlTree
lookupPeEnv	= M.lookup

addPeEntry	:: String -> XmlTree -> PeEnv -> PeEnv
addPeEntry	= M.insert

-- ------------------------------------------------------------

-- |
-- a filter for DTD processing
--
-- inclusion of external parts of DTD,
-- parameter entity substitution
-- conditional section evaluation
--
-- input tree must represent a complete document including root node

processDTD		:: XmlStateFilter a
processDTD
    = runInLocalURIContext
         ( processRoot
	   .>>
	   traceTree
	   .>>
	   traceSource
	 )
      `whenM` ( isRoot .> getChildren )
      where

      processRoot	:: XmlStateFilter a
      processRoot t
	  = do
	    sequence_ . map (\ (a, ts) -> setSysParamTree a ts) . toTreel . getAttrl $ t
	    setSysParam a_standalone ""
	    ( traceMsg 1 ("processDTD: process parameter entities")
	      .>>
	      liftMf (modifyChildren addDoctype)
	      .>>
	      processChildrenM substParamEntities
	      .>>
	      checkResult "in XML DTD processing"
	      ) `whenM` statusOk
	      $ t

      -- Adds an empty DOCTYPE node to document. Needed for documents without a DTD.
      -- Otherwise processor can not substitute predefined entities. If an xml declaration
      -- is present, the node is inserted after this node.
      addDoctype :: XmlSFilter
      addDoctype docChilds
          = if null doctype
            then if null xmlDecl
                 then [mkXDTDTree DOCTYPE [] []] ++ docChilds
                 else (head docChilds) : [mkXDTDTree DOCTYPE [] []] ++ (tail docChilds)
            else docChilds
            where
            doctype = isDoctype $$ docChilds
            xmlDecl = isPi t_xml $$ isXPi $$ docChilds


substParamEntities	:: XmlStateFilter a
substParamEntities
    = processParamEntities
      `whenM`
      isDoctype
      where

      processParamEntities	:: XmlStateFilter a
      processParamEntities t'
	  = do
	    (dtdPre, envPre) <- processPredef

	    -- process internal part of DTD with initial empty env for parameter entities
	    (dtdInt, envInt) <- processInt envPre t'

	    -- process external part of DTD with resulting env from internal DTD
	    dtdExt <- runInLocalURIContext (processExt envInt) t'

	    -- merge predefined, internal and external part
	    trace 2 "substParamEntities: merge internal and external DTD parts"
	    return (replaceChildren (foldl1 mergeDTDs [dtdPre, dtdInt, dtdExt]) t')

      processPredef
	  = do
	    trace 2 "substParamEntities: substitute predefined entities"
	    chain' emptyPeEnv (substParamEntity Internal $$< predefDTDPart)

      processInt env' n'
	  = do
	    trace 2 "substParamEntities: substitute parameter entities in internal DTD part"
	    chain' env' (substParamEntity Internal $$< getChildren n')

      processExt env' n'
	  = do
	    trace 2 "substParamEntities: process external part of DTD"
            extDtd <- processExternalDTD n'
	    trace 2 "substParamEntities: substitute parameter entities in external DTD part"
	    chain env' ( (substParamEntity External) $$< extDtd)

-- ------------------------------------------------------------
-- |
-- substitute parameter entities
-- result is the input tree with the list of text nodes as children or an error message

substPEref'	:: DTDPart -> PeEnv -> XmlFilter

substPEref' loc env n@(NTree (XDTD PEREF al) _)
    | isInternalRef	= xerr ("a parameter entity reference of " ++ peName' ++ " occurs in the internal subset of the DTD")

    | isUndefinedRef	= xerr ("parameter entity " ++ peName' ++ " not found (forward reference?)")

    | null baseUri	= [setChildren peContent n]

    | otherwise		= [(NTree (XDTD PEREF ((a_url,baseUri):al)) peContent)]
    where
    peName		= lookup1 a_peref al
    peName'		= show peName
    peVal		= lookupPeEnv peName env
    isInternalRef	= loc == Internal
    isUndefinedRef	= isNothing peVal

    (NTree (XDTD PENTITY peAl) peContent) = fromJust peVal
    baseUri		= lookup1 a_url peAl

    -- peContent		= getChildren $ fromJust peVal

substPEref' _ _ n
    = [n]

-- ------------------------------------------------------------

{-
dumpPE	:: DTDStateFilter
dumpPE t
    = do
      env <- getState
      ( traceMsg 2 ("dumpPE: parameter entity environment\n" ++ M.foldWithKey showPE "" env)
	.>>
	thisM ) t
    where
    showPE k v b = k ++ "\t" ++ show v ++ "\n" ++ b
-}

traceDTD	:: String -> XmlStateFilter a
traceDTD msg	= traceMsg 4 msg .>> traceTree

-- ------------------------------------------------------------

getExternalParamEntityValue	:: DTDStateFilter
getExternalParamEntityValue n@(NTree (XDTD PENTITY al) _cl)
    = do
      rl <- ( getXmlEntityContents
	      .>>
	      liftMf getChildren
	    )
	    $ newDocument' ((a_source, sysVal) : al)
      base <- getBaseURI
      if null rl
	 then issueErr ("illegal external parameter entity value for entity %" ++ peName ++";") n
	 else thisM (NTree (XDTD PENTITY ((a_url, base) : al)) rl)
    where
    sysVal = lookup1 k_system al
    peName = lookup1 a_name   al

getExternalParamEntityValue n
    = error ("getExternalParamEntityValue: illegal argument: " ++ show n)

-- ------------------------------------------------------------

-- |
-- the dtd parameter entity substitution filter

substParamEntity	:: DTDPart -> DTDStateFilter

substParamEntity loc n@(NTree xn _cs)
    | isDTDElemNode ENTITY xn					-- ENITITY and parameter ENTITY decl
	= traceDTD ("ENTITY declaration before DTD declaration parsing")
	  .>>
	  processChildrenM (substPeRefsInDTDdecl [])
          .>>
	  liftF parseXmlDTDdecl
          .>>
	  substRefsInEntityValue
          .>>
	  processEntityDecl
          .>>
          traceDTD ("ENTITY declaration after DTD declaration parsing")
          $ n

    | isDTDElemNode PEREF xn					-- parameter entity ref in DTD
	= substPeRefsInDTDpart [] n

    | isDTDElemNode ELEMENT xn					-- ELEMENT, ATTLIST, NOTATION
      ||
      isDTDElemNode ATTLIST xn
      ||
      isDTDElemNode NOTATION xn
	= traceDTD  "DTD declaration before PE substitution"
	  .>>
	  processChildrenM (substPeRefsInDTDdecl [])
          .>>
	  liftF parseXmlDTDdecl
          .>>
	  traceDTD "DTD declaration after DTD declaration parsing"
	  $ n

    | isDTDElemNode CONDSECT xn					-- conditional section
      &&
      loc == Internal
	  = do
	    issueErr "conditional sections in internal part of the DTD is not allowed" n
            return []

    | isDTDElemNode CONDSECT xn					-- conditional section
      &&
      loc == External
	  = let
	    (XDTD _ al) = xn
	    content	= mkXTextTree (lookup1 a_value al)
	    in
	    traceDTD "substParamEntity: process conditional section"
	    .>>
	    processChildrenM (substPeRefsInCondSect [])
            .>>
	    liftF parseXmlDTDdecl
	    .>>
            evalCond content
	    $ n

    | isXCmtNode xn						-- remove comments
	= noneM n

    | otherwise							-- default: keep things unchanged (should not occur)
	= thisM n
    where

    processEntityDecl	:: DTDStateFilter
    processEntityDecl n'@(NTree (XDTD ENTITY al) cs)	= if isExtern
	  then ( do
		 url <- getAbsolutURI sysVal			-- add the absolute URI
		 return [NTree (XDTD ENTITY ((a_url, url) : al)) cs]
	       )
	  else liftMf (substChildren (xmlTreesToText . getChildren)) n'
	where
	isExtern = hasEntry k_system al
	sysVal   = lookup1  k_system al
      
    processEntityDecl n'@(NTree (XDTD PENTITY al) _)
	= do
	  env <- getState
	  if (isJust . lookupPeEnv peName) $ env
	     then
	     issueWarn ("parameter entity " ++ show peName ++ " already defined") n
	     else
	     ( ifM isExternalParameterEntity
	           ( runInLocalURIContext getExternalParamEntityValue )
                   ( liftMf (substChildren (xmlTreesToText . getChildren)) )
	       .>>
	       addPE peName
	     ) n'
	  where
	  peName = lookup1 a_name al

    processEntityDecl n'
	= error ("processEntityDecl called with wrong argument" ++ show n')

    addPE	:: String -> DTDStateFilter
    addPE name t
	= do
	  trace 2 ("substParamEntity: add entity to env: " ++ xshow [t])
	  changeState $ addPeEntry name t
	  return []

    substPEref	:: DTDStateFilter
    substPEref n'
	= do
	  env <- getState
	  liftF (substPEref' loc env) $ n'

    substPeRefsInValue	:: [String] -> DTDStateFilter
    substPeRefsInValue recList n'@(NTree (XDTD PEREF al) _cl)
	= ( if peName `elem` recList
	    then issueErr ("recursive call of parameter entity " ++ show peName ++ " in entity value")
	    else ( substPEref
		   .>>
		   liftF parseXmlDTDEntityValue
		   .>>
		   liftF transfCharRef
		   .>>
		   ( substPeRefsInValue (peName : recList)
		     `whenM`
		     isPeRef
		   )
		 )
	  ) $ n'
	where
	peName = lookup1 a_peref al

    substPeRefsInValue _ n'
	= thisM n'

    substRefsInEntityValue	:: DTDStateFilter
    substRefsInEntityValue n'@(NTree (XDTD decl al) _cl)
	| decl `elem` [ENTITY, PENTITY]
	    = ( if hasEntry k_system al
		then thisM							-- externals not yet handled ??? really
		else processChildrenM ( liftF transfCharRef
					.>>
					substPeRefsInValue []
				      )
	      ) n'
    substRefsInEntityValue  n'
	= error ("substRefsInEntityValue called with wrong argument" ++ show n')

    runInPeContext	:: DTDStateFilter -> DTDStateFilter
    runInPeContext f n'@(NTree (XDTD PEREF al) _)
	| null base
	    = f n'
	| otherwise
	    = runInNewURIContext base f n'
	where
	base = lookup1 a_url al

    runInPeContext f n'
	= f n'

    substPeRefsInDTDdecl	:: [String] -> DTDStateFilter
    substPeRefsInDTDdecl recList n'@(NTree (XDTD PEREF al) _cl)
	= ( if peName `elem` recList
	    then issueErr ("recursive call of parameter entity " ++ show peName ++ " in DTD declaration")
	    else ( substPEref
		   .>>
		   traceDTD "substPeRefsInDTDdecl: parseXmlDTDdeclPart"
		   .>>
		   ( runInPeContext
		     ( liftF ( parseXmlDTDdeclPart )
		       .>>
		       traceDTD "substPeRefsInDTDdecl: after parseXmlDTDdeclPart"
		       .>>
		       processChildrenM ( substPeRefsInDTDdecl (peName : recList) )
		     )
		     `whenM`
		     isPeRef
		   )
		 )
	  ) $ n'
	where
	peName = lookup1 a_peref al

    substPeRefsInDTDdecl _ n'
	= thisM n'

    substPeRefsInDTDpart	:: [String] -> DTDStateFilter
    substPeRefsInDTDpart recList n'@(NTree (XDTD PEREF al) _cl)
	= ( if peName `elem` recList
	    then issueErr ("recursive call of parameter entity " ++ show peName ++ " in DTD part")
	    else ( substPEref
		   .>>
		   traceDTD "substPeRefsInDTDpart: parseXmlDTDPart"
		   .>>
		   runInPeContext
		   ( liftF (getChildren .> parseXmlDTDPart ("parameter entity " ++ show peName))
		     .>>
		     traceDTD "substPeRefsInDTDdecl: after parseXmlDTDPart"
		     .>>
		     substParamEntity loc -- cyclic check not yet done (peName : recList)
		   )
		 )
	  ) $ n'
	where
	peName = lookup1 a_peref al

    substPeRefsInDTDpart _ n'
	= thisM n'

    substPeRefsInCondSect	:: [String] -> DTDStateFilter
    substPeRefsInCondSect recList n'@(NTree (XDTD PEREF al) _cl)
	= ( if peName `elem` recList
	    then issueErr ("recursive call of parameter entity " ++ show peName ++ " in conditional section")
	    else ( substPEref
		   .>>
		   traceDTD "substPeRefsInCondSect: parseXmlDTDdeclPart"
		   .>>
		   ( runInPeContext
		     ( liftF ( parseXmlDTDdeclPart )
		       .>>
		       traceDTD "substPeRefsInCondSect: after parseXmlDTDdeclPart"
		       .>>
		       processChildrenM ( substPeRefsInCondSect (peName : recList) )
		     )
		     `whenM`
		     isPeRef
		   )
		 )
	  ) $ n'
	where
	peName = lookup1 a_peref al

    substPeRefsInCondSect _ n'
	= thisM n'


evalCond :: XmlTree -> DTDStateFilter
evalCond content (NTree (XText c) _)
    | c == k_include
	= liftF (parseXmlDTDPart "conditional section")
	  .>>
	  traceDTD "evalCond: DTD part"
	  .>>
	  substParamEntity External
          $ content 
    | otherwise
	= return []
evalCond _ n'
    = error ("evalCond: illegal argument: " ++ show n')

-- ------------------------------------------------------------

processExternalDTD	:: XmlStateFilter a
processExternalDTD n@(NTree (XDTD DOCTYPE al) _dtd)
    | null sysVal
	= return []
    | otherwise
	= do
	  checkStandalone
	  dtdContent <- ( getXmlEntityContents
			  .>>
			  traceMsg 2 ("processExternalDTD: parsing DTD part for " ++ show sysVal)
			  .>>
			  liftMf getChildren
			  .>>
			  liftF (parseXmlDTDPart sysVal)
			)
		       $ newDocument sysVal
	  trace 2 "processExternalDTD: parsing DTD part done"
	  traceTree $ mkRootTree [] dtdContent
	  return dtdContent
    where
    sysVal		= lookup1 k_system al
    checkStandalone	= do
			  _isAlone <- isStandaloneDocument
			  if False -- isAlone
			     then issueErr ("external DTD " ++ show sysVal ++ " specified for standalone document") n
			     else return []

processExternalDTD  _
    = return []

-- ------------------------------------------------------------
-- |
-- merge the external and the internal part of a DTD into one internal part.
-- parameter entity substitution should be made before applying this filter
-- internal definitions shadow external ones
--
--
-- preliminary: no real merge is done

mergeDTDs	:: XmlTrees -> XmlTrees -> XmlTrees
mergeDTDs dtdInt dtdExt
    = dtdInt ++ (mergeDTDentry dtdInt $$ dtdExt)

mergeDTDentry	:: XmlTrees -> XmlFilter
mergeDTDentry dtdPart
    = none `when` found
      where
      filterList = map filterDTDNode dtdPart	-- construct the list of filters
      found      = cat filterList		-- concatenate the filters (set union)


filterDTDNode	:: XmlTree -> XmlFilter

filterDTDNode (NTree (XDTD dtdElem al) _)
    | dtdElem `elem` [ELEMENT, NOTATION, ENTITY]
	= filterElement
	  where
	  filterElement n@(NTree (XDTD dtdElem' al') _cl')
	      | dtdElem == dtdElem' &&
		lookup a_name al' == lookup a_name al
		  = [n]
	      | otherwise
		  = []
	  filterElement _
	      = []

filterDTDNode (NTree (XDTD ATTLIST al) _)
    = filterAttlist
      where
      filterAttlist n@(NTree (XDTD ATTLIST al') _cl')
	  | lookup a_name  al' == lookup a_name  al &&
	    lookup a_value al' == lookup a_value al
		= [n]
      filterAttlist _
	  = []

filterDTDNode _
    =  none

-- ------------------------------------------------------------

predefinedEntities	:: String
predefinedEntities
    = concat [ "<!ENTITY lt   '&#38;#60;'>"
	     , "<!ENTITY gt   '&#62;'>"
	     , "<!ENTITY amp  '&#38;#38;'>"
	     , "<!ENTITY apos '&#39;'>"
	     , "<!ENTITY quot '&#34;'>"
	     ]

predefDTDPart		:: XmlTrees
predefDTDPart
    = parseXmlDTDPart "predefined entities" $ mkXTextTree predefinedEntities

-- ------------------------------------------------------------
--
-- 4.4 XML Processor Treatment of General Entities and References

data GeContext
    = ReferenceInContent
    | ReferenceInAttributeValue
    | ReferenceInEntityValue
    -- or OccursInAttributeValue				-- not used during substitution but during validation
    -- or ReferenceInDTD					-- not used: syntax check detects errors

type GeFct		= GeContext -> RecList -> GeStateFilter

type GeState res	= XState GeEnv res

type GeStateFilter	= XmlTree -> GeState XmlTrees

-- ------------------------------------------------------------

newtype GeEnv	= GeEnv (M.Map String GeFct)

emptyGeEnv	:: GeEnv
emptyGeEnv	= GeEnv M.empty

lookupGeEnv		:: String -> GeEnv -> Maybe GeFct
lookupGeEnv k (GeEnv env)
    = M.lookup k env

addGeEntry	:: String -> GeFct -> GeEnv -> GeEnv
addGeEntry k a (GeEnv env)
    = GeEnv $ M.insert k a env

-- ------------------------------------------------------------

-- |
-- substitution of general entities
--
-- input: a complete document tree including root node

processGeneralEntities	:: XmlStateFilter a
processGeneralEntities
    = ( traceMsg 1 "processGeneralEntities: collect and substitute general entities"
	.>>
	processEntities
	.>>
	checkResult "in general entity processing"
	.>>
	traceTree
	.>>
	traceSource
      )
      `whenM` statusOk
      where
      processEntities t'
	  = do
	    res <- chain initialEnv (processGeneralEntity ReferenceInContent [] $$< getChildren t')
	    return $ replaceChildren res t'
	    where
	    initialEnv = emptyGeEnv

-- ------------------------------------------------------------

processGeneralEntity	:: GeContext -> RecList -> GeStateFilter
processGeneralEntity cx rl n@(NTree (XDTD DOCTYPE _) dtdPart)
    = do
      res <- processGeneralEntity cx rl $$< dtdPart
      return $ replaceChildren res n

processGeneralEntity cx rl n@(NTree (XDTD ENTITY al) cl)
    | isIntern
	= do
	  trace 2 ("processGeneralEnity: general entity definition for " ++ show name)
	  value <- liftF (parseXmlGeneralEntityValue ("general internal entity " ++ show name)) $ mkXTextTree (xshow cl)
	  res   <- processGeneralEntity ReferenceInEntityValue (name:rl) $$< value
	  insertEntity name (substInternal res)
    | isExtern
	= do
	  baseUri <- getBaseURI
	  insertEntity name (substExternalParsed1Time baseUri)

    | isUnparsed
	= do
	  trace 2 ("processGeneralEnity: unparsed entity definition for " ++ show name)
	  insertEntity name (substUnparsed [])
    where
    isUnparsed	= not isIntern && not isExtern
    isExtern	= not isIntern && not (hasEntry k_ndata al)
    isIntern	= not (hasEntry k_system al)

    name	= lookup1 a_name   al
    -- sysVal	= lookup1 k_system al
    url		= lookup1 a_url    al		-- a_url contains absolut url for k_system
    context	= addEntry a_source url al

    processExternalEntityContents	:: XmlTrees -> GeState XmlTrees
    processExternalEntityContents cl'
	| null cl'						-- reading content did not succeed
	    = return []
	| null txt'						-- something weird in entity content
	    = do
	      issueErr ("illegal external parsed entity value for entity " ++ show name) n
	      return []
	| otherwise						-- o.k.: parse the contents
	    = do
	      res' <- liftF (parseXmlGeneralEntityValue ("external parsed entity " ++ show name)) $$< txt'
	      ( traceSource
		.>>
		traceTree ) $ mkRootTree (fromAttrl context) res'
	      return res'
	where
	txt' = (getChildren .> isXText) $$ cl'

    insertEntity	:: String -> GeFct -> GeState XmlTrees
    insertEntity name' fct'
	= do
	  geEnv' <- getState
	  case lookupGeEnv name' geEnv' of
	    Just _fct
		-> do
		   issueWarn ("entity " ++ show name ++ " already defined, repeated definition ignored") n
		   return []
            Nothing
		-> do
		   changeState $ addGeEntry name' fct' 
		   return $ this n

    -- --------
    --
    -- 4.4 XML Processor Treatment of Entities and References

    substInternal	:: XmlTrees -> GeContext -> RecList -> GeStateFilter
    substInternal nl ReferenceInContent rl' _n'
	= included nl rl'

    substInternal nl ReferenceInAttributeValue rl' _n'
	= includedInLiteral nl rl'

    substInternal _nl ReferenceInEntityValue _rl n'
	= bypassed n'

    -- --------

    substExternalParsed1Time	:: String -> GeContext -> RecList -> GeStateFilter
    substExternalParsed1Time baseUri' cx' rl' n'
	= do
	  trace 2 ("substExternalParsed1Time: read and parse external parsed entity " ++ show name)
	  res  <- runInLocalURIContext getContents' $ newDocument' context
	  changeState $ addGeEntry name (substExternalParsed res)
	  substExternalParsed res cx' rl' n' 
	  where
	  getContents'	:: GeStateFilter
	  getContents' t''
	      = do
		setBaseURI baseUri'
		rs' <- getXmlEntityContents t''
		processExternalEntityContents rs'


    substExternalParsed	:: XmlTrees -> GeContext -> RecList -> GeStateFilter
    substExternalParsed  nl ReferenceInContent rl' _n'
	= includedIfValidating nl rl'

    substExternalParsed _nl ReferenceInAttributeValue _rl _n'
	= forbidden "external parsed general" "in attribute value"

    substExternalParsed _nl ReferenceInEntityValue _rl n'
	= bypassed n'

    -- --------

    substUnparsed	:: XmlTrees -> GeContext -> RecList -> GeStateFilter
    substUnparsed _nl ReferenceInContent _rl _n'
	= forbidden "unparsed" "content"
    substUnparsed _nl ReferenceInAttributeValue _rl _n'
	= forbidden "unparsed" "attribute value"
    substUnparsed _nl ReferenceInEntityValue _rl _n'
	= forbidden "unparsed" "entity value"

    -- --------
							-- see XML 1.0 chapter:

    included nl	rl'					-- 4.4.2
	= processGeneralEntity cx (name:rl') $$< nl

    includedIfValidating				-- 4.4.3
	= included

    includedInLiteral					-- 4.4.5
	= included

    bypassed n'						-- 4.4.7
	= return $ this n'

    forbidden msg' cx'					-- 4.4.4
	= do
	  issueErr ("reference of " ++ msg' ++ show name ++ " forbidden in " ++ cx') n
          return []

							-- normalize default value in ATTLIST
processGeneralEntity _cx rl n@(NTree (XDTD ATTLIST al) _cl)
    | hasDefaultValue
	= do
	  res <- ( liftF (parseXmlAttrValue "default value of attribute ")
                   .>>
		   substGeneralEntityInAValue rl
		 ) $ mkXTextTree defaultValue
	  return $ addDTDAttr a_default (xshow res) n
    | otherwise
	= return $ this n
    where
    hasDefaultValue = hasEntry  a_default al
    defaultValue    = lookup1   a_default al

processGeneralEntity cx rl n@(NTree (XEntityRef name) _)
    = do
      trace 2 ("processGeneralEnity: entity reference for entity " ++ show name)
      trace 3 ("recursion list = " ++ show rl)
      geEnv <- getState
      case lookupGeEnv name geEnv of
        Just fct
	  -> if name `elem` rl
	     then do
		  issueErr ("recursive substitution of general entity reference " ++ show ref ++ " not processed") n
		  return nl
	     else do
		  fct cx rl n
        Nothing
	  -> do
	     issueErr ("general entity reference " ++ show ref ++ " not processed, no definition found, (forward reference?)") n
	     return nl
      where
      nl  = this n
      ref = xshow nl


processGeneralEntity cx rl n@(NTree (XTag _tagName al) cl)
    = do
      al' <- substGeneralEntityInAttr rl $$< al
      cl' <- processGeneralEntity cx rl  $$< cl
      return $ (replaceAttrl al' .> replaceChildren cl') n

processGeneralEntity _cx _rl n
    = return $ this n

-- ------------------------------------------------------------

substGeneralEntityInAttr	:: RecList -> XmlTree -> GeState XmlTrees
substGeneralEntityInAttr rl a@(NTree (XAttr _) aValue)
    = do
      nv <- substGeneralEntityInAValue rl $$< aValue
      return (replaceChildren nv a)

substGeneralEntityInAttr _ _
    = return []

substGeneralEntityInAValue	:: RecList -> GeStateFilter
substGeneralEntityInAValue rl
    = ( processGeneralEntity ReferenceInAttributeValue rl
	`whenM`
	isXEntityRef
      )
      .>>
      liftMf ( ( modifyText normalizeWhiteSpace `when` isXText)
	      .>
	      (transfCharRef `when` isXCharRef)
	    )
      where
      normalizeWhiteSpace
	  = map ( \c -> if c `elem` "\n\t\r" then ' ' else c )

-- ------------------------------------------------------------


