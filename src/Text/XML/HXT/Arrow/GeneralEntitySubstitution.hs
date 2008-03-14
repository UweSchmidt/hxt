-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.GeneralEntitySubstitution
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: GeneralEntitySubstitution.hs,v 1.13 2006/05/01 18:56:24 hxml Exp $

   general entity substitution

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.GeneralEntitySubstitution
    ( processGeneralEntities )
where

import Control.Arrow				-- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlIOStateArrow

import Text.XML.HXT.Arrow.ParserInterface
    ( parseXmlAttrValue
    , parseXmlGeneralEntityValue
    )

import Text.XML.HXT.Arrow.Edit
    ( transfCharRef
    )

import Text.XML.HXT.Arrow.DocumentInput
    ( getXmlEntityContents
    )

import Data.Maybe

import qualified Data.Map as M
    ( Map
    , empty
    , lookup
    , insert)

-- ------------------------------------------------------------

data GEContext
    = ReferenceInContent
    | ReferenceInAttributeValue
    | ReferenceInEntityValue
    -- or OccursInAttributeValue				-- not used during substitution but during validation
    -- or ReferenceInDTD					-- not used: syntax check detects errors

type GESubstArrow	= GEContext -> RecList -> GEArrow XmlTree XmlTree

type GEArrow b c	= IOStateArrow GEEnv b c

type RecList		= [String]

-- ------------------------------------------------------------

newtype GEEnv	= GEEnv (M.Map String GESubstArrow)

emptyGeEnv	:: GEEnv
emptyGeEnv	= GEEnv M.empty

lookupGeEnv		:: String -> GEEnv -> Maybe GESubstArrow
lookupGeEnv k (GEEnv env)
    = M.lookup k env

addGeEntry	:: String -> GESubstArrow -> GEEnv -> GEEnv
addGeEntry k a (GEEnv env)
    = GEEnv $ M.insert k a env

-- ------------------------------------------------------------

-- |
-- substitution of general entities
--
-- input: a complete document tree including root node

processGeneralEntities	:: IOStateArrow s XmlTree XmlTree
processGeneralEntities
    = ( traceMsg 1 "processGeneralEntities: collect and substitute general entities"
	>>>
	withOtherUserState emptyGeEnv (processChildren (processGeneralEntity ReferenceInContent []))
	>>>
	setDocumentStatusFromSystemState "in general entity processing"
	>>>
	traceTree
	>>>
	traceSource
      )
      `when`
      documentStatusOk

      
processGeneralEntity	:: GESubstArrow
processGeneralEntity context recl
    = choiceA [ isElem		:-> ( processAttrl (processChildren substEntitiesInAttrValue)
		                      >>>
		                      processChildren (processGeneralEntity context recl)
				    )
	      , isDTDDoctype	:-> processChildren (processGeneralEntity context recl)
	      , isDTDEntity	:-> addEntityDecl
	      , isDTDAttlist	:-> substEntitiesInAttrDefaultValue
	      , isEntityRef	:-> substEntityRef
	      , this 		:-> this
	      ]
    where
    addEntityDecl	:: GEArrow XmlTree XmlTree
    addEntityDecl
	= perform ( choiceA [ isIntern		:-> addInternalEntity		-- don't change sequence of cases
			    , isExtern		:-> addExternalEntity
			    , isUnparsed	:-> addUnparsedEntity
			    ]
		  )
	where
	isIntern	= none `when` hasDTDAttr k_system
	isExtern	= none `when` hasDTDAttr k_ndata
	isUnparsed	= this

    addInternalEntity	:: GEArrow XmlTree b
    addInternalEntity
	= ( ( getDTDAttrValue a_name
	      >>>
	      traceString 2 (("processGeneralEntity: general entity definition for " ++) . show)
	    )
	    &&&
	    xshow (getChildren >>> isText)
	  )
          >>>
	  applyA ( arr2 $ \ entity str ->
		   listA ( ( ( txt str
			       >>>
			       parseXmlGeneralEntityValue ("general internal entity" ++ show entity)
			       >>>
			       filterErrorMsg
			     )
			     `orElse` txt ""
			   )
			   >>>
			   processGeneralEntity ReferenceInEntityValue (entity : recl)
			 )
		   >>>
		   applyA (arr $ \ ts -> insertEntity (substInternal ts) entity)
		 )
	  >>>
	  none

    addExternalEntity	:: GEArrow XmlTree b
    addExternalEntity
	= ( ( getDTDAttrValue a_name
	      >>>
	      traceString 2 (("processGeneralEntity: external entity definition for " ++) . show)
            )
	    &&&
	    getDTDAttrValue a_url 			-- the absolute URL, not the relative in attr: k_system
	  )
	  >>>
	  applyA (arr2 $ \ entity uri -> insertEntity (substExternalParsed1Time uri) entity)
	  >>>
	  none

    addUnparsedEntity	:: GEArrow XmlTree b
    addUnparsedEntity
	= getDTDAttrValue a_name
	  >>>
	  traceString 2 (("processGeneralEntity: unparsed entity definition for " ++) . show)
          >>>
	  applyA (arr (insertEntity substUnparsed))
	  >>>
	  none

    insertEntity	:: (String -> GESubstArrow) -> String -> GEArrow b b
    insertEntity fct entity
	= ( getUserState
	    >>>
	    applyA (arr checkDefined)
	  )
	  `guards`
	  addEntity fct entity
	where
	checkDefined geEnv
	    = maybe ok alreadyDefined . lookupGeEnv entity $ geEnv
	    where
	    ok	= this
	    alreadyDefined _
		= issueWarn ("entity " ++ show entity ++ " already defined, repeated definition ignored")

    addEntity	:: (String -> GESubstArrow) -> String -> GEArrow b b
    addEntity fct entity
	= changeUserState ins
	where
	ins _ geEnv = addGeEntry entity (fct entity) geEnv

    substEntitiesInAttrDefaultValue	:: GEArrow XmlTree XmlTree
    substEntitiesInAttrDefaultValue
	= applyA ( xshow ( getDTDAttrValue a_default			-- parse the default value
			   >>>						-- substitute entities
			   mkText					-- and convert value into a string
			   >>>
			   parseXmlAttrValue "default value of attribute"
			   >>>
			   filterErrorMsg
			   >>>
			   substEntitiesInAttrValue
			 )
		   >>> arr (setDTDAttrValue a_default)
		 )
          `when` hasDTDAttr a_default

    substEntitiesInAttrValue	:: GEArrow XmlTree XmlTree
    substEntitiesInAttrValue
	= ( processGeneralEntity ReferenceInAttributeValue recl
	    `when`
	    isEntityRef
	  )
          >>>
	  changeText normalizeWhiteSpace
	  >>>
	  transfCharRef
	where
	normalizeWhiteSpace = map ( \c -> if c `elem` "\n\t\r" then ' ' else c )


    substEntityRef	:: GEArrow XmlTree XmlTree
    substEntityRef
	= applyA ( ( ( getEntityRef				-- get the entity name and the env
		       >>>					-- and compute the arrow to be applied
		       traceString 2 (("processGeneralEntity: entity reference for entity " ++) . show)
		       >>>
		       traceMsg 3 ("recursion list = " ++ show recl)
		     )
		     &&&
		     getUserState
		   ) >>>
		   arr2 substA
		 )
	  `orElse` this
	  where
	  substA	:: String -> GEEnv -> GEArrow XmlTree XmlTree
	  substA entity geEnv
	      = maybe entityNotFound entityFound . lookupGeEnv entity $ geEnv
	      where
	      errMsg msg
		  = issueErr msg

	      entityNotFound
		  = errMsg ("general entity reference \"&" ++ entity ++ ";\" not processed, no definition found, (forward reference?)")

	      entityFound fct
		  | entity `elem` recl
		      = errMsg ("general entity reference \"&" ++ entity ++ ";\" not processed, cyclic definition")
		  | otherwise
		      = fct context recl

    substExternalParsed1Time				:: String -> String -> GESubstArrow
    substExternalParsed1Time uri entity cx rl
 	= perform ( traceMsg 2 ("substExternalParsed1Time: read and parse external parsed entity " ++ show entity)
		    >>>
		    runInLocalURIContext ( root [sattr a_source uri] []		-- uri must be an absolute uri
					   >>>					-- abs uri is computed during parameter entity handling
					   listA ( getXmlEntityContents
						   >>>
						   processExternalEntityContents
						 )
					 )
		    >>>
		    applyA ( arr $ \ ts -> addEntity (substExternalParsed ts) entity )
		  )
	  >>>
	  processGeneralEntity cx rl
	where
	processExternalEntityContents	:: IOStateArrow s XmlTree XmlTree
	processExternalEntityContents
	    = ( ( documentStatusOk				-- reading entity succeeded
		  >>>						-- with content stored in a text node
		  (getChildren >>> isText)
		)
		`guards`
		( getChildren
		  >>>
		  parseXmlGeneralEntityValue ("external parsed entity " ++ show entity)
		  >>>
		  filterErrorMsg
		)
	      )
	      `orElse`
	      issueErr ("illegal value for external parsed entity " ++ show entity)

    substExternalParsed					:: XmlTrees -> String -> GESubstArrow
    substExternalParsed	ts entity ReferenceInContent rl	= includedIfValidating ts rl entity
    substExternalParsed	_  entity ReferenceInAttributeValue _
                                                        = forbidden entity "external parsed general" "in attribute value"
    substExternalParsed	_  _      ReferenceInEntityValue _
                                                        = bypassed

    substInternal					:: XmlTrees -> String -> GESubstArrow
    substInternal ts entity ReferenceInContent rl	= included          ts rl entity
    substInternal ts entity ReferenceInAttributeValue rl= includedInLiteral ts rl entity
    substInternal _  _      ReferenceInEntityValue _	= bypassed

    substUnparsed					:: String -> GESubstArrow
    substUnparsed entity ReferenceInContent        _	= forbidden entity "unparsed" "content"
    substUnparsed entity ReferenceInAttributeValue _	= forbidden entity "unparsed" "attribute value"
    substUnparsed entity ReferenceInEntityValue    _	= forbidden entity "unparsed" "entity value"

									-- XML 1.0 chapter 4.4.2
    included		:: XmlTrees -> RecList -> String -> GEArrow XmlTree XmlTree
    included ts rl entity
	= arrL (const ts)
	  >>>
	  processGeneralEntity context (entity : rl)

									-- XML 1.0 chapter 4.4.3
    includedIfValidating		:: XmlTrees -> RecList -> String -> GEArrow XmlTree XmlTree
    includedIfValidating
	= included

									-- XML 1.0 chapter 4.4.4
    forbidden		:: String -> String -> String -> GEArrow XmlTree XmlTree
    forbidden entity msg cx
 	= issueErr ("reference of " ++ msg ++ show entity ++ " forbidden in " ++ cx)

									-- XML 1.0 chapter 4.4.5
    includedInLiteral		:: XmlTrees -> RecList -> String -> GEArrow XmlTree XmlTree
    includedInLiteral
	= included

									-- XML 1.0 chapter 4.4.7
    bypassed		:: GEArrow XmlTree XmlTree
    bypassed
	= this

-- ------------------------------------------------------------
