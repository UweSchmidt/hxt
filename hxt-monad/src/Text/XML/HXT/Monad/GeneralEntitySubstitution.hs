-- ------------------------------------------------------------

module Text.XML.HXT.Monad.GeneralEntitySubstitution
    ( processGeneralEntities )
where

import           Control.Monad.Arrow

import           Text.XML.HXT.DOM.Interface

import           Text.XML.HXT.Monad.ArrowXml
import           Text.XML.HXT.Monad.DocumentInput   (getXmlEntityContents)
import           Text.XML.HXT.Monad.Edit            (transfCharRef)
import           Text.XML.HXT.Monad.ParserInterface (parseXmlEntityValueAsAttrValue, parseXmlEntityValueAsContent)
import           Text.XML.HXT.Monad.XmlState

import qualified Data.Map                           as M (Map, empty, insert,
                                                          lookup)

-- ------------------------------------------------------------

data GEContext
    = ReferenceInContent
    | ReferenceInAttributeValue
    | ReferenceInEntityValue
--  | OccursInAttributeValue                -- not used during substitution but during validation
--  | ReferenceInDTD                        -- not used: syntax check detects errors

type GESubstArrow       = GEContext -> RecList -> GEArrow XmlTree XmlTree

type GEArrow b c        = IOStateArrow GEEnv b c

type RecList            = [String]

-- ------------------------------------------------------------

newtype GEEnv   = GEEnv (M.Map String GESubstArrow)

emptyGeEnv      :: GEEnv
emptyGeEnv      = GEEnv M.empty

lookupGeEnv     :: String -> GEEnv -> Maybe GESubstArrow
lookupGeEnv k (GEEnv env)
    = M.lookup k env

addGeEntry      :: String -> GESubstArrow -> GEEnv -> GEEnv
addGeEntry k a (GEEnv env)
    = GEEnv $ M.insert k a env

-- ------------------------------------------------------------

-- |
-- substitution of general entities
--
-- input: a complete document tree including root node

processGeneralEntities  :: IOStateArrow s XmlTree XmlTree
processGeneralEntities
    = ( traceMsg 1 "processGeneralEntities: collect and substitute general entities"
        >=>
        withOtherUserState emptyGeEnv (processChildren (processGeneralEntity ReferenceInContent []))
        >=>
        setDocumentStatusFromSystemState "in general entity processing"
        >=>
        traceTree
        >=>
        traceSource
      )
      `when`
      documentStatusOk


processGeneralEntity    :: GESubstArrow
processGeneralEntity context recl
    = choiceA [ isElem          :-> ( processAttrl (processChildren substEntitiesInAttrValue)
                                      >=>
                                      processChildren (processGeneralEntity context recl)
                                    )
              , isEntityRef     :-> substEntityRef
              , isDTDDoctype    :-> processChildren (processGeneralEntity context recl)
              , isDTDEntity     :-> addEntityDecl
              , isDTDAttlist    :-> substEntitiesInAttrDefaultValue
              , this            :-> this
              ]
    where
    addEntityDecl       :: GEArrow XmlTree XmlTree
    addEntityDecl
        = perform ( choiceA [ isIntern          :-> addInternalEntity           -- don't change sequence of cases
                            , isExtern          :-> addExternalEntity
                            , isUnparsed        :-> addUnparsedEntity
                            ]
                  )
        where
        isIntern        = none `when` hasDTDAttr k_system
        isExtern        = none `when` hasDTDAttr k_ndata
        isUnparsed      = this

    addInternalEntity   :: GEArrow XmlTree b
    addInternalEntity
        = insertInternal $<<
          ( ( getDTDAttrValue a_name
              >=>
              traceValue 2 (("processGeneralEntity: general entity definition for " ++) . show)
            )
            &=&
            xshow (getChildren >=> isText)
          )
        where
        insertInternal entity contents
            = insertEntity (substInternal contents) entity
              >=>
              none

    addExternalEntity   :: GEArrow XmlTree b
    addExternalEntity
        = insertExternal $<<
          ( ( getDTDAttrValue a_name
              >=>
              traceValue 2 (("processGeneralEntity: external entity definition for " ++) . show)
            )
            &=&
            getDTDAttrValue a_url                       -- the absolute URL, not the relative in attr: k_system
          )
        where
        insertExternal entity uri
            = insertEntity (substExternalParsed1Time uri) entity
              >=>
              none

    addUnparsedEntity   :: GEArrow XmlTree b
    addUnparsedEntity
        = getDTDAttrValue a_name
          >=>
          traceValue 2 (("processGeneralEntity: unparsed entity definition for " ++) . show)
          >=>
          applyA (return . insertEntity substUnparsed)
          >=>
          none

    insertEntity        :: (String -> GESubstArrow) -> String -> GEArrow b b
    insertEntity fct entity
        = ( getUserState
            >=>
            applyA (return . checkDefined)
          )
          `guards`
          addEntity fct entity
        where
        checkDefined geEnv
            = maybe ok alreadyDefined . lookupGeEnv entity $ geEnv
            where
            ok  = this
            alreadyDefined _
                = issueWarn ("entity " ++ show entity ++ " already defined, repeated definition ignored")
                  >=>
                  none

    addEntity   :: (String -> GESubstArrow) -> String -> GEArrow b b
    addEntity fct entity
        = changeUserState ins
        where
        ins _ geEnv = addGeEntry entity (fct entity) geEnv

    substEntitiesInAttrDefaultValue     :: GEArrow XmlTree XmlTree
    substEntitiesInAttrDefaultValue
        = applyA ( xshow ( getDTDAttrValue a_default                    -- parse the default value
                           >=>                                          -- substitute entities
                           mkText                                       -- and convert value into a string
                           >=>
                           parseXmlEntityValueAsAttrValue "default value of attribute"
                           >=>
                           filterErrorMsg
                           >=>
                           substEntitiesInAttrValue
                         )
                   >=> return . setDTDAttrValue a_default
                 )
          `when` hasDTDAttr a_default

    substEntitiesInAttrValue    :: GEArrow XmlTree XmlTree
    substEntitiesInAttrValue
        = ( processGeneralEntity ReferenceInAttributeValue recl
            `when`
            isEntityRef
          )
          >=>
          changeText normalizeWhiteSpace
          >=>
          transfCharRef
        where
        normalizeWhiteSpace = map ( \c -> if c `elem` "\n\t\r" then ' ' else c )


    substEntityRef      :: GEArrow XmlTree XmlTree
    substEntityRef
        = applyA ( ( ( getEntityRef                             -- get the entity name and the env
                       >=>                                      -- and compute the arrow to be applied
                       traceValue 2 (("processGeneralEntity: entity reference for entity " ++) . show)
                       >=>
                       traceMsg 3 ("recursion list = " ++ show recl)
                     )
                     &=&
                     getUserState
                   ) >=>
                   arr2 substA
                 )
          where
          substA        :: String -> GEEnv -> GEArrow XmlTree XmlTree
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

    substExternalParsed1Time                            :: String -> String -> GESubstArrow
    substExternalParsed1Time uri entity cx rl
        = perform ( traceMsg 2 ("substExternalParsed1Time: read and parse external parsed entity " ++ show entity)
                    >=>
                    runInLocalURIContext ( root [sattr a_source uri] []         -- uri must be an absolute uri
                                           >=>                                  -- abs uri is computed during parameter entity handling
                                           getXmlEntityContents
                                           >=>
                                           processExternalEntityContents
                                         )
                    >=>
                    applyA ( return . \ s -> addEntity (substExternalParsed s) entity )
                  )
          >=>
          processGeneralEntity cx rl
        where
        processExternalEntityContents   :: IOStateArrow s XmlTree String
        processExternalEntityContents
            = ( ( ( documentStatusOk                              -- reading entity succeeded
                    >=>                                           -- with content stored in a text node
                    (getChildren >=> isText)
                  )
                  `guards`
                  this
                )
                `orElse`
                issueErr ("illegal value for external parsed entity " ++ show entity)
              )
              >=>
              xshow (getChildren >=> isText)


    substExternalParsed                                 :: String -> String -> GESubstArrow
    substExternalParsed s entity ReferenceInContent rl  = includedIfValidating s rl entity
    substExternalParsed _ entity ReferenceInAttributeValue _
                                                        = forbidden entity "external parsed general" "in attribute value"
    substExternalParsed _ _      ReferenceInEntityValue _
                                                        = bypassed

    substInternal                                       :: String -> String -> GESubstArrow
    substInternal s entity ReferenceInContent rl        = included          s rl entity
    substInternal s entity ReferenceInAttributeValue rl = includedInLiteral s rl entity
    substInternal _ _      ReferenceInEntityValue _     = bypassed

    substUnparsed                                       :: String -> GESubstArrow
    substUnparsed entity ReferenceInContent        _    = forbidden entity "unparsed" "content"
    substUnparsed entity ReferenceInAttributeValue _    = forbidden entity "unparsed" "attribute value"
    substUnparsed entity ReferenceInEntityValue    _    = forbidden entity "unparsed" "entity value"

                                                                        -- XML 1.0 chapter 4.4.2
    included            :: String -> RecList -> String -> GEArrow XmlTree XmlTree
    included s rl entity
        = traceMsg 3 ("substituting general entity " ++ show entity ++ " with value " ++ show s)
          >=>
          txt s
          >=>
          parseXmlEntityValueAsContent ("substituting general entity " ++ show entity ++ " in contents")
          >=>
          filterErrorMsg
          >=>
          processGeneralEntity context (entity : rl)

                                                                        -- XML 1.0 chapter 4.4.3
    includedIfValidating                :: String -> RecList -> String -> GEArrow XmlTree XmlTree
    includedIfValidating
        = included
                                                                        -- XML 1.0 chapter 4.4.4
    forbidden           :: String -> String -> String -> GEArrow XmlTree XmlTree
    forbidden entity msg cx
        = issueErr ("reference of " ++ msg ++ show entity ++ " forbidden in " ++ cx)

                                                                        -- XML 1.0 chapter 4.4.5
    includedInLiteral           :: String -> RecList -> String -> GEArrow XmlTree XmlTree
    includedInLiteral s rl entity
        = txt s
          >=>
          parseXmlEntityValueAsAttrValue ("substituting general entity " ++ show entity ++ " in attribute value")
          >=>
          filterErrorMsg
          >=>
          processGeneralEntity context (entity : rl)
                                                                        -- XML 1.0 chapter 4.4.7
    bypassed            :: GEArrow XmlTree XmlTree
    bypassed
        = this

-- ------------------------------------------------------------
