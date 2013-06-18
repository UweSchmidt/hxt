-- ------------------------------------------------------------

module Text.XML.HXT.Monad.DTDProcessing
    ( processDTD )
where

import           Control.Monad.Arrow

import qualified Data.Map                           as M (Map, empty, insert,
                                                          lookup)
import           Data.Maybe

import           Text.XML.HXT.DOM.Interface
import qualified Text.XML.HXT.DOM.XmlNode           as XN
import           Text.XML.HXT.Monad.ArrowXml
import           Text.XML.HXT.Monad.DocumentInput   (getXmlEntityContents)
import           Text.XML.HXT.Monad.Edit            (transfCharRef)
import           Text.XML.HXT.Monad.ParserInterface (parseXmlDTDEntityValue,
                                                     parseXmlDTDPart,
                                                     parseXmlDTDdecl,
                                                     parseXmlDTDdeclPart)
import           Text.XML.HXT.Monad.XmlState

-- ------------------------------------------------------------
--

data DTDPart            = Internal
                        | External
                          deriving (Eq)

type RecList            = [String]

type DTDStateArrow b c  = IOStateArrow PEEnv b c

-- ------------------------------------------------------------

newtype PEEnv           = PEEnv (M.Map String XmlTree)

emptyPeEnv      :: PEEnv
emptyPeEnv      = PEEnv M.empty

lookupPeEnv     :: String -> PEEnv -> Maybe XmlTree
lookupPeEnv k (PEEnv env)
    = M.lookup k env

addPeEntry      :: String -> XmlTree -> PEEnv -> PEEnv
addPeEntry k a (PEEnv env)
    = PEEnv $ M.insert k a env

getPeValue      :: DTDStateArrow String XmlTree
getPeValue
    = (this &=& getUserState)
      >=>
      arrL (\ (n, env) -> maybeToList . lookupPeEnv n $ env)

addPe           :: String -> DTDStateArrow XmlTree XmlTree
addPe n
    = traceMsg 2 ("substParamEntity: add entity " ++ show n ++ " to env")
      >=>
      changeUserState ins
    where
    ins t peEnv = addPeEntry n t peEnv

-- ------------------------------------------------------------

-- |
-- a filter for DTD processing
--
-- inclusion of external parts of DTD,
-- parameter entity substitution
-- conditional section evaluation
--
-- input tree must represent a complete document including root node

processDTD              :: IOStateArrow s XmlTree XmlTree
processDTD
    = runInLocalURIContext
         ( processRoot
           >=>
           traceTree
           >=>
           traceSource
         )
      `whenA` ( isRoot >=> getChildren )
      where

      processRoot       :: IOStateArrow s XmlTree XmlTree
      processRoot
          = ( traceMsg 1 ("processDTD: process parameter entities")
              >=>
              setSysAttrString a_standalone ""
              >=>
              processChildren substParamEntities
              >=>
              setDocumentStatusFromSystemState "in XML DTD processing"
              >=>
              traceMsg 1 ("processDTD: parameter entities processed")
            )
            `whenA`
            documentStatusOk

substParamEntities      :: IOStateArrow s XmlTree XmlTree
substParamEntities
    = withOtherUserState emptyPeEnv processParamEntities
      `whenA`
      isDTDDoctype
      where

      processParamEntities      :: DTDStateArrow XmlTree XmlTree
      processParamEntities
          = mergeEntities $<<< ( listA processPredef
                                 &=&
                                 listA processInt
                                 &=&
                                 listA (runInLocalURIContext processExt)
                               )
          where
          mergeEntities dtdPre dtdInt dtdExt
              =  replaceChildren (arrL $ const $ foldl1 mergeDTDs [dtdPre, dtdInt, dtdExt])

          processPredef
              = predefDTDPart   >=> substParamEntity Internal []

          processInt
              = getChildren     >=> substParamEntity Internal []

          processExt
              = externalDTDPart >=> substParamEntity External []

          mergeDTDs     :: XmlTrees -> XmlTrees -> XmlTrees
          mergeDTDs dtdInt dtdExt
              = dtdInt ++ (filter (filterDTDNodes dtdInt) dtdExt)

          filterDTDNodes        :: XmlTrees -> XmlTree -> Bool
          filterDTDNodes dtdPart t
              = not (any (filterDTDNode t) dtdPart)

          filterDTDNode :: XmlTree -> XmlTree -> Bool

          filterDTDNode t1 t2
              = fromMaybe False $
                do
                dp1 <- XN.getDTDPart t1
                dp2 <- XN.getDTDPart t2
                al1 <- XN.getDTDAttrl t1
                al2 <- XN.getDTDAttrl t2
                return ( dp1 == dp2
                         &&
                         ( dp1 `elem` [ELEMENT, NOTATION, ENTITY, ATTLIST] )
                         &&
                         ( lookup a_name al1 == lookup a_name al2 )
                         &&
                         ( dp1 /= ATTLIST
                           ||
                           lookup a_value al1 == lookup a_value al2
                         )
                       )

substParamEntity        :: DTDPart -> RecList -> DTDStateArrow XmlTree XmlTree
substParamEntity loc recList
    = choiceA
      [ isDTDEntity     :-> ( traceDTD "ENTITY declaration before DTD declaration parsing"
                              >=>
                              processChildren (substPeRefsInDTDdecl recList)
                              >=>
                              parseXmlDTDdecl
                              >=>
                              substPeRefsInEntityValue
                              >=>
                              traceDTD "ENTITY declaration after PE substitution"
                              >=>
                              processEntityDecl
                              >=>
                              traceDTD "ENTITY declaration after DTD declaration parsing"
                            )
      , ( isDTDElement
          <++>
          isDTDAttlist
          <++>
          isDTDNotation
        )               :-> ( traceDTD "DTD declaration before PE substitution"
                              >=>
                              processChildren (substPeRefsInDTDdecl recList)
                              >=>
                              parseXmlDTDdecl
                              >=>
                              traceDTD "DTD declaration after DTD declaration parsing"
                            )
      , isDTDPERef      :-> substPeRefsInDTDpart recList

      , isDTDCondSect   :-> ( if loc == Internal
                              then issueErr "conditional sections in internal part of the DTD is not allowed"
                              else evalCondSect $< getDTDAttrValue a_value
                            )
      , isCmt           :-> none
      , this            :-> this
      ]
    where
    processEntityDecl           :: DTDStateArrow XmlTree XmlTree
    processEntityDecl
        = choiceA
          [ isDTDEntity :-> ( ifA (hasDTDAttr k_system)
                              processExternalEntity
                              processInternalEntity
                            )
          , isDTDPEntity
                        :-> ( processParamEntity $< getDTDAttrValue a_name )
          , this        :-> none
          ]
        where
        processExternalEntity   :: DTDStateArrow XmlTree XmlTree        -- processing external entities is delayed until first usage
        processExternalEntity                                           -- only the current base uri must be remembered
            = setDTDAttrValue a_url $< ( getDTDAttrValue k_system >=> mkAbsURI )

        processInternalEntity   :: DTDStateArrow XmlTree XmlTree
        processInternalEntity
            = this                                                      -- everything is already done in substPeRefsInEntityValue

        processParamEntity      :: String -> DTDStateArrow XmlTree XmlTree
        processParamEntity peName
            = ifA (constA peName >=> getPeValue)
              ( issueWarn ("parameter entity " ++ show peName ++ " already defined")
                >=>
                none                                                    -- second def must be ignored
              )
              ( ( ifA ( hasDTDAttr k_system )                           -- is external param entity ?
                  ( setDTDAttrValue a_url $<                            -- store absolut url
                    ( getDTDAttrValue k_system >=> mkAbsURI )
                  )
                  -- this is too early, pe may be not referenced and file may be not there
                  -- ( runInLocalURIContext getExternalParamEntityValue )
                  ( this )                                              -- everything is already done in substPeRefsInEntityValue
                )
                >=>
                addPe peName
              )

    substPERef                  :: String -> DTDStateArrow XmlTree XmlTree
    substPERef pn
        = choiceA
          [ isUndefinedRef      :-> issueErr ("parameter entity " ++ show pn ++ " not found (forward reference?)")
          , isInternalRef       :-> issueErr ("a parameter entity reference of " ++ show pn ++ " occurs in the internal subset of the DTD")
          , isUnreadExternalRef :-> ( perform
                                      ( peVal                           -- load the external pe value
                                        >=>                             -- update the pe env
                                        getExternalParamEntityValue pn  -- and try again
                                        >=>
                                        addPe pn
                                      )
                                      >=>
                                      substPERef pn
                                    )
          , this                :-> substPE
          ]
          `whenA`
          isDTDPERef
        where
        peVal                   = constA pn >=> getPeValue

        isUnreadExternalRef     = ( peVal
                                    >=>
                                    getDTDAttrValue a_url
                                    >=>
                                    isA (not . null)
                                  )
                                  `guards`
                                  this

        isInternalRef   = none -- isA (const (loc == Internal))         -- TODO: check this restriction, it seams rather meaningless
        isUndefinedRef  = neg peVal
        substPE         = replaceChildren (peVal >=> getChildren)       -- store PE value in children component

    substPeRefsInEntityValue      :: DTDStateArrow XmlTree XmlTree
    substPeRefsInEntityValue
        = ( ( replaceChildren
              ( xshow ( getChildren                                     -- substitute char entites
                        >=>                                             -- and parameter references
                        transfCharRef                                   -- combine all pieces to a single string
                        >=>                                             -- as the new entity value
                        substPeRefsInValue []
                      )
                >=>
                mkText
              )
            )
            `whenNot`
            hasDTDAttr k_system                                         -- only apply for internal entities
          )
          `whenA`
          ( isDTDEntity <++> isDTDPEntity )                              -- only apply for entity declarations

    substPeRefsInDTDpart        :: RecList -> DTDStateArrow XmlTree XmlTree
    substPeRefsInDTDpart rl
        = recursionCheck "DTD part" rl subst
        where
        subst   :: RecList -> String -> DTDStateArrow XmlTree XmlTree
        subst recl pn
            = substPERef pn
              >=>
              traceDTD "substPeRefsInDTDdecl: before parseXmlDTDPart"
              >=>
              ( runInPeContext ( getChildren
                                 >=>
                                 ( (constA ("parameter entity: " ++ pn)) &=& this )
                                 >=>
                                 parseXmlDTDPart
                                 >=>
                                 traceDTD "substPeRefsInDTDpart: after parseXmlDTDPart"
                                 >=>
                                 substParamEntity loc (pn : recl)
                               )
                `whenA`
                isDTDPERef
              )

    substPeRefsInDTDdecl        :: RecList -> DTDStateArrow XmlTree XmlTree
    substPeRefsInDTDdecl rl
        = recursionCheck "DTD declaration" rl subst
        where
        subst   :: RecList -> String -> DTDStateArrow XmlTree XmlTree
        subst recl pn
            = substPERef pn
              >=>
              traceDTD "substPeRefsInDTDdecl: before parseXmlDTDdeclPart"
              >=>
              ( runInPeContext ( parseXmlDTDdeclPart
                                 >=>
                                 traceDTD "substPeRefsInDTDdecl: after parseXmlDTDdeclPart"
                                 >=>
                                 processChildren ( substPeRefsInDTDdecl (pn : recl) )
                               )
                `whenA`
                isDTDPERef
              )

    substPeRefsInValue          :: RecList -> DTDStateArrow XmlTree XmlTree
    substPeRefsInValue rl
        = recursionCheck "entity value" rl subst
        where
        subst   :: RecList -> String -> DTDStateArrow XmlTree XmlTree
        subst recl pn
            = substPERef pn
              >=>
              parseXmlDTDEntityValue
              >=>
              -- transfCharRef             this must be done somewhere else
              -- >=>
              substPeRefsInValue (pn : recl)

    substPeRefsInCondSect       :: RecList -> DTDStateArrow XmlTree XmlTree
    substPeRefsInCondSect rl
        = recursionCheck "conditional section" rl subst
        where
        subst   :: RecList -> String -> DTDStateArrow XmlTree XmlTree
        subst recl pn
            = substPERef pn
              >=>
              traceDTD "substPeRefsInCondSect: parseXmlDTDdeclPart"
              >=>
              runInPeContext ( parseXmlDTDdeclPart
                               >=>
                               traceDTD "substPeRefsInCondSect: after parseXmlDTDdeclPart"
                               >=>
                               processChildren ( substPeRefsInCondSect (pn : recl) )
                             )

    recursionCheck      :: String -> RecList -> (RecList -> String -> DTDStateArrow XmlTree XmlTree) -> DTDStateArrow XmlTree XmlTree
    recursionCheck wher rl subst
        = ( recusiveSubst  $< getDTDAttrValue a_peref )
          `whenA`
          isDTDPERef
        where
        recusiveSubst name
            | name `elem` rl
                = issueErr ("recursive call of parameter entity " ++ show name ++ " in " ++ wher)
            | otherwise
                = subst rl name

    runInPeContext      :: DTDStateArrow XmlTree XmlTree -> DTDStateArrow XmlTree XmlTree
    runInPeContext f
        = ( runWithNewBase $< getDTDAttrValue a_url )
          `orElse`
          f
        where
        runWithNewBase base
            = runInLocalURIContext
              ( perform (constA base >=> setBaseURI)
                >=>
                f
              )

    evalCondSect        :: String ->  DTDStateArrow XmlTree XmlTree
    evalCondSect content
        = traceDTD "evalCondSect: process conditional section"
          >=>
          processChildren (substPeRefsInCondSect [])
          >=>
          parseXmlDTDdecl
          >=>
          ( hasText (== k_include)
            `guards`
            ( ( constA "conditional section" &=& txt content )
              >=>
              parseXmlDTDPart
              >=>
              traceMsg 2 "evalCond: include DTD part"
              >=>
              substParamEntity External recList
            )
          )

predefDTDPart           :: DTDStateArrow XmlTree XmlTree
predefDTDPart
    = ( constA "predefined entities"
        &=&
        ( constA predefinedEntities >=> mkText)
      )
      >=>
      parseXmlDTDPart
    where
    predefinedEntities  :: String
    predefinedEntities
        = concat [ "<!ENTITY lt   '&#38;#60;'>"
                 , "<!ENTITY gt   '&#62;'>"
                 , "<!ENTITY amp  '&#38;#38;'>"
                 , "<!ENTITY apos '&#39;'>"
                 , "<!ENTITY quot '&#34;'>"
                 ]

externalDTDPart         :: DTDStateArrow XmlTree XmlTree
externalDTDPart
    = isDTDDoctype
      `guards`
      ( hasDTDAttr k_system
        `guards`
        ( getExternalDTDPart $< getDTDAttrValue k_system )
      )

getExternalDTDPart      :: String -> DTDStateArrow XmlTree XmlTree
getExternalDTDPart src
    = root [sattr a_source src] []
      >=>
      getXmlEntityContents
      >=>
      replaceChildren ( ( constA src &=& getChildren )
                        >=>
                        parseXmlDTDPart
                      )
      >=>
      traceDoc "processExternalDTD: parsing DTD part done"
      >=>
      getChildren

getExternalParamEntityValue     :: String -> DTDStateArrow XmlTree XmlTree
getExternalParamEntityValue pn
    = isDTDPEntity
      `guards`
      ( setEntityValue $< ( listA ( getEntityValue $< getDTDAttrValue a_url ) ) )
    where
    getEntityValue      :: String -> DTDStateArrow XmlTree XmlTree
    getEntityValue url
        = root [sattr a_source url] []
          >=>
          runInLocalURIContext getXmlEntityContents
          >=>
          traceMsg 2 ("getExternalParamEntityValue: contents read for " ++ show pn ++ " from " ++ show url)
          >=>
          getChildren

    setEntityValue      :: XmlTrees -> DTDStateArrow XmlTree XmlTree
    setEntityValue res
        | null res
            = issueErr ("illegal external parameter entity value for entity %" ++ pn ++";")
        | otherwise
            = replaceChildren (constL res)
              >=>
              setDTDAttrValue a_url ""                          -- mark entity as read

traceDTD        :: String -> DTDStateArrow XmlTree XmlTree
traceDTD msg    = traceMsg 3 msg >=> traceTree

-- ------------------------------------------------------------


