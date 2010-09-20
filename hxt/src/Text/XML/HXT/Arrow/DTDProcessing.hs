-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.DTDProcessing
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   DTD processing function for
   including external parts of a DTD
   parameter entity substitution and general entity substitution

   Implemtation completely done with arrows

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.DTDProcessing
    ( processDTD
    )
where

import Control.Arrow                            -- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import           Text.XML.HXT.DOM.Interface
import qualified Text.XML.HXT.DOM.XmlNode as XN

import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState

import Text.XML.HXT.Arrow.ParserInterface
    ( parseXmlDTDdecl
    , parseXmlDTDdeclPart
    , parseXmlDTDEntityValue
    , parseXmlDTDPart
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
    , insert
    )

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
    = (this &&& getUserState)
      >>>
      arrL (\ (n, env) -> maybeToList . lookupPeEnv n $ env)

addPe           :: String -> DTDStateArrow XmlTree XmlTree
addPe n
    = traceMsg 2 ("substParamEntity: add entity " ++ show n ++ " to env")
      >>>
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
           >>>
           traceTree
           >>>
           traceSource
         )
      `when` ( isRoot >>> getChildren )
      where

      processRoot       :: IOStateArrow s XmlTree XmlTree
      processRoot
          = ( traceMsg 1 ("processDTD: process parameter entities")
              >>>
              setSysAttrString a_standalone ""
              >>>
              ( addDocType
                `whenNot`
                ( getChildren >>> isDTDDoctype )
              )
              >>>
              processChildren substParamEntities
              >>>
              setDocumentStatusFromSystemState "in XML DTD processing"
            )
            `when`
            documentStatusOk
          where
          addDocType
              = replaceChildren ( (getChildren >>> isXmlPi)
                                  <+>
                                  mkDTDDoctype [] none
                                  <+>
                                  (getChildren >>> neg isXmlPi)
                                )

substParamEntities      :: IOStateArrow s XmlTree XmlTree
substParamEntities
    = withOtherUserState emptyPeEnv processParamEntities
      `when`
      isDTDDoctype
      where

      processParamEntities      :: DTDStateArrow XmlTree XmlTree
      processParamEntities
          = mergeEntities $<<< ( listA processPredef
                                 &&&
                                 listA processInt
                                 &&&
                                 listA (runInLocalURIContext processExt)
                               )
          where
          mergeEntities dtdPre dtdInt dtdExt
              =  replaceChildren (arrL $ const $ foldl1 mergeDTDs [dtdPre, dtdInt, dtdExt])

          processPredef
              = predefDTDPart   >>> substParamEntity Internal []

          processInt
              = getChildren     >>> substParamEntity Internal []

          processExt
              = externalDTDPart >>> substParamEntity External []

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
                              >>>
                              processChildren (substPeRefsInDTDdecl recList)
                              >>>
                              parseXmlDTDdecl
                              >>>
                              substRefsInEntityValue
                              >>>
                              processEntityDecl
                              >>>
                              traceDTD "ENTITY declaration after DTD declaration parsing"
                            )
      , ( isDTDElement
          <+>
          isDTDAttlist
          <+>
          isDTDNotation
        )               :-> ( traceDTD "DTD declaration before PE substitution"
                              >>>
                              processChildren (substPeRefsInDTDdecl recList)
                              >>>
                              parseXmlDTDdecl
                              >>>
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
            = setDTDAttrValue a_url $< ( getDTDAttrValue k_system >>> mkAbsURI )

        processInternalEntity   :: DTDStateArrow XmlTree XmlTree        -- just combine all parts of the entity value
        processInternalEntity                                           -- into one string
            = replaceChildren (xshow getChildren >>> mkText)

        processParamEntity      :: String -> DTDStateArrow XmlTree XmlTree
        processParamEntity peName
            = ifA (constA peName >>> getPeValue)
              ( issueWarn ("parameter entity " ++ show peName ++ " already defined") )
              ( ( ifA ( hasDTDAttr k_system )                           -- is external param entity ?
                  ( runInLocalURIContext getExternalParamEntityValue )
                  ( replaceChildren (xshow getChildren >>> mkText) )    -- just combine all parts of the entity value into one string
                )
                >>>
                addPe peName
              )

    substPERef                  :: String -> DTDStateArrow XmlTree XmlTree
    substPERef pn
        = choiceA
          [ isInternalRef       :-> issueErr ("a parameter entity reference of " ++ show pn ++ " occurs in the internal subset of the DTD")
          , isUndefinedRef      :-> issueErr ("parameter entity " ++ show pn ++ " not found (forward reference?)")
          , this                :-> substPE
          ]
          `when`
          isDTDPERef
        where
        isInternalRef   = isA (const (loc == Internal))
        peVal           = constA pn >>> getPeValue
        isUndefinedRef  = neg peVal
        substPE
            = replaceChildren (peVal >>> getChildren)                   -- store PE value in children component
              >>>
              ( ( setBase $< (peVal >>> getDTDAttrValue a_url) )        -- store base uri for external refs
                `orElse`
                this
              )
            where
            setBase uri = setDTDAttrValue a_url uri

    substRefsInEntityValue      :: DTDStateArrow XmlTree XmlTree
    substRefsInEntityValue
        = ( ( processChildren ( transfCharRef
                                >>>
                                substPeRefsInValue []
                              )
            )
            `whenNot`
            hasDTDAttr k_system                                         -- only apply for internal entities
          )
          `when`
          ( isDTDEntity <+> isDTDPEntity )                              -- only apply for entity declarations

    substPeRefsInDTDpart        :: RecList -> DTDStateArrow XmlTree XmlTree
    substPeRefsInDTDpart rl
        = recursionCheck "DTD part" rl subst
        where
        subst   :: RecList -> String -> DTDStateArrow XmlTree XmlTree
        subst recl pn
            = substPERef pn
              >>>
              traceDTD "substPeRefsInDTDdecl: before parseXmlDTDPart"
              >>>
              ( runInPeContext ( getChildren
                                 >>>
                                 ( (constA ("parameter entity: " ++ pn)) &&& this )
                                 >>>
                                 parseXmlDTDPart
                                 >>>
                                 traceDTD "substPeRefsInDTDpart: after parseXmlDTDPart"
                                 >>>
                                 substParamEntity loc (pn : recl)
                               )
                `when`
                isDTDPERef
              )

    substPeRefsInDTDdecl        :: RecList -> DTDStateArrow XmlTree XmlTree
    substPeRefsInDTDdecl rl
        = recursionCheck "DTD declaration" rl subst
        where
        subst   :: RecList -> String -> DTDStateArrow XmlTree XmlTree
        subst recl pn
            = substPERef pn
              >>>
              traceDTD "substPeRefsInDTDdecl: before parseXmlDTDdeclPart"
              >>>
              ( runInPeContext ( parseXmlDTDdeclPart
                                 >>>
                                 traceDTD "substPeRefsInDTDdecl: after parseXmlDTDdeclPart"
                                 >>>
                                 processChildren ( substPeRefsInDTDdecl (pn : recl) )
                               )
                `when`
                isDTDPERef
              )

    substPeRefsInValue          :: RecList -> DTDStateArrow XmlTree XmlTree
    substPeRefsInValue rl
        = recursionCheck "entity value" rl subst
        where
        subst   :: RecList -> String -> DTDStateArrow XmlTree XmlTree
        subst recl pn
            = substPERef pn
              >>>
              parseXmlDTDEntityValue
              >>>
              transfCharRef
              >>>
              substPeRefsInValue (pn : recl)

    substPeRefsInCondSect       :: RecList -> DTDStateArrow XmlTree XmlTree
    substPeRefsInCondSect rl
        = recursionCheck "conditional section" rl subst
        where
        subst   :: RecList -> String -> DTDStateArrow XmlTree XmlTree
        subst recl pn
            = substPERef pn
              >>>
              traceDTD "substPeRefsInCondSect: parseXmlDTDdeclPart"
              >>>
              runInPeContext ( parseXmlDTDdeclPart
                               >>>
                               traceDTD "substPeRefsInCondSect: after parseXmlDTDdeclPart"
                               >>>
                               processChildren ( substPeRefsInCondSect (pn : recl) )
                             )

    recursionCheck      :: String -> RecList -> (RecList -> String -> DTDStateArrow XmlTree XmlTree) -> DTDStateArrow XmlTree XmlTree
    recursionCheck wher rl subst
        = ( recusiveSubst  $< getDTDAttrValue a_peref )
          `when`
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
              ( perform (constA base >>> setBaseURI)
                >>>
                f
              )

    evalCondSect        :: String ->  DTDStateArrow XmlTree XmlTree
    evalCondSect content
        = traceDTD "evalCondSect: process conditional section"
          >>>
          processChildren (substPeRefsInCondSect [])
          >>>
          parseXmlDTDdecl
          >>>
          ( hasText (== k_include)
            `guards`
            ( ( constA "conditional section" &&& txt content )
              >>>
              parseXmlDTDPart
              >>>
              traceMsg 2 "evalCond: include DTD part"
              >>>
              substParamEntity External recList
            )
          )

predefDTDPart           :: DTDStateArrow XmlTree XmlTree
predefDTDPart
    = ( constA "predefined entities"
        &&&
        ( constA predefinedEntities >>> mkText)
      )
      >>>
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
      >>>
      getXmlEntityContents
      >>>
      replaceChildren ( ( constA src &&& getChildren )
                        >>>
                        parseXmlDTDPart
                      )
      >>>
      traceDoc "processExternalDTD: parsing DTD part done"
      >>>
      getChildren

getExternalParamEntityValue     :: DTDStateArrow XmlTree XmlTree
getExternalParamEntityValue
    = isDTDPEntity
      `guards`
      ( setEntityValue $<<< ( getDTDAttrl
                              &&&
                              listA ( getEntityValue $< getDTDAttrl )
                              &&&
                              getBaseURI
                            )
      )
    where
    getEntityValue      :: Attributes -> DTDStateArrow XmlTree XmlTree
    getEntityValue al
        = root [sattr a_source (lookup1 k_system al){- <+> catA (map (uncurry sattr) al)-}] []
          >>>
          getXmlEntityContents
          >>>
          traceMsg 2 "getExternalParamEntityValue: contents read"
          >>>
          getChildren

    setEntityValue      :: Attributes -> XmlTrees -> String -> DTDStateArrow XmlTree XmlTree
    setEntityValue al res base
        | null res
            = issueErr ("illegal external parameter entity value for entity %" ++ peName ++";")
        | otherwise
            = mkDTDElem PENTITY ((a_url, base) : al) (arrL $ const res)
        where
        peName = lookup1 a_name al

traceDTD        :: String -> DTDStateArrow XmlTree XmlTree
traceDTD msg    = traceMsg 3 msg >>> traceTree

-- ------------------------------------------------------------


