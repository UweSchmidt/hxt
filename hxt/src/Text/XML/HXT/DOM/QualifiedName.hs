{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DOM.QualifiedName
   Copyright  : Copyright (C) 2011 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   The types and functions for qualified names

-}

-- ------------------------------------------------------------

module Text.XML.HXT.DOM.QualifiedName
    ( QName
    , XName(unXN)
    , NsEnv

    , mkQName
    , mkName
    , mkNsName
    , mkSNsName
    , mkPrefixLocalPart

    , equivQName
    , equivUri
    , equalQNameBy

    , namePrefix
    , localPart
    , namespaceUri

    , newXName
    , nullXName
    , isNullXName
    , newQName

    , mkQName'
    , namePrefix'
    , localPart'
    , namespaceUri'

    , setNamePrefix'
    , setLocalPart'
    , setNamespaceUri'

    , qualifiedName
    , qualifiedName'
    , universalName
    , universalUri
    , buildUniversalName

    , normalizeNsUri

    , setNamespace                      -- namespace related functions
    , isNCName
    , isWellformedQualifiedName
    , isWellformedQName
    , isWellformedNSDecl
    , isWellformedNameSpaceName
    , isNameSpaceName
    , isDeclaredNamespace

    , xmlNamespaceXName
    , xmlXName
    , xmlnsNamespaceXName
    , xmlnsXName
    , xmlnsQN

    , toNsEnv
    )

where

{-
import           Debug.Trace
 -}

import           Control.Arrow                  ( (***) )

import           Control.DeepSeq
import           Control.FlatSeq

import           Data.AssocList
import           Data.Binary
import           Data.Char                      ( toLower )
import           Data.IORef
import           Data.List                      ( isPrefixOf )
import qualified Data.Map               as M
import           Data.Typeable

import           System.IO.Unsafe               ( unsafePerformIO )

import           Text.XML.HXT.DOM.XmlKeywords   ( a_xml
                                                , a_xmlns
                                                , xmlNamespace
                                                , xmlnsNamespace
                                                )

import Data.Char.Properties.XMLCharProps        ( isXmlNCNameStartChar
                                                , isXmlNCNameChar
                                                )

-- -----------------------------------------------------------------------------

-- | XML names are represented by Strings, but these strings do not mix up with normal strings.
-- Names are always reduced to normal form, and they are stored internally in a name cache
-- for sharing equal names by the same data structure

data XName                      = XN { _idXN    :: ! Int        -- for optimization of equality test, see Eq instance
                                     ,  unXN    ::   String
                                     }
                                  deriving (Typeable)

instance Eq XName where
    (XN id1 _) == (XN id2 _)    = id1 == id2

instance Ord XName where
    compare (XN _ n1) (XN _ n2) = compare n1 n2
{-
instance Read XName where
    readsPrec p str             = [ (newXName x, y) | (x, y) <- readsPrec p str ]

instance Show XName where
    show (XN _ s)               = show s
-}
instance NFData XName where
    rnf (XN _ s)                = rnf s

instance WNFData XName where
    rwnf (XN _ s)               = rnf s

instance Binary XName where
    put (XN _ s)                = put s
    get                         = do
                                  s <- get
                                  return $! newXName s

-----------------------------------------------------------------------------

-- |
-- Type for the namespace association list, used when propagating namespaces by
-- modifying the 'QName' values in a tree

type NsEnv              = AssocList XName XName

-----------------------------------------------------------------------------

-- |
-- Namespace support for element and attribute names.
--
-- A qualified name consists of a name prefix, a local name
-- and a namespace uri.
-- All modules, which are not namespace aware, use only the 'localPart' component.
-- When dealing with namespaces, the document tree must be processed by 'Text.XML.HXT.Arrow.Namespace.propagateNamespaces'
-- to split names of structure \"prefix:localPart\" and label the name with the apropriate namespace uri

data QName      = QN { localPart'       :: ! XName
                     , namePrefix'      :: ! XName
                     , namespaceUri'    :: ! XName
                     }
             deriving (Typeable)

-- -----------------------------------------------------------------------------

-- | Two QNames are equal if (1. case) namespaces are both empty and the qualified names
-- (prefix:localpart) are the same or (2. case) namespaces are set and namespaces and
-- local parts are equal

instance Eq QName where
    (QN lp1 px1 ns1) == (QN lp2 px2 ns2)
        | ns1 /= ns2            = False                 -- namespaces are set and differ
        | not (isNullXName ns1) = lp1 == lp2            -- namespaces are set and are equal: local parts must be equal
        | otherwise             = lp1 == lp2            -- no namespaces are set: local parts must be equal
                                  &&                    -- and prefixes are not set or they are equal
                                  px1 == px2

instance Ord QName where
  compare (QN lp1 px1 ns1) (QN lp2 px2 ns2)
      | isNullXName ns1 && isNullXName ns2              -- no namespaces set: px is significant
          = compare (px1, lp1) (px2, lp2)
      | otherwise                                       -- namespace aware cmp: ns is significant, px is irrelevant
          = compare (lp1, ns1) (lp2, ns2)

instance NFData  QName where rnf x = seq x ()
instance WNFData QName

instance Show QName where
    show                        = showQN

-- -----------------------------------------------------------------------------

instance Binary QName where
    put (QN lp px ns)   = put (unXN px) >>
                          put (unXN lp) >>
                          put (unXN ns)
    get                 = do
                          px <- get
                          lp <- get
                          ns <- get
                          return $! newNsName lp px ns
                          --     ^^
                          -- strict apply !!!
                          -- build the QNames strict, else the name sharing optimization will not be in effect

-- -----------------------------------------------------------------------------

isNullXName             :: XName -> Bool
isNullXName             = (== nullXName)
{-# INLINE isNullXName #-}

namePrefix              :: QName -> String
namePrefix              = unXN . namePrefix'
{-# INLINE namePrefix #-}

localPart               :: QName -> String
localPart               = unXN . localPart'
{-# INLINE localPart #-}

namespaceUri            :: QName -> String
namespaceUri            = unXN . namespaceUri'
{-# INLINE namespaceUri #-}

-- ------------------------------------------------------------

-- | set name prefix

setNamespaceUri'                        :: XName -> QName -> QName
setNamespaceUri' ns (QN lp px _ns)      = newQName lp px ns

-- | set local part

setLocalPart'                           :: XName -> QName -> QName
setLocalPart' lp (QN _lp px ns)         = newQName lp px ns

-- | set name prefix

setNamePrefix'                          :: XName -> QName -> QName
setNamePrefix' px (QN lp _px ns)        = newQName lp px ns

-- ------------------------------------------------------------

-- |
-- builds the full name \"prefix:localPart\", if prefix is not null, else the local part is the result

qualifiedName                   :: QName -> String
qualifiedName (QN lp px _ns)
    | isNullXName px            = unXN lp
    | otherwise                 = unXN px ++ (':' : unXN lp)

-- | functional list version of qualifiedName used in xshow

qualifiedName'                   :: QName -> String -> String
qualifiedName' (QN lp px _ns)
    | isNullXName px            = (unXN lp ++)
    | otherwise                 = (unXN px ++) . (':' :) . (unXN lp ++)

-- |
-- builds the \"universal\" name, that is the namespace uri surrounded with \"{\" and \"}\" followed by the local part
-- (specialisation of 'buildUniversalName')

universalName                   :: QName -> String
universalName                   = buildUniversalName (\ ns lp -> '{' : ns ++ '}' : lp)

-- |
-- builds an \"universal\" uri, that is the namespace uri followed by the local part. This is usefull for RDF applications,
-- where the subject, predicate and object often are concatenated from namespace uri and local part
-- (specialisation of 'buildUniversalName')

universalUri                    :: QName -> String
universalUri                    = buildUniversalName (++)

-- |
-- builds a string from the namespace uri and the local part. If the namespace uri is empty, the local part is returned, else
-- namespace uri and local part are combined with the combining function given by the first parameter

buildUniversalName              :: (String -> String -> String) -> QName -> String
buildUniversalName bf n@(QN _lp _px ns)
    | isNullXName ns            = localPart n
    | otherwise                 = unXN ns `bf` localPart n

showQN                          :: QName -> String
showQN n
    | null ns                   = show $ qualifiedName n
    | otherwise                 = show $ "{" ++ ns ++ "}" ++ qualifiedName n
    where
    ns = namespaceUri n

-- ------------------------------------------------------------
--
-- internal XName functions

mkQName'                        :: XName -> XName -> XName -> QName
mkQName' px lp ns               = newQName lp px ns
{-# DEPRECATED mkQName' "use newQName instead with lp px ns param seq " #-}

-- ------------------------------------------------------------

-- |
-- constructs a simple name, with prefix and localPart but without a namespace uri.
--
-- see also 'mkQName', 'mkName'

mkPrefixLocalPart               :: String -> String -> QName
mkPrefixLocalPart px lp
    | null px                   = newLpName lp
    | otherwise                 = newPxName lp px

-- |
-- constructs a simple, namespace unaware name.
-- If the name is in @prefix:localpart@ form and the prefix is not empty
-- the name is split internally into
-- a prefix and a local part.

mkName                          :: String -> QName
mkName n
    | (':' `elem` n)
      &&
      not (null px)                     -- more restrictive: isWellformedQualifiedName n
                                = newPxName lp px
    | otherwise                 = newLpName n
    where
    (px, (_ : lp)) = span (/= ':') n

-- |
-- constructs a complete qualified name with 'namePrefix', 'localPart' and 'namespaceUri'.
-- This function can be used to build not wellformed prefix:localpart names.
-- The XPath module uses wildcard names like @xxx:*@. These must be build with 'mkQName'
-- and not with mkName.

mkQName                         :: String -> String -> String -> QName
mkQName px lp ns
    | null ns                   = mkPrefixLocalPart px lp
    | otherwise                 = newNsName lp px ns

-- ------------------------------------------------------------

-- |
-- old name for 'mkName'

mkSNsName                       :: String -> QName
mkSNsName                       = mkName
{-# DEPRECATED mkSNsName "use mkName instead" #-}

-- |
-- constructs a simple, namespace aware name, with prefix:localPart as first parameter,
-- namspace uri as second.
--
-- see also 'mkName', 'mkPrefixLocalPart'

{-
mkNsName                          :: String -> String -> QName
mkNsName n ns                     = trace ("mkNsName: " ++ show n ++ " " ++ show ns) (mkNsName' n ns)
-}

mkNsName                          :: String -> String -> QName
mkNsName n ns
    | null ns                   = qn
    | otherwise                 = setNamespaceUri' ns' qn
    where
    qn                          = mkName n
    ns'                         = newXName ns

-- ------------------------------------------------------------

-- | Equivalent QNames are defined as follows: The URIs are normalized before comparison.
-- Comparison is done with 'equalQNameBy' and 'equivUri'

equivQName                      :: QName -> QName -> Bool
equivQName                      = equalQNameBy equivUri

-- | Comparison of normalized namespace URIs using 'normalizeNsUri'

equivUri                        :: String -> String -> Bool
equivUri x y                    = normalizeNsUri x == normalizeNsUri y

-- | Sometimes a weaker equality relation than 'equalQName' is appropriate, e.g no case significance in names, ...
-- a name normalization function can be applied to the strings before comparing. Called by 'equalQName' and
-- 'equivQName'

equalQNameBy                    :: (String -> String -> Bool) -> QName -> QName -> Bool
equalQNameBy equiv q1 q2        = localPart q1 == localPart q2
                                  &&
                                  (namespaceUri q1 `equiv` namespaceUri q2)

-- |  Normalization of URIs: Normalization is done by conversion into lowercase letters. A trailing \"\/\" is ignored

normalizeNsUri                  :: String -> String
normalizeNsUri                  = map toLower . stripSlash
    where
    stripSlash ""               = ""
    stripSlash s
        | last s == '/'         = init s
        | otherwise             = s

-- -----------------------------------------------------------------------------

-- Namespace predicates

-- |
-- Compute the name prefix and the namespace uri for a qualified name.
--
-- This function does not test whether the name is a wellformed qualified name.
-- see Namespaces in XML Rule [6] to [8]. Error checking is done with separate functions,
-- see 'isWellformedQName' and 'isWellformedQualifiedName' for error checking.

setNamespace                    :: NsEnv -> QName -> QName
setNamespace env n@(QN lp px _ns)
                                = maybe n (\ ns -> newQName lp px ns) . lookup px $ env

-- -----------------------------------------------------------------------------
--

-- |
-- test for wellformed NCName, rule [4] XML Namespaces

isNCName                        :: String -> Bool
isNCName []                     = False
isNCName n                      = and ( zipWith ($)
                                        (isXmlNCNameStartChar : repeat isXmlNCNameChar)
                                        n
                                      )

-- |
-- test for wellformed QName, rule [6] XML Namespaces
-- predicate is used in filter 'valdateNamespaces'.

isWellformedQualifiedName       :: String -> Bool
isWellformedQualifiedName s
    | null lp                   = isNCName px
    | otherwise                 = isNCName px && isNCName (tail lp)
    where
    (px, lp)                    = span (/= ':') s

-- |
-- test for wellformed QName values.
-- A QName is wellformed, if the local part is a NCName, the namePrefix, if not empty, is also a NCName.
-- predicate is used in filter 'valdateNamespaces'.

isWellformedQName               :: QName -> Bool
isWellformedQName (QN lp px _ns)
                                = (isNCName . unXN) lp                          -- rule [8] XML Namespaces
                                  &&
                                  ( isNullXName px
                                    ||
                                    (isNCName . unXN) px                        -- rule [7] XML Namespaces
                                  )

-- |
-- test whether an attribute name is a namesapce declaration name.
-- If this is not the case True is the result, else
-- the name must be a well formed namespace name:
-- All namespace prefixes starting with \"xml\" are reserved for XML related definitions.
-- predicate is used in filter 'valdateNamespaces'.

isWellformedNSDecl              :: QName -> Bool
isWellformedNSDecl n
                                = not (isNameSpaceName n)
                                  ||
                                  isWellformedNameSpaceName n

-- |
-- test for a namespace name to be well formed

isWellformedNameSpaceName       :: QName -> Bool
isWellformedNameSpaceName n@(QN lp px _ns)
    | isNullXName px            = lp == xmlnsXName
    | otherwise                 = px == xmlnsXName
                                  &&
                                  not (null lp')
                                  &&
                                  not (a_xml `isPrefixOf` lp')
    where
    lp'                         = localPart n


-- |
-- test whether a name is a namespace declaration attribute name

isNameSpaceName                         :: QName -> Bool
isNameSpaceName (QN lp px _ns)
    | isNullXName px                    = lp == xmlnsXName
    | otherwise                         = px == xmlnsXName

-- |
--
-- predicate is used in filter 'valdateNamespaces'.

isDeclaredNamespace                     :: QName -> Bool
isDeclaredNamespace (QN _lp px ns)
    | isNullXName px                    = True                          -- no namespace used
    | px == xmlnsXName                  = ns == xmlnsNamespaceXName     -- "xmlns" has a predefined namespace uri
    | px == xmlXName                    = ns == xmlNamespaceXName       -- "xml" has a predefiend namespace"
    | otherwise                         = not (isNullXName ns)          -- namespace values are not empty

-- -----------------------------------------------------------------------------

toNsEnv                         :: AssocList String String -> NsEnv
toNsEnv                         = map (newXName *** newXName)

-- -----------------------------------------------------------------------------

-- the name and string cache

data NameCache          = NC { _newXN    :: ! Int                                       -- next free name id
                             , _xnCache  :: ! (M.Map String XName)
                             , _qnCache  :: ! (M.Map (XName, XName, XName) QName)       -- we need another type than QName
                             }                                                          -- for the key because of the unusable
                                                                                        -- Eq instance of QName
type ChangeNameCache r  = NameCache -> (NameCache, r)

-- ------------------------------------------------------------

-- | the internal cache for QNames (and name strings)

theNameCache            :: IORef NameCache
theNameCache            = unsafePerformIO (newIORef $ initialCache)
{-# NOINLINE theNameCache #-}

initialXNames           :: [XName]

nullXName
 , xmlnsNamespaceXName
 , xmlnsXName
 , xmlNamespaceXName
 , xmlXName             :: XName

initialXNames@
 [ nullXName
 , xmlnsNamespaceXName
 , xmlnsXName
 , xmlNamespaceXName
 , xmlXName
 ]                      = zipWith XN [0..] $
                          [ ""
                          , xmlnsNamespace
                          , a_xmlns
                          , xmlNamespace
                          , a_xml
                          ]

initialQNames           :: [QName]

xmlnsQN                 :: QName

initialQNames@
 [xmlnsQN]              = [QN xmlnsXName nullXName xmlnsNamespaceXName]

initialCache            :: NameCache
initialCache            = NC
                          (length initialXNames)
                          (M.fromList $ map (\ xn -> (unXN xn, xn)) initialXNames)
                          (M.fromList $ map (\ qn@(QN lp px ns) -> ((lp, px, ns), qn)) initialQNames)

-- ------------------------------------------------------------

changeNameCache         :: NFData r => ChangeNameCache r -> r
changeNameCache action  = unsafePerformIO changeNameCache'
    where
    action' c = 
      let r = action c 
      in 
       fst r `seq` r    -- eval name cache to whnf
       
    changeNameCache' = 
      do
      -- putStrLn "modify cache"
      res <- atomicModifyIORef theNameCache action'
      -- putStrLn "cache modified"
      return res

{-# NOINLINE changeNameCache #-}

newXName'               :: String -> ChangeNameCache XName
newXName' n c@(NC nxn xm qm)
                        = case M.lookup n xm of
                          Just xn       -> (c, xn)
                          Nothing       -> let nxn' = nxn + 1 in
                                           let xn   = (XN nxn n) in
                                           let xm'  = M.insert n xn xm in
                                           -- trace ("newXName: XN " ++ show nxn ++ " " ++ show n) $
                                           rnf xn `seq` (NC nxn' xm' qm, xn)

newQName'               :: XName -> XName -> XName -> ChangeNameCache QName
newQName' lp px ns c@(NC nxn xm qm)
                        = case M.lookup q' qm of
                          Just qn       -> -- trace ("oldQName: " ++ show qn) $                 -- log evaluation sequence
                                           (c, qn)
                          Nothing       -> let qm'  = M.insert q' q qm in
                                           -- trace ("newQName: " ++ show q) $                  -- log insertion of a new QName
                                           q `seq` (NC nxn xm qm', q)
    where
    q'                  = (lp, px, ns)
    q                   = QN lp px ns

andThen                 :: ChangeNameCache r1 ->
                           (r1 -> ChangeNameCache r2) -> ChangeNameCache r2
andThen a1 a2 c0        = let (c1, r1) = a1 c0 in
                          (a2 r1) c1

newXName                :: String -> XName
newXName n              = changeNameCache $
                          newXName' n

newQName                :: XName -> XName -> XName -> QName
newQName lp px ns       = lp `seq` px `seq` ns `seq`            -- XNames must be evaluated, else MVar blocks
                          ( changeNameCache $
                            newQName' lp px ns
                          )

newLpName               :: String -> QName
newLpName lp            = changeNameCache $
                          newXName' lp `andThen` \ lp' ->
                          newQName' lp' nullXName nullXName

newPxName               :: String -> String -> QName
newPxName lp px         = changeNameCache $
                          newXName' lp `andThen` \ lp' ->
                          newXName' px `andThen` \ px' ->
                          newQName' lp' px' nullXName

newNsName               :: String -> String -> String -> QName
newNsName lp px ns      = changeNameCache $
                          newXName' lp `andThen` \ lp' ->
                          newXName' px `andThen` \ px' ->
                          newXName' ns `andThen` \ ns' ->
                          newQName' lp' px' ns'

-----------------------------------------------------------------------------
