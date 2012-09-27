{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

{- |
   Module     : Text.XML.HXT.XMLSchema.ValidationTypes
   Copyright  : Copyright (C) 2012 Thorben Guelck, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Contains the basic datatypes which are used during validation.
-}

module Text.XML.HXT.XMLSchema.ValidationTypes
where

import Text.XML.HXT.Core           ( QName
                                   , XmlTree
                                   , c_warn
                                   , c_err
                                   )
import Text.XML.HXT.DOM.ShowXml    ( xshow )

-- import Text.XML.HXT.Arrow.XmlRegex ( XmlRegex )

import Control.Monad.Identity      ( Identity
                                   , runIdentity
                                   )

import Control.Monad.Reader        ( ReaderT
                                   , runReaderT
                                   , asks
                                   )

import Control.Monad.Writer        ( WriterT
                                   , runWriterT
                                   , tell
                                   )

import Data.Map                    ( Map )

import Text.XML.HXT.XMLSchema.Regex

-- ----------------------------------------

-- | The environment used during validation
data SValEnv       = SValEnv
                   { xpath           :: XPath
                   , elemDesc        :: ElemDesc
                   , allElemDesc     :: SubElemDesc	-- all declared elements, used with wilcard contents
                   }

-- | Simple XPath representation
type XPath         = String

-- | Description for an element under test
data ElemDesc      = ElemDesc
                   { errmsg       :: Maybe String
                   , attrDesc     :: AttrDesc
                   , mixedContent :: Bool
                   , contentModel :: XmlRegex'
                   , subElemDesc  :: SubElemDesc
                   , wildcards    :: Wildcards		-- redundant: never read, only set
                   , sttf         :: MaybeSTTF		-- in case of simple types Just the string test, else Nothing
                   }

-- | Description for allowed attributes of an element
type AttrDesc      = (AttrMap, AttrWildcards)

-- | Table for regular attributes
type AttrMap       = Map QName AttrMapVal

-- | Entry for AttrMap: required-flag and test function
type AttrMapVal    = (Bool, STTF)

-- | List of test functions for attribute wildcards
type AttrWildcards = [QName -> Bool]

-- | Table of possible subelems and their descriptions
type SubElemDesc   = Map QName ElemDesc

-- | List of wildcard specs
type Wildcards     = [Wildcard]

-- | Pair of namespace test and action
data Wildcard      = WC (QName -> Bool) WildcardClass

-- | wildcard validation strategie
data WildcardClass = Skip | Lax | Strict

-- | SimpleType test function to validate basic values
type STTF          = String -> SVal Bool

-- | SimpleType test function to validate basic values
type MaybeSTTF     = Maybe STTF

-- | Validation result contains the validation status and log
type SValResult    = (Bool, SValLog)

-- | Validation log is a list of error levels, XPaths and messages
type SValLog       = [SValLogMsg]

-- | Validation log message
type SValLogMsg    = (Int, XPath, String)

-- | Schema validation monad
type SVal a        = ReaderT SValEnv (WriterT SValLog Identity) a

-- | Runs a computation in the schema validation monad
runSVal :: SValEnv -> SVal a -> (a, SValLog)
runSVal env val = runIdentity $ runWriterT $ runReaderT val env

-- | the result state of a matching test with XmlTree regular expressions
--
-- the result is a function for testing the content model of all elements
-- contained in the children of an element

type XmlRegexState = SVal Bool

-- | A substitute for XmlRegex from hxt Arrow.XmlRegex

type XmlRegex' = Regex XmlRegexState XmlTree

instance ShowSym XmlTree where
    showSym = cut 80 . xshow . (:[])

cut :: Int -> String -> String
cut n s
    | null rest   = s'
    | otherwise   = s' ++ "..."
    where
      (s', rest)  = splitAt n s


-- ----------------------------------------

-- | Creates a SimpleType test function which creates a warning and always succeeds
mkWarnSTTF :: String -> STTF
mkWarnSTTF msg
  = \ _ -> mkLogSTTF c_warn id msg

mkWarnSTTF'' :: (XPath -> XPath) -> String -> SVal Bool
mkWarnSTTF''
    = mkLogSTTF c_warn

-- | Creates a SimpleType test function which creates an error and always fails
mkErrorSTTF :: String -> STTF
mkErrorSTTF msg
  = \ _ -> mkErrorSTTF' msg

mkErrorSTTF' :: STTF
mkErrorSTTF' msg
    = mkLogSTTF c_err id msg

mkErrorSTTF'' :: (XPath -> XPath) -> String -> SVal Bool
mkErrorSTTF''
    = mkLogSTTF c_err

mkLogSTTF :: Int -> (XPath -> XPath) -> String -> SVal Bool
mkLogSTTF lev fpos msg
  = do pos <- asks xpath
       tell [(lev, fpos pos, msg)]
       return (lev < c_err)

-- ----------------------------------------
--
-- simple tracing

-- {-
logg :: [String] -> SVal ()
logg msg
    = do pos <- asks xpath
         tell [(0, pos, unwords msg)]
-- -}

{-
logg :: [String] -> SVal ()
logg msg
    = return ()
{-# INLINE #-}
-- -}

-- ----------------------------------------

