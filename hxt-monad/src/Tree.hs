{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Tree
where

import           Control.Applicative           ()
import           Control.Monad                 ()
import           Control.Monad.Arrow
import           Control.Monad.ArrowExc        ()
import           Control.Monad.ArrowIf         ()
import           Control.Monad.ArrowIO         ()
import           Control.Monad.ArrowList
import           Control.Monad.ArrowNF         ()
import           Control.Monad.ArrowState
import           Control.Monad.ArrowTree       ()
import           Control.Monad.Error           ()
import           Control.Monad.MonadSequence
import           Control.Monad.State           ()
import           Data.Monoid                   ()

import           Data.Sequence.IOSequence
import           Data.Sequence.IOStateSequence
import qualified Data.Sequence.List            as SL
import qualified Data.Sequence.ListWithFailure as FL
import           Data.Sequence.StateSequence
import qualified Data.Sequence.Tree            as ST
import qualified Data.Sequence.TreeWithFailure as FT

import           Text.XML.HXT.Monad.ArrowXml   ()


-- ----------------------------------------

type LA       a b = a ->                 SL.Seq   b
type SLA    s a b = a -> StateSequence   SL.Seq s b
type IOLA     a b = a -> IOSequence      SL.Seq   b
type IOSLA  s a b = a -> IOStateSequence SL.Seq s b

type LA1      a b = a ->                 FL.Seq   b
type SLA1   s a b = a -> StateSequence   FL.Seq s b
type IOLA1    a b = a -> IOSequence      FL.Seq   b
type IOSLA1 s a b = a -> IOStateSequence FL.Seq s b

type LA2      a b = a ->                 ST.Tree   b
type SLA2   s a b = a -> StateSequence   ST.Tree s b
type IOLA2    a b = a -> IOSequence      ST.Tree   b
type IOSLA2 s a b = a -> IOStateSequence ST.Tree s b

type LA3      a b = a ->                 FT.Tree   b
type SLA3   s a b = a -> StateSequence   FT.Tree s b
type IOLA3    a b = a -> IOSequence      FT.Tree   b
type IOSLA3 s a b = a -> IOStateSequence FT.Tree s b

-- type StateIOTree s a = StateT s IOTree a

-- type SIOLA s a b = a -> StateIOTree s b

type IntSLA a b = SLA2 Int a b

mk2 :: Sequence s => a -> a -> s a
mk2 x y = fromList [x, y]

tt1,tt7,tt8 :: MonadSeq s => s Integer
tt1 = fromList [1]
tt7 = fromList [1..7]
tt8 = fromList [1..8]

st1 :: (Sequence s) => [Int] -> StateSequence s Int Int
st1 = fromList >>> (this <+> arr (+10)) >>> arr (+1) >>> perform (changeState (+) . const 1)

st1' :: SLA Int [Int] Int
st1' = st1

st1'' :: SLA2 Int [Int] Int
st1'' = st1

st11 :: SLA1 Int [Int] Int
st11 = st1

st13 :: SLA3 Int [Int] Int
st13 = st1


