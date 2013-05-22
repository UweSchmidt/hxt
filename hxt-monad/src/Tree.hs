{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Tree
{- just for testing
    ( Tree
    , isNull
    , isFail
    , fromList
    , toList
    )
-- -}
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Arrow
import           Control.Monad.ArrowExc
import           Control.Monad.ArrowIf
import           Control.Monad.ArrowIO
import           Control.Monad.ArrowList
import           Control.Monad.ArrowState
import           Control.Monad.Error
import           Control.Monad.MonadSequence
import           Control.Monad.State.Strict
-- import           Data.List                   (partition)
-- import           Data.Sequence.IOList        -- old stuff
import           Data.Sequence.IOSequence
import           Data.Sequence.IOTree
import qualified Data.Sequence.List            as SL
import qualified Data.Sequence.ListWithFailure as FL
import           Data.Sequence.StateSequence
import           Data.Sequence.Tree
import           Data.Monoid

-- ----------------------------------------

type LA       a b = a ->               SL.Seq   b
type SLA    s a b = a -> StateSequence SL.Seq s b
type IOLA     a b = a -> IOSequence    SL.Seq   b

type LA1      a b = a ->               FL.Seq   b
type SLA1   s a b = a -> StateSequence FL.Seq s b
type IOLA1    a b = a -> IOSequence    FL.Seq   b

type LA2      a b = a -> Tree            b
type SLA2   s a b = a -> StateSequence Tree s b
type IOLA2    a b = a -> IOTree          b

type StateIOTree s a = StateT s IOTree a

type SIOLA s a b = a -> StateIOTree s b

type IntSLA a b = SLA2 Int a b

mk2 :: a -> a -> Tree a
mk2 x y = Bin (Tip x) (Tip y)

tt1,tt7,tt8 :: Tree Integer
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


