{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ------------------------------------------------------------

module Main
where

import           Control.DeepSeq

import           Criterion.Main

import qualified Data.ByteString.Char8                   as B
import qualified Data.ByteString.Lazy.Char8              as BL
import           Data.String                             (IsString (..))
import qualified Data.Text                               as T
import qualified Data.Text.Lazy                          as TL

import           Text.Regex.XMLSchema.Generic
import           Text.Regex.XMLSchema.Generic.StringLike

-- import           Debug.Trace

-- ------------------------------------------------------------

type BS    = B.ByteString
type BL    = BL.ByteString
type Text  = T.Text
type TextL = TL.Text

-- ------------------------------------------------------------

benchSTB ::  String ->
             (forall s . (NFData s, StringLike s) => s -> [s]) ->
             (String, String -> [String]) ->
             String ->
             Benchmark
benchSTB name fct ref inp
  = benchSTB' name fct ref $! mkInput inp

benchSTB' :: String ->
             (forall s . (NFData s, StringLike s) => s -> [s]) ->
             (String, String -> [String]) ->
             (String, Text, TextL, BS, BL) ->
             Benchmark
benchSTB' name fct (refName, ref) (s, t, tl, bs, bl)
  = bgroup name
    [ bench refName           $ nf ref s
    , bench "String"          $ nf fct s
    , bench "Text"            $ nf fct t
    , bench "Text.Lazy"       $ nf fct tl
    , bench "ByteString"      $ nf fct bs
    , bench "ByteString.Lazy" $ nf fct bl
    ]

mkInput :: String -> (String, Text, TextL, BS, BL)
mkInput s
  = rnf t5 `seq` t5
    where
      t5 = (s, fromString s, fromString s, fromString s, fromString s)
      
words' :: StringLike s => s -> [s]
words' inp
  = tokenize (fromString "\\w+") inp

    
main    :: IO ()
main
    = do
      defaultMain [ benchSTB "100,000-words" words' ("words", words)
                    $ unwords (replicate 100000 "1234567890")
                  ]

-- ------------------------------------------------------------
