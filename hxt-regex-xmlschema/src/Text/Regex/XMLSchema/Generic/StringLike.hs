{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- ------------------------------------------------------------

{- |
   Copyright  : Copyright (C) 2014- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt <uwe@fh-wedel.de>
   Stability  : stable
-}

-- ------------------------------------------------------------

module Text.Regex.XMLSchema.Generic.StringLike
where

import Data.Maybe
import Data.String      (IsString(..))

import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL

-- ------------------------------------------------------------

-- | /WARNING/: This StringLike class is /not/ intended for use outside this regex library.
-- It provides an abstraction for String's as used inside this library.
-- It allows the library to work with String (list of Char),
-- ByteString.Char8, ByteString.Lazy.Char8,
-- Data.Text and Data.Text.Lazy.
--
-- The class is similar to the StringLike class in the tagsoup package

class (Eq a, IsString a, Show a) => StringLike a where
  emptyS     :: a
  uncons     :: a -> Maybe (Char, a)
  nullS      :: a -> Bool
  headS      :: a -> Char
  takeS      :: Int -> a -> a
  dropS      :: Int -> a -> a
  appendS    :: a -> a -> a
  concatS    :: [a] -> a
  toString   :: a -> String

  nullS      = isNothing . uncons
  headS (uncons -> Just (c, _))
             = c
  headS _    = error "headS: empty StringLike"
  concatS    = foldl appendS emptyS
  
  {-# INLINE nullS   #-}
  {-# INLINE headS   #-}
  {-# INLINE concatS #-}
  
-- ------------------------------------------------------------

instance StringLike String where
  emptyS           = []
  uncons (x : xs) = Just (x, xs)
  uncons ""       = Nothing
  nullS           = null
  headS           = head
  takeS           = take
  dropS           = drop
  appendS         = (++)
  concatS         = concat
  toString        = id

  {-# INLINE emptyS     #-}
  {-# INLINE uncons     #-}
  {-# INLINE nullS      #-}
  {-# INLINE takeS      #-}
  {-# INLINE dropS      #-}
  {-# INLINE appendS    #-}
  {-# INLINE concatS    #-}
  {-# INLINE toString   #-}

-- ------------------------------------------------------------

instance StringLike T.Text where
  emptyS     = T.empty
  uncons     = T.uncons
  nullS      = T.null
  headS      = T.head
  takeS      = T.take
  dropS      = T.drop
  appendS    = T.append
  concatS    = T.concat
  toString   = T.unpack

  {-# INLINE emptyS     #-}
  {-# INLINE uncons     #-}
  {-# INLINE nullS      #-}
  {-# INLINE takeS      #-}
  {-# INLINE dropS      #-}
  {-# INLINE appendS    #-}
  {-# INLINE concatS    #-}
  {-# INLINE toString   #-}

-- ------------------------------------------------------------

instance StringLike TL.Text where
  emptyS     = TL.empty
  uncons     = TL.uncons
  nullS      = TL.null
  headS      = TL.head
  takeS      = TL.take . toEnum
  dropS      = TL.drop . toEnum
  appendS    = TL.append
  concatS    = TL.concat
  toString   = TL.unpack

  {-# INLINE emptyS     #-}
  {-# INLINE uncons     #-}
  {-# INLINE nullS      #-}
  {-# INLINE takeS      #-}
  {-# INLINE dropS      #-}
  {-# INLINE appendS    #-}
  {-# INLINE concatS    #-}
  {-# INLINE toString   #-}

-- ------------------------------------------------------------

instance StringLike B.ByteString where
  emptyS     = B.empty
  uncons     = B.uncons
  nullS      = B.null
  headS      = B.head
  takeS      = B.take
  dropS      = B.drop
  appendS    = B.append
  concatS    = B.concat
  toString   = B.unpack

  {-# INLINE emptyS     #-}
  {-# INLINE uncons     #-}
  {-# INLINE nullS      #-}
  {-# INLINE takeS      #-}
  {-# INLINE dropS      #-}
  {-# INLINE appendS    #-}
  {-# INLINE concatS    #-}
  {-# INLINE toString   #-}

-- ------------------------------------------------------------

instance StringLike BL.ByteString where
  emptyS     = BL.empty
  uncons     = BL.uncons
  nullS      = BL.null
  headS      = BL.head
  takeS      = BL.take . toEnum
  dropS      = BL.drop . toEnum
  appendS    = BL.append
  concatS    = BL.concat
  toString   = BL.unpack

  {-# INLINE emptyS     #-}
  {-# INLINE uncons     #-}
  {-# INLINE nullS      #-}
  {-# INLINE takeS      #-}
  {-# INLINE dropS      #-}
  {-# INLINE appendS    #-}
  {-# INLINE concatS    #-}
  {-# INLINE toString   #-}

-- ------------------------------------------------------------

    
