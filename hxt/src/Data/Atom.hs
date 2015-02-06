{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : Data.Atom
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: non-portable

   Unique Atoms generated from Strings and
   managed as flyweights

   Data.Atom can be used for caching and storage optimisation
   of frequently used strings. An @Atom@ is constructed from a @String@.
   For two equal strings the identical atom is returned.

   This module can be used for optimizing memory usage when working with
   strings or names. Many applications use data types like
   @Map String SomeAttribute@ where a rather fixed set of keys is used.
   Especially XML applications often work with a limited set of element and attribute names.
   For these applications it becomes more memory efficient when working with types like
   @Map Atom SomeAttribute@ and convert the keys into atoms before operating
   on such a map.

   Internally this module manages a map of atoms. The atoms are internally represented
   by @ByteString@s. When creating a new atom from a string, the string is first converted
   into an UTF8 @Word8@ sequence, which is packed into a @ByteString@. This @ByteString@ is looked
   up in the table of atoms. If it is already there, the value in the map is used as atom, else
   the new @ByteString@ is inserted into the map.

   Of course the implementation of this name cache uses @unsavePerformIO@.
   The global cache is managed by ue of an @IORef@ and atomicModifyIORef.

   The following laws hold for atoms

   >
   > s  ==       t => newAtom s  ==       newAtom t
   > s `compare` t => newAtom s `compare` newAtom t
   > show . newAtom == id

   Equality test for @Atom@s runs in /O(1)/, it is just a pointer comarison.
   The @Ord@ comparisons have the same runtime like the @ByteString@ comparisons.
   Internally there is an UTF8 comparison, but UTF8 encoding preserves the total order.

   Warning: The internal cache never shrinks during execution. So using it in a
   undisciplined way can lead to memory leaks.
-}

-----------------------------------------------------------------------------

module Data.Atom (
   -- * Atom objects
   Atom,                -- instance (Eq, Ord, Read, Show)
   newAtom,             -- :: String -> Atom
   share                -- :: String -> String
 ) where

import           Control.DeepSeq

import           Data.ByteString          (ByteString, pack, unpack)
import           Data.ByteString.Internal (c2w, toForeignPtr, w2c)
import           Data.IORef
import qualified Data.Map                 as M
import           Data.String.Unicode      (unicodeToUtf8)
import           Data.String.UTF8Decoding (decodeUtf8)
import           Data.Typeable

import           System.IO.Unsafe         (unsafePerformIO)

-- ------------------------------------------------------------

type Atoms      = M.Map ByteString ByteString

newtype Atom    = A { bs :: ByteString }
                  deriving (Typeable)

-- ------------------------------------------------------------

-- | the internal cache for the strings

theAtoms        :: IORef Atoms
theAtoms        = unsafePerformIO (newIORef M.empty)
{-# NOINLINE theAtoms #-}

-- | insert a bytestring into the atom cache

insertAtom      :: ByteString -> Atoms -> (Atoms, Atom)
insertAtom s m  = maybe (M.insert s s m, A s)
                        (\ s' -> (m, A s'))
                  .
                  M.lookup s $ m

-- | creation of an @Atom@ from a @String@

newAtom         :: String -> Atom
newAtom         = unsafePerformIO . newAtom'
{-# NOINLINE newAtom #-}

-- | The internal operation running in the IO monad
newAtom'        :: String -> IO Atom
newAtom' s      = do
                  -- putStrLn "insert atom into cache"
                  res <- atomicModifyIORef theAtoms insert
                  -- putStrLn "atom cache updated"
                  return res
  where
    insert m    = let r = insertAtom (pack. map c2w . unicodeToUtf8 $ s) m
                  in
                   fst r `seq` r

-- | Insert a @String@ into the atom cache and convert the atom back into a @String@.
--
-- locically @share == id@ holds, but internally equal strings share the same memory.

share           :: String -> String
share           = show . newAtom

instance Eq Atom where
    a1 == a2    = fp1 == fp2
                  where
                  (fp1, _, _) = toForeignPtr . bs $ a1
                  (fp2, _, _) = toForeignPtr . bs $ a2

instance Ord Atom where
    compare a1 a2
                | a1 == a2      = EQ
                | otherwise     = compare (bs a1) (bs a2)

instance Read Atom where
    readsPrec p str = [ (newAtom x, y) | (x, y) <- readsPrec p str ]

instance Show Atom where
    show        = fst . decodeUtf8 . map w2c . unpack . bs
    -- show     = show . toForeignPtr . bs                      -- for debug only

instance NFData Atom where
    rnf x = seq x ()

-----------------------------------------------------------------------------
