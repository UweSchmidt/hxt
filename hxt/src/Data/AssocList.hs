-- |
-- simple key value assocciation list
-- implemented as unordered list of pairs
--
-- Version : $Id: AssocList.hs,v 1.2 2005/05/27 13:15:23 hxml Exp $

module Data.AssocList
    ( module Data.AssocList
    )
where

import Data.Maybe

type AssocList k v = [(k, v)]

-- lookup       = lookup from Prelude

-- | lookup with default value

lookupDef       :: Eq k => v -> k -> AssocList k v -> v
lookupDef d k   = fromMaybe d . lookup k

-- | lookup with empty list (empty string) as default value

lookup1         :: Eq k => k -> AssocList k [e] -> [e]
lookup1 k       = fromMaybe [] . lookup k

-- | test for existence of a key

hasEntry        :: Eq k => k -> AssocList k v -> Bool
hasEntry k      = isJust . lookup k

-- | add an entry, remove an existing entry before adding the new one at the top of the list, addEntry is strict

addEntry        :: Eq k => k -> v -> AssocList k v -> AssocList k v
addEntry k v l  = ( (k,v) : ) $! delEntry k l

 -- let l' = delEntry k l in seq l' ((k, v) : l')

-- | add a whole list of entries with 'addEntry'

addEntries      :: Eq k => AssocList k v -> AssocList k v -> AssocList k v
addEntries      = foldr (.) id . map (uncurry addEntry) . reverse

-- | delete an entry, delEntry is strict
delEntry        :: Eq k => k -> AssocList k v -> AssocList k v
delEntry _ []   = []
delEntry k (x@(k1,_) : rest)
    | k == k1   = rest
    | otherwise = ( x : ) $! delEntry k rest

-- delEntry k   = filter ((/= k) . fst)

-- | delete a list of entries with 'delEntry'

delEntries      :: Eq k => [k] -> AssocList k v -> AssocList k v
delEntries      = foldl (.) id . map delEntry

-- -----------------------------------------------------------------------------


