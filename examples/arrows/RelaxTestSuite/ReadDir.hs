{- |
   Read a complete directory hierarchy and return it as a tree.
   Copied and modified from Peter Simons (<http://cryp.to/>).

   $Id: ReadDir.hs,v 1.1 2005/09/02 17:09:38 hxml Exp $
   
-}

module ReadDir
    ( Entry(..)
    , readDir
    )

where

import System.Directory
  ( getDirectoryContents
  , doesDirectoryExist
  )
import Data.List
  ( find
  , sort
  , isSuffixOf
  )
import Maybe
  ( fromMaybe )


data Entry = Dir [Entry] -- ^ Directory with a list of subdirectories and files
           | DirContent FilePath [FilePath] -- ^ Relax NG schema and list of XML documents
     deriving Show


-- ------------------------------------------------------------

-- |
-- Read the complete directory hierarchy starting at the FilePath given by the first 
-- parameter and return a tree representing it. Directories contain their
-- sub-entries in alphabetic order.
--
-- Each 'DirContent' represents a Relax NG schema and all XML Documents
-- in the directory.
--
-- The function may throw exceptions when IO fails.

readDir :: FilePath -> FilePath -> IO Entry
readDir pre p
  = do 
    dir     <- getDirectoryContents $ pre ++ "/" ++ p
    entries <- getEntries $ sort $ clean [".", "..", "CVS"] dir
    return $ Dir entries
  where
  getEntries :: [FilePath] -> IO [Entry]
  getEntries xs
    = do
      dirs     <- mapM toEntryDir xs
      files    <- mapM toEntryFiles xs
      xmlfiles <- return $ [ f | f <- files, ".xml" `isSuffixOf` f]
      rngfile  <- return $ fromMaybe "" $ find (".rng" `isSuffixOf`) files
      return $ if rngfile /= ""
              then (DirContent rngfile xmlfiles):dirs
              else dirs
  
  toEntryDir :: FilePath -> IO Entry
  toEntryDir x = do
              isDir <- doesDirectoryExist $ path ++ "/" ++ x
              if isDir then readDir path x else return $ Dir []
              where path = pre ++ "/" ++ p              
  
  toEntryFiles :: FilePath -> IO FilePath
  toEntryFiles x = do
                  isDir <- doesDirectoryExist path
                  return $ if isDir then "" else path 
              where path = pre ++ p ++ "/" ++ x              
  
  clean :: [String] -> [FilePath] -> [FilePath]
  clean ver xs = [ x | x <- xs, not $ elem x ver]

-- ------------------------------------------------------------
