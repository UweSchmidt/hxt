-- generate a mapping from file name extensions to mimetypes
--
-- input is a file with a format like /etc/mime.types on debian linux
--
-- output is a haskell module with a mimetype table

module Main
where

import Data.Char
import Data.List
import Data.Maybe

import System.FilePath
import System.IO
import System.Environment
import System.Exit

-- ------------------------------------------------------------

main	:: IO ()
main
    = do
      argv <- getArgs
      genTable ( if null argv
		 then defaultFile
		 else head argv
	       )
      exitProg False

-- ------------------------------------------------------------

exitProg	:: Bool -> IO a
exitProg True	= exitWith (ExitFailure 1)
exitProg False	= exitWith ExitSuccess

-- ------------------------------------------------------------

defaultFile	:: FilePath
defaultFile	= "/etc/mime.types"

genTable	:: FilePath -> IO ()
genTable inp
    = do
      h <- open inp
      c <- hGetContents h
      r <- return $ genCode inp c
      putStrLn r
      close inp h
    where
    open "-"	= return stdout
    open f	= openFile f ReadMode

    close "-" _ = return ()
    close _   h = hClose h

genCode	:: FilePath -> String -> String
genCode inp
    = unlines
      . (part1 inp ++)
      . (++ part3)
      . part2
      . parseMimeTypeTable

part1 :: FilePath -> [String]
part1 n
    = [ "-- | default mime type table"
      , "-- "
      , "-- this file is generated from file " ++ n
      , ""
      , "module MimeTypeDefaults"
      , "where"
      , ""
      , "-- | the table with the mapping from file name extensions to mime types"
      , ""
      , "mimeTypeDefaults :: [(String, String)]"
      , "mimeTypeDefaults"
      ]

part2 :: [(String, String)] -> [String]
part2 []
    = [ "  = []" ]
part2 [x1]
    = [ "  = [ " ++ sh x1 ++ " ]" ]
part2 (x1:xs)
    = ( "  = [ " ++ sh x1 ) :
      map (("    , " ++) . sh) xs
    
sh (e, t) = "(" ++ show e ++ ",\t" ++ show t ++ ")"

part3 :: [String]
part3
    = [ "   ]" ]

-- ------------------------------------------------------------

parseMimeTypeTable	:: String -> [(String, String)]
parseMimeTypeTable
    = sort
      . concat
      . map buildPairs
      . map words
      . filter (not . ("#" `isPrefixOf`))
      . filter (not . all (isSpace))
      . lines
    where
    buildPairs	:: [String] -> [(String, String)]
    buildPairs	[] = []
    buildPairs	(mt:exts) = map (\ x -> (x, mt)) $ exts

-- ------------------------------------------------------------

