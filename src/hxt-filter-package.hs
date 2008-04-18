module Main
where

import Data.List
import System.Environment

part1	:: [String]
part1
    = [ "-- arch-tag: Haskell XML Toolbox main description file"
      , "name: hxt-filter"
      ]

version	:: String
version
    = "version: "

part2	:: [String]
part2
    = [ "license: OtherLicense"
      , "license-file: LICENCE"
      , "maintainer: Uwe Schmidt <uwe@fh-wedel.de>"
      , "stability: stable"
      , "category: XML"
      , "synopsis: A collection of tools for processing XML with Haskell (Filter variant). "
      , "description:  The Haskell XML Toolbox bases on the ideas of HaXml and HXML." ++
	" This package is a compatibitlity package for old software working with the filter approach like in HaXml" ++
	" For new projects it's recomended to use the arrow based library (hxt)."
      , "homepage: http://www.fh-wedel.de/~si/HXmlToolbox/index.html"
      , "copyright: Copyright (c) 2005-8 Uwe Schmidt"
      ]

part3a	:: [String]
part3a
    = [ "tested-with: ghc-6.8"
      , "exposed: True"
      , "exposed-modules:"
      ]

part3b :: [String]
part3b
    = [ "build-type: Simple"
      , "cabal-version: >=1.2"
      , ""
      , "flag splitBase"
      , "  description: Choose the new smaller, split-up base package."
      , ""
      , "library"
      , " exposed-modules:"
      ]

part4a	:: String -> [String]
part4a dir
    = [ "hs-source-dirs: ."
      , "ghc-options: -Wall -O2 -fglasgow-exts"
      , "import-dirs: " ++ dir ++ "/imports"
      , "library-dirs: " ++ dir
      , "hs-libraries: HShxt-filter"
      , "depends: base, haskell98, parsec, HTTP, HUnit, network, containers, directory, process, hxt-8.0.0"
      ]

part4b :: [String]
part4b
    = [ " hs-source-dirs: src"
      , " ghc-options: -Wall"
      , " extensions: MultiParamTypeClasses DeriveDataTypeable FunctionalDependencies FlexibleInstances"
      , ""
      , " build-depends: base, haskell98 >= 1, parsec >= 2.1 && < 3, HTTP, HUnit >= 1.2, network >= 2.1, hxt >= 8.0"
      , " if flag(splitBase)"
      , "   build-depends: base >= 3, containers >= 0.1, directory >= 1, process >= 1"
      , " else"
      , "   build-depends: base < 3"
      ]

main	:: IO()
main
    = do
      vers : cabal : installdir : modules <- getArgs
      putStrLn (cabalFile vers cabal installdir modules)

cabalFile	:: String -> String -> String -> [String] -> String
cabalFile vers cabal installdir modules
    = unlines $
      part1 ++
      [ version ++ vers ] ++
      part2 ++
      ( if isCabal
	then part3b
	else part3a
      ) ++
      [ml modules] ++
      ( if isCabal
	then part4b
	else part4a installdir
      )
    where
    isCabal = cabal == "cabal"
    ml = foldr1 (\ x y -> x ++ ",\n" ++ y) . sort . map editPath
    editPath = ("  " ++) . map slash2dot . reverse . drop 1 . dropWhile (/= '.') . reverse . removeLeadingDot
    slash2dot '/' = '.'
    slash2dot c   = c
    removeLeadingDot ('.':'/':path)	= path
    removeLeadingDot path		= path