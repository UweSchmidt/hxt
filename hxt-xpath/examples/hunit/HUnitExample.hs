-- |
-- HUnit - Haskell XML Toolbox examples and tests for arrows
--
-- Author: Uwe Schmidt uwe@fh-wedel.de
--

module Main where

import System
import Test.HUnit
import Text.XML.HXT.Arrow
import Text.XML.HXT.XPath


-- |
-- auxiliary function to make haskell string constants with quotes more readable

singleToDoubleQuote	:: String -> String
singleToDoubleQuote
    = map (\ c -> if c == '\'' then '\"' else c)

testLA	:: String -> String -> LA XmlTree XmlTree -> Test
testLA doc expected f
    = TestCase $ assertEqual "LA XmlTree XmlTree:" [expected] res
      where
      res = runLA (xread >>> xshow f) doc

testLAString	:: String -> String -> LA XmlTree String -> Test
testLAString doc expected f
    = TestCase $ assertEqual "LA XmlTree String:" [expected] res
      where
      res = runLA (xread >>> f) doc

mkTestSeqLA	:: String -> [(String, LA XmlTree XmlTree)] -> [Test]
mkTestSeqLA doc
    = map (\ (res, f) -> testLA doc (singleToDoubleQuote res) f)

nodeSetTests	:: Test
nodeSetTests
    = TestList $
      [ TestLabel "node set and simple XPath tests with getXPathTrees" $
	TestList $
	mkTestSeqLA doc (testGetXPathTrees tests)

      , TestLabel "node set and simple XPath tests with getXPathNodeSet" $
	TestList $
	mkTestSeqLA doc (testGetXPathNodes tests)

      , TestLabel "node set and simple XPath tests with processFromNodeSet" $
        TestList $
        mkTestSeqLA doc (testProcessXPath processTests)

      , TestLabel "node set and simple XPath tests with processXPathTrees" $
        TestList $
        mkTestSeqLA doc (testProcessXPath' processTests)
      ]
    where
    doc = "<x p='.'>.0<x p='.1'>.1.0</x>.2<y p='.3'>.3.0<x p='.3.1'>.3.1.0</x></y>.4</x>"

    testGetXPathTrees = map (\ (r, xp) -> (r, getXPathTrees xp))			-- these arrows are equivalent
    testGetXPathNodes = map (\ (r, xp) -> (r, getFromNodeSet $< getXPathNodeSet xp))	-- except for the ordering of the result set
											-- which does not matter for these tests
    testProcessXPath  = map (\ (r, xp, a) -> (r, processFromNodeSet a $< getXPathNodeSet xp))
    testProcessXPath' = map (\ (r, xp, a) -> (r, processXPathTrees a xp))

    tests = [ (doc						, "/x"	)
	    , ("<y p='.3'>.3.0<x p='.3.1'>.3.1.0</x></y>"	, "/x/y"	)
	    , ("<x p='.3.1'>.3.1.0</x>"				, "/x/y/x"	)
	    , (".0.2.4"						, "/x/text()"	)
	    , (".3.0"						, "/x/y/text()"	)
	    , ("<x p='.1'>.1.0</x><x p='.3.1'>.3.1.0</x>"	, "/x//x"	)
	    ]

    processTests
	 = [ ("<x p='.'>x<x p='.1'>x</x>x<y p='.3'>x<x p='.3.1'>x</x></y>x</x>",	  "//text()",		changeText (const "x")	)
	   , ("<x p='.'>.0<x p='.1'>.1.0</x>.2<y p='.3'>.3.0<x p='.3.1'>x</x></y>.4</x>", "/x/y/x/text()",	changeText (const "x")	)
	   , ("<x p='.'>.0<x p='.1'>.1.0</x>.2<y p='.3'>.3.0</y>.4</x>", 		  "/x/y/x",		none			)
	   , ("<x p='.'>.0<x p='.1'>.1.0</x>.2<y p='.3'>.3.0zzz</y>.4</x>",		  "/x/y/x",		txt "zzz"		)
	   , ("<x p='.'>.0<x p='.1'>.1.0</x>.2<y p='.3'>.3.0<x p='.3.1' q='3.2'>.3.1.0</x></y>.4</x>",
											  "/x/y/x",		addAttr "q" "3.2"	)
	   ]
-- |
-- the complete set of test cases

allTests	:: Test
allTests
    = TestList
      [ nodeSetTests
      ]

main	:: IO ()
main
    = do
      c <- runTestTT allTests
      putStrLn $ show c
      let errs = errors c
	  fails = failures c
      System.exitWith (codeGet errs fails)

codeGet	:: Int -> Int -> ExitCode
codeGet errs fails
    | fails > 0       = ExitFailure 2
    | errs > 0        = ExitFailure 1
    | otherwise       = ExitSuccess

-- ----------------------------------------------------------
