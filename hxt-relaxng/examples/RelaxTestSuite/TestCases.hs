-- |
-- run the Relax NG Testsuite created be James Clark (<http://www.relaxng.org/#test-suites>)
-- 

module TestCases
  ( runTest )
where

import Text.XML.HXT.RelaxNG.Validator
import Text.XML.HXT.RelaxNG.Schema
import Text.XML.HXT.Arrow 
import Text.XML.HXT.DOM.FormatXmlTree

import Test.HUnit
import Data.List 
  ( isSuffixOf )

import ReadDir

-- ------------------------------------------------------------

{- | run the Relax NG Testsuite

   * 1.parameter  :  Directory containing the testcases
   
   - return  : HUnit statistics
   
example test program:

> main :: IO ()
> main = do
>        res <- runTest "./testCases"
>        putStrLn $ show res

-}  

runTest :: FilePath ->  IO Counts
runTest filePath
    = do
      dir     <- readDir filePath ""
      runTestTT $ TestList $ generateTestCases [] [dir] relaxSchemaTree


generateTestCases :: Attributes -> [Entry] -> XmlTree -> [Test]

generateTestCases _  [] _
    = []

generateTestCases _  [ DirContent "" _ ] _
    = []

generateTestCases al [ DirContent rngFile [] ] spezi
    = [generateTestCase al rngFile "" spezi]

generateTestCases al [ DirContent rngFile xmlFiles ] spezi
    = map (\x -> generateTestCase al rngFile x spezi) xmlFiles

generateTestCases al [ Dir xs ] spezi
    = generateTestCases al xs spezi

generateTestCases al (x:xs) spezi
    = generateTestCases al [x] spezi ++ generateTestCases al xs spezi

 
generateTestCase :: Attributes -> FilePath -> FilePath -> XmlTree -> Test
generateTestCase al rngFile xmlFile spezi
  = TestLabel formatLabel $ 
    TestCase $  
    do
    res <- runX $ constA spezi 
                  >>> 
                  validateWithSpezification al xmlFile rngFile
    assertBool (formatOutput res) 
               ( if ("i.rng" `isSuffixOf` rngFile) 
                 then not (null res)                                    -- the result of an incorrect schema must not be an empty list of errors
                 else ("i.xml" `isSuffixOf` xmlFile) == not (null res)
                                                                        -- correct schema, but incorrect document:
                                                                        -- the result of in incorrect xml document
                                                                        -- must not be an empty list of errors
               )
    where
    formatLabel  = "Schema=" ++ rngFile ++ 
                   (if xmlFile /= "" then ", instanz=" ++ xmlFile else "")
    formatOutput = concat . map formatXmlTree

-- ------------------------------------------------------------
