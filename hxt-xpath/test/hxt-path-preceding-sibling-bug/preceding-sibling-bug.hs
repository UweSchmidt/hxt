{-# LANGUAGE Arrows #-}

import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import System.Process
import System.Exit

test f = do
  doc <- readFile "five.xml"
  runX $ constA doc >>> xread >>> f >>> getChildren >>> getText

preceding :: (ArrowXml a) => String -> a XmlTree XmlTree
preceding s =
  getXPathTrees ("top/third/preceding-sibling::*[" ++ s ++ "]")
  
following :: (ArrowXml a) => String -> a XmlTree XmlTree
following s =
  getXPathTrees ("top/third/following-sibling::*[" ++ s ++ "]")
  

t1 = test $ preceding "1"
t2 = test $ preceding "last()"
t3 = test $ following "1"
t4 = test $ following "last()"

runC :: String -> IO ()
runC command = do
  handle <- runCommand command
  exitCode <- waitForProcess handle
  case exitCode of
        ExitSuccess -> return ()
        ExitFailure x ->  putStrLn $ "Exit code:[" ++ (show x) ++ "]"

run_hxt_xpath = do
  putStrLn "Running tests with hxt-xpath"
  a <- t1
  b <- t2
  c <- t3
  d <- t4
  putStrLn $ concat (a++b++c++d)
  putStrLn "Running tests with xalan"
  runC xalan 
  e <- readFile "xalan-result.xml"
  putStrLn $ drop 1 . dropWhile (/= '>') $ e
  putStrLn "As you can see, they don't agree"

xalan  = "xalan -in five.xml -xsl test-siblings.xsl >xalan-result.xml"

main = run_hxt_xpath

{-

On my system, this results in:

Running tests with hxt-xpath
1245
Running tests with xalan
2145
As you can see, they don't agree


-}