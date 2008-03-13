module Main
where

import Text.XML.HXT.Arrow
import qualified Text.XML.HXT.Arrow.ParserInterface      as V
import qualified Text.XML.HXT.Validator.ValidationFilter as F

-- old stuff
-- validateDoc			= arrL VA.validate
-- transformDoc			= arrL VA.transform

import System.Environment

main	:: IO ()
main	= do
	  args <- getArgs
	  main1 ( if null args		-- assumption: prog is called in test dir
		  then "../examples/w3ctest/xml-test-suite/xmlconf/xmlconf.xml"
		  else head args
		)
	  -- runX (testDTDVali src)
	  return ()

-- test1 = main1 "/home/uwe/haskell/hxt/curr/examples/w3ctest/xml-test-suite/xmlconf/xmlconf.xml"

main1	:: String -> IO ()
main1 config
    = do
      runX (w3cValidationSuite config)
      return ()

w3cValidationSuite	:: String -> IOSArrow XmlTree XmlTree
w3cValidationSuite config
    = readDocument [ (a_validate, v_1)
		   , (a_canonicalize, v_1)
		   ] config
      >>>
      ( processTestSuite $< getBaseURI )

processTestSuite	:: String -> IOSArrow XmlTree XmlTree
processTestSuite base
    = getChildren
      >>>
      hasName "TESTSUITE"
      >>>
      perform ( getAttrValue "PROFILE"
		>>>
		arr (("running test suite " ++) . show)
		>>>
		arrIO putStrLn
	      )
      >>>
      getChildren
      >>>
      processTestCasesAndTests base

processTestCasesAndTests	:: String -> IOSArrow XmlTree XmlTree
processTestCasesAndTests base
    = choiceA
      [ hasName "TESTCASES" :-> (processTestCases $< xmlBase base)
      , hasName "TEST"      :-> processTest base
      , this                :-> none
      ]

processTestCases	:: String -> IOSArrow XmlTree XmlTree
processTestCases base
    = perform ( getAttrValue "PROFILE"
		>>>
		arr (("running test cases " ++) . show)
		>>>
		arrIO putStrLn
	      )
      >>>
      getChildren >>> processTestCasesAndTests base

processTest	:: String -> IOSArrow XmlTree XmlTree
processTest base
    = runTest $< ( ( getAttrValue "URI"
		     &&&
		     constA base ) >>> expandURI )
    where
    runTest uri
	= perform ( arrIO0 $ putStrLn ("running test " ++ show uri))
	  >>>
	  perform (runInLocalURIContext (testDTDVali uri))
	  >>>
	  clearErrStatus

xmlBase	:: String -> IOSArrow XmlTree String
xmlBase base
    = ( ( getAttrValue "xml:base"
	  &&&
	  constA base
	) >>> expandURI ) `withDefault` base

oldValidateDoc
  , oldTransformDoc		:: ArrowList a => a XmlTree XmlTree

oldValidateDoc	= arrL F.validate
oldTransformDoc	= arrL F.transform

testDTDVali	:: String -> IOSArrow XmlTree XmlTree
testDTDVali src
    = errorMsgIgnore
      >>>
      readDocument [ (a_validate, v_0)
		   , (a_canonicalize, v_0)
		   , (a_trace, v_0)
		   ] src
      >>>
      errorMsgStderr
      >>>
      ( documentStatusOk
	`guards`
	( ( evalValidate src $<< ( (listA (oldValidateDoc >>> (processChildren none)))
			       &&&
			       listA (fromLA V.validateDoc)
			     )
	  >>>
	  ( evalTransform $<< oldTransformDoc &&& (fromLA V.transformDoc) )
	  >>>
	  this -- writeDocument [] ""
	  )
	)
      )

evalValidate	:: String -> XmlTrees -> XmlTrees -> IOSArrow XmlTree XmlTree
evalValidate src err1 err2
    | err1 == err2
	= perform ( arrIO0 (putStrLn $ "no differences in " ++ show (length err1) ++ " error messages in " ++ show src))
    | otherwise
	= perform ( arrIO0 ( putStrLn $
			    unlines [ "validation results differ in " ++ show src
				    , show err1
				    , show err2
				    , "old error messages"
				    ] ) )
	  >>>
	  perform ( constL err1 >>> filterErrorMsg )
	  >>>
	  perform ( arrIO0 ( putStrLn $
			    unlines [ ""
				    , "new error messages"
				    ] ) )
	  >>>
	  perform ( constL err2 >>> filterErrorMsg )


evalTransform	:: XmlTree -> XmlTree -> IOSArrow XmlTree XmlTree
evalTransform t1 t2
    | runLA (xshow this) t1 == runLA (xshow this) t2
	= this
    | otherwise
	= perform ( arrIO0 ( putStrLn $
			     unlines [ "transformation results differ"
				     , "old document"
				     ] ) )
	  >>>
	  perform ( xshow (constA t1) >>> arrIO putStrLn )
	  >>>
	  perform ( arrIO0 ( putStrLn "newDocument" ) )
	  >>>
	  perform ( xshow (constA t2) >>> arrIO putStrLn )
	  