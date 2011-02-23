--
-- RunTestCases - run the W3C test case suite with the Parser of the Haskell XML Toolbox
--
-- Author :s .\\artin Schmidt, Uwe Schmidt

module Main
where

import Text.XML.HXT.Parser	-- import all stuff for parsing, validating, and transforming XML

import System.IO		-- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe

import Network.URI

-- ------------------------------------------------------------

-- |
-- the starting point

main :: IO ()
main
    = do
      argv <- getArgs
      al   <- cmdlineOpts argv
      runTestCases $ newDocument' al

-- ------------------------------------------------------------
--
-- the "real" main program
--
-- runs in the trivial XmlState monad (with user state set to ())
-- so IO and access to global options is possible

type XCmd	= XState () ()
type XTCmd	= XmlTree -> XCmd

runTestCases	:: XmlTree -> IO ()
runTestCases t
    = run' $ parseTestDescr t
      where
      parseTestDescr	:: XTCmd
      parseTestDescr t'
	  = do
	    verbMsg "==> reading XML test suite control document"
	    res <- ( ( getWellformedDoc	-- do not validate, the W3C control file isn't valid
		       .>>
		       setTestCaseBaseURI
		     )
		     `containingM`
		     ( getChildren
		       .>
		       isTag "TESTSUITE"
		     )
		   ) t'
	    if null res
	       then io $ do
			 hPutStrLn stderr "no valid XML test suite control document: root tag must be TESTSUITE"
			 sorry
	       else return ()
	    execTestCases (head res)

      execTestCases	:: XTCmd
      execTestCases t'
	  = do
	    verbMsg "==> selecting test cases"
	    let absURI = valueOf transferURI t'
	    let base   = show . fromJust $ (fromJust . parseURI) "" `relativeTo` (fromJust . parseURI) absURI
	    verbMsg ("==> abs URI: " ++ absURI ++ " base URI: " ++ base)

	    res <- liftMf (editTestCases base)
		   .>>
		   liftMf selectTests
		   .>> 
		   runTests
		   $ t'

	    ( liftMf ( gatherStatistics
		      .>
		      processChildren ( formatStatistics
					+++
					formatResult base
					+++
					isXText
				      )
		    )
              .>>
              outTree ) $$< replaceChildren res t'

	    io $ lucky

      selectTests	:: XmlFilter
      selectTests
	  = deep (isTag "TEST")						-- select all TEST tags with the appropriate
	    `containing`						-- TYPE or ID
	    ( if null selectTestId && null selectTestClass
	      then
	      this
	      else
	      if null selectTestId
	      then
	      "TYPE" `hasValue` (== selectTestClass)
	      else
	      "ID" `hasValue` (== selectTestId)
	    )

      editTestCases	:: String -> XmlFilter
      editTestCases prefix
	  = processTopDownUntil (editTestCaseURI prefix)

      editTestCaseURI	:: String -> XmlFilter
      editTestCaseURI prefix
	  = cat [ selectBase						-- select the xml:base attribute and edit the subtrees
		  `containing`
		  ( isTag "TESTCASES" .> getValue "xml:base" )

		, ( editURI						-- test case found: add the xml:base prefix to URI attr
		    .>
		    ( editOUTPUT `when` getValue "OUTPUT" )		-- and to the OUTPUT attr
		  )
		  `containing`
		  (isTag "TEST" .> getValue "URI")
		]							-- else: do nothing
	    where
	    selectBase	:: XmlFilter
	    selectBase t'
		= processChildren (processTopDownUntil (editTestCaseURI newPrefix)) t'
		  where
		  newPrefix = prefix ++ valueOf "xml:base" t'

	    editURI	:: XmlFilter
	    editURI
		= modifyAttr "URI" (prefix ++)

	    editOUTPUT	:: XmlFilter
	    editOUTPUT
		= modifyAttr "OUTPUT" (prefix ++)

      runTests t'
	  = do
	    verbMsg ("==> run" ++ testMsg)
	    oldTraceLevel <- getTraceLevel
	    setTraceLevel trcTest
	    res <- run1Test testTYPE t'
	    setTraceLevel oldTraceLevel
	    verbMsg ("==> end" ++ testMsg)
	    return res
	  where
	  testTYPE = valueOf "TYPE" t'
	  testURI  = valueOf "URI"  t'
	  -- testID   = valueOf "ID"   t'
	  testOUT  = valueOf "OUTPUT" t'
	  testMsg  = "\tURI=" ++ testURI ++  "\tTYPE=" ++ testTYPE
	  testRoot = newDocument testURI

	  run1Test "valid"   tc
	      = do
		res <- ( getWellformedDoc
			 .>>
			 getValidatedDoc
			 .>>
			 ( if null testOUT
			   then thisM
			   else ( liftMf
				  .>>
				  liftMf (processChildren (isXTag +++ isXPi))
				  .>>
				  compareResult
				)
			 )
		       ) testRoot
		let root = head res
		let errLev = intValueOf a_status root
		addTestCaseResults errLev
				   ( if null (getChildren root)
				     then ("ERROR","Result of valid document differs from expected output")
				     else ("PASSED", "Valid document detected")
				   )
				   ("ERROR","Validation function did find errors in valid document")
				   ("FATAL","test aborted") tc


	  run1Test "invalid" tc
	      = do
		res1 <- getWellformedDoc testRoot
		res2 <- getValidatedDoc $$< res1
		let errLev = intValueOf a_status (head res2)
		addTestCaseResults errLev
				   ( if null (getChildren (head res1))
				     then ("PASSED / ERROR"
					  , "Parser found errors, but validation functions should find errors " ++
					    "Note: Some VC are checked by the parser, so this test might be OK!"
					  )
				     else ("PASSED","errors detected")
				   )
				   ("ERROR","Validation function did not find errors")
				   ("FATAL","test aborted") tc

	  run1Test "not-wf" tc
	      = do
		res1 <- getWellformedDoc testRoot
		res2 <- getValidatedDoc $$< res1
		let errLev = intValueOf a_status (head res2)
		addTestCaseResults errLev
				   ("ERROR","parser did not find errors")
				   ( if not (null (getChildren (head res1))) && null (getChildren (head res2))
				     then ("PASSED / ERROR"
					  , "Validation functions found errors, but parser should find errors " ++
					    "Note: Some WF constraints are checked by the validatation functions, so this test might be OK!"
					  )
				     else ("PASSED","errors detected")
				   )
				   ("FATAL","test aborted") tc

	  run1Test "error" tc
	      = do
		res <- ( getWellformedDoc
			 .>>
			 getValidatedDoc
		       ) testRoot
		let errLev = intValueOf a_status (head res)
		addTestCaseResults errLev
				   ("ERROR","parser and validation did not find errors")
				   ("PASSED","errors detected")
				   ("FATAL","test aborted") tc

	  run1Test _ tc
	      = addTestCaseResults c_fatal ("","") ("","") ("FATAL","test class not supported") tc

	  compareResult doc
	      = do
		verbMsg ("==> compare with reference " ++ show testOUT)
		ref <- ( getWellformedDoc
			 .>>
			 liftMf canonicalizeAllNodes
			 .>>
			 liftMf (processChildren (isXTag +++ isXPi))
		       ) $ newDocument testOUT
		let resStr = xshow . getChildren $ doc
		let refStr = xshow . getChildren $$ ref
		if resStr == refStr
		   then
		   return [doc]
		   else
		   do
		   verbMsg ( "==> content differs\n" ++
			     "==> expected:\n" ++ refStr ++ "\n" ++
			     "==> got:\n" ++ resStr ++ "\n"
			   )
		   
		   return [doc]

      addTestCaseResults lev (oks, okd) (es, ed) (fs, fd)
	  = liftMf
	    ( addAttr "RESULT" (status lev)
	      .>
	      addAttr "DESCR" (descr lev)
	    )
	    where
	    status l
		| l == c_err   = es
		| l == c_fatal = fs
		| otherwise    = oks
	    descr l
		| l == c_err   = ed
		| l == c_fatal = fd
		| otherwise    = okd

      setTestCaseBaseURI
	  = performAction
	    (\ t2 -> setBaseURI $ valueOf transferURI t2)

      outTree
	  = performAction
	    (\ t2 -> io
	             $ hPutStrLn stdout ((xshow . getChildren) t2)
	    )

      gatherStatistics	:: XmlFilter
      gatherStatistics t'
	  = replaceChildren (statistics t' ++ cases) t'
	    where
	    cases	= getChildren .> isTag "TEST" $ t'
	    passeds	= "RESULT" `hasValue` (== "PASSED") $$ cases
	    passErrs	= "RESULT" `hasValue` (== "PASSED / ERROR") $$ cases
	    errors	= "RESULT" `hasValue` (== "ERROR") $$ cases
	    fatals	= "RESULT" `hasValue` (== "FATAL") $$ cases
	    testFile	= valueOf transferURI t'

	    statistics
		= cat [ statEnt "TEST FILE     " testFile
		      , txt "\n"
		      , statNum "TEST CASES    " cases
		      , statNum "PASSED        " passeds
		      , statNum "PASSED / ERROR" passErrs
		      , statNum "ERROR         " errors
		      , statNum "FATAL         " fatals
		      , txt "\n"
		      ]

	    statEnt n v
		= etag "STATISTICS"
		  .> addAttr "NAME"  n
	          .> addAttr "VALUE" v

	    statNum n v
		= statEnt n (formatStr 8 . show . length $ v)
		  where
		  formatStr len str
		      = reverse . take len $ (reverse str ++ repeat ' ')

      formatStatistics	:: XmlFilter
      formatStatistics
	  = isTag "STATISTICS"
	    `guards`
	    cat [ getValue "NAME"
		, txt ": "
		, getValue "VALUE"
		, txt "\n"
		]

      formatResult	:: String -> XmlFilter
      formatResult base
	  = isTag "TEST"
	    `guards`
	    ( ( if onlyErr
		then "RESULT" `hasValue` (/= "PASSED")
		else this
	      )
              `guards`
	      cat [ getValue "RESULT"
		  , txt "\tURI="
		  , getValue "URI" .> modifyText (\ uri -> drop (length base) uri)
		  , txt "\tTYPE="
		  , getValue "TYPE"
		  , txt "\tID="
		  , getValue "ID"
		  , txt "\n"
		  , if noDescr
		    then none
		    else cat [ txt "\tdescription: "
			     , getValue "DESCR"
			     , txt "\n"
			     , deep isXText
			     , txt "\n"
			     ]
		  ]
	    )
	    
      verbMsg	:: String -> XCmd
      verbMsg msg
	  = if verbose
	    then io $ hPutStrLn stderr msg
	    else return ()

      selectTestClass	= xshow . getValue "tests"	$ t
      selectTestId	= xshow . getValue "test-id"	$ t
      verbose		= satisfies (hasAttr "verbose")   t
      onlyErr		= satisfies (hasAttr "only-err")  t
      noDescr		= satisfies (hasAttr "no-descr")  t
      trcTest		:: Int
      trcTest		= read . ("0" ++) . xshow . hasValue "trace-tests" (all (`elem` ['0'..'9'])) $ t

-- ------------------------------------------------------------

sorry :: IO a
sorry = exitWith (ExitFailure (-1))

lucky :: IO a
lucky = exitWith ExitSuccess

-- ------------------------------------------------------------
--
-- the options definition part
-- see doc for System.Console.GetOpt

progName	:: String
progName	= "RunTestCases"
    
options 	:: [OptDescr (String, String)]
options
    = [ Option "v"	["verbose"]	(NoArg  (att "verbose"  "1"))	"display a message if document is well-formed or valid"
      , Option "q"	["no-output"]	(NoArg  (att "quiet"    "1"))	"no output of resulting document"
      , Option "h?"	["help"]	(NoArg  (att "usage"    "1"))	"this message"
      , Option "p"	["proxy"]	(ReqArg  prx "PROXY")		"proxy for http access (e.g. \"www-cache:3128\")"
      , Option ""	[a_use_curl]	(NoArg  (att a_use_curl "1"))	"HTTP access via external program \"curl\", more stable, more general, less efficient"
      , Option ""	[a_options_curl]	(ReqArg (att a_options_curl) "STR")	"additional curl options, e.g. for timeout, ..."
      , Option "e"	["encoding"]	(ReqArg  enc "CHARSET")		("default document encoding (" ++ utf8 ++ ", " ++ isoLatin1 ++ ", " ++ usAscii ++ ", ...)")
      , Option ""	["test-type"]	(ReqArg  tst "TEST-TYPE")	"one of \"valid\", \"invalid\", \"not-wf\", \"error\" (default: all test cases)"
      , Option ""	["test-id"]	(ReqArg  tid "TEST-ID")		"test case ID for selecting one specific test case"
      , Option ""	["only-err"]	(NoArg  (att "only-err" "1"))	"report only test cases, that did not pass"
      , Option ""	["no-descr"]	(NoArg  (att "no-descr" "1"))	"don't report test description"
      , Option "t"	["trace-main"]	(OptArg (trc a_trace) "LEVEL")	("trace level (0-4) for " ++ progName ++ ", default 1")
      , Option ""	["trace-tests"]	(OptArg (trc "trace-tests") "LEVEL")	"trace level (0-4) for test cases, default 1"
      ]
    where
    att n v	= (n, v)
    prx = att "proxy"
    enc = att "encoding"
    tid = att "test-id"
    trc a = att a  . show . max 0 . min 9 . (read :: String -> Int) . ('0':) . filter (`elem` "0123456789") . fromMaybe "1"
    tst = att "tests" . check
	  where
	  val = ["valid", "invalid", "not-wf", "error"]
	  check v
	      | v `elem` val	= v
	      | otherwise	= "none"

usage		:: [String] -> IO a
usage errl
    | null errl
	= do
	  hPutStrLn stdout use
	  lucky
    | otherwise
	= do
	  hPutStrLn stderr (concat errl ++ "\n" ++ use)
	  sorry
    where
    header = "RunTestCases - Test Validating XML Parser of the Haskell XML Toolbox\n\n" ++
             "  input document must be a XML document conforming to DTD \"testcases.dtd\" from W3C\n" ++
             "  W3C conformance tests can be found under \"http://www.w3.org/XML/Test/\",\n" ++
             "  official control file is\n" ++
             "  \"http://dev.w3.org/cvsweb/~checkout~/2001/XML-Test-Suite/xmlconf/xmlconf.xml\"\n" ++
             "  test protocol and error messages are written to stderr\n" ++
             "  test result summary is written to stdout\n" ++
             "\n" ++
             "Usage: " ++ progName ++ " [OPTION...] <URI or FILE>"
    use    = usageInfo header options

cmdlineOpts 	:: [String] -> IO (Attributes)
cmdlineOpts argv
    = case (getOpt Permute options argv) of
      (ol,n,[]  )
	  -> do
	     sa <- src n
	     help (lookup "usage" ol)
	     tst . fromMaybe "" . lookup "tests" $ ol
	     return (ol ++ sa)
      (_,_,errs)
	  -> usage errs
    where
    src []	= return [("source", "")]
    src [uri]	= return [("source", uri)]
    src _	= usage ["only one input uri or file allowed\n"]

    tst "none"	= usage ["illegal argument for tests option\n"]
    tst _	= return ()

    help Nothing	= return ()
    help (Just _)	= usage []

-- ------------------------------------------------------------
