module Main
where

import Control.Monad	( sequence_
                        )

import Data.Maybe

import System.Exit

import Text.XML.HXT.Core

-- ------------------------------------------------------------

data TestSuite
    = TS
      { tscases   :: [TestCases]
      , tsprofile :: String
      }
      deriving (Eq, Show)

data TestCases
    = Tcs
      { profile :: String
      , xmlbase :: String
      , tcases  :: [TCase]
      }
      deriving (Eq, Show)

data TCase
    = TCE Test
    | TCS TestCases
      deriving (Eq, Show)

data Test
    = Test
      { tentities :: TestEntities
      , tid       :: String
      , toutput   :: String
      , toutput3  :: String
      , tsections :: String
      , ttype     :: TestType
      , turi      :: String
      , tns       :: Bool
      , tdescr    :: String
      }
      deriving (Eq, Show)

data TestEntities
    = Both | None | Parameter | General
      deriving (Eq, Show)

data TestType
    = Valid | Invalid | NotWellformed | Error
      deriving (Eq, Show)

-- ------------------------------------------------------------

xpTestSuite	:: PU TestSuite
xpTestSuite
    = xpElem "TESTSUITE" $
      xpWrap ( \ (ts, tp) -> TS ts tp
             , \ (TS ts tp) -> (ts, tp)
             ) $
      xpPair (xpList $ xpTestCases)
             (xpDefault "" $ xpAttr "PROFILE" xpText0)

xpTestCases	:: PU TestCases
xpTestCases
    = xpElem "TESTCASES" $
      xpWrap ( \ (p, x, ts) -> Tcs p x ts
             , \ (Tcs p x ts) -> (p, x, ts)
             ) $
      xpTriple (xpDefault "" $ xpAttr "PROFILE"  xpText0)
               (xpDefault "" $ xpAttr "xml:base" xpText0)
               (xpList xpTCase)

xpTCase		:: PU TCase
xpTCase
    = xpAlt tag ps
    where
    tag (TCE _) = 0
    tag (TCS _) = 1
    ps = [ xpWrap ( TCE
                  , \ (TCE t) -> t
                  ) $
           xpTest
         , xpWrap ( TCS
                  , \ (TCS ts) -> ts
                  ) $
           xpTestCases
         ]

xpTest		:: PU Test
xpTest
    = xpElem "TEST" $
      xpWrap ( \ (x1, x2, x3, x4, x5, x6, x7, x8, x9) -> Test x1 x2 x3 x4 x5 x6 x7 x8 x9
             , \ (Test x1 x2 x3 x4 x5 x6 x7 x8 x9) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9)
             ) $
      -- TODO xpDefault (None, "xxx", "", "", "", Error, "", False, "no test") $ 
      xp9Tuple (xpAttr "ENTITIES" xpTestEntities)
               (xpAttr "ID" xpText0)
               (xpDefault "" $ xpAttr "OUTPUT" xpText0)
               (xpDefault "" $ xpAttr "OUTPUT3" xpText0)
               (xpAttr "SECTIONS" xpText0)
               (xpAttr "TYPE" xpTestType)
               (xpAttr "URI" xpText0)
               (xpAttr "NAMESPACE" xpNamespace)
               (xpDefault "xxx" $ xpXmlText)	-- TODO

xpTestType	:: PU TestType
xpTestType
    = xpWrap ( \ x -> fromJust . lookup x $ ttypes
             , \ y -> fromJust . lookup y $ ttypes'
             ) $
      xpText
    where
    ttypes  = [("valid", Valid), ("invalid", Invalid), ("not-wf", NotWellformed), ("error", Error)]
    ttypes' = map (\ (x, y) -> (y, x)) ttypes

xpNamespace	:: PU Bool
xpNamespace
    = xpWrap ( \ x -> fromJust . lookup x $ ns
             , \ y -> fromJust . lookup y $ ns'
             ) $
      xpText
    where
    ns  = [("yes", True), ("no", False)]
    ns' = map (\ (x, y) -> (y, x)) ns


xpTestEntities	:: PU TestEntities
xpTestEntities
    = xpWrap ( \ x -> fromJust . lookup x $ tentities
             , \ y -> fromJust . lookup y $ tentities'
             ) $
      xpText
    where
    tentities  = [("both", Both), ("none", None), ("parameter", Parameter), ("general", General)]
    tentities' = map (\ (x, y) -> (y, x)) tentities

-- ------------------------------------------------------------

ts0 = TS
      [ Tcs "1. testcase" "xyz/abc" $
        [ TCE $ Test None "id1" "out" "" "section 1" Error "emil.xml" False "emil sein test"
        ]
      , Tcs "2. testcase suite" "abc/123" $
        [ TCS $
          Tcs "2.1 testcase" "456" $
          [ TCE $ Test None "id21" "out" "" "section 2.1" Error "egon.xml" False "egon sein test"
          ]
        ]
      ]
      "test profile"

-- ------------------------------------------------------------

main	:: IO ()
main
    = do
      r <- runX
           ( readDocument [ withValidate yes
                          , withTrace 1
                          , withRemoveWS yes
                          , withCanonicalize yes
                          ] "xml-test-suite/xmlconf/xmlconf.xml"
             >>>
             xunpickleVal xpTestSuite
             >>>
             perform (arrIO $ runTests "xml-test-suite/xmlconf/")
             >>>
             this -- xpickleDocument xpTestSuite [withIndent yes] "" -- TODO
           )
      exitWith ( if null r
                 then ExitFailure 1
                 else ExitSuccess
               )

-- ------------------------------------------------------------

runTests	:: String -> TestSuite -> IO ()
runTests xmlbase ts
    = do
      putStrLn $ "test suite contains " ++ show (noOfTests ts) ++ " tests"
      runTest xmlbase ts

noOfTests
    = sum . map nts . tscases
    where
    nts :: TestCases -> Int
    nts = sum . map ntc . tcases
    ntc :: TCase -> Int
    ntc (TCE _ ) = 1
    ntc (TCS ts) = nts ts

-- ------------------------------------------------------------

class RunTest t where
    runTest	:: String -> t -> IO ()

instance RunTest TestSuite where
    runTest base (TS ts pr)
        = do
          putStrLn $ "run test suite " ++ pr ++ " ( base is " ++ show base ++ " )"
          sequence_ $ map (runTest base) ts

instance RunTest TestCases where
    runTest base (Tcs pr xmlbase ts)
        = do
          putStrLn $ "run test cases in " ++ pr ++ " ( base is " ++ show newbase ++ " )"
          sequence_ $ map (runTest newbase) ts
          where
          newbase = base ++ xmlbase

instance RunTest TCase where
    runTest base (TCE t)
        = runTest base t
    runTest base (TCS ts)
        = runTest base ts

instance RunTest Test where

    runTest base (Test entities id' out out3 sect Valid uri ns descr)
        = do
          putStrLn $ "run test case " ++ show newuri
          r <- runX
               ( (run1 Valid) out base uri
                 `orElse`
                 tfat "test aborted"
               )
          putStrLn $ show $ head r
          where
          newuri = base ++ uri

          run1 Valid = validTest
          run1 _     = notYetDone

    runTest base (Test entities id' out out3 sect typ uri ns descr)
        = return ()

notYetDone	:: String -> String -> String -> IOSArrow XmlTree TestResult
notYetDone out base uri
    = tfat $ "test " ++ show (base ++ uri) ++ " not yet implemented"

validTest	:: String -> String -> String -> IOSArrow XmlTree TestResult
validTest out base uri
    = readDocument [withValidate yes] uri'
      >>>
      ( if null out
        then this
        else ( canonicalizeAllNodes
               >>>
               processChildren (isElem <+> isPi)
               >>>
               compareResults $<
               ( readDocument [withValidate no] out'
                 >>>
                 processChildren (isElem <+> isPi)
               )
             )
      )
      >>>
      evalRes (ifA getChildren
	          (tpas "Valid document detected")
 		  (terr "Result of valid document differs from expected output")
              )
              (terr "Validation function did find errors in valid document")
	      (tfat "test aborted")
    where
    uri' = base ++ uri
    out' = base ++ out

compareResults :: XmlTree-> IOSArrow XmlTree XmlTree
compareResults expected
    = perform $
      ( xshow getChildren
        &&&
        xshow (constA expected >>> getChildren)
      )
      >>>
      ( ifA ( arr2 (==) >>> isA id )
            ( arr fst )
            ( perform showDiff
              >>>
              arr fst
              >>>
              issueErr "result differs from expected result"
            )
      )
    where
    showDiff
        = arrIO (\ res -> putStrLn ("got:\n" ++ res))
          ***
          arrIO (\ exp -> putStrLn ("expected:\n" ++ exp))

data TestResult
    = TestPassed String
    | TestError  String
    | TestFatal  String
      deriving (Show)

tpas, terr, tfat :: ArrowXml a => String -> a b TestResult

tpas    = constA . TestPassed
terr	= constA . TestError
tfat    = constA . TestFatal

evalRes	::  IOSArrow XmlTree TestResult -> IOSArrow XmlTree TestResult -> IOSArrow XmlTree TestResult -> IOSArrow XmlTree TestResult
evalRes p e f
    = eval' $< getErrStatus
    where
    eval' i
        | i == c_err   = e
        | i == c_fatal = f
        | otherwise    = p

-- ------------------------------------------------------------
