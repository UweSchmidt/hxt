module Main
where

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Text.XML.HXT.RelaxNG
import Text.XML.HXT.XPath
import Text.XML.HXT.XSLT
import Text.XML.HXT.Cache

#ifdef curl
import Text.XML.HXT.Curl
#endif

#ifdef tagsoup
import Text.XML.HXT.TagSoup
#endif

-- ----------------------------------------------------------

import Data.Maybe

import System.Directory
import System.Exit
import System.IO

import Test.HUnit

-- ----------------------------------------------------------

example1        :: (String, String)
example1        = ( "example1.xml"
                  , unlines $
                    [ "<?xml version='1.0'?>"
                    , "<html> <head> </head> <body> </body> </html>"
                    ]
                  )

example2        :: (String, String)
example2        = ( "example2.xml"
                  , unlines $
                    [ "<?xml version='1.0'?>"
                    , "<html><head/><body/></html>"
                    ]
                  )

example3        :: (String, String)
example3        = ( "example3.xml"
                  , unlines $
                    [ "<?xml version='1.0'?>"
                    , "<html>&auml;</html>"
                    ]
                  )

example4        :: (String, String)
example4        = ( "example4.html"
                  , unlines $
                    [ "<html>&auml;</html>"
                    ]
                  )

example5        :: (String, String)
example5        = ( "example5.xml"
                  , unlines $
                    [ "<?xml version='1.0'?>"
                    , "<html><!-- xxx --></html>"
                    ]
                  )

example6        :: (String, String)
example6        = ( "example6.xml"
                  , unlines $
                    [ "<?xml version='1.0'?>"
                    , "<!DOCTYPE a ["
                    , "<!ELEMENT a EMPTY>"
                    , "]>"
                    , "<a></a>"
                    ]
                  )

example7        :: (String, String)
example7        = ( "example7.xml"
                  , unlines $
                    [ "<?xml version='1.0'?>"
                    , "<!DOCTYPE a ["
                    , "<!ELEMENT a EMPTY>"
                    , "]>"
                    , "<a>x</a>"
                    ]
                  )

example8        :: (String, String)
example8        = ( "example8.xml"
                  , unlines $
                    [ "<?xml version='1.0'?>"
                    , "<n:a xmlns:n='nnn'/>"
                    ]
                  )

example9        :: (String, String)
example9        = ( "example9.xml"
                  , unlines $
                    [ "<?xml version='1.0'?>"
                    , "<n:a xmlns:m='nnn'/>"
                    ]
                  )

example10       :: (String, String)
example10       = ( "example10.xml"
                  , unlines $
                    [ "<a><![CDATA[x]]></a>"
                    ]
                  )

example11       :: (String, String)
example11       = ( "example11.txt"
                  , unlines $
                    [ "hello"
                    ]
                  )

examples        :: [(String, String)]
examples        = [ example1
                  , example2
                  , example3
                  , example4
                  , example5
                  , example6
                  , example7
                  , example8
                  , example9
                  , example10
                  , example11
                  ]

urlw3
  , urlw3c	:: String
urlw3		= "http://www.w3.org/"
urlw3c		= "http://www.w3c.org/"

genInputFile    :: (String, String) -> IO ()
genInputFile f  = do
                  remInputFile (fst f)
                  uncurry writeFile f

remInputFile    :: String -> IO ()
remInputFile f  = do
                  e <- doesFileExist f
                  if e
                     then removeFile f
                     else return ()

genInputFiles   :: IO ()
genInputFiles   = sequence_ $ map genInputFile $ examples

remInputFiles   :: IO ()
remInputFiles   = sequence_ $ map (remInputFile . fst) $ examples

-- ----------------------------------------------------------

configParseTests        :: Test
configParseTests
    = TestLabel "Test simple parser configurations" $
      TestList $
      zipWith mkTest [1..] $
      [ ( "example1.xml"
        , []
        , "<html> <head> </head> <body> </body> </html>"
        )
      , ( "example1.xml"
        , [withRemoveWS no]
        , "<html> <head> </head> <body> </body> </html>"
        )
      , ( "example1.xml"
        , [withRemoveWS yes]
        , "<html><head/><body/></html>"
        )
      , ( "example2.xml"
        , [withRemoveWS no]
        , "<html><head/><body/></html>"
        )
      , ( "example2.xml"
        , [withRemoveWS yes]
        , "<html><head/><body/></html>"
        )
      , ( "example2.xml"
        , [withRemoveWS yes]
        , "<html><head/><body/></html>"
        )

      , ( "example3.xml"
        , []
        , ""                    -- parser error in &auml;
        )
      , ( "example4.html"
        , [withParseHTML yes]
        , "<html>\228</html>"   -- no parse error
        )
      , ( "example4.html"
        , [withParseHTML no]
        , ""                    -- parse XML: error
        )
      , ( "example3.xml"
        , [withParseByMimeType yes]
        , ""                    -- parser error in &auml;
        )
      , ( "example4.html"
        , [withParseByMimeType yes]
        , "<html>\228</html>"   -- no parse error
        )


      , ( "example5.xml"
        , []
        , "<html/>"
        )
      , ( "example5.xml"
        , [withPreserveComment no]
        , "<html/>"
        )
      , ( "example5.xml"
        , [withPreserveComment yes]
        , "<html><!-- xxx --></html>"
        )

      , ( "example6.xml"                -- withValidate
        , []
        , "<a/>"
        )
      , ( "example6.xml"
        , [withValidate yes]
        , "<a/>"
        )
      , ( "example6.xml"
        , [withValidate no]
        , "<a/>"
        )
      , ( "example7.xml"
        , []
        , ""
        )
      , ( "example7.xml"
        , [withValidate yes]
        , ""
        )
      , ( "example7.xml"
        , [withValidate no]
        , "<a>x</a>"
        )

      , ( "example8.xml"                -- namespace check
        , []
        , "<n:a xmlns:n=\"nnn\"/>"
        )
      , ( "example8.xml"
        , [withCheckNamespaces no]
        , "<n:a xmlns:n=\"nnn\"/>"
        )
      , ( "example8.xml"
        , [withCheckNamespaces yes]
        , "<n:a xmlns:n=\"nnn\"/>"
        )
      , ( "example9.xml"
        , []
        , "<n:a xmlns:m=\"nnn\"/>"
        )
      , ( "example9.xml"
        , [withCheckNamespaces no]
        , "<n:a xmlns:m=\"nnn\"/>"
        )
      , ( "example9.xml"
        , [withCheckNamespaces yes]
        , ""
        )

      , ( "example10.xml"
        , []
        , "<a>x</a>"
        )
      , ( "example10.xml"
        , [withCanonicalize yes]
        , "<a>x</a>"
        )
      , ( "example10.xml"
        , [withParseHTML yes, withCanonicalize no]
        , "<a><![CDATA[x]]></a>\n"
        )

      , ( "example11.txt"               -- parse by mimetype and none XML/HTML contents
        , []
        , ""
        )
      , ( "example11.txt"
        , [withParseByMimeType yes]
        , "hello\n"
        )
      , ( "example11.txt"
        , [withIgnoreNoneXmlContents yes, withParseByMimeType yes]
        , ""
        )
      ]
    where
    mkTest cnt (prog, config, exp)
        = TestCase $
          do
          res <- runX $
                 configSysVars config
                 >>>
                 readDocument [] prog
                 >>>
                 writeDocumentToString [withRemoveWS no]
          assertEqual (show cnt ++ ". Simple parser test:") exp (concat res)

-- ----------------------------------------------------------

#ifdef tagsoup
configTagSoupTests      :: Test
configTagSoupTests
    = TestLabel "Test TagSoup parser configurations" $
      TestList $
      zipWith mkTest [1..] $
      [
        ( "example2.xml"
        , []
        , "<html><head/><body/></html>"
        )
      , ( "example2.xml"
        , [withRemoveWS yes]
        , "<html><head/><body/></html>"
        )
      , ( "example3.xml"
        , [withTagSoup]
        , "<html>\228</html>"
        )

      , ( "example5.xml"
        , [withPreserveComment no]
        , "<html/>"
        )
      , ( "example5.xml"
        , [withPreserveComment yes]
        , "<html><!-- xxx --></html>"
        )


      , ( "example8.xml"                -- namespace check
        , []
        , "<n:a xmlns:n=\"nnn\"/>"
        )
      , ( "example8.xml"
        , [withCheckNamespaces no]
        , "<n:a xmlns:n=\"nnn\"/>"
        )
      , ( "example8.xml"
        , [withCheckNamespaces yes]
        , "<n:a xmlns:n=\"nnn\"/>"
        )
      , ( "example9.xml"
        , []
        , "<n:a xmlns:m=\"nnn\"/>"
        )
      , ( "example9.xml"
        , [withCheckNamespaces no]
        , "<n:a xmlns:m=\"nnn\"/>"
        )
      , ( "example9.xml"
        , [withCheckNamespaces yes]
        , ""
        )

      , ( "example10.xml"
        , []
        , "<a>x</a>"
        )
      , ( "example10.xml"
        , [withCanonicalize yes]
        , "<a>x</a>"
        )
      , ( "example10.xml"               -- tagsoup removes CDATAs
        , [withParseHTML yes, withCanonicalize no]
        , "<a>x</a>"
        )

      , ( "example11.txt"               -- parse by mimetype and none XML/HTML contents
        , []
        , ""
        )
      , ( "example11.txt"
        , [withParseByMimeType yes]
        , "hello\n"			-- TODO parse by mimetype does not work together with tagsoup
        )
      , ( "example11.txt"
        , [withIgnoreNoneXmlContents yes, withParseByMimeType yes]
        , ""
        )
      ]
    where
    mkTest cnt (prog, config, exp)
        = TestCase $
          do
          res <- runX $
                 configSysVars (config ++ [withTagSoup])
                 >>>
                 readDocument [] prog
                 >>>
                 writeDocumentToString [withRemoveWS no]
          assertEqual (show cnt ++ ". TagSoup parser test:") exp (concat res)
#else
configTagSoupTests      :: Test
configTagSoupTests      = TestList []
#endif

-- ----------------------------------------------------------

#ifdef curl
configCurlTests      :: Test
configCurlTests
    = TestLabel "Test Curl parser configurations" $
      TestList $
      zipWith mkTest [1..] $
      [ ( urlw3, []
        , getAttrValue transferStatus
        , "999"
        )
      , ( urlw3, [withCurl []
                 ,withValidate no
                 ]
        , getAttrValue transferStatus
        , "200"
        )
      , ( urlw3c, [withCurl []
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "301"
        )
      ]
    where
    mkTest cnt (url, config, proc, exp)
        = TestCase $
          do
          res <- runX $
                 configSysVars config
                 >>>
                 readDocument [] url
                 >>>
                 proc
          assertEqual (show cnt ++ ". Curl HTTP test:") exp (concat res)
#else
configCurlTests      :: Test
configCurlTests      = TestList []
#endif

-- ----------------------------------------------------------

allTests        :: Test
allTests
    = TestList
      [ TestLabel "Generate test input files" $
        TestCase genInputFiles

      , configParseTests
      , configTagSoupTests
      , configCurlTests

      , TestLabel "Remove test input files" $
        TestCase remInputFiles
      ]

main    :: IO ()
main
    = do
      genInputFiles
      c <- runTestTT allTests
      putStrLn $ show c
      let errs = errors c
          fails = failures c
      exitWith (codeGet errs fails)

codeGet :: Int -> Int -> ExitCode
codeGet errs fails
    | fails > 0       = ExitFailure 2
    | errs > 0        = ExitFailure 1
    | otherwise       = ExitSuccess

-- ----------------------------------------------------------
