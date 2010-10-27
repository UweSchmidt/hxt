module Main
where

import Text.XML.HXT.Core

#ifdef http
import Text.XML.HXT.HTTP
#endif

#ifdef relaxng
import Text.XML.HXT.RelaxNG
#endif

#ifdef xpath
import Text.XML.HXT.XPath
#endif

#ifdef xslt
import Text.XML.HXT.XSLT ()
#endif

#ifdef cache
#ifdef curl
import Text.XML.HXT.Cache
import Text.XML.HXT.Arrow.XmlCache
#ifdef bzip
import Codec.Compression.BZip ( compress, decompress )
#endif
#endif
#endif

#ifdef curl
import Text.XML.HXT.Curl
#endif

#ifdef tagsoup
import Text.XML.HXT.TagSoup
#endif

-- ----------------------------------------------------------

import Data.Maybe       ()

import System           ( system )
import System.Directory
import System.Exit
import System.IO        ()

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

example2a       :: (String, String)
example2a       = ( "example2a.xml"
                  , unlines $
                    [ "<?xml version='1.0' encoding='UTF-8'?>"
                    , "<html><head/><body>&lt;&#224;&gt;</body></html>"
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

example12       :: (String, String)
example12       = ( "example12.xml"
                  , "<foo/>"
                  )

example12i      :: (String, String)
example12i      = ( "example12i.xml"
                  , "<foo>x</foo>"
                  )

example12s      :: (String, String)
example12s      = ( "example12.rng"
                  , unlines $
                    [ "<grammar xmlns=\"http://relaxng.org/ns/structure/1.0\">"
                    , "  <start>"
                    , "    <element name=\"foo\">"
                    , "      <empty/>"
                    , "    </element>"
                    , "  </start>"
                    , "</grammar>"
                    ]
                  )

example13o      :: (String, String)
example13o      = ( "example13.o"
                  , "<foo>\228</foo>"
                  )


examples        :: [(String, String)]
examples        = [ example1
                  , example2
                  , example2a
                  , example3
                  , example4
                  , example5
                  , example6
                  , example7
                  , example8
                  , example9
                  , example10
                  , example11
                  , example12
                  , example12i
                  , example12s
                  , example13o
                  ]

urlw3
  , urlw3h, urlw3c, html32      :: String

urlw3           = "http://www.w3.org/"
urlw3h          = "http://www.w3.org/Home.html"
urlw3c          = "http://www.w3c.org/"
html32          = "http://www.w3.org/TR/REC-html32-19970114"

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

cacheDir        :: String
cacheDir        = "./.hxt-test-cache"

genCacheDir     :: IO ()
genCacheDir     = createDirectoryIfMissing True cacheDir

remCacheDir     :: IO ()
remCacheDir     = system ("find " ++ cacheDir ++ " -type f | xargs rm -f") >> return ()
                  -- removeDirectoryRecursive cacheDir

-- ----------------------------------------------------------

mkTest0         :: String -> (String, SysConfigList, String) -> Test
mkTest0 msg (prog, config, expected)
    = mkTest2 msg (prog, config, writeDocumentToString [withRemoveWS no], expected)

mkTest1         :: String -> (String, SysConfigList, String) -> Test
mkTest1 msg (prog, config, expected)
    = mkTest2 msg (prog, [], writeDocumentToString config, expected)

mkTest2         :: String -> (String, SysConfigList, IOSArrow XmlTree String, String) -> Test
mkTest2 msg (url, config, proc, expected)
    = mkTest msg (config, (readDocument [] url >>> proc), expected)

mkTest          :: String -> (SysConfigList, IOSArrow XmlTree String, String) -> Test
mkTest msg (config, proc, expected)
    = TestCase $
      do res <- runX $
                configSysVars config
                >>>
                proc
         assertEqual msg expected (concat res)

-- ----------------------------------------------------------

configParseTests        :: Test
configParseTests
    = TestLabel "Test simple parser configurations" $
      TestList $
      map (mkTest0 "Simple parser test") $
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
      , ( "example13.o"
        , [withValidate no]
        , "<foo>\195\164</foo>"         -- no utf-8 decoding
        )
      , ( "example13.o"
        , [withValidate no, withFileMimeType "text/xml"]
        , "<foo>\228</foo>"             -- utf-8 decoding forced
        )
      ]

-- ----------------------------------------------------------

#ifdef relaxng
configRelaxTests        :: Test
configRelaxTests
    = TestLabel "Test RelaxNG configurations" $
      TestList $
      map (mkTest0 "RelaxNG test") $
      [ ( "example12.xml"
        , [ withTrace 1
          , withRelaxNG "example12.rng"
          ]
        , "<foo/>"
        )
      , ( "example12i.xml"
        , [ withRelaxNG "example12.rng" ]
        , ""
        )
      ]
#else
configRelaxTests        :: Test
configRelaxTests        = TestList []
#endif

-- ----------------------------------------------------------

configOutputTests        :: Test
configOutputTests
    = TestLabel "Test output configurations" $
      TestList $
      map (mkTest1 "Output test") $
      [ ( "example1.xml"
        , []
        , "<html> <head> </head> <body> </body> </html>"
        )
      , ( "example1.xml"
        , [withIndent yes]
        , "<html>\n  <head/>\n  <body/>\n</html>\n"
        )
      , ( "example1.xml"
        , [withRemoveWS yes]
        , "<html><head/><body/></html>"
        )
      , ( "example1.xml"
        , [ withRemoveWS yes
          , withNoEmptyElemFor ["head"]
          ]
        , "<html><head></head><body/></html>"
        )
      , ( "example2a.xml"
        , [ withRemoveWS yes
          , withOutputXML
          ]
        , "<html><head/><body>&lt;\224></body></html>"
        )
      , ( "example2a.xml"
        , [ withRemoveWS yes
          , withOutputHTML
          ]
        , "<html><head></head><body>&lt;&agrave;></body></html>"
        )
      , ( "example2a.xml"
        , [ withRemoveWS yes
          , withOutputXHTML
          ]
        , "<html><head></head><body>&lt;\224></body></html>"
        )
      , ( "example2a.xml"
        , [ withRemoveWS yes
          , withOutputPLAIN
          ]
        , "<html><head/><body><\224></body></html>"
        )
      , ( "example2a.xml"
        , [ withRemoveWS yes
          , withOutputXHTML
          , withOutputEncoding usAscii
          ]
        , "<html><head></head><body>&lt;&#224;></body></html>"
        )
      , ( "example2a.xml"
        , [ withRemoveWS yes
          , withOutputXHTML
          , withOutputEncoding utf8
          ]
        , "<html><head></head><body>&lt;\195\160></body></html>"
        )
      , ( "example2a.xml"
        , [ withRemoveWS yes
          , withOutputXHTML
          , withOutputEncoding usAscii
          , withXmlPi yes
          ]
        , "<?xml version=\"1.0\" encoding=\"US-ASCII\"?>\n<html><head></head><body>&lt;&#224;></body></html>"
        )
      , ( "example2a.xml"
        , [ withRemoveWS yes
          , withOutputXHTML
          , withOutputEncoding utf8
          , withXmlPi yes
          ]
        , "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<html><head></head><body>&lt;\195\160></body></html>"
        )
      ]

-- ----------------------------------------------------------

configPickleTests        :: Test
configPickleTests
    = TestLabel "Pickle tests output configurations" $
      TestList $
      map (mkTest "Pickle test") $
      [ ( []
        , xunpickleDocument pick [withValidate no] "example10.xml"
        , "x"
        )
      , ( []
        , xunpickleDocument pick [withValidate no] "example10.xml"
          >>>
          xpickleVal pick
          >>>
          writeDocumentToString []
        , "<a>x</a>"
        )
      ]
    where
    pick = xpElem "a" $ xpText

-- ----------------------------------------------------------

#ifdef xpath
configXPathTests        :: Test
configXPathTests
    = TestLabel "XPath tests" $
      TestList $
      map (mkTest2 "XPath test") $
      [ ( "example10.xml"
        , [withValidate no]
        , xshow $ getXPathTreesInDoc "/a/text()"
        , "x"
        )
      , ( "example10.xml"
        , [withValidate no]
        , xshow $ getXPathTreesInDoc "/a"
        , "<a>x</a>"
        )
      ]
#else
configXPathTests        :: Test
configXPathTests        = TestList []
#endif

-- ----------------------------------------------------------

#ifdef tagsoup
configTagSoupTests      :: Test
configTagSoupTests
    = TestLabel "Test TagSoup parser configurations" $
      TestList $
      map (mkTest0 "TagSoup parser test" . (\ (u,c,r) -> (u, withTagSoup : c, r))) $
      [ ( "example2.xml"
        , []
        , "<html><head/><body/></html>"
        )
      , ( "example2.xml"
        , [withRemoveWS yes]
        , "<html><head/><body/></html>"
        )
      , ( "example3.xml"
        , []
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
        , [ withParseHTML yes
          , withCanonicalize no
          ]
        , "<a>x</a>"
        )

      , ( "example11.txt"               -- parse by mimetype and none XML/HTML contents
        , []
        , ""
        )
      , ( "example11.txt"
        , [ withParseByMimeType yes ]
        , "hello\n"
        )
      , ( "example11.txt"
        , [ withIgnoreNoneXmlContents yes
          , withParseByMimeType yes]
        , ""
        )
      ]
#else
configTagSoupTests      :: Test
configTagSoupTests      = TestList []
#endif

-- ----------------------------------------------------------

#ifdef curl
configCurlTests      :: Test
configCurlTests
    = TestLabel "Test Curl input configurations" $
      TestList $
      map (mkTest2 "Curl HTTP test") $
      [ ( urlw3, []
        , getAttrValue transferStatus
        , "999"
        )
      , ( urlw3, [ withCurl []
                 , withValidate no
                 ]
        , getAttrValue transferStatus
        , "200"
        )
      , ( urlw3c, [ withCurl []                 -- permanently moved
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "301"
        )
      , ( urlw3c, [ withCurl []                 -- permanently moved
                  , withRedirect yes            -- with redirect
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "200"
        )
      , ( urlw3,  [ withCurl [("max-filesize", "500000")]       -- file size limit o.k.
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "200"
        )
      , ( urlw3,  [ withCurl [("max-filesize", "500")]          -- file size limit exceeded
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "999"
        )
      , ( urlw3, [ withCurl []
                 , withProxy "xyz"                              -- this proxy should not exist
                 , withValidate no
                 ]
        , getAttrValue transferStatus
        , "999"
        )
      , ( urlw3, [ withCurl []
                 , withProxy "localhost:4742"                   -- this proxy also should not exist
                 , withValidate no
                 ]
        , getAttrValue transferStatus
        , "999"
        )
      , ( html32, [ withCurl [(a_if_modified_since, "Mon, 05 Apr 1999 23:08:57 GMT")] -- empty body
                  , withParseHTML yes
                  , withValidate no
                  ]
        , getAttrValue transferStatus
        , "304"
        )
      , ( html32, [ withCurl []
                  , withInputOption a_if_modified_since "Mon, 05 Apr 1999 23:08:57 GMT" -- empty body
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "304"
        )
      , ( html32, [ withCurl [(a_if_modified_since, "Sat, 03 Apr 1999 23:08:57 GMT")] -- full body
                  , withParseHTML yes
                  , withWarnings  no
                  ]
        , getAttrValue transferStatus
        , "200"
        )
      ]
#else
configCurlTests      :: Test
configCurlTests      = TestList []
#endif

-- ----------------------------------------------------------

#ifdef http
configHttpTests      :: Test
configHttpTests
    = TestLabel "Test HTTP input configurations" $
      TestList $
      map (mkTest2 "HTTP input test") $
      [ ( urlw3, []
        , getAttrValue transferStatus
        , "999"
        )
      , ( urlw3, [ withHTTP []
                 , withValidate no
                 ]
        , getAttrValue transferStatus
        , "200"
        )
      , ( urlw3c, [ withHTTP []                 -- permanently moved
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "301"
        )
      , ( urlw3c, [ withHTTP []                 -- permanently moved
                  , withRedirect yes            -- with redirect
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "200"
        )
      , ( urlw3, [ withHTTP []
                 , withProxy "xyz"                              -- this proxy should not exist
                 , withValidate no
                 ]
        , getAttrValue transferStatus
        , "999"
        )
      , ( urlw3, [ withHTTP []
                 , withProxy "localhost:4742"                   -- this proxy also should not exist
                 , withValidate no
                 ]
        , getAttrValue transferStatus
        , "999"
        )
      , ( html32, [ withHTTP [(a_if_modified_since, "Mon, 05 Apr 1999 23:08:57 GMT")] -- empty body
                  , withParseHTML yes
                  , withValidate no
                  ]
        , getAttrValue transferStatus
        , "304"
        )
      , ( html32, [ withHTTP []
                  , withInputOption a_if_modified_since "Mon, 05 Apr 1999 23:08:57 GMT" -- empty body
                  , withParseHTML yes
                  ]
        , getAttrValue transferStatus
        , "304"
        )
      , ( html32, [ withHTTP [(a_if_modified_since, "Sat, 03 Apr 1999 23:08:57 GMT")] -- full body
                  , withParseHTML yes
                  , withWarnings  no
                  ]
        , getAttrValue transferStatus
        , "200"
        )
      ]
#else
configHttpTests      :: Test
configHttpTests      = TestList []
#endif

-- ----------------------------------------------------------

#ifdef cache
configCacheTests      :: Test
configCacheTests
    = TestLabel "Test Cache input configurations" $
      TestList $
      [ TestLabel "Generate cache dir" $
        TestCase genCacheDir
      ]
      ++
      map (mkTest2 "Cache HTTP test")
#ifdef curl
      [ ( urlw3
        , [ withCurl []
          , withoutCache
          , withValidate no
          ]
        , getAttrValue transferStatus
        , "200"
        )
      , ( urlw3                                 -- cache write
        , [ withCurl []
          , withTrace 1
          , withCache cacheDir 10 False
          , withValidate no
          ]
        , ( getAttrValue transferStatus
            &&&
            ( constA urlw3
              >>>
              ( (isInCache >>> constA ",ok")
                `orElse`
                constA ",not in cache"
              )
            )
          )
          >>> arr (uncurry (++))
        , "200,ok"
        )
      , ( urlw3                                 -- cache hit
        , [ withCurl []
          , withTrace 1
          , withCache cacheDir 10 False
          , withValidate no
          ]
        , perform (arrIO0 (system "sleep 5"))   -- delay for next test
          >>>
          getAttrValue transferStatus
        , "200"
        )
      , ( urlw3                                 -- cache out of date, refresh
        , [ withCurl []
          , withTrace 1
          , withCache cacheDir 1 False
          , withValidate no
          ]
        , getAttrValue transferStatus
        , "200"
        )
#ifdef bzip
      , ( urlw3h                                        -- cache write
        , [ withCurl []
          , withTrace 1
          , withCache cacheDir 10 False
          , withCompression (compress, decompress)  -- compress serialized data with bzip
          , withValidate no                         -- for saving cache space and IO operations
          ]                                         -- compression saves about 90% of space
        , ( getAttrValue transferStatus
            &&&
            ( constA urlw3h
              >>>
              ( (isInCache >>> constA ",ok")
                `orElse`
                constA ",not in cache"
              )
            )
          )
          >>> arr (uncurry (++))
        , "200,ok"
        )
      , ( urlw3h                                        -- cache hit
        , [ withCurl []
          , withTrace 1
          , withCache cacheDir 10 False
          , withCompression (compress, decompress)
          , withValidate no
          ]
        , perform (arrIO0 (system "sleep 5"))           -- delay for next test
          >>>
          getAttrValue transferStatus
        , "200"
        )
      , ( urlw3h                                        -- cache out of date, refresh
        , [ withCurl []
          , withTrace 1
          , withCache cacheDir 1 False
          , withCompression (compress, decompress)
          , withValidate no
          ]
        , getAttrValue transferStatus
        , "200"
        )
#endif
      ]
#else
      []
#endif
      ++
      [ TestLabel "Remove cache dir" $
        TestCase remCacheDir
      ]
#else
configCacheTests      :: Test
configCacheTests      = TestList []
#endif


-- ----------------------------------------------------------

allTests        :: Test
allTests
    = TestList
      [ TestLabel "Generate test input files" $
        TestCase genInputFiles

      , configParseTests
      , configOutputTests
      , configPickleTests
      , configXPathTests
      , configRelaxTests
      , configTagSoupTests
      , configCurlTests
      , configHttpTests
      , configCacheTests

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
