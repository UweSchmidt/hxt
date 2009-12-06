-- |
-- HUnit - Haskell XML Toolbox examples and tests for arrows
--
-- Author: Uwe Schmidt uwe@fh-wedel.de
--
-- Version : $Id: HUnitExample.hs,v 1.4 2006/09/06 06:35:28 hxml Exp $

module Main where

import System
import Test.HUnit
import Text.XML.HXT.Arrow
import Data.Char

{-
aMinimalDoc	:: XmlTree
aMinimalDoc	= mkRootTree [] (xtext "<?xml version='1.0'?><x/>")

aSimpleXHTMLDoc	:: XmlTree
aSimpleXHTMLDoc
    = mkRootTree
      ( xattr a_source "simple.xml" )
      ( xtext 
	( concatMap (++ "\n")
	  [ "<?xml version='1.0'?>"
	  , "<html>"
	  , " <head><title>A Simple XHTML Document</title></head>"
	  , " <body>"
	  , " </body>"
	  , "</html>"
	  ]
	)
      )

mkMinimalDoc	:: String -> XmlTree
mkMinimalDoc cont
    = mkRootTree [] (xtext cont)

mkInputDoc	:: String -> XmlTree
mkInputDoc src
    = mkRootTree ( xattr a_source src) []

mkDoc	:: String -> XmlTree
mkDoc cont
    = mkRootTree
      ( xattr a_source "a test")
      ( xtext ("<?xml version='1.0' encoding='" ++ isoLatin1 ++ "'?>\n" ++ cont) )

-- |
-- compare trees

testEqualTrees	:: XmlTrees -> XmlTrees -> Test
testEqualTrees e1 e2
    = TestCase $
      assertEqual "" e1 e2

-- |
-- testDocFilter creates a test for the application of a pure
-- functional filter to a XML tree. The result is converted into
-- the external string representation and compared with the expected
-- result.
--
-- see also: 'testStateFilter' and 'testEditFilter'

testDocFilter	:: String -> XmlFilter -> XmlTree -> Test
testDocFilter expected xfilter doc
    = TestCase $
      assertEqual "" expected (xshow $ xfilter doc)

-- |
-- testStateFilter creates a test for monadic filters, doing IO and using
-- the internal state, consisting of program options and status information
-- The filter is applied to a XML tree, the result is converted to a string
-- and this result is compared with the expected result.
--
-- see also: 'testEditFilter' for pure functional filters

testStateFilter	:: String -> XmlStateFilter () -> XmlTree -> Test
testStateFilter expected xfilter input
    = TestCase $
      do
      res <- run' $ xfilter input
      assertEqual "" expected (xshow res)

-- |
-- The test bed for edit filter for external documents.
-- The input tree must specify the external document to be read.
-- This document is checked for wellformedness, canonicalized
-- (char refs substituted, ...) and processed with the edit filter.
-- The result is transformed into the string representation and compared
-- with the expected string.

testEditFilter	:: String -> XmlFilter -> XmlTree -> Test
testEditFilter expected xfilter input
    = TestCase $
      do
      res <- run'
	     $ ( getWellformedDoc					-- read the document
		 .>>
		 liftMf canonicalizeAllNodes				-- normalize the document and remove <?xml ... ?> decl
		 .>>
		 liftMf ( processChildren xfilter )			-- apply the editing filter only to the 'real' document
		 .>>				  			-- nodes, not to nodes surrounding the document tag
		 putXmlTree						-- just trace output
		 .>>
		 liftMf getChildren					-- just for comparing the document contents
	       )
	     input
      assertEqual "" expected (xshow res)


-- |
-- tree versus filter construction tests
-- documents may be constructed by tree operations
-- or with the more flexible filter operations applied to an arbitray tree

treeConstructionTests :: Test
treeConstructionTests
    = TestLabel "tree and filter construction tests" $
      TestList $
      map (\ (t, f) -> testEqualTrees t (f undefined) )
      [ (xtext "xyz",				txt "xyz"			)
      , (xcmt "cmt",				cmt "cmt"			)
      , (xtag "img" [] [],			tag "img" [] []			)
      , (xtag "img" [] [],			etag "img"			)

      , (xtag "img" (xattr "b" "13") [],	tag "img" [sattr "b" "13"] []	)

      , (xtag "img" (xattr "b" "13") [],	atag "img" [sattr "b" "13"]	)

      , (xtag "t" (xattr "a" "42" ++ xattr "b" "id")
	          (xtext "abc"),
						tag "t" [sattr "a" "42", sattr "b" "id"]
	 						[txt "abc"]		)
      ]

-- |
-- construction tests are all applied to the empty document,
-- no information is used from that document,
-- but construction is lifted to the filter level.
-- this is more general than explicit construction.
-- in more complex examples some information may be extracted from
-- the source, some can be constant.
-- 
-- see also: 'testDocFilter'

constructionTests	:: Test
constructionTests
    = TestLabel "document construction tests" $
      TestList $
      map (\ (res, f) -> testDocFilter res f undefined)		-- undefined or something else, e.g. emptyRoot
      [ ("<x/>"		, mkXTag  "x" none none	)
      , ("<x/>"		, tag "x"  [] []	)		-- syntactically more comfortable shortcut for mkXTag
      , ("<x/>"		, stag "x" []		)		-- shortcut for tags without attributes
      , ("<x/>"		, atag "x" []		)		-- shortcut for empty tags with attributes
      , ("<x/>"		, etag "x"		)		-- shortcut for empty tags without attributes

      , ("<x a=\"1\"/>"
			, mkXTag "x" (mkXAttr "a" (mkXText "1")) none	)
      , ("<x a=\"1\">42</x>"
			, mkXTag "x" (mkXAttr "a" (mkXText "1")) (mkXText "42")	)
      , ("<x a=\"1\">42</x>"
			, tag "x" [ attr "a" $ txt "1" ] [ txt "42" ]	)
      , ("<x a=\"1\">42</x>"
			, tag "x" [ sattr "a" "1" ] [ txt "42" ]	)

      , ("<x a=\"1\"/>"	, atag "x" [ sattr "a" "1" ]	)

      , ("<x a=\"1\" b=\"2\">42<y/>43</x>"
			, tag "x" [ sattr "a" "1"
				  , sattr "b" "2"
				  ] [ txt  "42"
				    , etag "y"
				    , txt  "43"
				    ]	)
      , ("<!-- a comment -->"
			, mkXCmt $ txt " a comment "	)
      , ("<!--a comment-->"
			, cmt "a comment"	)		-- short cut for uncomputed comment

      , ("<!-- an illegal XML comment containing 2 -- in comment text -->"
			, cmt " an illegal XML comment containing 2 -- in comment text "	)

      , ("&#42;"   	, mkXCharRef 42		)
      , ("&lt;"    	, mkXEntityRef "lt"	)
      , ("<![CDATA[<abc>]]>"
			, mkXCdata $ txt "<abc>")
      , ("<![CDATA[<abc>]]>"			-- short cut for uncomputed cdata content
			, cdata "<abc>")
      , ("<?abc xyz?>"	, mkXPi "abc" $ txt "xyz"	)
      , ("<?abc xyz?>"	, spi "abc" "xyz"	)	-- short cut for an uncomputed pi content
      ]

-- |
-- simple selection tests

simpleSelectionTests	:: Test
simpleSelectionTests
    = TestLabel "simple tree selection tests" $
      TestList $
      map (\ (res, f) -> testDocFilter (singleToDoubleQuote res) f simpleDoc)
      [ (src		, this	)
      , ("1"		, getValue "a"	)
      , (""		, getValue "b"	)
      , (body1		, getChildren	)

				-- text selection
      , ("xyz"		, getChildren .> isXText	)
      , ("xyz"		, getChildren .> isText "xyz"	)
      , (""		, getChildren .> isText "abc"	)
      , ("xyz"		, getChildren .> isOfText (>="x")	)
      , (""		, getChildren .> isOfText (<"x")	)
      , (""		, getChildren .> isWhiteSpace	)

				-- tag selection
      , ("<t a='42' b='id'>abc</t><img b='13'/><y x='0'/><z/>"
			, getChildren .> isXTag	)

				-- comment selection
      , ("<!--cmt-->"	, getChildren .> isXCmt	)

				-- text OR tag selection
      , ("<t a='42' b='id'>abc</t><img b='13'/>xyz<y x='0'/><z/>"
			, getChildren .> (isXTag +++ isXText)	)

				-- generalised OR
      , ("<t a='42' b='id'>abc</t><img b='13'/>xyz<y x='0'/><z/>"
			, getChildren .> cat [isXTag, isXText]	)

      , ("abc"		, getChildren .> getChildren	)

				-- atribute value selection
      , ("1"		, getValue "a"	)

      , ("<t a='42' b='id'>abc</t>"
			, getChildren .> isTag "t"	)

				-- attribute selection
      , ("<t a='42' b='id'>abc</t>"
			, getChildren .> hasAttr "a"	)

      , ("42"		, getChildren .> getValue "a"	)
      , ("42"		, getChildren .> hasValue "a" (all isDigit)	)
      , ("13"		, getChildren .> hasValue "b" (all isDigit)	)
      , (""		, getChildren .> isTag "t" .> hasValue "b" (all isDigit)	)
      , ("42"		, getChildren .> hasValue "a" (== "42")	)
      , (""		, getChildren .> hasValue "a" (== "43")	)
      , ("42"		, getChildren .> hasValue "a" (\ str -> ((read str)::Int) > 40)	)
      , ("42"		, getChildren .> hasValue "a" (not . null)	)
      , ("id13"		, getChildren .> hasValue "b" (const True)	)
      , ("13"		, getChildren .> isTag "img" .> hasValue "b" (const True)	)
      , (""		, getChildren .> hasValue "c" (const True)	)

      , (" a='42' b='id' b='13' x='0'"
			, getChildren .> getAttrl	)

      , (" a='42' b='id'"
			, getChildren .> isTag "t" .> getAttrl	)

      , ("42id"		, getChildren .> isTag "t" .> getAttrl .> getChildren	)

      , (" b='id' b='13'"
			, getChildren .> getAttrl .> isAttr "b"	)

      , (" b='13'"	, getChildren .> isTag "img" .> getAttrl .> isAttr "b"	)
      , ("13"		, getChildren .> isTag "img" .> getAttrl .> isAttr "b" .> getChildren	)
      , ("13"		, getChildren .> isTag "img" .> getValue "b"	)
      , ("timgyz"	, getChildren .> getName	)
      , ("t"		, getChildren .> isTag "t" .> getName	)
      , ("abbx"		, getChildren .> getAttrl  .> getName	)
      , ("bb"		, getChildren .> getAttrl  .> isAttr "b" .> getName	)
      , ("ab"		, getChildren .> isTag "t" .> getAttrl   .> getName	)
      , ("b"		, getChildren .> isTag "t" .> getAttrl   .> isAttr "b" .> getName	)

				-- deep search for text or tags
      , ("abcxyz"       , deep isXText	)
      , ("abc"	        , deep (isTag "t") .> deep isXText	)
      ]
    where
    src = "<r a='1'>" ++ body1 ++ "</r>"
    body1 = "<!--cmt-->"
	    ++ "<t a='42' b='id'>abc</t>"
	    ++ "<img b='13'/>"
	    ++ "xyz"
	    ++ "<y x='0'/>"
	    ++ "<z/>"

    sDocFilter
	= tag "r" [ sattr "a" "1" ]
	    [ cmt "cmt"
	    , tag "t" [ sattr "a" "42", sattr "b" "id" ]
		[ txt "abc"
		]
	    , atag "img" [ sattr "b" "13" ]
	    , txt "xyz"
	    , atag "y" [ sattr "x" "0" ]
	    , etag "z"
	    ]
    simpleDoc = head $ sDocFilter undefined

-- |
-- simple tests with namespaces

namespaceTests	:: Test
namespaceTests
    = TestLabel "namespace tests" $
      TestList $
      map (\ (res, f) -> testDocFilter (singleToDoubleQuote res) f simpleDoc)
      [ (src		, this	)
					-- select all "t" tags, even nested ones (multi) and extract the text from the body
      , ("sssaaa"	, multi (isTag "t"                    .> getChildren .> isXText)	)

					-- select all text within all tags of a specific namespace
      , ("sssaaazzzeee"	, multi (isXTag .> hasNamespace "dns" .> getChildren .> isXText)	)
      , ("bbb"		, multi (isXTag .> hasNamespace "n1"  .> getChildren .> isXText)	)
      , ("cccdddfffggg"	, multi (isXTag .> hasNamespace "n2"  .> getChildren .> isXText)	)

					-- select all text within all tags with a specific local name
      , ("sssaaabbbcccdddzzz"
			, multi (isXTag .> hasLocalPart "t"   .> getChildren .> isXText)	)
      , ("eee"		, multi (isXTag .> hasLocalPart "e"   .> getChildren .> isXText)	)

					-- select all text within all tags with a specific prefix
      , ("bbbddd"	, multi (isXTag .> hasPrefix "x"      .> getChildren .> isXText)	)
      , ("ccc"		, multi (isXTag .> hasPrefix "y"      .> getChildren .> isXText)	)
      , ("zzz"		, multi (isXTag .> hasPrefix "z"      .> getChildren .> isXText)	)
      , ("sssaaaeeefffggg"
			, multi (isXTag .> hasPrefix ""       .> getChildren .> isXText)	)

					-- select all attribute values of a specific namespace
					-- !!! the default namespace is not propagated to attribute names
      , ("z5"		, multi (isXTag .> getAttrl .> hasNamespace "dns"   .> getChildren)	)
      , ("2z4"		, multi (isXTag .> getAttrl .> hasNamespace "n1"    .> getChildren)	)
      , ("x3y3u4"	, multi (isXTag .> getAttrl .> hasNamespace "n2"    .> getChildren)	)
      , ("dnsn2"	, multi (isXTag .> getAttrl .> hasLocalPart "xmlns" .> getChildren)	)

					-- select namespaces from all namespace declarations
      , ("dnsn1n2dnsn2n2"
			, multi (isXTag .> getAttrl .> isNamespaceDecl .> getChildren)	)

					-- all default namespace declarations
      , ("dnsn2"	, multi (isXTag .> getAttrl .> hasLocalPart "xmlns" .> getChildren)	)

					-- all none default namespace declarations
      , ("n1n2dnsn2"	, multi (isXTag .> getAttrl .> hasPrefix "xmlns" .> getChildren)	)
      , ("n1n2"		, multi (isXTag .> getAttrl .> hasPrefix "xmlns" .> hasLocalPart "x" .> getChildren)	)
      , ("n2"		, multi (isXTag .> getAttrl .> hasPrefix "xmlns" .> hasLocalPart "y" .> getChildren)	)
      , ("dns"		, multi (isXTag .> getAttrl .> hasPrefix "xmlns" .> hasLocalPart "z" .> getChildren)	)
      ]
    where
						-- the text representation of the tree
						-- with ' instead of " for readablility of the string constants
    src = "<t xmlns='dns' xmlns:x='n1' xmlns:y='n2' xmlns:z='dns'>"
	  ++ "sss"
	  ++ "<t a='1' x:a='2'>aaa</t>"
	  ++ "<x:t a='x1' y:a='x3'>bbb</x:t>"
	  ++ "<y:t a='y1' y:a='y3'>ccc</y:t>"
	  ++ "<x:t xmlns:x='n2' x:a='u4'>ddd</x:t>"
	  ++ "<z:t x:a='z4' z:a='z5'>zzz</z:t>"
	  ++ "<e b='42'>eee</e>"
	  ++ "<f xmlns='n2'>fff<g>ggg</g></f>" ++
	  "</t>"

						-- the filter for generating the test document
    sDocFilter
	= tag "t" [ sattr "xmlns"   "dns"	-- 4 namespace declarations
		  , sattr "xmlns:x" "n1"
		  , sattr "xmlns:y" "n2"
		  , sattr "xmlns:z" "dns"
		  ]
            [ txt "sss"
	    , tag "t" [ sattr "a"   "1"
		      , sattr "x:a" "2" ]
	        [ txt "aaa"
		]
	    , tag "x:t" [ sattr "a"   "x1"
			, sattr "y:a" "x3"
			]
	        [ txt "bbb"
		]
	    , tag "y:t" [ sattr "a"   "y1"
			, sattr "y:a" "y3"
			]
	        [ txt "ccc"
		]
	    , tag "x:t" [ sattr "xmlns:x" "n2"	-- redefinition of prefix x:
			, sattr "x:a" "u4"
			]
	        [ txt "ddd"
		]
	    , tag "z:t" [ sattr "x:a" "z4"
			, sattr "z:a" "z5"
			]
	        [ txt "zzz"
		]
	    , tag "e" [ sattr "b" "42" ]
                [ txt "eee"
		]
	    , tag "f" [ sattr "xmlns" "n2"	-- redefinition of default namespace
		      ]
                [ txt "fff"
		, tag "g" [ ]
		    [ txt "ggg" ]
		]
	    ]
						-- create document and propagate namespaces
    simpleDoc = head $ (sDocFilter .> propagateNamespaces) undefined

-- |
-- minimal tests for parsing data.
--
-- the tests are document trees with a root
-- node containing a single text node with the document source.
-- That tree is passed to the parser, which substitutes the text node with the parsed xml tree.
-- In general this may be a list of trees containing also the
-- surrounding parts like the xml declaraion, the document type definition
-- possibly comments and processing instructions.
--
-- see also: 'testStateFilter'

parseTests	:: Test
parseTests
    = TestLabel "minimal parser tests" $
      TestList 
      [ testStateFilter "</><x/><//>"	parseXmlDoc				mini	-- the tree inclusive root node
      , testStateFilter "<x/>"	(parseXmlDoc .>> liftMf getChildren)		mini	-- only the content
      , testStateFilter "<x/>"	(parseXmlDoc .>> liftMf getChildren)		min2	-- same content
      , testStateFilter ""	(parseXmlDoc .>> liftMf getChildren)		minErr	-- syntax error: content empty
      , testStateFilter errr	(parseXmlDoc .>> liftMf (getValue a_status))	minErr	-- syntax error reported in a_status
      ]
    where
    errr	= show c_err
    mini	= mkMinimalDoc "<x/>"
    min2	= mkMinimalDoc "<x></x>"
    minErr	= mkMinimalDoc "<x>"


-- |
-- simple external file containing one tag

mini1		:: XmlTree
mini1		= newDocument "mini1.xml"		-- test documents

-- |
-- simple external file containing one tag

mini2		:: XmlTree
mini2		= newDocument "mini2.xml"

-- |
-- simple not existing external file

notThere	:: XmlTree
notThere	= newDocument "notThere.xml"		-- does not exist

-- |
-- These test check the access to external files
-- and error reporting in case of input or parse errors
-- Some test issue tree like trace output of the documents read
-- to show the internal structure and nesting

inputTests	:: Test
inputTests
    = TestLabel "input tests" $
      TestList
      [ testStateFilter "mini1.xml"
	                        ( putXmlTree .>>
				  liftMf (getValue a_source)
				)					mini1
      , testStateFilter ok	( liftMf (getValue a_status)
				)					mini1
      , testStateFilter ""	( liftMf getChildren
				)					mini1
      , testStateFilter "<x/>"	( getXmlContents .>>
				  putXmlTree .>>
				  liftMf getChildren
				)					mini1
      , testStateFilter ok	( getXmlContents .>>
				  liftMf (getValue a_status)
				)					mini1
      , testStateFilter "4"	( getXmlContents .>>
				  liftMf getContentLength .>>
				  putXmlTree .>>
				  liftMf (getValue a_contentLength)
				)					mini1
      , testStateFilter ok	( getXmlContents .>>
				  liftMf (getValue a_status)
				)					mini2
      , testStateFilter ""	( getXmlContents .>>
				  putXmlTree .>>
				  liftMf getChildren
				)					notThere
      , testStateFilter fat	( getXmlContents .>>
				  liftMf (getValue a_status)
				)					notThere
      ]
    where
    ok	= show c_ok
    fat	= show c_fatal

-- |
-- A few tests for removing parts of a document tree.
-- As input tree the document in file mini2.xml is used.

removeTests	:: Test
removeTests
    = TestLabel "document transformation tests" $
      TestList
      [ testEditFilter allText	getAllText				mini2
      , testEditFilter allData  (removeAllWhiteSpace .> getAllText)	mini2
      , testEditFilter allImp	(removeIgnorableData .> getAllText)	mini2
      , testEditFilter allDat	(removeMeta .> getAllText)		mini2
      ]
    where
    getAllText			= deep isXText
    containsUnknownOrIgnore	= getChildren .> ( isText "unknown"
						   +++
						   isText "ignore"
						 )
    removeIgnorableData		= processTopDown ( none
						   `when`
						   ( isTag "data" .> containsUnknownOrIgnore )
						 )
    removeMeta			= processTopDown ( none
						   `when`
						   ( isTag "data" .> hasValue "class" ( == "meta" ) )
						 )
    allText	= " ignore unknown important important "
    allData	= "ignoreunknownimportantimportant"
    allImp	= "   important important "
    allDat	= " ignore  important  "


ys :: XmlTree
ys = head $
     tag "s:Envelope"
     [ sattr "xmlns:s" "http://schemas.xmlsoap.org/soap/envelope/",
       sattr "xmlns:xenc" "http://www.w3.org/2001/04/xmlenc#",
       sattr "xmlns:eg" "http://example.org/paymentv2",
       sattr "xmlns:wsse" "http://schemas.xmlsoap.org/ws/2002/12/secext"
     ]
     [ stag "s:Header"
       [ stag "wsse:Security"
         [ tag "wsse:BinarySecurityToken"
           [ sattr "ValueType" "wsse:Kerberosv5ST",
             sattr "EncodingType" "wsse:Base64Binary"
           ]
           [ txt "QMwcAG ..."
           ]
	 ]
       ],
       stag "s:Body"
       [ stag "eg:OrderCurrency"
         [ stag "eg:Name"
           [ txt "John Smith" ],
	   tag "eg:Amount"
           [ sattr "Currency" "'USD'" ]
	   [ txt "1000" ],
	   tag "eg:CreditCard"
           [ sattr "Limit" "'5000'",
	     sattr "Currency" "'GBP'"
	   ]
	   [ stag "xenc:EncryptedData"
             [ atag "xenc:EncryptionMethod"
               [ sattr "Algorithm" "http://www.w3.org/2001/04/xmlenc#tripledes-cbc" ]
             ],
	     stag "xenc:CipherData"
             [ stag "xenc:CipherValue"
               [ txt "r5KipsDV ..." ]
             ]
	   ]
         ]
       ]
     ]
     .> propagateNamespaces
     .> indentDoc
     $ undefined
-}

-- |
-- ausiliary function to make haskell string constants with quotes more readable

singleToDoubleQuote	:: String -> String
singleToDoubleQuote
    = map (\ c -> if c == '\'' then '"' else c)



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

simpleTests	:: Test
simpleTests
    = TestLabel "simple LA tests" $
      TestList $
      mkTestSeqLA doc tests
    where
    doc = "<x a='b'>0<y c='d'>1</y>2<z/></x>"
    tests = [ (doc			, this	)
	    , ("0<y c='d'>1</y>2<z/>"	, getChildren	)
	    , ("<y c='d'>1</y><z/>"	, getChildren >>> isElem	)
	    , ("02"			, getChildren >>> isText	)
	    , ("<z/>"			, getChildren >>> hasName "z"	)
	    , ("<z/>"			, getChildren >>> (hasName "x" <+> hasName "z")	)
	    , ("<y c='d'>1</y><z/>"	, getChildren >>> (hasName "y" <+> hasName "z")	)
	    , ("1"			, getChildren >>> getChildren	)
	    , ("1"			, getChildren >>> getChildren >>> isText	)
	    , (""			, getChildren >>> getChildren >>> isElem	)

	    , ("b"			, getAttrValue "a" >>> mkText	)
	    , (""			, getAttrValue "z" >>> mkText	)
	    , ("d"			, getChildren >>> getAttrValue "c" >>> mkText	)
	    , ("1"			, hasName "x" >>> getChildren >>> hasName "y" >>> getChildren	)

	    , ("012"			, deep isText 	)
	    , ("12"			, deep (hasText (all (/= '0'))) 	)
	    , (doc			, deep isElem	)
	    , ("<y c='d'>1</y><z/>"	, getChildren >>> deep isElem	)
	    , ("<y c='d'>1</y>"		, deep (hasName "y")	)
	    , ("<y c='d'>1</y>"		, deep (hasAttr "c")	)
	    , ("1"			, deep (hasAttr "c") >>> getChildren	)
	    , ("<y c='d'>1</y><z/>"	, deepest isElem	)
	    , ("<x a='b'>0<y c='d'>1</y>2<z/></x><y c='d'>1</y><z/>"	, multi isElem	)
	    , ("<x a='b'/><y c='d'/><z/>"	, multi isElem >>> replaceChildren none	)
	    , ("xyz"			, multi (isElem >>> getName) >>> mkText	)
	    , ("b"			, getAttrValue "a"  >>> mkText	)
	    , (""			, getAttrValue "aa" >>> mkText	)
	    , ("b"			, getAttrValue0 "a"  >>> mkText	)
	    , ("z"			, withDefault (getAttrValue0 "aa") "z" >>> mkText	)

	    ]

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
      [ simpleTests
      , nodeSetTests
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
