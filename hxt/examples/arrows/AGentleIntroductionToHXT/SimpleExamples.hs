{- |
   $Id: SimpleExamples.hs,v 1.3 2006/11/17 17:16:24 hxml Exp $
  
  The examples from the HXT tutorial at haskell.org "http://www.haskell.org/haskellwiki/HXT"
-}

module Main
where

import Text.XML.HXT.Arrow		-- import HXT stuff
import Text.XML.HXT.XPath

import Data.List			-- auxiliary functions
import Data.Maybe

import System.Environment
import System.Console.GetOpt()
import System.Exit


-- | call this program with 3 arguments,
-- the function name, see list of examples,
-- the input URL or file
-- and the output file, - for stdout
--
-- example: SimpleExamples selectAllText http://www.haskell.org/ -

main :: IO ()
main
    = do
      argv <- getArgs
      (al, fct, src, dst) <- cmdlineOpts argv
      [rc]  <- runX (application al fct src dst)
      if rc >= c_err
	 then exitWith (ExitFailure (-1))
	 else exitWith ExitSuccess

application	:: Attributes -> String -> String -> String -> IOSArrow b Int
application al fct src dst
    = readDocument al src
      >>>
      processChildren (processRootElement fct `when` isElem)
      >>>
      writeDocument ( (a_indent, v_1)
		      : (a_output_encoding, isoLatin1)
		      : al
		    ) dst
      >>>
      getErrStatus

-- | the dummy for the boring stuff of option evaluation,
-- usually done with 'System.Console.GetOpt'

cmdlineOpts 	:: [String] -> IO (Attributes, String, String, String)
cmdlineOpts argv
    = return ([(a_validate, v_0),(a_parse_html, v_1)], argv!!0, argv!!1, argv!!2)


-- | the processing examples

examples	:: [ (String, IOSArrow XmlTree XmlTree) ]
examples
    = [ ( "selectAllText",	selectAllText	)
      , ( "selectAllTextAndAltValues",	selectAllTextAndAltValues	)
      , ( "selectAllTextAndRealAltValues",	selectAllTextAndRealAltValues	)
      , ( "addRefIcon",		addRefIcon	)
      , ( "helloWorld",		helloWorld	)
      , ( "helloWorld2",	helloWorld2	)
      , ( "imageTable",		imageTable	)
      , ( "imageTable0",	imageTable0	)
      , ( "imageTable1",	imageTable1	)
      , ( "imageTable2",	imageTable2	)
      , ( "imageTable3",	imageTable3	)
      , ( "toAbsHRefs",		toAbsHRefs	)
      , ( "toAbsRefs",		toAbsRefs	)
      , ( "toAbsRefs1",		toAbsRefs1	)
      ]

processRootElement	:: String -> IOSArrow XmlTree XmlTree
processRootElement fct
    = fromMaybe this . lookup fct $ examples

-- | selection arrows

selectAllText	:: ArrowXml a => a XmlTree XmlTree
selectAllText
    = selem "the-plain-text" [ deep isText ]		-- create a root element, neccessary for wellformed XML output

selectAllTextAndAltValues	:: ArrowXml a => a XmlTree XmlTree
selectAllTextAndAltValues
    = selem "the-plain-text"
      [ deep
	( isText
	  <+>
	  ( isElem >>> hasName "img"
	    >>>
	    getAttrValue "alt"
	    >>>
	    mkText
	  )
	)
      ]

selectAllTextAndRealAltValues	:: ArrowXml a => a XmlTree XmlTree
selectAllTextAndRealAltValues
    = selem "the-plain-text"
      [ deep
	( isText
	  <+>
	  ( isElem >>> hasName "img"
	    >>>
	    getAttrValue "alt"
	    >>>
	    isA significant
	    >>>
	    arr addBrackets
	    >>>
	    mkText
	  )
	)
      ]
    where
    significant :: String -> Bool
    significant = not . all (`elem` " \n\r\t")

    addBrackets :: String -> String
    addBrackets s
	=  " [[ " ++ s ++ " ]] "

-- | transformation arrows

addRefIcon	:: ArrowXml a => a XmlTree XmlTree
addRefIcon
    = processTopDown
      ( addImg
	`when`
	isExternalRef

      )
    where
    isExternalRef
	= isElem
	  >>>
	  hasName "a"
          >>>
	  hasAttr "href"
	  >>>
	  getAttrValue "href"
	  >>>
	  isA isExtRef
	where
	isExtRef
	    = isPrefixOf "http:"
    addImg
	= replaceChildren
	  ( getChildren
	    <+>
	    imgElement
	  )

    imgElement
	= mkelem "img"
	  [ sattr "src" "/icons/ref.png"
	  , sattr "alt" "external ref"
	  ] []

-- | construction examples

helloWorld	:: ArrowXml a => a XmlTree XmlTree
helloWorld
    = mkelem "html" []
      [ mkelem "head" []
	[ mkelem "title" []
	  [ txt "Hello World" ]
	]
      , mkelem "body"
	[ sattr "class" "haskell" ]
	[ mkelem "h1" []
	  [ txt "Hello World" ]
	]
      ]

helloWorld2	:: ArrowXml a => a XmlTree XmlTree
helloWorld2
    = selem "html"
      [ selem "head"
	[ selem "title"
	  [ txt "Hello World" ]
	]
      , mkelem "body"
	[ sattr "class" "haskell" ]
	[ selem "h1"
	  [ txt "Hello World" ]
	]
      ]

imageTable	:: ArrowXml a => a XmlTree XmlTree
imageTable
    = selem "html"
      [ selem "head"
	[ selem "title"
	  [ txt "Images in Page" ]
	]
      , selem "body"
	[ selem "h1"
	  [ txt "Images in Page" ]
	, selem "table"
	  [ collectImages
	    >>>
	    genTableRows
	  ]
	]
      ]
    where
    genTableRows
	= selem "tr"
	  [ selem "td"
	    [ getAttrValue "src" >>> mkText ]
	  ]

imageTable0	:: ArrowXml a => a XmlTree XmlTree
imageTable0
    = selem "html"
      [ pageHeader
      , selem "body"
	[ selem "h1"
	  [ txt "Images in Page" ]
	, selem "table"
	  [ collectImages
	    >>>
	    genTableRows
	  ]
	]
      ]
    where
    pageHeader
	= constA "<head><title>Images in Page</title></head>"
	  >>>
	  xread
    genTableRows
	= selem "tr"
	  [ selem "td"
	    [ getAttrValue "src" >>> mkText ]
	  ]

imageTable1	:: ArrowXml a => a XmlTree XmlTree
imageTable1
    = selem "html"
      [ selem "head"
	[ selem "title"
	  [ txt "Images in Page" ]
	]
      , selem "body"
	[ selem "h1"
	  [ txt "Images in Page" ]
	, selem "table"
	  [ collectImages
	    >>>
	    genTableRows
	  ]
	]
      ]

imageTable2	:: IOStateArrow s XmlTree XmlTree
imageTable2
    = selem "html"
      [ selem "head"
	[ selem "title"
	  [ txt "Images in Page" ]
	]
      , selem "body"
	[ selem "h1"
	  [ txt "Images in Page" ]
	, selem "table"
	  [ collectImages
	    >>>
	    mkAbsImageRef
	    >>>
	    genTableRows
	  ]
	]
      ]

imageTable3	:: IOStateArrow s XmlTree XmlTree
imageTable3
    = insertTreeTemplate
        pageTemplate				-- the page template
        [ hasText (=="ImageList") :-> images]	-- fill hole "ImageList" with image descriptions
    where
    images
	= collectImages
	  >>>
	  mkAbsImageRef
	  >>>
	  genTableRows

    pageTemplate
	= constA "<html><head><title>Images in Page</title></head><body><h1>Images in Page</h1><table>ImageList</table></body></html>"
	  >>>
	  xread

collectImages	:: ArrowXml a => a XmlTree XmlTree
collectImages
    = deep ( isElem >>> hasName "img" )

genTableRows	:: ArrowXml a => a XmlTree XmlTree
genTableRows
    = selem "tr"
      [ selem "td"			-- (1)
	[ this                      -- (1.1)
	]
      , selem "td"                  -- (2)
	[ getAttrValue "src"
	  >>>
	  mkText
	  >>>
	  mkelem "a"                -- (2.1)
	  [ attr "href" this ]
	  [ this ]
	]
      , selem "td"                  -- (3)
	[ ( getAttrValue "width"
	    &&&                     -- (3.1)
	    getAttrValue "height"
	  )
	  >>>
	  arr2 geometry             -- (3.2)
	  >>>
	  mkText
	]
      , selem "td"                  -- (4)
	[ getAttrValue "alt"
	  >>>
	  mkText
	]
      ]
    where
    geometry :: String -> String -> String
    geometry "" ""
	= ""
    geometry w h
	= w ++ "x" ++ h

mkAbsImageRef :: IOStateArrow s XmlTree XmlTree
mkAbsImageRef
    = processAttrl (mkAbsRef `when` hasName "src")
      where
      mkAbsRef
	  = replaceChildren
	    ( xshow getChildren
	      >>>
	      ( mkAbsURI `orElse` this )
	      >>>
	      mkText
	    )

toAbsHRefs	:: IOStateArrow s XmlTree XmlTree
toAbsHRefs
    = ( mkAbsHRefs $< computeBaseRef )
      >>>
      removeBaseElement

removeBaseElement	:: ArrowXml a => a XmlTree XmlTree
removeBaseElement
    = processChildren
      ( processChildren ( none
			  `when`
			  ( isElem >>> hasName "base" )
			)
       `when`
       ( isElem >>> hasName "head" )
      )

mkAbsHRefs	:: ArrowXml a => String -> a XmlTree XmlTree
mkAbsHRefs base
    = processTopDown editHRef
    where
    editHRef
	= processAttrl ( changeAttrValue (absHRef base)
			 `when`
			 hasName "href"
		       )
	  `when`
	  ( isElem >>> hasName "a" )
	where

	absHRef	:: String -> String -> String
	absHRef base url
	    = fromMaybe url . expandURIString url $ base

toAbsRefs	:: IOStateArrow s XmlTree XmlTree
toAbsRefs
    = ( mkAbsRefs $< computeBaseRef )
      >>>
      removeBaseElement

mkAbsRefs0	:: ArrowXml a => String -> a XmlTree XmlTree
mkAbsRefs0 base
    = processTopDown ( editRef "a" "href"
		       >>>
		       editRef "img" "src"
		       >>>
		       editRef "link" "href"
		       >>>
		       editRef "script" "src"
		     )
    where
    editRef en an
	= processAttrl ( changeAttrValue (absHRef base)
			 `when`
			 hasName an
		       )
	  `when`
	  ( isElem >>> hasName en )
	where

	absHRef	:: String -> String -> String
	absHRef base url
	    = fromMaybe url . expandURIString url $ base

mkAbsRefs	:: ArrowXml a => String -> a XmlTree XmlTree
mkAbsRefs base
    = processTopDown editRefs
    where
    editRefs
	= seqA . map (uncurry editRef)
	  $
	  [ ("a", "href")
	  , ("img", "src")
	  , ("link", "href")
	  , ("script", "src")	    -- and more
	  ]

    editRef en an
	= processAttrl ( changeAttrValue (absHRef base)
			 `when`
			 hasName an
		       )
	  `when`
	  ( isElem >>> hasName en )
	where

	absHRef	:: String -> String -> String
	absHRef base url
	    = fromMaybe url . expandURIString url $ base

computeBaseRef	:: IOStateArrow s XmlTree String
computeBaseRef
    = ( ( ( isElem >>> hasName "html"
	    >>>
	    getChildren
	    >>>
	    isElem >>> hasName "head"
	    >>>
	    getChildren
	    >>>
	    isElem >>> hasName "base"
	    >>>
	    getAttrValue "href"
	  )
	  &&&
	  getBaseURI
	)
	>>> expandURI
      )
      `orElse` getBaseURI


getDescendends :: ArrowXml a => [String] -> a XmlTree XmlTree
getDescendends
    = foldl1 (\ x y -> x >>> getChildren >>> y)
      .
      map (\ n -> isElem >>> hasName n)

computeBaseRef1	:: IOStateArrow s XmlTree String
computeBaseRef1
    = ( ( ( getDescendends ["html","head","base"]
	    >>>
	    getAttrValue "href"
	  )
	  &&&
	  getBaseURI
	)
	>>> expandURI
      )
      `orElse` getBaseURI

computeBaseRef2	:: IOStateArrow s XmlTree String
computeBaseRef2
    = ( ( xshow (getXPathTrees "/html/head/base@href")
	  &&&
	  getBaseURI
	)
	>>> expandURI
      )
      `orElse` getBaseURI

toAbsRefs1	:: IOStateArrow s XmlTree XmlTree
toAbsRefs1
    = ( mkAbsRefs $< computeBaseRef1 )
      >>>
      removeBaseElement
