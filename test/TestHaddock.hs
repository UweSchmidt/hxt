module Main
where

import Data.List
import Data.Maybe ()
import System.Environment

import Text.XML.HXT.Arrow
import Text.XML.HXT.Arrow.XmlRegex

main	:: IO ()
main	= do
	  [src, dst] <- getArgs
	  main1 src dst

test	:: IO ()
test	= main1 "http://www.fh-wedel.de/~si/HXmlToolbox/hdoc_arrow/Text-XML-HXT-Arrow-ReadDocument.html" ""

main1	:: String -> String -> IO ()
main1 src dst
    = do
      runX ( readDocument [ (a_parse_html, v_1)
			  , (a_tagsoup, v_1)
			  , (a_issue_warnings, v_0)
			  ] src
             >>>
             fromLA (processChildren processDocumentRootElement)
	     >>>
	     writeDocument [ (a_output_html, v_1)
			   , (a_no_xml_pi, v_1)
			   , (a_indent, v_1)
			   ] dst
	   )
      return ()

processDocumentRootElement	:: LA XmlTree XmlTree
processDocumentRootElement
    = processXPathTrees
      (replaceChildren (processTableRows (getChildren >>> declAndDocChildren)))
      "/html/body/table"

declAndDocChildren	:: LA XmlTree XmlTree
declAndDocChildren	= (isDecl <+> isDoc) `guards` this

isDecl			:: LA XmlTree XmlTree
isDecl
    = hasName "tr" />
      hasName "td" >>> ( hasAttrValue "class" (== "decl")
			 `guards`
			 ( getChildren >>> hasName "a" >>> hasAttr "name" )
		       )

isDoc			:: LA XmlTree XmlTree
isDoc
    = hasName "tr" />
      hasName "td" >>> hasAttrValue "class" (== "doc")

getDeclName			:: LA XmlTree String
getDeclName
    = listA (hasName "tr" /> hasName "td" /> hasName "a" >>> getAttrValue "name")>>. concat
      >>> (arr $ drop 4)

processTableRows			:: LA XmlTree XmlTree -> LA XmlTree XmlTree
processTableRows ts
    = groupDeclDoc (remLeadingDocElems ts) {- >>> prune 3 -}

-- regex for a leading class="doc" row

leadingDoc		:: XmlRegex
leadingDoc		= mkStar (mkPrimA isDoc)

-- regex for a class="decl" class="doc" sequence

declDoc			:: XmlRegex
declDoc			= mkSeq (mkPrimA isDecl) leadingDoc

-- remove a leading class="doc" row this does not form a declaration
-- split the list of trees and throw away the first part

remLeadingDocElems	:: LA XmlTree XmlTree -> LA XmlTree XmlTree
remLeadingDocElems ts 	= (splitRegexA leadingDoc ts >>^ snd) >>> unlistA

-- group the "tr" trees for a declaration together, build a "tr class="decl"" element and
-- throw the old "tr" s away

groupDeclDoc		:: LA XmlTree XmlTree -> LA XmlTree XmlTree
groupDeclDoc ts		= scanRegexA declDoc ts >>>
			  mkelem "tr"
			  [ sattr "class" "decl"
			  , attr  "id" (unlistA >>> getDeclName >>> mkText)
			  ] [unlistA >>> getChildren]

-- just for making the test output more compact

prune		:: Int -> LA XmlTree XmlTree
prune 0		= none
prune i		= processChildren (prune (i-1))

