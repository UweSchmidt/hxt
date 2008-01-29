-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.TagSoup
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   lasy HTML and simpe XML parser implemented with tagsoup

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.TagSoup
    ( parseHtmlTagSoup
    )
where

import Text.HTML.TagSoup

import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.XmlTreeFunctions

import Text.XML.HXT.DOM.Unicode
    ( isXmlSpaceChar
    )

import Text.XML.HXT.Parser.HtmlParsec
    ( isEmptyHtmlTag
    , isInnerHtmlTagOf
    , closesHtmlTag
    )

type Tags	= [Tag]

parseHtmlTagSoup	:: Bool -> Bool -> Bool -> Bool -> String -> String -> XmlTrees
parseHtmlTagSoup withWarnings withComment removeWhiteSpace asHtml doc
    = ( fst
	. buildCont []
	. ( if asHtml
	    then canonicalizeTags
	    else id
	  )
	. parseTags
      )
    where
    warn w
	| withWarnings	= xwarn (show doc ++ " " ++ w)
	| otherwise	= []
    cmt c
	| withComment	= xcmt c
	| otherwise	= []
    txt t
	| removeWhiteSpace
	  &&
	  all isXmlSpaceChar t
			= xtext t
	| otherwise	= []

    isEmptyElem n	= asHtml && isEmptyHtmlTag n
    isInnerElem n  n1	= asHtml && n `isInnerHtmlTagOf` n1
    closesElem  ns n1	= asHtml
			  &&
			  not (null ns)
			  &&
			  n1 `closesHtmlTag` (head ns)

    buildCont	:: [String] -> Tags -> (XmlTrees, Tags)
    buildCont ns (TagText t : ts1)
	= (txt t ++ rl, rest)
	  where
	  (rl, rest) = buildCont ns ts1

    buildCont ns (TagComment c : ts1)
	= (cmt c ++ rl, rest)
	  where
	  (rl, rest) = buildCont ns ts1

    buildCont ns (TagWarning w : ts1)
	= ( warn w ++ rl, rest)
	  where
	  (rl, rest) = buildCont ns ts1

    buildCont ns (TagPosition _ _ : ts1)
	= buildCont ns ts1

    buildCont (n:_) (TagClose n1 : ts1)				-- a normal closing tag
	| n1 == n	= ([], ts1)

    buildCont (ns@(n:_)) (ts@(TagClose n1 : _ts1))		-- n1 closes n implicitly
	| n `isInnerElem` n1					-- e.g. <td>...</tr>
			= buildCont ns (TagClose n : ts)	-- insert a </td>

    buildCont ns (TagClose n1 : ts1)				-- ignore a redundant closing tag for empty an empty element
	| isEmptyElem n1
			= buildCont ns ts1			-- e.g. a </hr>

    buildCont ns@(n:ns1) (ts@(TagClose n1 : _ts1))		-- insert a missing closing tag
	| n1 `elem` ns1	= (warn ("closing tag " ++ show n ++ " expected, but " ++ show n1 ++ " found") ++ rl, rest)
			  where
			  (rl, rest) = buildCont ns (TagClose n : ts)

    buildCont ns (TagClose n1 : ts1)				-- ignore a wrong closing tag
			= (warn ("no opening tag for closing tag " ++ show n1) ++ rl, rest)
			  where
			  (rl, rest) = buildCont ns ts1

    buildCont ns (ts@(TagOpen n1 al : ts1))
	| isPiDT n1	= buildCont ns ts1
	| isEmptyElem n1
			= let
			  (rl, rest) = buildCont ns ts1
			  in
			  (mkXTagTree n1 (mkAttrs al) [] : rl, rest)
	| closesElem ns n1
			= buildCont ns (TagClose (head ns) : ts)
	| otherwise	= let
			  (cs, rest1)	= buildCont (n1:ns) ts1
			  (rl, rest)	= buildCont     ns  rest1
			  in
			  (mkXTagTree n1 (mkAttrs al) cs : rl, rest)
	where
	isPiDT ('?':_)	= True
	isPiDT ('!':_)	= True
	isPiDT _	= False
	mkAttrs		= map (uncurry mkAttr)
	mkAttr an av	= mkXAttrTree an (xtext av)

    buildCont [] []						-- normal end of parse
	= ([],[])

    buildCont ns@(n:_) ts@[]					-- missing closing tags at end of parse
	= (warn ("insert missing closing tag " ++ show n) ++ rl, rest)
	  where
	  (rl, rest) = buildCont ns (TagClose n : ts)

-- ------------------------------------------------------------
