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

module TagSoupTest
    ( parseHtmlTagSoup
    , parseHtmlTagSoupM
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

import Control.Monad
import Data.Maybe


type Tags	= [Tag]

newtype Parser a = P { parse :: Tags -> Maybe (a, Tags)}

instance Monad Parser where
    return x	= P $ \ ts -> Just (x, ts)
    p >>= f	= P $ \ ts -> case parse p ts of
			      Nothing	      -> Nothing
			      Just (res, ts') -> parse (f res) ts'

instance MonadPlus Parser where
    mzero	= P $ const Nothing
    p `mplus` q	= P $ \ ts -> case parse p ts of
		              s@(Just (_, _)) -> s
			      Nothing	      -> parse q ts

eof		:: Parser ()
eof		= P $ \ ts -> if null ts then Just ((),ts) else Nothing

tagText		:: Parser String
tagText		= P sym
    		  where
		  sym (TagText t : ts1)	= Just (t, ts1)
		  sym _			= Nothing

tagCmt		:: Parser String
tagCmt		= P sym
		  where
		  sym (TagComment c : ts1)
		      			= Just (c, ts1)
	  	  sym _			= Nothing

tagWarn		:: Parser String
tagWarn		= P sym
	  	  where
		  sym (TagWarning w : ts1)
					= Just (w, ts1)
		  sym _			= Nothing

tagPos		:: Parser (Int, Int)
tagPos		= P sym
		  where
		  sym (TagPosition l c : ts1)
					= Just ((l, c), ts1)
		  sym _			= Nothing

tagCls		:: Parser String
tagCls		= P sym
		  where
		  sym (TagClose n : ts1)
					= Just (n, ts1)
		  sym _			= Nothing

tagOpn		:: Parser (String, [(String,String)])
tagOpn		= P sym
		  where
		  sym (TagOpen n al : ts1)
					= Just ((n, al), ts1)
		  sym _			= Nothing

pushBack	:: Tag -> Parser ()
pushBack t	= P $ \ ts -> Just ((), t:ts)

insCls		:: String -> Parser ()
insCls n	= pushBack (TagClose n)

insOpn		:: String -> [(String, String)] -> Parser ()
insOpn n al	= pushBack (TagOpen n al)


parseHtmlTagSoupM	:: Bool -> Bool -> Bool -> Bool -> String -> String -> XmlTrees
parseHtmlTagSoupM withWarnings withComment removeWhiteSpace asHtml doc
    = ( maybe [] fst
	. parse (buildContM [])
	. ( if asHtml
	    then canonicalizeTags
	    else id
	  )
	. parseTags
      )
    where
    warn
	| withWarnings	= \ w -> xwarn (show doc ++ " " ++ w)
	| otherwise	= const []
    cmt
	| withComment	= xcmt
	| otherwise	= const []
    txt
	| removeWhiteSpace
			= \ t ->
			  if all isXmlSpaceChar t
			  then []
			  else xtext t
	| otherwise	= xtext

    isEmptyElem
	| asHtml	= isEmptyHtmlTag
	| otherwise	= const False

    isInnerElem
	| asHtml	= isInnerHtmlTagOf
	| otherwise	= const (const False)

    closesElem
	| asHtml	= \ ns n1 ->
			  not (null ns)
			  &&
			  n1 `closesHtmlTag` (head ns)
	| otherwise	= const (const False)

    buildContM	:: [String] -> Parser XmlTrees
    buildContM ns
	= ( do
	    t <- tagText
	    rl <- buildContM ns
	    return (txt t ++ rl) )
	  `mplus`
	  ( do
	    c <- tagCmt
	    rl <- buildContM ns
	    return (cmt c ++ rl) )
	  `mplus`
	  ( do
	    w <- tagWarn
	    rl <- buildContM ns
	    return (warn w ++ rl) )
	  `mplus`
	  ( do
	    tagPos
	    buildContM ns )
	  `mplus`
	  ( do
	    n <- tagCls
	    closeTag ns n
	  )
	  `mplus`
	  ( do
	    (n,al) <- tagOpn
	    openTag ns n al
	  )
	  `mplus`
	  ( do
	    eof
	    closeAll ns
	  )
	  `mplus`
	  ( return (warn "parse error in tagsoup tree construction")
	  )
	where
	closeTag		:: [String] -> String -> Parser XmlTrees
	closeTag (n':_) n1
	    | n' == n1		= return []			-- a normal closing tag
								-- all other cases try to repair wrong html
	closeTag (ns'@(n':_)) n1				-- n1 closes n implicitly
	    | n' `isInnerElem` n1				-- e.g. <td>...</tr>
				= do
				  insCls n1			-- pushback </n1>
				  insCls n'			-- insert a </n'>
				  buildContM ns'		-- try again
	closeTag ns' n1
	    | isEmptyElem n1	= buildContM ns'		-- ignore a redundant closing tag for empty element
	closeTag ns'@(n':ns1') n1				-- insert a missing closing tag
	    | n1 `elem` ns1'	= do
				  insCls n1
				  insCls n'
				  rl <- buildContM ns'
				  return ( warn ("closing tag " ++ show n' ++
						 " expected, but " ++ show n1 ++ " found")
					   ++ rl
					 )
	closeTag ns' n1						-- ignore a wrong closing tag
				= do
				  rl <- buildContM ns'
				  return ( warn ("no opening tag for closing tag " ++ show n1)
					   ++ rl
					 )

	openTag			:: [String] -> String -> [(String, String)] -> Parser XmlTrees
	openTag ns' n1 al
	    | isPiDT n1		= buildContM ns'
	    | isEmptyElem n1
				= do
				  rl <- buildContM ns'
				  return (mkXTagTree n1 (mkAttrs al) [] : rl)
	    | closesElem ns' n1	= do
				  insOpn n1 al
				  insCls (head ns')
				  buildContM ns'
	    | otherwise		= do
				  cs <- buildContM (n1:ns')
				  rl <- buildContM ns'
				  return (mkXTagTree n1 (mkAttrs al) cs : rl)
	    where
	    isPiDT ('?':_)	= True
	    isPiDT ('!':_)	= True
	    isPiDT _		= False
	    mkAttrs		= map (uncurry mkAttr)
	    mkAttr an av	= mkXAttrTree an (xtext av)

	closeAll		:: [String] -> Parser XmlTrees
	closeAll ns'		= return (concatMap wrn ns')
				  where
				  wrn = warn . ("insert missing closing tag " ++) . show

-- ------------------------------------------------------------

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
    warn
	| withWarnings	= \ w -> xwarn (show doc ++ " " ++ w)
	| otherwise	= const []
    cmt
	| withComment	= xcmt
	| otherwise	= const []
    txt
	| removeWhiteSpace
			= \ t ->
			  if all isXmlSpaceChar t
			  then []
			  else xtext t
	| otherwise	= xtext

    isEmptyElem
	| asHtml	= isEmptyHtmlTag
	| otherwise	= const False

    isInnerElem
	| asHtml	= isInnerHtmlTagOf
	| otherwise	= const (const False)

    closesElem
	| asHtml	= \ ns n1 ->
			  not (null ns)
			  &&
			  n1 `closesHtmlTag` (head ns)
	| otherwise	= const (const False)

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
