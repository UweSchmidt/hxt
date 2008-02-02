-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.TagSoup
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   lasy HTML and simpe XML parser implemented with tagsoup
   parsing is done with a very simple monadic top down parser

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.TagSoup
    ( parseHtmlTagSoup
    )
where

-- ------------------------------------------------------------

{-
import Text.HTML.TagSoup

import Text.XML.HXT.DOM.Unicode
    ( isXmlSpaceChar
    )

import Text.XML.HXT.Parser.HtmlParsec
    ( isEmptyHtmlTag
    , isInnerHtmlTagOf
    , closesHtmlTag
    )

import Text.XML.HXT.Arrow.DOMInterface
    ( XmlTrees
    , c_warn
    , mkSNsName
    )
import Text.XML.HXT.Arrow.XmlNode
    ( isElem
    , mkError
    , mkCmt
    , mkText
    , mkElement
    , mkAttr
    )

import Control.Monad
import Data.Maybe

type Tags	= [Tag]

-- this monadic parser is not lasy, because of the Maybe result type.
-- this Maybe (Just for success, Nothing for failure) is checked, when taking
-- the result out ( maybe [] fst . parse ... )

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


parseHtmlTagSoup	:: Bool -> Bool -> Bool -> Bool -> String -> String -> XmlTrees
parseHtmlTagSoup withWarnings withComment removeWhiteSpace asHtml doc
    = ( maybe [] (docRootElem . fst)
	. parse (buildContM [])
	. ( if asHtml
	    then canonicalizeTags
	    else id
	  )
	. parseTags
      )
    where
    -- This is essential for lasy parsing:
    -- the call of "take 1" stops parsing, when the first element is detected
    -- no check on end of input sequence is required to build this (0- or 1-element list)
    -- so eof is never checked unneccessarily

    docRootElem
	= take 1 . filter isElem
--	where
--	isElm (NTree (XTag _ _) _)	= True
--	isElm _				= False

    wrap		= (:[])

    warn
	| withWarnings	= wrap . mkError c_warn . show . (doc ++) . (" " ++)
	| otherwise	= const []
    cmt
	| withComment	= wrap . mkCmt
	| otherwise	= const []
    txt
	| removeWhiteSpace
			= \ t ->
			  if all isXmlSpaceChar t
			  then []
			  else wrap . mkText $ t
	| otherwise	= wrap . mkText

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
	    (n,al) <- tagOpn
	    openTag ns n al
	  )
	  `mplus`
	  ( do
	    n <- tagCls
	    closeTag ns n
	  )
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
				  return (mkElement (mkQN n1) (mkAttrs al) [] : rl)
	    | closesElem ns' n1	= do
				  insOpn n1 al
				  insCls (head ns')
				  buildContM ns'
	    | otherwise		= do
				  cs <- buildContM (n1:ns')
				  rl <- buildContM ns'
				  return (mkElement (mkQN n1) (mkAttrs al) cs : rl)
	    where
	    isPiDT ('?':_)	= True
	    isPiDT ('!':_)	= True
	    isPiDT _		= False
	    mkAttrs		= map (uncurry mkA)
	    mkA an av		= mkAttr (mkQN an) (wrap . mkText $ av)
	    mkQN n		= mkSNsName n

	closeAll		:: [String] -> Parser XmlTrees
	closeAll ns'		= return (concatMap wrn ns')
				  where
				  wrn = warn . ("insert missing closing tag " ++) . show

-}

-- ------------------------------------------------------------

{-
-- this parser is lasy, it always succeds and does not need to test eof
-- for successful parsing, so the parse can be abandoned, when only a part
-- of the input is required.

import Text.HTML.TagSoup

import Data.Tree.NTree.TypeDefs
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
    = ( docRootElem
	. fst
	. buildCont []
	. ( if asHtml
	    then canonicalizeTags
	    else id
	  )
	. parseTags
      )
    where
    -- This is essential for lasy parsing:
    -- the call of "take 1" stops parsing, when the first element is detected
    -- no check on end of input sequence is required to build this (0- or 1-element list)
    -- so eof is never checked unneccessarily

    docRootElem
	= take 1 . filter isElm
	where
	isElm (NTree (XTag _ _) _)	= True
	isElm _				= False

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
-}
-- ------------------------------------------------------------

-- this parser is the monadic variant of the lasy parser above, it always succeds and does not need to test eof
-- for successful parsing, so the parse can be abandoned, when only a part
-- of the input is required.

import Text.HTML.TagSoup

import Text.XML.HXT.DOM.Unicode
    ( isXmlSpaceChar
    )

import Text.XML.HXT.Parser.HtmlParsec
    ( isEmptyHtmlTag
    , isInnerHtmlTagOf
    , closesHtmlTag
    )

import Text.XML.HXT.Arrow.DOMInterface
    ( XmlTrees
    , c_warn
    , mkSNsName
    )
import Text.XML.HXT.Arrow.XmlNode
    ( isElem
    , mkError
    , mkCmt
    , mkText
    , mkElement
    , mkAttr
    )

-- ------------------------------------------------------------

type Tags	= [Tag]

newtype Parser a = P { parse :: Tags -> (a, Tags)}

instance Monad Parser where
    return x	= P $ \ ts -> (x, ts)
    p >>= f	= P $ \ ts -> let
			      ~(res, ts') = parse p ts
                              in
			      parse (f res) ts'

cond		:: Parser Bool -> Parser a -> Parser a -> Parser a
cond c t e	= do
		  p <- c
		  if p then t else e

lookAhead	:: (Tag -> Bool) -> Parser Bool
lookAhead p	= P $ \ ts -> (not (null ts) && p (head ts), ts)

-- ----------------------------------------
-- primitive look ahead tests

isEof		:: Parser Bool
isEof		= P $ \ ts -> (null ts, ts)

isText		:: Parser Bool
isText		= lookAhead is
		  where
		  is (TagText _) = True
		  is _		 = False

isCmt		:: Parser Bool
isCmt		= lookAhead is
		  where
		  is (TagComment _) = True
		  is _		    = False

isWarn		:: Parser Bool
isWarn		= lookAhead is
		  where
		  is (TagWarning _) = True
		  is _		    = False

isPos		:: Parser Bool
isPos		= lookAhead is
		  where
		  is (TagPosition _ _) = True
		  is _		 = False

isCls		:: Parser Bool
isCls		= lookAhead is
		  where
		  is (TagClose _) = True
		  is _		  = False

isOpn		:: Parser Bool
isOpn		= lookAhead is
		  where
		  is (TagOpen _ _) = True
		  is _		   = False

-- ----------------------------------------
-- primitive symbol parsers

getText		:: Parser String
getText		= P sym
    		  where
		  sym (TagText t : ts1)	= (t, ts1)
		  sym _			= undefined

getCmt		:: Parser String
getCmt		= P sym
		  where
		  sym (TagComment c : ts1)
		      			= (c, ts1)
	  	  sym _			= undefined

getWarn		:: Parser String
getWarn		= P sym
	  	  where
		  sym (TagWarning w : ts1)
					= (w, ts1)
		  sym _			= undefined

getPos		:: Parser (Int, Int)
getPos		= P sym
		  where
		  sym (TagPosition l c : ts1)
					= ((l, c), ts1)
		  sym _			= undefined

getCls		:: Parser String
getCls		= P sym
		  where
		  sym (TagClose n : ts1)
					= (n, ts1)
		  sym _			= undefined

getOpn		:: Parser (String, [(String,String)])
getOpn		= P sym
		  where
		  sym (TagOpen n al : ts1)
					= ((n, al), ts1)
		  sym _			= undefined

-- ----------------------------------------

-- pushback parsers for inserting missing tags

pushBack	:: Tag -> Parser ()
pushBack t	= P $ \ ts -> ((), t:ts)

insCls		:: String -> Parser ()
insCls n	= pushBack (TagClose n)

insOpn		:: String -> [(String, String)] -> Parser ()
insOpn n al	= pushBack (TagOpen n al)

-- ----------------------------------------

-- the main function

parseHtmlTagSoup	:: Bool -> Bool -> Bool -> Bool -> String -> String -> XmlTrees
parseHtmlTagSoup withWarnings withComment removeWhiteSpace asHtml doc
    = ( docRootElem
	. fst
	. parse (buildCont [])
	. ( if asHtml
	    then canonicalizeTags
	    else id
	  )
	. parseTags
      )
    where
    -- This is essential for lasy parsing:
    -- the call of "take 1" stops parsing, when the first element is detected
    -- no check on end of input sequence is required to build this (0- or 1-element list)
    -- so eof is never checked unneccessarily

    docRootElem
	= take 1 . filter isElem

    wrap		= (:[])

    warn
	| withWarnings	= wrap . mkError c_warn . show . (doc ++) . (" " ++)
	| otherwise	= const []
    cmt
	| withComment	= wrap . mkCmt
	| otherwise	= const []
    txt
	| removeWhiteSpace
			= \ t ->
			  if all isXmlSpaceChar t
			  then []
			  else wrap . mkText $ t
	| otherwise	= wrap . mkText

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

    buildCont	:: [String] -> Parser XmlTrees
    buildCont ns
	= cond isText ( do
			t <- getText
			rl <- buildCont ns
			return (txt t ++ rl)
		      )
	  ( cond isOpn ( do
			 (n,al) <- getOpn
			 openTag ns n al
		       )
	    ( cond isCls ( do
			   n <- getCls
			   closeTag ns n
			 )
	      ( cond isCmt ( do
			     c <- getCmt
			     rl <- buildCont ns
			     return (cmt c ++ rl)
			   )
		( cond isWarn ( do
				w <- getWarn
				rl <- buildCont ns
				return (warn w ++ rl)
			      )
		  ( cond isPos ( do
				 getPos
				 buildCont ns
			       )
		    ( cond isEof ( do
				   isEof
				   closeAll ns
				 )
		      ( return (warn "parse error in tagsoup tree construction")
		      )
		    )
		  )
		)
	      )
	    )
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
				  buildCont ns'			-- try again
	closeTag ns' n1
	    | isEmptyElem n1	= buildCont ns'			-- ignore a redundant closing tag for empty element
	closeTag ns'@(n':ns1') n1				-- insert a missing closing tag
	    | n1 `elem` ns1'	= do
				  insCls n1
				  insCls n'
				  rl <- buildCont ns'
				  return ( warn ("closing tag " ++ show n' ++
						 " expected, but " ++ show n1 ++ " found")
					   ++ rl
					 )
	closeTag ns' n1						-- ignore a wrong closing tag
				= do
				  rl <- buildCont ns'
				  return ( warn ("no opening tag for closing tag " ++ show n1)
					   ++ rl
					 )

	openTag			:: [String] -> String -> [(String, String)] -> Parser XmlTrees
	openTag ns' n1 al
	    | isPiDT n1		= buildCont ns'
	    | isEmptyElem n1
				= do
				  rl <- buildCont ns'
				  return (mkElement (mkQN n1) (mkAttrs al) [] : rl)
	    | closesElem ns' n1	= do
				  insOpn n1 al
				  insCls (head ns')
				  buildCont ns'
	    | otherwise		= do
				  cs <- buildCont (n1:ns')
				  rl <- buildCont ns'
				  return (mkElement (mkQN n1) (mkAttrs al) cs : rl)
	    where
	    isPiDT ('?':_)	= True
	    isPiDT ('!':_)	= True
	    isPiDT _		= False
	    mkAttrs		= map (uncurry mkA)
	    mkA an av		= mkAttr (mkQN an) (wrap . mkText $ av)
	    mkQN n		= mkSNsName n

	closeAll		:: [String] -> Parser XmlTrees
	closeAll ns'		= return (concatMap wrn ns')
				  where
				  wrn = warn . ("insert missing closing tag " ++) . show

-- ------------------------------------------------------------
