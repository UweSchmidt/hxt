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
    )
where

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
-- the main parser

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

p1 = parseHtmlTagSoup True True False True "emil"

t1 = p1 "yyyy<a>bbb</a>xxx"
