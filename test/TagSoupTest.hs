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

import Text.XML.HXT.DOM.NamespacePredicates
    ( isWellformedQualifiedName
    )

import Text.XML.HXT.Parser.HtmlParsec
    ( isEmptyHtmlTag
    , isInnerHtmlTagOf
    , closesHtmlTag
    )

import Text.XML.HXT.Arrow.DOMInterface
    ( XmlTrees
    , QName
    , c_warn
    , mkName
    , mkPrefixLocalPart
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

import Data.Maybe

type Tags	= [Tag]
type NameTable	= [(String,String)]
type State	= (Tags, NameTable)

newtype Parser a = P { parse :: State -> (a, State)}

instance Monad Parser where
    return x	= P $ \ ts -> (x, ts)
    p >>= f	= P $ \ ts -> let
			      ~(res, ts') = parse p ts
                              in
			      parse (f res) ts'

runParser	:: Parser a -> Tags -> a
runParser p ts	= fst . parse p $ (ts, [])

-- ----------------------------------------

cond		:: Parser Bool -> Parser a -> Parser a -> Parser a
cond c t e	= do
		  p <- c
		  if p then t else e

lookAhead	:: (Tag -> Bool) -> Parser Bool
lookAhead p	= P $ \ s@(ts, _) -> (not (null ts) && p (head ts), s)

-- ----------------------------------------
-- primitive look ahead tests

isEof		:: Parser Bool
isEof		= P $ \ ~s@(ts, _) -> (null ts, s)

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

getTag		:: Parser Tag
getTag		= P $ \ ~(t1:ts1, nt) -> (t1, (ts1, nt))

getSym		:: (Tag -> a) -> Parser a
getSym f	= do
		  t <- getTag
		  return (f t)

getText		:: Parser String
getText		= getSym sym
    		  where
		  sym (TagText t)	= t
		  sym _			= undefined

getCmt		:: Parser String
getCmt		= getSym sym
		  where
		  sym (TagComment c)	= c
	  	  sym _			= undefined

getWarn		:: Parser String
getWarn		= getSym sym
	  	  where
		  sym (TagWarning w)	= w
		  sym _			= undefined

getPos		:: Parser (Int, Int)
getPos		= getSym sym
		  where
		  sym (TagPosition l c)	= (l, c)
		  sym _			= undefined

getCls		:: Parser String
getCls		= getSym sym
		  where
		  sym (TagClose n)	= n
		  sym _			= undefined

getOpn		:: Parser (String, [(String,String)])
getOpn		= getSym sym
		  where
		  sym (TagOpen n al)	= (n, al)
		  sym _			= undefined

-- ----------------------------------------
-- pushback parsers for inserting missing tags

pushBack	:: Tag -> Parser ()
pushBack t	= P $ \ ~(ts, nt) -> ((), (t:ts, nt))

insCls		:: String -> Parser ()
insCls n	= pushBack (TagClose n)

insOpn		:: String -> [(String, String)] -> Parser ()
insOpn n al	= pushBack (TagOpen n al)

-- ----------------------------------------

insertName	:: String -> Parser String
insertName n	= P $ \ ~(ts, nt) -> let
				     (n', nt') = insert n nt
				     in
				     (n', (ts, nt'))
		  where
		  insert s nt
		      | isJust r	= (fromJust r, nt)
		      | otherwise	= (s, (s,s) : nt)
		      where
		      r = lookup s nt

mkQN		:: String -> Parser QName
mkQN s
    | isSimpleName			= do					-- the most frequent case
					  s' <- insertName s
					  return (mkName s')
    | isWellformedQualifiedName s	= do					-- qualified name
					  px' <- insertName px
					  lp' <- insertName lp
					  return (mkPrefixLocalPart px' lp')
    | otherwise				= do					-- not a namespace conformant name
					  s' <- insertName s
					  return (mkName s')
    where
    isSimpleName	= all (/= ':') s
    (px, (_ : lp))	= span(/= ':') s

-- ----------------------------------------
-- the main parser

parseHtmlTagSoup	:: Bool -> Bool -> Bool -> Bool -> String -> String -> XmlTrees
parseHtmlTagSoup withWarnings withComment removeWhiteSpace asHtml doc
    = ( docRootElem
	. runParser (buildCont [])
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
	openTag ns' n1 al1
	    | isPiDT n1		= buildCont ns'
	    | isEmptyElem n1
				= do
				  qn <- mkQN n1
				  al <- mkAttrs al1
				  rl <- buildCont ns'
				  return (mkElement qn al [] : rl)
	    | closesElem ns' n1	= do
				  insOpn n1 al1
				  insCls (head ns')
				  buildCont ns'
	    | otherwise		= do
				  qn <- mkQN n1
				  al <- mkAttrs al1
				  cs <- buildCont (n1:ns')
				  rl <- buildCont ns'
				  return (mkElement qn al cs : rl)
	    where
	    isPiDT ('?':_)	= True
	    isPiDT ('!':_)	= True
	    isPiDT _		= False
	    mkAttrs		= mapM (uncurry mkA)
	    mkA an av		= do
				  qan <- mkQN an
				  return (mkAttr qan (wrap . mkText $ av))

	closeAll		:: [String] -> Parser XmlTrees
	closeAll ns'		= return (concatMap wrn ns')
				  where
				  wrn = warn . ("insert missing closing tag " ++) . show

-- ------------------------------------------------------------

p1 = parseHtmlTagSoup True True False True "emil"

tt1 = p1 "yyyy<x:a href='x:xxx'>bbb</x:a>xxx"
