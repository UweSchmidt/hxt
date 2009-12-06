-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Parser.TagSoup
   Copyright  : Copyright (C) 2005-2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   lazy HTML and simpe XML parser implemented with tagsoup
   parsing is done with a very simple monadic top down parser

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Parser.TagSoup
    ( parseHtmlTagSoup
    )
where

-- ------------------------------------------------------------

import Data.Char (toLower)
import Data.Maybe

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity		( lookupNumericEntity
					)
import Text.XML.HXT.DOM.Unicode		( isXmlSpaceChar
					)
import Text.XML.HXT.Parser.HtmlParsec	( isEmptyHtmlTag
					, isInnerHtmlTagOf
					, closesHtmlTag
					)
import Text.XML.HXT.Parser.XhtmlEntities
import Text.XML.HXT.DOM.Interface	( XmlTrees
					, QName
					, NsEnv
					, toNsEnv
					, newXName
					, nullXName
					, mkQName'
					, mkName
					, isWellformedQualifiedName
					, c_warn
					, a_xml
					, a_xmlns
					, xmlNamespace
					, xmlnsNamespace
					)
import Text.XML.HXT.DOM.XmlNode		( isElem
					, mkError
					, mkCmt
					, mkText
					, mkElement
					, mkAttr
					)

-- ---------------------------------------- 

-- The name table contains the id map. All element and attribute names are stored
-- the first time they are ecountered, and later always this name is used,
-- not a string built by the parser.

type Tags		= [Tag]

type Context		= ([String], NsEnv)

type State		= Tags

newtype Parser a 	= P { parse :: State -> (a, State)}

instance Monad Parser where
    return x	= P $ \ ts -> (x, ts)
    p >>= f	= P $ \ ts -> let
			      (res, ts') = parse p ts
                              in
			      parse (f res) ts'

runParser	:: Parser a -> Tags -> a
runParser p ts	= fst . parse p $ ts

-- ----------------------------------------

cond		:: Parser Bool -> Parser a -> Parser a -> Parser a
cond c t e	= do
		  p <- c
		  if p then t else e

lookAhead	:: (Tag -> Bool) -> Parser Bool
lookAhead p	= P $ \ s -> (not (null s) && p (head s), s)

-- ----------------------------------------
-- primitive look ahead tests

isEof		:: Parser Bool
isEof		= P $ \ s -> (null s, s)

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
getTag		= P $ \ (t1:ts1) -> (t1, ts1)

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
pushBack t	= P $ \ ts -> ((), t:ts)

insCls		:: String -> Parser ()
insCls n	= pushBack (TagClose n)

insOpn		:: String -> [(String, String)] -> Parser ()
insOpn n al	= pushBack (TagOpen n al)

-- ----------------------------------------

mkQN		:: Bool -> Bool -> NsEnv -> String -> Parser QName
mkQN withNamespaces isAttr env s
    | withNamespaces
	= return qn1
    | otherwise
	= return qn0
    where
    qn1
	| isAttr && isSimpleName	= s'
	| isSimpleName			= mkQName' nullXName (newXName s) (nsUri nullXName)
	| isWellformedQualifiedName s	= mkQName' px'        lp'         (nsUri px')
	| otherwise			= s'
    qn0					= s'

    nsUri x				= fromMaybe nullXName . lookup x $ env
    isSimpleName			= all (/= ':') s
    (px, (_ : lp))			= span(/= ':') s
    px'                 		= newXName px
    lp'                 		= newXName lp
    s'                                  = mkName   s

extendNsEnv	:: Bool -> [(String, String)] -> NsEnv -> NsEnv
extendNsEnv withNamespaces al1 env
    | withNamespaces
	= toNsEnv (concatMap (uncurry addNs) al1) ++ env
    | otherwise
	= env
    where
    addNs n v
	| px == a_xmlns
	  &&
	  (null lp || (not . null . tail $ lp))
	    = [(drop 1 lp, v)]
	| otherwise
	    = []
	where
	(px, lp) = span (/= ':') n

-- ----------------------------------------

-- own entity lookup to prevent problems with &amp; and tagsoup hack for IE

lookupEntity	:: Bool -> Bool -> String -> [Tag]
lookupEntity withWarnings _asHtml e0@('#':e)
    = case lookupNumericEntity e of
      Just c  -> [ TagText [c] ]
      Nothing -> ( TagText $ "&" ++ e0 ++ ";") :
		 if withWarnings
		 then [TagWarning $ "illegal char reference: &" ++ e ++ ";"]
		 else []

lookupEntity withWarnings asHtml e
    = case (lookup e entities) of
      Just x  -> [ TagText [toEnum x]]
      Nothing -> (TagText $ "&" ++ e ++ ";") :
		 if withWarnings
		 then [TagWarning $ "Unknown entity reference: &" ++ e ++ ";"]
		 else []
    where
    entities
	| asHtml    = xhtmlEntities
	| otherwise = xhtmlEntities -- xmlEntities (TODO: xhtml is xml and html)

-- ----------------------------------------

-- |
-- Turns all element and attribute names to lower case
-- even !DOCTYPE stuff. But this is discarded when parsing the tagsoup

lowerCaseNames :: [Tag] -> [Tag]
lowerCaseNames
    = map f
    where
    f (TagOpen name attrs)
        = TagOpen (nameToLower name) (map attrToLower attrs)
    f (TagClose name)
	= TagClose (nameToLower name)
    f a = a
    nameToLower          = map toLower
    attrToLower (an, av) = (nameToLower an, av)

-- ----------------------------------------
-- the main parser

parseHtmlTagSoup	:: Bool -> Bool -> Bool -> Bool -> Bool -> String -> String -> XmlTrees
parseHtmlTagSoup withNamespaces withWarnings withComment removeWhiteSpace asHtml doc
    = ( docRootElem
	. runParser (buildCont initContext)
	. ( if asHtml
	    then lowerCaseNames
	    else id
	  )
	. parseTagsOptions (parseOptions { optTagWarning   = withWarnings
					 , optLookupEntity = lookupEntity withWarnings asHtml
					 }
			   )
      )
    where
    -- This is essential for lazy parsing:
    -- the call of "take 1" stops parsing, when the first element is detected
    -- no check on end of input sequence is required to build this (0- or 1-element list)
    -- so eof is never checked unneccessarily

    docRootElem
	= take 1 . filter isElem

    initContext		= ( []
			  , toNsEnv $
			    [ (a_xml,   xmlNamespace)
			    , (a_xmlns, xmlnsNamespace)
			    ]
			  )

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

    buildCont	:: Context -> Parser XmlTrees
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
	closeTag		:: Context -> String -> Parser XmlTrees
	closeTag ((n':_), _) n1
	    | n' == n1		= return []			-- a normal closing tag
								-- all other cases try to repair wrong html
	closeTag ns'@((n':_), _) n1				-- n1 closes n implicitly
	    | n' `isInnerElem` n1				-- e.g. <td>...</tr>
				= do
				  insCls n1			-- pushback </n1>
				  insCls n'			-- insert a </n'>
				  buildCont ns'			-- try again
	closeTag ns' n1
	    | isEmptyElem n1	= buildCont ns'			-- ignore a redundant closing tag for empty element
	closeTag ns'@((n':ns1'), _) n1				-- insert a missing closing tag
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

	openTag			:: Context -> String -> [(String, String)] -> Parser XmlTrees
	openTag cx'@(ns',env') n1 al1
	    | isPiDT n1		= buildCont cx'
	    | isEmptyElem n1
				= do
				  qn <- mkElemQN nenv n1
				  al <- mkAttrs al1
				  rl <- buildCont cx'
				  return (mkElement qn al [] : rl)
	    | closesElem ns' n1	= do
				  insOpn n1 al1
				  insCls (head ns')
				  buildCont cx'
	    | otherwise		= do
				  qn <- mkElemQN nenv n1
				  al <- mkAttrs al1
				  cs <- buildCont ((n1 : ns'), nenv)
				  rl <- buildCont cx'
				  return (mkElement qn al cs : rl)
	    where
	    nenv		= extendNsEnv withNamespaces al1 env'
	    mkElemQN		= mkQN withNamespaces False
	    mkAttrQN		= mkQN withNamespaces True
	    isPiDT ('?':_)	= True
	    isPiDT ('!':_)	= True
	    isPiDT _		= False
	    mkAttrs		= mapM (uncurry mkA)
	    mkA an av		= do
				  qan <- mkAttrQN nenv an
				  return (mkAttr qan (wrap . mkText $ av))

	closeAll		:: ([String], NsEnv) -> Parser XmlTrees
	closeAll (ns',_)	= return (concatMap wrn ns')
				  where
				  wrn = warn . ("insert missing closing tag " ++) . show

-- ------------------------------------------------------------
