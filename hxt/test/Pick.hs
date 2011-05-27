module Main where

import Text.XML.HXT.Core

-- ----------------------------------------
--
-- mixed content parts
-- 1. solution: XmlTrees as data

data StringWithMarkup = MS {unMS :: XmlTrees}
			deriving (Eq, Show)

xpickleMS :: PU StringWithMarkup
xpickleMS = xpElem "html"     $
	    xpElem "body"     $
	    xpWrap (MS, unMS) $
	    xpTrees

s1 :: StringWithMarkup
s1 = MS $ runLA xread $ "abc<a href='xyz'>some link</a>"

main1
    = do
      runX (constA s1
	    >>>
	    xpickleDocument xpickleMS [withIndent yes] "x.html"
	   )
      runX (xunpickleDocument xpickleMS [withRemoveWS yes] "x.html"
	    >>>
	    arrIO  print
	   )
      return ()

-- ----------------------------------------
--
-- mixed content parts
-- 2. solution: String with markup as data

data StringWithMarkup2 = MS2 {unMS2 :: String}
			 deriving (Eq, Show)

xpickleMS2 :: PU StringWithMarkup2
xpickleMS2 = xpElem "html"     $
	     xpElem "body"     $
	     xpWrap (MS2, unMS2) $
	     xpXmlText

s12 :: StringWithMarkup2
s12 = MS2 "abc<a href='xyz'>some link</a>"

main12
    = do
      runX (constA s12
	    >>>
	    xpickleDocument xpickleMS2 [withIndent yes] "x.html"
	   )
      runX (xunpickleDocument xpickleMS2 [withRemoveWS yes] "x.html"
	    >>>
	    arrIO  print
	   )
      return ()

-- ----------------------------------------
--
-- ignoring parts of a document
-- a convenient combinator
-- this combinator removes a whole element with given name
-- during unpickling

xpIgnoreElem	:: String -> PU a -> PU a
xpIgnoreElem name xp
    = xpWrap (snd, \ x -> (Nothing, x)) $
      xpPair (xpOption (xpElem name $ xpTrees)) xp

s2 = "<html><head>xxx</head><body>abc<a href='xyz'>some link</a></body></html>"
s3 = "<html><body>abc<a href='xyz'>some link</a></body></html>"

xpickleMS3 :: PU StringWithMarkup
xpickleMS3
    = xpElem       "html" $
      xpIgnoreElem "head" $
      xpElem       "body" $
      xpWrap (MS, unMS)   $
      xpTrees

main2
    = do
      runX (constA s2 >>> readFromString [withRemoveWS yes]
	    >>>
	    xunpickleVal xpickleMS3
	    >>>
	    arrIO print
	   )
      runX (constA s3 >>> readFromString [withRemoveWS yes]
	    >>>
	    xunpickleVal xpickleMS3
	    >>>
	    arrIO print
	   )
      return ()

-- ----------------------------------------
