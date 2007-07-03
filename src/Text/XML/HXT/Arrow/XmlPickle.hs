-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.XmlPickle
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id$

Pickler functions for converting between user defined data types
and XmlTree data. Usefull for persistent storage and retreival
of arbitray data as XML documents

This module is an adaptation of the pickler combinators
developed by Andrew Kennedy
( http:\/\/research.microsoft.com\/~akenn\/fun\/picklercombinators.pdf )

The difference to Kennedys approach is that the target is not
a list of Chars but a list of XmlTrees. The basic picklers will
convert data into XML text nodes. New are the picklers for
creating elements and attributes.

One extension was neccessary: The unpickling may fail.
Therefore the unpickler has a Maybe result type.
Failure is used to unpickle optional elements
(Maybe data) and lists of arbitray length

There is an example program demonstrating the use
of the picklers for a none trivial data structure.
(see \"examples\/arrows\/pickle\" directory)

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.XmlPickle
    ( PU(..)
    , xpZero
    , xpUnit
    , xpLift
    , xpLiftMaybe
    , xpCondSeq
    , xpSeq
    , xpList
    , xpText
    , xpText0
    , xpPrim
    , xpChoice
    , xpWrap
    , xpWrapMaybe
    , xpPair
    , xpTriple
    , xp4Tuple
    , xpOption
    , xpAlt
    , xpElem
    , xpAttr
    , pickleDoc
    , unpickleDoc
    , XmlPickler
    , xpickle
    , xpickleDocument
    , xunpickleDocument
    , uncurry3
    , uncurry4
    )
where

import           Data.Maybe

import           Control.Arrow.ListArrows
import           Text.XML.HXT.Arrow.DOMInterface
import           Text.XML.HXT.Arrow.XmlArrow
import           Text.XML.HXT.Arrow.XmlIOStateArrow
import           Text.XML.HXT.Arrow.ReadDocument
import           Text.XML.HXT.Arrow.WriteDocument
import qualified Text.XML.HXT.Arrow.XmlNode as XN

-- ------------------------------------------------------------

data St		= St { attributes :: [XmlTree]
		     , contents   :: [XmlTree]
		     }

data PU a	= PU { appPickle   :: (a, St) -> St
		     , appUnPickle :: St -> (Maybe a, St)
		     }

emptySt		:: St
emptySt		=  St { attributes = []
		      , contents   = []
		      }

addAtt		:: XmlTree -> St -> St
addAtt x s	= s {attributes = x : attributes s}

addCont		:: XmlTree -> St -> St
addCont x s	= s {contents = x : contents s}

dropCont	:: St -> St
dropCont s	= s { contents = drop 1 (contents s) }

getAtt		:: String -> St -> Maybe XmlTree
getAtt name s
    = listToMaybe $
      runLA ( arrL attributes
	      >>>
	      isAttr >>> hasName name
	    ) s

getCont		:: St -> Maybe XmlTree
getCont s	= listToMaybe . contents $ s

-- ------------------------------------------------------------

{-
pickle	:: PU a -> a -> XmlTree
pickle p v = head . contents . appPickle p $ (v, emptySt)

unpickle :: PU a -> XmlTree -> Maybe a
unpickle p t = fst . appUnPickle p $ addCont t emptySt
-}

pickleDoc	:: PU a -> a -> XmlTree
pickleDoc p v
    = XN.mkRoot (attributes st) (contents st)
    where
    st = appPickle p (v, emptySt)

unpickleDoc :: PU a -> XmlTree -> Maybe a
unpickleDoc p t
    | XN.isRoot t
	= fst . appUnPickle p $ St { attributes = fromJust . XN.getAttrl $  t
				   , contents   =            XN.getChildren t
				   }
    | otherwise
	= Nothing

-- ------------------------------------------------------------

-- the zero pickler, fails always during unpickling

xpZero			:: PU a
xpZero			=  PU { appPickle   = snd
			      , appUnPickle = \ s -> (Nothing, s)
			      }

-- unit pickler

xpUnit			:: PU ()
xpUnit			= xpLift ()

-- lift a value to a pickler

xpLift			:: a -> PU a
xpLift x		=  PU { appPickle   = snd
			      , appUnPickle = \ s -> (Just x, s)
			      }

-- lift a Maybe to a pickler, Nothing is mapped to the zero pickler

xpLiftMaybe		:: Maybe a -> PU a
xpLiftMaybe Nothing	= xpZero
xpLiftMaybe (Just x) 	= xpLift x


-- unpickle combinator for sequence and choice. When 1. unpickler fails,
-- the 2. one is taken, else the 3. configured with the result from the 1.
-- is taken

xpCondSeq	:: PU b -> (b -> a) -> PU a -> (a -> PU b) -> PU b
xpCondSeq pd f pa k
    = PU { appPickle   = ( \ (b, s) ->
	                   let
			   a  = f b
			   pb = k a
			   in
			   appPickle pa (a, (appPickle pb (b, s)))
			 )
	 , appUnPickle = ( \ s ->
			   let
			   (a, s') = appUnPickle pa s
			   in
			   case a of
			   Nothing -> appUnPickle pd     s
			   Just a' -> appUnPickle (k a') s'
			 )
	 }


-- combine two picklers sequentially. If the first fails during
-- unpickling, the whole unpickler fails

xpSeq	:: (b -> a) -> PU a -> (a -> PU b) -> PU b
xpSeq	= xpCondSeq xpZero


-- run two picklers in sequence like with xpSeq.
-- When during unpickling the first fails,
-- an alternative pickler (first argument) is applied.
-- This pickler only is used as combinator for unpickling.
 
xpChoice		:: PU b -> PU a -> (a -> PU b) -> PU b
xpChoice pb	= xpCondSeq pb undefined


-- map value into another domain and apply pickler there

xpWrap			:: (a -> b, b -> a) -> PU a -> PU b
xpWrap (i, j) pa		= xpSeq j pa (xpLift . i)

-- map value into another domain. If the inverse mapping is
-- undefined (Nothing), the unpickler fails

xpWrapMaybe		:: (a -> Maybe b, b -> a) -> PU a -> PU b
xpWrapMaybe (i, j) pa	= xpSeq j pa (xpLiftMaybe . i)

-- pickle a pair of values

xpPair	:: PU a -> PU b -> PU (a, b)
xpPair pa pb
    = xpSeq fst pa (\ a ->
      xpSeq snd pb (\ b ->
      xpLift (a,b)))

-- pickle a 3-tuple

xpTriple	:: PU a -> PU b -> PU c -> PU (a, b, c)
xpTriple pa pb pc
    = xpWrap (toTriple, fromTriple) (xpPair pa (xpPair pb pc))
    where
    toTriple   ~(a, ~(b, c)) = (a,  b, c )
    fromTriple ~(a,   b, c ) = (a, (b, c))

-- pickle a 4-tuple

xp4Tuple	:: PU a -> PU b -> PU c -> PU d -> PU (a, b, c, d)
xp4Tuple pa pb pc pd
    = xpWrap (toQuad, fromQuad) (xpPair pa (xpPair pb (xpPair pc pd)))
    where
    toQuad   ~(a, ~(b, ~(c, d))) = (a,  b,  c, d  )
    fromQuad ~(a,   b,   c, d  ) = (a, (b, (c, d)))


-- pickle a string into an XML text node

xpText	:: PU String
xpText	= PU { appPickle   = \ (s, st) -> addCont (XN.mkText s) st
	     , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleString st)
	     }
    where
    unpickleString st
	= do
	  t <- getCont st
	  s <- XN.getText t
	  return (Just s, dropCont st)

-- pickle a possibly empty string into an XML node.
--
-- Must be used for all places, where empty strings could be form a text node.
-- Empty contents of an element is not represented by an empty text node,
-- so the unpickling must be handled with care.
-- This pickler is implemented by use of the option pickler.

xpText0	:: PU String
xpText0
    = xpWrap (fromMaybe "", emptyToNothing) $ xpOption xpText
    where
    emptyToNothing "" = Nothing
    emptyToNothing x  = Just x

-- pickle an arbitrary value by applyling show during pickling
-- and read during unpickling. Real pickling is done with string

xpPrim	:: (Read a, Show a) => PU a
xpPrim
    = xpWrapMaybe (readMaybe, show) xpText
    where
    readMaybe	:: Read a => String -> Maybe a
    readMaybe str
	= val (reads str)
	where
	val [(x,"")] = Just x
	val _        = Nothing

-- encoding of optional data by ignoring the Nothing case during pickling
-- and relying on failure during unpickling to recompute the Nothing case

xpOption	:: PU a -> PU (Maybe a)
xpOption pa
    = PU { appPickle   = ( \ (a, st) ->
			   case a of
			   Nothing -> st
			   Just x  -> appPickle pa (x, st)
			 )

	 , appUnPickle = appUnPickle $
	                 xpChoice (xpLift Nothing) pa (xpLift . Just)
	 }

-- encoding of list values by pickling all list elements sequentially
-- end relying on failure for detecting the end of the list during unpickling

xpList	:: PU a -> PU [a]
xpList pa
    = PU { appPickle   = ( \ (a, st) ->
			   case a of
			   []  -> st
			   _:_ -> appPickle pc (a, st)
			 )
	 , appUnPickle = appUnPickle $
                         xpChoice (xpLift []) pa
	                   (\ x -> xpSeq id (xpList pa) (\xs -> xpLift (x:xs))
			 )
	 }
      where
      pc = xpSeq head  pa       (\ x ->
	   xpSeq tail (xpList pa) (\ xs ->
	   xpLift (x:xs)))

-- pickler for sum data types, every constructor is mapped to an index into the list of picklers.
-- The index is used only during pickling, not during unpickling, there the 1. match is taken

xpAlt	:: (a -> Int) -> [PU a] -> PU a
xpAlt tag ps
    = PU { appPickle   = ( \ (a, st) ->
			   let
			   pa = ps !! (tag a)
			   in
			   appPickle pa (a, st)
			 )
	 , appUnPickle = appUnPickle $
	                 ( case ps of
			   []     -> xpZero
			   pa:ps1 -> xpChoice (xpAlt tag ps1) pa xpLift
			 )
	 }

-- pickler for wrapping/unwrapping data into an XML element with given name

xpElem	:: String -> PU a -> PU a
xpElem name pa
    = PU { appPickle   = ( \ (a, st) ->
			   let
	                   st' = appPickle pa (a, emptySt)
			   in
			   addCont (XN.mkElement (mkName name) (attributes st') (contents st')) st
			 )
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleElement st)
	 }
      where
      unpickleElement st
	  = do
	    t <- getCont st
	    n <- XN.getElemName t
	    if qualifiedName n /= name
	       then fail "element name does not match"
	       else do
		    let cs = XN.getChildren t
		    al <- XN.getAttrl t
		    res <- fst . appUnPickle pa $ St {attributes = al, contents = cs}
		    return (Just res, dropCont st)

-- pickler for storing/retreiving data into/from an attribute value

xpAttr	:: String -> PU a -> PU a
xpAttr name pa
    = PU { appPickle   = ( \ (a, st) ->
			   let
			   st' = appPickle pa (a, emptySt)
			   in
			   addAtt (XN.mkAttr (mkName name) (contents st')) st
			 )
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleAttr st)
	 }
      where
      unpickleAttr st
	  = do
	    a <- getAtt name st
	    let av = XN.getChildren a
	    res <- fst . appUnPickle pa $ St {attributes = [], contents = av}
	    return (Just res, st)	-- attribute is not removed from attribute list,
					-- attributes are selected by name

-- ------------------------------------------------------------

-- mothers little helper
--
-- in conjuction with triple and quadruple for complex
-- algebraic data types

uncurry3			:: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a, b, c)		= f a b c

uncurry4			:: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(a, b, c, d)	= f a b c d

-- ------------------------------------------------------------

class XmlPickler a where
    xpickle :: PU a

instance XmlPickler Int where
    xpickle = xpPrim

instance XmlPickler Integer where
    xpickle = xpPrim

{-
  no instance of XmlPickler Char
  because then every text would be encoded
  char by char, because of the instance for lists

instance XmlPickler Char where
    xpickle = xpPrim
-}

instance XmlPickler () where
    xpickle = xpUnit

instance (XmlPickler a, XmlPickler b) => XmlPickler (a,b) where
    xpickle = xpPair xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c) => XmlPickler (a,b,c) where
    xpickle = xpTriple xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d) => XmlPickler (a,b,c,d) where
    xpickle = xp4Tuple xpickle xpickle xpickle xpickle

instance XmlPickler a => XmlPickler [a] where
    xpickle = xpList xpickle

instance XmlPickler a => XmlPickler (Maybe a) where
    xpickle = xpOption xpickle

-- ------------------------------------------------------------

-- the main arrows

-- | store an arbitray value in a persistent XML document
--
-- The pickler converts a value into an XML tree, this is written out with
-- 'Text.XML.HXT.Arrow.writeDocument'. The option list is passed to 'Text.XML.HXT.Arrow.writeDocument'

xpickleDocument		:: PU a -> Attributes -> String -> IOStateArrow s a XmlTree
xpickleDocument xp al dest
    = arr (pickleDoc xp)
      >>>
      writeDocument al dest

-- | read an arbitray value from an XML document
--
-- The document is read with 'Text.XML.HXT.Arrow.readDocument'. Options are passed
-- to 'Text.XML.HXT.Arrow.readDocument'. The conversion from XmlTree is done with the
-- pickler

xunpickleDocument	:: PU a -> Attributes -> String -> IOStateArrow s b a
xunpickleDocument xp al src
    = readDocument  al src
      >>>
      arrL (maybeToList . unpickleDoc xp)

-- ------------------------------------------------------------
