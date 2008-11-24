-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.Pickle.Xml
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

module Text.XML.HXT.Arrow.Pickle.Xml
where

import           Data.Maybe
import		 Data.Map (Map)
import qualified Data.Map as M

import           Text.XML.HXT.DOM.Interface
import qualified Text.XML.HXT.DOM.XmlNode as XN

import           Control.Arrow.ListArrows
import           Text.XML.HXT.Arrow.XmlArrow
import           Text.XML.HXT.Arrow.Pickle.Schema
import           Text.XML.HXT.Arrow.ReadDocument  (xread)
import           Text.XML.HXT.Arrow.Edit          (xshowEscapeXml)

-- ------------------------------------------------------------

data St		= St { attributes :: [XmlTree]
		     , contents   :: [XmlTree]
		     }

data PU a	= PU { appPickle   :: (a, St) -> St
		     , appUnPickle :: St -> (Maybe a, St)
		     , theSchema   :: Schema
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

getAtt		:: QName -> St -> Maybe XmlTree
getAtt qn s
    = listToMaybe $
      runLA ( arrL attributes
	      >>>
	      isAttr >>> hasQName qn
	    ) s

getCont		:: St -> Maybe XmlTree
getCont s	= listToMaybe . contents $ s

-- ------------------------------------------------------------

-- | conversion of an arbitrary value into an XML document tree.
--
-- The pickler, first parameter, controls the conversion process.
-- Result is a complete document tree including a root node

pickleDoc	:: PU a -> a -> XmlTree
pickleDoc p v
    = XN.mkRoot (attributes st) (contents st)
    where
    st = appPickle p (v, emptySt)

-- | Conversion of an XML document tree into an arbitrary data type
--
-- The inverse of 'pickleDoc'.
-- This law should hold for all picklers: @ unpickle px . pickle px $ v == Just v @.
-- Not every possible combination of picklers make sense.
-- For reconverting a value from an XML tree, is becomes neccessary,
-- to introduce \"enough\" markup for unpickling the value

unpickleDoc :: PU a -> XmlTree -> Maybe a
unpickleDoc p t
    | XN.isRoot t
	= fst . appUnPickle p $ St { attributes = fromJust . XN.getAttrl $  t
				   , contents   =            XN.getChildren t
				   }
    | otherwise
	= unpickleDoc p (XN.mkRoot [] [t])

-- ------------------------------------------------------------

-- | The zero pickler
--
-- Encodes nothing, fails always during unpickling

xpZero			:: PU a
xpZero			=  PU { appPickle   = snd
			      , appUnPickle = \ s -> (Nothing, s)
			      , theSchema   = scNull
			      }

-- unit pickler

xpUnit			:: PU ()
xpUnit			= xpLift ()

-- | Lift a value to a pickler
--
-- When pickling, nothing is encoded, when unpickling, the given value is inserted.
-- This pickler always succeeds.

xpLift			:: a -> PU a
xpLift x		=  PU { appPickle   = snd
			      , appUnPickle = \ s -> (Just x, s)
			      , theSchema   = scEmpty
			      }

-- | Lift a Maybe value to a pickler.
--
-- @Nothing@ is mapped to the zero pickler, @Just x@ is pickled with @xpLift x@.

xpLiftMaybe		:: Maybe a -> PU a
xpLiftMaybe v		= (xpLiftMaybe' v) { theSchema = scOption scEmpty }
    where
    xpLiftMaybe' Nothing	= xpZero
    xpLiftMaybe' (Just x) 	= xpLift x


-- | pickle\/unpickle combinator for sequence and choice.
--
-- When the first unpickler fails,
-- the second one is taken, else the third one configured with the result from the first
-- is taken. This pickler is a generalisation for 'xpSeq' and 'xpChoice' .
--
-- The schema must be attached later, e.g. in xpPair or other higher level combinators

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
	 , theSchema   = undefined
	 }


-- | Combine two picklers sequentially.
--
-- If the first fails during
-- unpickling, the whole unpickler fails

xpSeq	:: (b -> a) -> PU a -> (a -> PU b) -> PU b
xpSeq	= xpCondSeq xpZero


-- | combine tow picklers with a choice
--
-- Run two picklers in sequence like with xpSeq.
-- When during unpickling the first one fails,
-- an alternative pickler (first argument) is applied.
-- This pickler is only used as combinator for unpickling.
 
xpChoice		:: PU b -> PU a -> (a -> PU b) -> PU b
xpChoice pb	= xpCondSeq pb undefined


-- | map value into another domain and apply pickler there
--
-- One of the most often used picklers.

xpWrap			:: (a -> b, b -> a) -> PU a -> PU b
xpWrap (i, j) pa	= (xpSeq j pa (xpLift . i)) { theSchema = theSchema pa }

-- | like 'xpWrap', but if the inverse mapping is undefined, the unpickler fails
--
-- Map a value into another domain. If the inverse mapping is
-- undefined (Nothing), the unpickler fails

xpWrapMaybe		:: (a -> Maybe b, b -> a) -> PU a -> PU b
xpWrapMaybe (i, j) pa	= (xpSeq j pa (xpLiftMaybe . i)) { theSchema = theSchema pa }

-- | pickle a pair of values sequentially
--
-- Used for pairs or together with wrap for pickling
-- algebraic data types with two components

xpPair	:: PU a -> PU b -> PU (a, b)
xpPair pa pb
    = ( xpSeq fst pa (\ a ->
        xpSeq snd pb (\ b ->
        xpLift (a,b)))
      ) { theSchema = scSeq (theSchema pa) (theSchema pb) }

-- | Like 'xpPair' but for triples

xpTriple	:: PU a -> PU b -> PU c -> PU (a, b, c)
xpTriple pa pb pc
    = xpWrap (toTriple, fromTriple) (xpPair pa (xpPair pb pc))
    where
    toTriple   ~(a, ~(b, c)) = (a,  b, c )
    fromTriple ~(a,   b, c ) = (a, (b, c))

-- | Like 'xpPair' and 'xpTriple' but for 4-tuples

xp4Tuple	:: PU a -> PU b -> PU c -> PU d -> PU (a, b, c, d)
xp4Tuple pa pb pc pd
    = xpWrap (toQuad, fromQuad) (xpPair pa (xpPair pb (xpPair pc pd)))
    where
    toQuad   ~(a, ~(b, ~(c, d))) = (a,  b,  c, d  )
    fromQuad ~(a,   b,   c, d  ) = (a, (b, (c, d)))

-- | Like 'xpPair' and 'xpTriple' but for 5-tuples

xp5Tuple	:: PU a -> PU b -> PU c -> PU d -> PU e -> PU (a, b, c, d, e)
xp5Tuple pa pb pc pd pe
    = xpWrap (toQuint, fromQuint) (xpPair pa (xpPair pb (xpPair pc (xpPair pd pe))))
    where
    toQuint   ~(a, ~(b, ~(c, ~(d, e)))) = (a,  b,  c,  d, e   )
    fromQuint ~(a,   b,   c,   d, e   ) = (a, (b, (c, (d, e))))

-- | Like 'xpPair' and 'xpTriple' but for 6-tuples

xp6Tuple	:: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU (a, b, c, d, e, f)
xp6Tuple pa pb pc pd pe pf
    = xpWrap (toSix, fromSix) (xpPair pa (xpPair pb (xpPair pc (xpPair pd (xpPair pe pf)))))
    where
    toSix   ~(a, ~(b, ~(c, ~(d, ~(e, f))))) = (a,  b,  c,  d,  e, f    )
    fromSix ~(a,   b,   c,   d,   e, f)     = (a, (b, (c, (d, (e, f)))))

-- | Pickle a string into an XML text node
--
-- One of the most often used primitive picklers. Attention:
-- For pickling empty strings use 'xpText0'. If the text has a more
-- specific datatype than xsd:string, use 'xpTextDT'

xpText	:: PU String
xpText	= xpTextDT scString1

-- | Pickle a string into an XML text node
--
-- Text pickler with a description of the structure of the text
-- by a schema. A schema for a data type can be defined by 'Text.XML.HXT.Arrow.Pickle.Schema.scDT'.
-- In 'Text.XML.HXT.Arrow.Pickle.Schema' there are some more functions for creating
-- simple datatype descriptions.

xpTextDT	:: Schema -> PU String
xpTextDT sc
    = PU { appPickle   = \ (s, st) -> addCont (XN.mkText s) st
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleString st)
	 , theSchema   = sc
	 }
    where
    unpickleString st
	= do
	  t <- getCont st
	  s <- XN.getText t
	  return (Just s, dropCont st)

-- | Pickle a possibly empty string into an XML node.
--
-- Must be used in all places, where empty strings are legal values.
-- If the content of an element can be an empty string, this string disapears
-- during storing the DOM into a document and reparse the document.
-- So the empty text node becomes nothing, and the pickler must deliver an empty string,
-- if there is no text node in the document.

xpText0	:: PU String
xpText0	= xpText0DT scString1

-- | Pickle a possibly empty string with a datatype description into an XML node.
--
-- Like 'xpText0' but with extra Parameter for datatype description as in 'xpTextDT'.

xpText0DT	:: Schema -> PU String
xpText0DT sc
    = xpWrap (fromMaybe "", emptyToNothing) $ xpOption $ xpTextDT sc
    where
    emptyToNothing "" = Nothing
    emptyToNothing x  = Just x

-- | Pickle an arbitrary value by applyling show during pickling
-- and read during unpickling.
--
-- Real pickling is then done with 'xpText'.
-- One of the most often used pimitive picklers. Applicable for all
-- types which are instances of @Read@ and @Show@

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

-- ------------------------------------------------------------

-- | Pickle an XmlTree by just adding it
--
-- Usefull for components of type XmlTree in other data structures

xpTree	:: PU XmlTree
xpTree	= PU { appPickle   = \ (s, st) -> addCont s st
	     , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleTree st)
	     , theSchema   = Any
	     }
    where
    unpickleTree st
	= do
	  t <- getCont st
	  return (Just t, dropCont st)

-- | Pickle a whole list of XmlTrees by just adding the list, unpickle is done by taking all element contens.
--
-- This pickler should always combined with 'xpElem' for taking the whole contents of an element.

xpTrees	:: PU [XmlTree]
xpTrees	= (xpList xpTree) { theSchema = Any }

-- | Pickle a string representing XML contents by inserting the tree representation into the XML document.
--
-- Unpickling is done by converting the contents with
-- 'Text.XML.HXT.Arrow.Edit.xshowEscapeXml' into a string,
-- this function will escape all XML special chars, such that pickling the value back becomes save.
-- Pickling is done with 'Text.XML.HXT.Arrow.ReadDocument.xread'

xpXmlText	:: PU String
xpXmlText
    = xpWrap ( showXML, readXML ) $ xpTrees
    where
    showXML = concat . runLA ( xshowEscapeXml unlistA )
    readXML = runLA xread

-- ------------------------------------------------------------

-- | Encoding of optional data by ignoring the Nothing case during pickling
-- and relying on failure during unpickling to recompute the Nothing case
--
-- The default pickler for Maybe types

xpOption	:: PU a -> PU (Maybe a)
xpOption pa
    = PU { appPickle   = ( \ (a, st) ->
			   case a of
			   Nothing -> st
			   Just x  -> appPickle pa (x, st)
			 )

	 , appUnPickle = appUnPickle $
	                 xpChoice (xpLift Nothing) pa (xpLift . Just)

         , theSchema   = scOption (theSchema pa)
	 }

-- | Optional conversion with default value
--
-- The default value is not encoded in the XML document,
-- during unpickling the default value is inserted if the pickler fails

xpDefault	:: (Eq a) => a -> PU a -> PU a
xpDefault df
    = xpWrap ( fromMaybe df
	     , \ x -> if x == df then Nothing else Just x
	     ) .
      xpOption

-- ------------------------------------------------------------

-- | Encoding of list values by pickling all list elements sequentially.
--
-- Unpickler relies on failure for detecting the end of the list.
-- The standard pickler for lists. Can also be used in combination with 'xpWrap'
-- for constructing set and map picklers

xpList	:: PU a -> PU [a]
xpList pa
    = PU { appPickle   = ( \ (a, st) ->
			   case a of
			   []  -> st
			   _:_ -> appPickle pc (a, st)
			 )
	 , appUnPickle = appUnPickle $
                         xpChoice (xpLift []) pa
	                   (\ x -> xpSeq id (xpList pa) (\xs -> xpLift (x:xs)))

	 , theSchema   = scList (theSchema pa)
	 }
      where
      pc = xpSeq head  pa       (\ x ->
	   xpSeq tail (xpList pa) (\ xs ->
	   xpLift (x:xs)))

-- | Encoding of a none empty list of values
--
-- Attention: when calling this pickler with an empty list,
-- an internal error \"head of empty list is raised\".

xpList1	:: PU a -> PU [a]
xpList1 pa
    = ( xpWrap (\ (x, xs) -> x : xs
	       ,\ (x : xs) -> (x, xs)
	       ) $
	xpPair pa (xpList pa)
      ) { theSchema = scList1 (theSchema pa) }

-- ------------------------------------------------------------

-- | Standard pickler for maps
--
-- This pickler converts a map into a list of pairs.
-- All key value pairs are mapped to an element with name (1.arg),
-- the key is encoded as an attribute named by the 2. argument,
-- the 3. arg is the pickler for the keys, the last one for the values

xpMap	:: Ord k => String -> String -> PU k -> PU v -> PU (Map k v)
xpMap en an xpk xpv
    = xpWrap ( M.fromList
	     , M.toList
	     ) $
      xpList $
      xpElem en $
      xpPair ( xpAttr an $ xpk ) xpv


-- ------------------------------------------------------------

-- | Pickler for sum data types.
--
-- Every constructor is mapped to an index into the list of picklers.
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
	 , theSchema   = scAlts (map theSchema ps)
	 }

-- ------------------------------------------------------------

-- | Pickler for wrapping\/unwrapping data into an XML element
--
-- Extra parameter is the element name given as a QName. THE pickler for constructing
-- nested structures
--
-- Example:
--
-- > xpElemQN (mkName "number") $ xpickle
--
-- will map an (42::Int) onto
--
-- > <number>42</number>

xpElemQN	:: QName -> PU a -> PU a
xpElemQN qn pa
    = PU { appPickle   = ( \ (a, st) ->
			   let
	                   st' = appPickle pa (a, emptySt)
			   in
			   addCont (XN.mkElement qn (attributes st') (contents st')) st
			 )
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleElement st)
	 , theSchema   = scElem (qualifiedName qn) (theSchema pa)
	 }
      where
      unpickleElement st
	  = do
	    t <- getCont st
	    n <- XN.getElemName t
	    if n /= qn
	       then fail ("element name " ++ show n ++ " does not match" ++ show qn)
	       else do
		    let cs = XN.getChildren t
		    al <- XN.getAttrl t
		    res <- fst . appUnPickle pa $ St {attributes = al, contents = cs}
		    return (Just res, dropCont st)

-- | convenient Pickler for xpElemQN
--
-- > xpElem n = xpElemQN (mkName n)

xpElem		:: String -> PU a -> PU a
xpElem		= xpElemQN . mkName

-- ------------------------------------------------------------

-- | Pickler for wrapping\/unwrapping data into an XML element with an attribute with given value
--
-- To make XML structures flexible but limit the number of different elements, it's sometimes
-- useful to use a kind of generic element with a key value structure
--
-- Example:
--
-- > <attr name="key1">value1</attr>
-- > <attr name="key2">value2</attr>
-- > <attr name="key3">value3</attr>
--
-- the Haskell datatype may look like this
--
-- > type T = T { key1 :: Int ; key2 :: String ; key3 :: Double }
--
-- Then the picker for that type looks like this
--
-- > xpT :: PU T
-- > xpT = xpWrap ( uncurry3 T, \ t -> (key1 t, key2 t, key3 t) ) $
-- >       xpTriple (xpElemWithAttrValue "attr" "name" "key1" $ xpickle)
-- >                (xpElemWithAttrValue "attr" "name" "key2" $ xpText0)
-- >                (xpElemWithAttrValue "attr" "name" "key3" $ xpickle)

xpElemWithAttrValue	:: String -> String -> String -> PU a -> PU a
xpElemWithAttrValue name an av pa
    = PU { appPickle   = ( \ (a, st) ->
			   let
	                   st' = appPickle pa' (a, emptySt)
			   in
			   addCont (XN.mkElement (mkName name) (attributes st') (contents st')) st
			 )
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleElement st)
	 , theSchema   = scElem name (theSchema pa')
	 }
      where
      pa' = xpAddFixedAttr an av $ pa
      noMatch = null . runLA ( isElem
			       >>>
			       hasName name
			       >>>
			       hasAttrValue an (==av)
			     )
      unpickleElement st
	  = do
	    t <- getCont st
	    if noMatch t
	       then fail "element name or attr value does not match"
	       else do
		    let cs = XN.getChildren t
		    al <- XN.getAttrl t
		    res <- fst . appUnPickle pa $ St {attributes = al, contents = cs}
		    return (Just res, dropCont st)

-- ------------------------------------------------------------

-- | Pickler for storing\/retreiving data into\/from an attribute value
--
-- The attribute is inserted in the surrounding element constructed by the 'xpElem' pickler

xpAttrQN	:: QName -> PU a -> PU a
xpAttrQN qn pa
    = PU { appPickle   = ( \ (a, st) ->
			   let
			   st' = appPickle pa (a, emptySt)
			   in
			   addAtt (XN.mkAttr qn (contents st')) st
			 )
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleAttr st)
	 , theSchema   = scAttr (qualifiedName qn) (theSchema pa)
	 }
      where
      unpickleAttr st
	  = do
	    a <- getAtt qn st
	    let av = XN.getChildren a
	    res <- fst . appUnPickle pa $ St {attributes = [], contents = av}
	    return (Just res, st)	-- attribute is not removed from attribute list,
					-- attributes are selected by name

-- | convenient Pickler for xpAttrQN
--
-- > xpAttr n = xpAttrQN (mkName n)

xpAttr		:: String -> PU a -> PU a
xpAttr		= xpAttrQN . mkName

-- | Add an optional attribute for an optional value (Maybe a).

xpAttrImplied	:: String -> PU a -> PU (Maybe a)
xpAttrImplied name pa
    = xpOption $ xpAttr name pa

xpAttrFixed	:: String -> String -> PU ()
xpAttrFixed name val
    = ( xpWrapMaybe ( \ v -> if v == val then Just () else Nothing
		    , const val
		    ) $
	xpAttr name xpText
      ) { theSchema   = scAttr name (scFixed val) }

-- | Add an attribute with a fixed value.
--
-- Useful e.g. to declare namespaces. Is implemented by 'xpAttrFixed'

xpAddFixedAttr	:: String -> String -> PU a -> PU a
xpAddFixedAttr name val pa
    = xpWrap ( snd
	     , (,) ()
	     ) $
      xpPair (xpAttrFixed name val) pa

-- ------------------------------------------------------------

-- | The class for overloading 'xpickle', the default pickler

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

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e) => XmlPickler (a,b,c,d,e) where
    xpickle = xp5Tuple xpickle xpickle xpickle xpickle xpickle

instance XmlPickler a => XmlPickler [a] where
    xpickle = xpList xpickle

instance XmlPickler a => XmlPickler (Maybe a) where
    xpickle = xpOption xpickle

-- ------------------------------------------------------------
