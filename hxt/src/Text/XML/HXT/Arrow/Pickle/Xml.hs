{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.Pickle.Xml
   Copyright  : Copyright (C) 2005-2012 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Pickler functions for converting between user defined data types
   and XmlTree data. Usefull for persistent storage and retreival
   of arbitray data as XML documents.

   This module is an adaptation of the pickler combinators
   developed by Andrew Kennedy
   ( http:\/\/research.microsoft.com\/~akenn\/fun\/picklercombinators.pdf ).

   The difference to Kennedys approach is that the target is not
   a list of Chars but a list of XmlTrees. The basic picklers will
   convert data into XML text nodes. New are the picklers for
   creating elements and attributes.

   One extension was neccessary: The unpickling may fail.

   Old: Therefore the unpickler has a Maybe result type.
   Failure is used to unpickle optional elements
   (Maybe data) and lists of arbitray length.

   Since hxt-9.2.0: The unpicklers are implemented as
   a parser monad with an Either err val result type.
   This enables appropriate error messages , when unpickling
   XML stuff, that is not generated with the picklers and which contains
   some elements and/or attributes that are not handled when unpickling.

   There is an example program demonstrating the use
   of the picklers for a none trivial data structure.
   (see \"examples\/arrows\/pickle\" directory in the hxt distribution)

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.Pickle.Xml
where

import           Control.Applicative              (Applicative (..))
import           Control.Arrow.ArrowList
import           Control.Arrow.ListArrows
import           Control.Monad                    ()
#if MIN_VERSION_mtl(2,2,0)
import           Control.Monad.Except             (MonadError (..))
#else
import           Control.Monad.Error              (MonadError (..))
#endif
import           Control.Monad.State              (MonadState (..), gets,
                                                   modify)

import           Data.Char                        (isDigit)
import           Data.List                        (foldl')
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, fromMaybe)

import           Text.XML.HXT.Arrow.Edit          (xshowEscapeXml)
import           Text.XML.HXT.Arrow.Pickle.Schema
import           Text.XML.HXT.Arrow.ReadDocument  (xread)
import           Text.XML.HXT.Arrow.WriteDocument (writeDocumentToString)
import           Text.XML.HXT.Arrow.XmlState
import           Text.XML.HXT.DOM.Interface
import qualified Text.XML.HXT.DOM.ShowXml         as XN
import qualified Text.XML.HXT.DOM.XmlNode         as XN

{- just for embedded test cases, prefix with -- to activate
import           Text.XML.HXT.Arrow.XmlArrow
import qualified Control.Arrow.ListArrows         as X
-- -}

-- ------------------------------------------------------------

data St         = St { attributes :: [XmlTree]
                     , contents   :: [XmlTree]
                     , nesting    :: Int                -- the remaining 3 fields are used only for unpickling
                     , pname      :: QName              -- to generate appropriate error messages
                     , pelem      :: Bool
                     } deriving (Show)

data PU a       = PU { appPickle   :: Pickler a         -- (a, St) -> St
                     , appUnPickle :: Unpickler a
                     , theSchema   :: Schema
                     }

-- --------------------
--
-- The pickler

type Pickler a          = a -> St -> St

-- --------------------
--
-- The unpickler monad, a combination of state and error monad

newtype Unpickler a     = UP { runUP :: St -> (UnpickleVal a, St) }

type UnpickleVal a      = Either UnpickleErr a

type UnpickleErr        = (String, St)

instance Functor Unpickler where
    fmap f u    = UP $ \ st ->
                  let (r, st') = runUP u st in (fmap f r, st')

instance Applicative Unpickler where
    pure a      = UP $ \ st -> (Right a, st)
    uf <*> ua   = UP $ \ st ->
                  let (f, st') = runUP uf st in
                  case f of
                    Left err -> (Left err, st')
                    Right f' -> runUP (fmap f' ua) st'

instance Monad Unpickler where
    return      = pure
    u >>= f     = UP $ \ st ->
                  let (r, st') = runUP u st in
                  case r of
                    Left err -> (Left err, st')
                    Right v  -> runUP (f v) st'
    fail        = throwMsg                              -- don't use fail, use throwError


instance MonadState St Unpickler where
    get         = UP $ \ st -> (Right st, st)
    put st      = UP $ \ _  -> (Right (), st)

instance MonadError UnpickleErr Unpickler where
    throwError err
                = UP $ \ st -> (Left err, st)

    -- redundant, not (yet) used
    catchError u handler
                = UP $ \ st ->
                  let (r, st') = runUP u st in
                  case r of
                    Left err -> runUP (handler err) st  -- not st', state will be reset in error case
                    _        -> (r, st')

throwMsg        :: String -> Unpickler a
throwMsg msg    = UP $ \ st -> (Left (msg, st), st)

-- | Choice combinator for unpickling
--
-- first 2 arguments are applied sequentially, but if the 1. one fails the
-- 3. arg is applied

mchoice         :: Unpickler a -> (a -> Unpickler b) -> Unpickler b -> Unpickler b
mchoice u f v   = UP $ \ st ->
                  let (r, st') = runUP u st in
                  case r of
                    Right x
                        -> runUP (f x) st'                      -- success
                    Left e@(_msg, st'')
                        -> if nesting st'' == nesting st        -- true: failure in parsing curr contents
                           then runUP v st                      -- try the alternative unpickler
                           else (Left e, st')                   -- false: failure in unpickling a subtree of
                                                                -- the current contents, so the whole unpickler
                                                                -- must fail

-- | Lift a Maybe value into the Unpickler monad.
--
-- The 1. arg is the attached error message

liftMaybe       :: String -> Maybe a -> Unpickler a
liftMaybe e v  = case v of
                    Nothing -> throwMsg e
                    Just x  -> return x

-- | Lift an Either value into the Unpickler monad

liftUnpickleVal         :: UnpickleVal a -> Unpickler a
liftUnpickleVal v       = UP $ \ st -> (v, st)

-- --------------------

getCont         :: Unpickler XmlTree
getCont         = do cs <- gets contents
                     case cs of
                       []       -> throwMsg "no more contents to be read"
                       (x : xs) -> do modify (\ s -> s {contents = xs})
                                      return x

getAtt          :: QName -> Unpickler XmlTree
getAtt qn       = do as <- gets attributes
                     case findAtt as of
                       Nothing -> throwMsg $ "no attribute value found for " ++ show qn
                       Just (a, as') -> do modify (\ s -> s {attributes = as'})
                                           return a
    where
      findAtt   = findElem (maybe False (== qn) . XN.getAttrName)

getNSAtt        :: String -> Unpickler ()
getNSAtt ns     = do as <- gets attributes
                     case findNS as of
                       Nothing        -> throwMsg $
                                         "no namespace declaration found for namespace " ++ show ns
                       Just (_a, as') -> do modify (\ s -> s {attributes = as'})
                                            return ()
    where
      isNS t    = (fromMaybe False . fmap isNameSpaceName . XN.getAttrName $ t)
                  &&
                  XN.xshow (XN.getChildren t) == ns
      findNS    = findElem isNS

-- --------------------

emptySt         :: St
emptySt         =  St { attributes = []
                      , contents   = []
                      , nesting    = 0
                      , pname      = mkName "/"
                      , pelem      = True
                      }

putAtt          :: QName -> [XmlTree] -> St -> St
putAtt qn v s   = s {attributes = x : attributes s}
                  where
                    x = XN.mkAttr qn v
{-# INLINE putAtt #-}

putCont         :: XmlTree -> St -> St
putCont x s     = s {contents = x : contents s}
{-# INLINE putCont #-}

-- --------------------
--
-- generally useful function for splitting a value from a list

findElem       :: (a -> Bool) -> [a] -> Maybe (a, [a])
findElem p     = find' id
    where
      find' _ []         = Nothing
      find' prefix (x : xs)
          | p x          = Just (x, prefix xs)
          | otherwise    = find' (prefix . (x:)) xs

-- ------------------------------------------------------------
--
-- | Format the context of an error message.

formatSt                :: St -> String
formatSt st             = fcx ++
                          fa (attributes st) ++
                          fc (contents   st)
    where
      fcx               = "\n" ++ "context:    " ++
                          ( if pelem st
                            then "element"
                            else "attribute"
                          ) ++
                          " " ++ show (pname st)
      fc []             = ""
      fc cs             = "\n" ++ "contents:   " ++ formatXML cs
      fa []             = ""
      fa as             = "\n" ++ "attributes: " ++ formatXML as
      formatXML         = format 80 . showXML
      showXML           = concat . runLA ( xshowEscapeXml unlistA )
      format n s        = let s' = take (n + 1) s in
                          if length s' <= n then s' else take n s ++ "..."

-- ------------------------------------------------------------

-- | conversion of an arbitrary value into an XML document tree.
--
-- The pickler, first parameter, controls the conversion process.
-- Result is a complete document tree including a root node

pickleDoc       :: PU a -> a -> XmlTree
pickleDoc p v   = XN.mkRoot (attributes st) (contents st)
    where
      st        = appPickle p v emptySt

-- | Conversion of an XML document tree into an arbitrary data type
--
-- The inverse of 'pickleDoc'.
-- This law should hold for all picklers: @ unpickle px . pickle px $ v == Just v @.
-- Not every possible combination of picklers does make sense.
-- For reconverting a value from an XML tree, is becomes neccessary,
-- to introduce \"enough\" markup for unpickling the value

unpickleDoc     :: PU a -> XmlTree -> Maybe a
unpickleDoc p   = either (const Nothing) Just
                  . unpickleDoc' p

-- | Like unpickleDoc but with a (sometimes) useful error message, when unpickling failed.

unpickleDoc'    :: PU a -> XmlTree -> Either String a
unpickleDoc' p t
    | XN.isRoot t       = mapErr $
                          unpickleElem' p 0              t
    | otherwise         = unpickleDoc'  p (XN.mkRoot [] [t])
    where
      mapErr            = either ( Left .
                                   \ (msg, st) -> msg ++ formatSt st
                                 ) Right

-- | The main entry for unpickling, called by unpickleDoc

unpickleElem'   :: PU a -> Int -> XmlTree -> UnpickleVal a
unpickleElem' p l t
    = fst
      . runUP (appUnPickle p)
      $ St { attributes = fromMaybe [] .
                          XN.getAttrl $  t
           , contents   = XN.getChildren t
           , nesting    = l
           , pname      = fromJust .
                          XN.getName  $  t
           , pelem      = XN.isElem      t
           }

-- ------------------------------------------------------------

-- | Pickles a value, then writes the document to a string.

showPickled :: (XmlPickler a) => SysConfigList -> a -> String
showPickled a = concat . (pickleDoc xpickle >>> runLA (writeDocumentToString a))

-- ------------------------------------------------------------

-- | The zero pickler
--
-- Encodes nothing, fails always during unpickling

xpZero                  :: String -> PU a
xpZero err              =  PU { appPickle   = const id
                              , appUnPickle = throwMsg err
                              , theSchema   = scNull
                              }

-- | unit pickler

xpUnit                  :: PU ()
xpUnit                  = xpLift ()

-- | Check EOF pickler.
--
-- When pickling, this behaves like the unit pickler.
-- The unpickler fails, when there is some unprocessed XML contents left.

xpCheckEmptyContents    :: PU a -> PU a
xpCheckEmptyContents pa =  PU { appPickle   = appPickle pa
                              , appUnPickle = do res <- appUnPickle pa
                                                 cs <- gets contents
                                                 if null cs
                                                    then return res
                                                    else contentsLeft
                              , theSchema   = scNull
                              }
    where
      contentsLeft      = throwMsg
                          "xpCheckEmptyContents: unprocessed XML content detected"

-- | Like xpCheckEmptyContents, but checks the attribute list

xpCheckEmptyAttributes  :: PU a -> PU a
xpCheckEmptyAttributes pa
                        =  PU { appPickle   = appPickle pa
                              , appUnPickle = do res <- appUnPickle pa
                                                 as <- gets attributes
                                                 if null as
                                                    then return res
                                                    else attributesLeft
                              , theSchema   = scNull
                              }
    where
      attributesLeft    = throwMsg
                          "xpCheckEmptyAttributes: unprocessed XML attribute(s) detected"

-- | Composition of xpCheckEmptyContents and xpCheckAttributes

xpCheckEmpty            :: PU a -> PU a
xpCheckEmpty            = xpCheckEmptyAttributes . xpCheckEmptyContents

xpLift                  :: a -> PU a
xpLift x                =  PU { appPickle   = const id
                              , appUnPickle = return x
                              , theSchema   = scEmpty
                              }

-- | Lift a Maybe value to a pickler.
--
-- @Nothing@ is mapped to the zero pickler, @Just x@ is pickled with @xpLift x@.

xpLiftMaybe                     :: Maybe a -> PU a
xpLiftMaybe v                   = (xpLiftMaybe'' v) { theSchema = scOption scEmpty }
    where
    xpLiftMaybe'' Nothing       = xpZero "xpLiftMaybe: got Nothing"
    xpLiftMaybe'' (Just x)      = xpLift x

xpLiftEither                    :: Either String a -> PU a
xpLiftEither v                  = (xpLiftEither'' v) { theSchema = scOption scEmpty }
    where
    xpLiftEither'' (Left err)   = xpZero err
    xpLiftEither'' (Right x)    = xpLift x

-- | Combine two picklers sequentially.
--
-- If the first fails during
-- unpickling, the whole unpickler fails

xpSeq           :: (b -> a) -> PU a -> (a -> PU b) -> PU b
xpSeq f pa k
    = PU { appPickle  = ( \ b ->
                          let a = f b in
                          appPickle pa a . appPickle (k a) b
                         )
         , appUnPickle = appUnPickle pa >>= (appUnPickle . k)
         , theSchema   = undefined
         }

-- | First apply a fixed pickler/unpickler, then a 2. one
--
-- If the first fails during unpickling, the whole pickler fails.
-- This can be used to check some properties of the input, e.g. whether
-- a given fixed attribute or a namespace declaration exists ('xpAddFixedAttr', 'xpAddNSDecl')
-- or to filter the input, e.g. to ignore some elements or attributes ('xpFilterCont', 'xpFilterAttr').
--
-- When pickling, this can be used to insert some fixed XML pieces, e.g. namespace declarations,
-- class attributes or other stuff.

xpSeq'          :: PU () -> PU a -> PU a
xpSeq' pa       = xpWrap ( snd
                         , \ y -> ((), y)
                         ) .
                  xpPair pa

-- | combine tow picklers with a choice
--
-- Run two picklers in sequence like with xpSeq.
-- If during unpickling the first one fails,
-- an alternative pickler (first argument) is applied.
-- This pickler is only used as combinator for unpickling.

xpChoice                :: PU b -> PU a -> (a -> PU b) -> Unpickler b
xpChoice pb pa k        = mchoice (appUnPickle pa) (appUnPickle . k) (appUnPickle pb)


-- | map value into another domain and apply pickler there
--
-- One of the most often used picklers.

xpWrap                  :: (a -> b, b -> a) -> PU a -> PU b
xpWrap (i, j) pa        = (xpSeq j pa (xpLift . i)) { theSchema = theSchema pa }

-- | like 'xpWrap', but if the inverse mapping is undefined, the unpickler fails
--
-- Map a value into another domain. If the inverse mapping is
-- undefined (Nothing), the unpickler fails
--
-- Deprecated: Use xpWrapEither, this gives better error messages

xpWrapMaybe             :: (a -> Maybe b, b -> a) -> PU a -> PU b
xpWrapMaybe (i, j) pa   = (xpSeq j pa (xpLiftMaybe . i)) { theSchema = theSchema pa }

-- | like 'xpWrap', but if the inverse mapping is undefined, the unpickler fails
--
-- Map a value into another domain. If the inverse mapping is
-- undefined, the unpickler fails with an error message in the Left component

xpWrapEither             :: (a -> Either String b, b -> a) -> PU a -> PU b
xpWrapEither (i, j) pa   = (xpSeq j pa (xpLiftEither . i)) { theSchema = theSchema pa }

-- ------------------------------------------------------------

-- | pickle a pair of values sequentially
--
-- Used for pairs or together with wrap for pickling
-- algebraic data types with two components

xpPair  :: PU a -> PU b -> PU (a, b)
xpPair pa pb
    = ( xpSeq fst pa (\ a ->
        xpSeq snd pb (\ b ->
        xpLift (a,b)))
      ) { theSchema = scSeq (theSchema pa) (theSchema pb) }

-- | Like 'xpPair' but for triples

xpTriple        :: PU a -> PU b -> PU c -> PU (a, b, c)
xpTriple pa pb pc
    = xpWrap (toTriple, fromTriple) (xpPair pa (xpPair pb pc))
    where
    toTriple   ~(a, ~(b, c)) = (a,  b, c )
    fromTriple ~(a,   b, c ) = (a, (b, c))

-- | Like 'xpPair' and 'xpTriple' but for 4-tuples

xp4Tuple        :: PU a -> PU b -> PU c -> PU d -> PU (a, b, c, d)
xp4Tuple pa pb pc pd
    = xpWrap (toQuad, fromQuad) (xpPair pa (xpPair pb (xpPair pc pd)))
    where
    toQuad   ~(a, ~(b, ~(c, d))) = (a,  b,  c, d  )
    fromQuad ~(a,   b,   c, d  ) = (a, (b, (c, d)))

-- | Like 'xpPair' and 'xpTriple' but for 5-tuples

xp5Tuple        :: PU a -> PU b -> PU c -> PU d -> PU e -> PU (a, b, c, d, e)
xp5Tuple pa pb pc pd pe
    = xpWrap (toQuint, fromQuint) (xpPair pa (xpPair pb (xpPair pc (xpPair pd pe))))
    where
    toQuint   ~(a, ~(b, ~(c, ~(d, e)))) = (a,  b,  c,  d, e   )
    fromQuint ~(a,   b,   c,   d, e   ) = (a, (b, (c, (d, e))))

-- | Like 'xpPair' and 'xpTriple' but for 6-tuples

xp6Tuple        :: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU (a, b, c, d, e, f)
xp6Tuple pa pb pc pd pe pf
    = xpWrap (toSix, fromSix) (xpPair pa (xpPair pb (xpPair pc (xpPair pd (xpPair pe pf)))))
    where
    toSix   ~(a, ~(b, ~(c, ~(d, ~(e, f))))) = (a,  b,  c,  d,  e, f    )
    fromSix ~(a,   b,   c,   d,   e, f)     = (a, (b, (c, (d, (e, f)))))

-- ------------------------------------------------------------

-- | Like 'xpPair' and 'xpTriple' but for 7-tuples
--
-- Thanks to Tony Morris for doing xp7Tuple, ..., xp24Tuple.

xp7Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
            PU f -> PU g -> PU (a, b, c, d, e, f, g)
xp7Tuple a b c d e f g
    = xpWrap ( \ (a, (b, c, d, e, f, g)) -> (a, b, c, d, e, f, g)
             , \ (a, b, c, d, e, f, g)   -> (a, (b, c, d, e, f, g))
             )
      (xpPair a (xp6Tuple b c d e f g))

xp8Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
            PU f -> PU g -> PU h -> PU (a, b, c, d, e, f, g, h)
xp8Tuple a b c d e f g h
    = xpWrap ( \ ((a, b), (c, d, e, f, g, h)) -> (a, b, c, d, e, f, g, h)
             , \ (a, b, c, d, e, f, g, h) -> ((a, b), (c, d, e, f, g, h))
             )
      (xpPair (xpPair a b) (xp6Tuple c d e f g h))

xp9Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
            PU f -> PU g -> PU h -> PU i -> PU (a, b, c, d, e, f, g, h, i)
xp9Tuple a b c d e f g h i
    = xpWrap ( \ ((a, b, c), (d, e, f, g, h, i)) -> (a, b, c, d, e, f, g, h, i)
             , \ (a, b, c, d, e, f, g, h, i) -> ((a, b, c), (d, e, f, g, h, i))
             )
      (xpPair (xpTriple a b c) (xp6Tuple d e f g h i))

xp10Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU (a, b, c, d, e, f, g, h, i, j)
xp10Tuple a b c d e f g h i j
    = xpWrap ( \ ((a, b, c, d), (e, f, g, h, i, j)) -> (a, b, c, d, e, f, g, h, i, j)
             , \ (a, b, c, d, e, f, g, h, i, j) -> ((a, b, c, d), (e, f, g, h, i, j))
             )
      (xpPair (xp4Tuple a b c d) (xp6Tuple e f g h i j))

xp11Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU (a, b, c, d, e, f, g, h, i, j, k)
xp11Tuple a b c d e f g h i j k
    = xpWrap ( \ ((a, b, c, d, e), (f, g, h, i, j, k)) -> (a, b, c, d, e, f, g, h, i, j, k)
             , \ (a, b, c, d, e, f, g, h, i, j, k) -> ((a, b, c, d, e), (f, g, h, i, j, k))
             )
      (xpPair (xp5Tuple a b c d e) (xp6Tuple f g h i j k))

xp12Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU (a, b, c, d, e, f, g, h, i, j, k, l)
xp12Tuple a b c d e f g h i j k l
    = xpWrap ( \ ((a, b, c, d, e, f), (g, h, i, j, k, l)) -> (a, b, c, d, e, f, g, h, i, j, k, l)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l) -> ((a, b, c, d, e, f), (g, h, i, j, k, l))
             )
      (xpPair (xp6Tuple a b c d e f) (xp6Tuple g h i j k l))

xp13Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m)
xp13Tuple a b c d e f g h i j k l m
    = xpWrap ( \ (a, (b, c, d, e, f, g), (h, i, j, k, l, m)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m) -> (a, (b, c, d, e, f, g), (h, i, j, k, l, m))
             )
      (xpTriple a (xp6Tuple b c d e f g) (xp6Tuple h i j k l m))

xp14Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
xp14Tuple a b c d e f g h i j k l m n
    = xpWrap ( \ ((a, b), (c, d, e, f, g, h), (i, j, k, l, m, n)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> ((a, b), (c, d, e, f, g, h), (i, j, k, l, m, n))
             )
      (xpTriple (xpPair a b) (xp6Tuple c d e f g h) (xp6Tuple i j k l m n))

xp15Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
xp15Tuple a b c d e f g h i j k l m n o
    = xpWrap ( \ ((a, b, c), (d, e, f, g, h, i), (j, k, l, m, n, o)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> ((a, b, c), (d, e, f, g, h, i), (j, k, l, m, n, o))
             )
      (xpTriple (xpTriple a b c) (xp6Tuple d e f g h i) (xp6Tuple j k l m n o))

xp16Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
xp16Tuple a b c d e f g h i j k l m n o p
    = xpWrap ( \ ((a, b, c, d), (e, f, g, h, i, j), (k, l, m, n, o, p)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> ((a, b, c, d), (e, f, g, h, i, j), (k, l, m, n, o, p))
             )
      (xpTriple (xp4Tuple a b c d) (xp6Tuple e f g h i j) (xp6Tuple k l m n o p))

xp17Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU q -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
xp17Tuple a b c d e f g h i j k l m n o p q
    = xpWrap ( \ ((a, b, c, d, e), (f, g, h, i, j, k), (l, m, n, o, p, q)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> ((a, b, c, d, e), (f, g, h, i, j, k), (l, m, n, o, p, q))
             )
      (xpTriple (xp5Tuple a b c d e) (xp6Tuple f g h i j k) (xp6Tuple l m n o p q))

xp18Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU q -> PU r -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
xp18Tuple a b c d e f g h i j k l m n o p q r
    = xpWrap ( \ ((a, b, c, d, e, f), (g, h, i, j, k, l), (m, n, o, p, q, r)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> ((a, b, c, d, e, f), (g, h, i, j, k, l), (m, n, o, p, q, r))
             )
      (xpTriple (xp6Tuple a b c d e f) (xp6Tuple g h i j k l) (xp6Tuple m n o p q r))

xp19Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU q -> PU r -> PU s -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
xp19Tuple a b c d e f g h i j k l m n o p q r s
    = xpWrap ( \ (a, (b, c, d, e, f, g), (h, i, j, k, l, m), (n, o, p, q, r, s)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> (a, (b, c, d, e, f, g), (h, i, j, k, l, m), (n, o, p, q, r, s))
             )
      (xp4Tuple a (xp6Tuple b c d e f g) (xp6Tuple h i j k l m) (xp6Tuple n o p q r s))

xp20Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU q -> PU r -> PU s -> PU t ->
             PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
xp20Tuple a b c d e f g h i j k l m n o p q r s t
    = xpWrap ( \ ((a, b), (c, d, e, f, g, h), (i, j, k, l, m, n), (o, p, q, r, s, t)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> ((a, b), (c, d, e, f, g, h), (i, j, k, l, m, n), (o, p, q, r, s, t))
             )
      (xp4Tuple (xpPair a b) (xp6Tuple c d e f g h) (xp6Tuple i j k l m n) (xp6Tuple o p q r s t))

xp21Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU q -> PU r -> PU s -> PU t ->
             PU u -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
xp21Tuple a b c d e f g h i j k l m n o p q r s t u
    = xpWrap ( \ ((a, b, c), (d, e, f, g, h, i), (j, k, l, m, n, o), (p, q, r, s, t, u)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) -> ((a, b, c), (d, e, f, g, h, i), (j, k, l, m, n, o), (p, q, r, s, t, u))
             )
      (xp4Tuple (xpTriple a b c) (xp6Tuple d e f g h i) (xp6Tuple j k l m n o) (xp6Tuple p q r s t u))

xp22Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU q -> PU r -> PU s -> PU t ->
             PU u -> PU v -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
xp22Tuple a b c d e f g h i j k l m n o p q r s t u v
    = xpWrap ( \ ((a, b, c, d), (e, f, g, h, i, j), (k, l, m, n, o, p), (q, r, s, t, u, v)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) -> ((a, b, c, d), (e, f, g, h, i, j), (k, l, m, n, o, p), (q, r, s, t, u, v))
             )
      (xp4Tuple (xp4Tuple a b c d) (xp6Tuple e f g h i j) (xp6Tuple k l m n o p) (xp6Tuple q r s t u v))

xp23Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU q -> PU r -> PU s -> PU t ->
             PU u -> PU v -> PU w -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
xp23Tuple a b c d e f g h i j k l m n o p q r s t u v w
    = xpWrap ( \ ((a, b, c, d, e), (f, g, h, i, j, k), (l, m, n, o, p, q), (r, s, t, u, v, w)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) -> ((a, b, c, d, e), (f, g, h, i, j, k), (l, m, n, o, p, q), (r, s, t, u, v, w))
             )
      (xp4Tuple (xp5Tuple a b c d e) (xp6Tuple f g h i j k) (xp6Tuple l m n o p q) (xp6Tuple r s t u v w))

-- | Hopefully no one needs a xp25Tuple

xp24Tuple :: PU a -> PU b -> PU c -> PU d -> PU e ->
             PU f -> PU g -> PU h -> PU i -> PU j ->
             PU k -> PU l -> PU m -> PU n -> PU o ->
             PU p -> PU q -> PU r -> PU s -> PU t ->
             PU u -> PU v -> PU w -> PU x -> PU (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)
xp24Tuple a b c d e f g h i j k l m n o p q r s t u v w x
    = xpWrap ( \ ((a, b, c, d, e, f), (g, h, i, j, k, l), (m, n, o, p, q, r), (s, t, u, v, w, x)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)
             , \ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) -> ((a, b, c, d, e, f), (g, h, i, j, k, l), (m, n, o, p, q, r), (s, t, u, v, w, x))
             )
      (xp4Tuple (xp6Tuple a b c d e f) (xp6Tuple g h i j k l) (xp6Tuple m n o p q r) (xp6Tuple s t u v w x))

-- ------------------------------------------------------------


-- | Pickle a string into an XML text node
--
-- One of the most often used primitive picklers. Attention:
-- For pickling empty strings use 'xpText0'. If the text has a more
-- specific datatype than xsd:string, use 'xpTextDT'

xpText  :: PU String
xpText  = xpTextDT scString1
{-# INLINE xpText #-}

-- | Pickle a string into an XML text node
--
-- Text pickler with a description of the structure of the text
-- by a schema. A schema for a data type can be defined by 'Text.XML.HXT.Arrow.Pickle.Schema.scDT'.
-- In 'Text.XML.HXT.Arrow.Pickle.Schema' there are some more functions for creating
-- simple datatype descriptions.

xpTextDT        :: Schema -> PU String
xpTextDT sc     = PU { appPickle   = putCont . XN.mkText
                     , appUnPickle = do t <- getCont
                                        liftMaybe "xpText: XML text expected" $ XN.getText t
                     , theSchema   = sc
                     }

-- | Pickle a possibly empty string into an XML node.
--
-- Must be used in all places, where empty strings are legal values.
-- If the content of an element can be an empty string, this string disapears
-- during storing the DOM into a document and reparse the document.
-- So the empty text node becomes nothing, and the pickler must deliver an empty string,
-- if there is no text node in the document.

xpText0         :: PU String
xpText0         = xpText0DT scString1
{-# INLINE xpText0 #-}

-- | Pickle a possibly empty string with a datatype description into an XML node.
--
-- Like 'xpText0' but with extra Parameter for datatype description as in 'xpTextDT'.

xpText0DT       :: Schema -> PU String
xpText0DT sc    = xpWrap (fromMaybe "", emptyToNothing) $
                  xpOption $
                  xpTextDT sc
    where
    emptyToNothing "" = Nothing
    emptyToNothing x  = Just x

-- | Pickle an arbitrary value by applyling show during pickling
-- and read during unpickling.
--
-- Real pickling is then done with 'xpText'.
-- One of the most often used pimitive picklers. Applicable for all
-- types which are instances of @Read@ and @Show@

xpPrim                  :: (Read a, Show a) => PU a
xpPrim                  = xpWrapEither (readMaybe, show) xpText
    where
    readMaybe           :: Read a => String -> Either String a
    readMaybe str       = val (reads str)
        where
          val [(x,"")]  = Right x
          val _         = Left $ "xpPrim: reading string " ++ show str ++ " failed"

-- | Pickle an Int
xpInt                   :: PU Int
xpInt                   = xpWrapEither (readMaybe, show) xpText
    where
      readMaybe xs@(_:_)
          | all isDigit xs = Right . foldl' (\ r c -> 10 * r + (fromEnum c - fromEnum '0')) 0 $ xs
      readMaybe ('-' : xs) = fmap (0 -) . readMaybe $ xs
      readMaybe ('+' : xs) =              readMaybe $ xs
      readMaybe        xs  = Left $ "xpInt: reading an Int from string " ++ show xs ++ " failed"

-- ------------------------------------------------------------

-- | Pickle an XmlTree by just adding it
--
-- Usefull for components of type XmlTree in other data structures

xpTree          :: PU XmlTree
xpTree          = PU { appPickle   = putCont
                     , appUnPickle = getCont
                     , theSchema   = Any
                     }

-- | Pickle a whole list of XmlTrees by just adding the list, unpickle is done by taking all element contents.
--
-- This pickler should always be combined with 'xpElem' for taking the whole contents of an element.

xpTrees         :: PU [XmlTree]
xpTrees         = (xpList xpTree) { theSchema = Any }

-- | Pickle a string representing XML contents by inserting the tree representation into the XML document.
--
-- Unpickling is done by converting the contents with
-- 'Text.XML.HXT.Arrow.Edit.xshowEscapeXml' into a string,
-- this function will escape all XML special chars, such that pickling the value back becomes save.
-- Pickling is done with 'Text.XML.HXT.Arrow.ReadDocument.xread'

xpXmlText       :: PU String
xpXmlText       = xpWrap ( showXML, readXML ) $ xpTrees
    where
      showXML   = concat . runLA ( xshowEscapeXml unlistA )
      readXML   = runLA xread

-- ------------------------------------------------------------

-- | Encoding of optional data by ignoring the Nothing case during pickling
-- and relying on failure during unpickling to recompute the Nothing case
--
-- The default pickler for Maybe types

xpOption        :: PU a -> PU (Maybe a)
xpOption pa     = PU { appPickle  = ( \ a ->
                                      case a of
                                        Nothing -> id
                                        Just x  -> appPickle pa x
                                    )

                     , appUnPickle = xpChoice (xpLift Nothing) pa (xpLift . Just)

                     , theSchema   = scOption (theSchema pa)
                     }

-- | Optional conversion with default value
--
-- The default value is not encoded in the XML document,
-- during unpickling the default value is inserted if the pickler fails

xpDefault       :: (Eq a) => a -> PU a -> PU a
xpDefault df    = xpWrap ( fromMaybe df
                         , \ x -> if x == df then Nothing else Just x
                         ) .
                  xpOption

-- ------------------------------------------------------------

-- | Encoding of list values by pickling all list elements sequentially.
--
-- Unpickler relies on failure for detecting the end of the list.
-- The standard pickler for lists. Can also be used in combination with 'xpWrap'
-- for constructing set and map picklers

xpList          :: PU a -> PU [a]
xpList pa       = PU { appPickle  = ( \ a ->
                                      case a of
                                        []  -> id
                                        _:_ -> appPickle pc a
                                    )
                     , appUnPickle = xpChoice
                                     (xpLift [])
                                     pa
                                     (\ x -> xpSeq id (xpList pa) (\xs -> xpLift (x:xs)))

                     , theSchema   = scList (theSchema pa)
                     }
      where
      pc        = xpSeq head  pa         (\ x  ->
                  xpSeq tail (xpList pa) (\ xs ->
                  xpLift (x:xs)          ))

-- | Encoding of a none empty list of values
--
-- Attention: when calling this pickler with an empty list,
-- an internal error \"head of empty list is raised\".

xpList1         :: PU a -> PU [a]
xpList1 pa      = ( xpWrap (\ (x, xs) -> x : xs
                           ,\ x -> (head x, tail x)
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

xpMap           :: Ord k => String -> String -> PU k -> PU v -> PU (Map k v)
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

xpAlt           :: (a -> Int) -> [PU a] -> PU a
xpAlt tag ps    = PU { appPickle   = \ a ->
                                     appPickle (ps !! tag a) a

                     , appUnPickle = case ps of
                                       []     -> throwMsg "xpAlt: no matching unpickler found for a sum datatype"
                                       pa:ps1 -> xpChoice (xpAlt tag ps1) pa xpLift

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

xpElemQN        :: QName -> PU a -> PU a
xpElemQN qn pa  = PU { appPickle   = ( \ a ->
                                       let st' = appPickle pa a emptySt in
                                       putCont (XN.mkElement qn (attributes st') (contents st'))
                                     )
                     , appUnPickle = upElem
                     , theSchema   = scElem (qualifiedName qn) (theSchema pa)
                     }
      where
      upElem    = do t <- getCont
                     n <- liftMaybe "xpElem: XML element expected" $ XN.getElemName t
                     if n /= qn
                        then throwMsg ("xpElem: got element name " ++ show n ++ ", but expected " ++ show qn)
                        else do l <- gets nesting
                                liftUnpickleVal $ unpickleElem' (xpCheckEmpty pa) (l + 1) t

-- | convenient Pickler for xpElemQN
--
-- > xpElem n = xpElemQN (mkName n)

xpElem          :: String -> PU a -> PU a
xpElem          = xpElemQN . mkName

-- | convenient Pickler for xpElemQN
--   for pickling elements with respect to namespaces
--
-- > xpElemNS ns px lp = xpElemQN (mkQName px lp ns)

xpElemNS        :: String -> String -> String -> PU a -> PU a
xpElemNS ns px lp
                = xpElemQN $ mkQName px lp ns

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

xpElemWithAttrValue     :: String -> String -> String -> PU a -> PU a
xpElemWithAttrValue name an av pa
                = xpElem name $
                  xpAddFixedAttr an av $
                  pa

-- ------------------------------------------------------------

-- | Pickler for storing\/retreiving data into\/from an attribute value
--
-- The attribute is inserted in the surrounding element constructed by the 'xpElem' pickler

xpAttrQN        :: QName -> PU a -> PU a
xpAttrQN qn pa  = PU { appPickle   = ( \ a ->
                                       let st' = appPickle pa a emptySt in
                                       putAtt qn (contents st')
                                     )
                     , appUnPickle = upAttr
                     , theSchema   = scAttr (qualifiedName qn) (theSchema pa)
                     }
      where
      upAttr    = do a <- getAtt qn
                     l <- gets nesting
                     liftUnpickleVal $ unpickleElem' (xpCheckEmptyContents pa) l a

-- | convenient Pickler for xpAttrQN
--
-- > xpAttr n = xpAttrQN (mkName n)

xpAttr          :: String -> PU a -> PU a
xpAttr          = xpAttrQN . mkName

-- | convenient Pickler for xpAttrQN
--
-- > xpAttr ns px lp = xpAttrQN (mkQName px lp ns)

xpAttrNS        :: String -> String -> String -> PU a -> PU a
xpAttrNS ns px lp
                = xpAttrQN (mkQName px lp ns)

-- | A text attribute.
xpTextAttr      :: String -> PU String
xpTextAttr      = flip xpAttr xpText

-- | Add an optional attribute for an optional value (Maybe a).

xpAttrImplied   :: String -> PU a -> PU (Maybe a)
xpAttrImplied name pa
                = xpOption $ xpAttr name pa

xpAttrFixed     :: String -> String -> PU ()
xpAttrFixed name val
                = ( xpWrapEither ( \ v ->
                                   if v == val
                                   then Right ()
                                   else Left ( "xpAttrFixed: value "
                                               ++ show val
                                               ++ " expected, but got "
                                               ++ show v
                                             )
                                 , const val
                                 ) $
                    xpAttr name xpText
                  ) { theSchema   = scAttr name (scFixed val) }

-- | Add/Check an attribute with a fixed value.
--

xpAddFixedAttr  :: String -> String -> PU a -> PU a
xpAddFixedAttr name val
                = xpSeq' $ xpAttrFixed name val

-- | Add a namespace declaration.
--
-- When generating XML the namespace decl is added,
-- when reading a document, the unpickler checks
-- whether there is a namespace declaration for the given
-- namespace URI (2. arg)

xpAddNSDecl  :: String -> String -> PU a -> PU a
xpAddNSDecl name val
                = xpSeq' $ xpAttrNSDecl name' val
    where
      name'
          | null name = "xmlns"
          | otherwise = "xmlns:" ++ name

xpAttrNSDecl     :: String -> String -> PU ()
xpAttrNSDecl name ns
                 = PU { appPickle   = const $ putAtt (mkName name) [XN.mkText ns]
                      , appUnPickle = getNSAtt ns
                      , theSchema   = scAttr name (scFixed ns)
                      }

-- ------------------------------------------------------------

xpIgnoreCont    :: LA XmlTree XmlTree -> PU ()
xpIgnoreCont    = xpIgnoreInput $ \ mf s -> s {contents   = mf $ contents   s}

xpIgnoreAttr    :: LA XmlTree XmlTree -> PU ()
xpIgnoreAttr    = xpIgnoreInput $ \ mf s -> s {attributes = mf $ attributes s}

-- | When unpickling, filter the contents of the element currently processed,
-- before applying the pickler argument
--
-- Maybe useful to ignore some stuff in the input, or to do some cleanup before unpickling.

xpFilterCont    :: LA XmlTree XmlTree -> PU a -> PU a
xpFilterCont f  = xpSeq' $ xpIgnoreCont f

-- | Same as 'xpFilterCont' but for the  attribute list of the element currently processed.
--
-- Maybe useful to ignore some stuff in the input, e.g. class attributes, or to do some cleanup before unpickling.

xpFilterAttr    :: LA XmlTree XmlTree -> PU a -> PU a
xpFilterAttr f  = xpSeq' $ xpIgnoreAttr f

xpIgnoreInput   :: (([XmlTree] -> [XmlTree]) -> St -> St) -> LA XmlTree XmlTree -> PU ()
xpIgnoreInput m f
                =  PU { appPickle   = const id
                      , appUnPickle = do modify (m filterCont)
                                         return ()
                      , theSchema   = scNull
                      }
    where
      filterCont = runLA (unlistA >>> f)

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

{- begin embeded test cases

-- ------------------------------------------------------------
--
-- a somewhat complex data structure
-- for representing programs of a simple
-- imperative language

type Program    = Stmt

type StmtList   = [Stmt]

data Stmt
    = Assign  Ident  Expr
    | Stmts   StmtList
    | If      Expr  Stmt (Maybe Stmt)
    | While   Expr  Stmt
      deriving (Eq, Show)

type Ident      = String

data Expr
    = IntConst  Int
    | BoolConst Bool
    | Var       Ident
    | UnExpr    UnOp  Expr
    | BinExpr   Op    Expr  Expr
      deriving (Eq, Show)

data Op
    = Add | Sub | Mul | Div | Mod | Eq | Neq
      deriving (Eq, Ord, Enum, Show)

data UnOp
    = UPlus | UMinus | Neg
      deriving (Eq, Ord, Read, Show)

-- ------------------------------------------------------------
--
-- the pickler definition for the data types

-- the main pickler

xpProgram :: PU Program
xpProgram = xpElem "program" $
            xpAddNSDecl "" "program42" $
            xpickle

xpMissingRootElement    :: PU Program
xpMissingRootElement    = xpickle

instance XmlPickler UnOp where
    xpickle = xpPrim

instance XmlPickler Op where
    xpickle = xpWrap (toEnum, fromEnum) xpPrim

instance XmlPickler Expr where
    xpickle = xpAlt tag ps
        where
        tag (IntConst _    ) = 0
        tag (BoolConst _   ) = 1
        tag (Var _         ) = 2
        tag (UnExpr _ _    ) = 3
        tag (BinExpr _ _ _ ) = 4
        ps = [ xpWrap ( IntConst
                      , \ (IntConst i ) -> i
                      ) $
               ( xpElem "int"   $
                 xpAttr "value" $
                 xpickle
               )

             , xpWrap ( BoolConst
                      , \ (BoolConst b) -> b
                      ) $
               ( xpElem "bool"  $
                 xpAttr "value" $
                 xpWrap (toEnum, fromEnum) xpickle
               )

             , xpWrap ( Var
                      , \ (Var n)       -> n
                      ) $
               ( xpElem "var"   $
                 xpAttr "name"  $
                 xpText
               )

             , xpWrap ( uncurry UnExpr
                      , \ (UnExpr op e) -> (op, e)
                      ) $
               ( xpElem "unex" $
                 xpPair (xpAttr "op" xpickle)
                         xpickle
               )

             , xpWrap ( uncurry3 $ BinExpr
                      , \ (BinExpr op e1 e2) -> (op, e1, e2)
                      ) $
               ( xpElem "binex" $
                 xpTriple (xpAttr "op" xpickle)
                           xpickle
                           xpickle
               )
             ]

instance XmlPickler Stmt where
    xpickle = xpAlt tag ps
        where
        tag ( Assign _ _ ) = 0
        tag ( Stmts _ )    = 1
        tag ( If _ _ _ )   = 2
        tag ( While _ _ )  = 3
        ps = [ xpWrap ( uncurry Assign
                      , \ (Assign n v) -> (n, v)
                      ) $
               ( xpElem "assign" $
                 xpFilterCont (neg $ hasName "comment" <+> isText) $  -- test case test7: remove uninteresting stuff
                 xpPair (xpAttr "name" xpText)
                         xpickle
               )
             , xpWrap ( Stmts
                      , \ (Stmts sl) -> sl
                      ) $
               ( xpElem "block" $
                 xpList xpickle
               )
             , xpWrap ( uncurry3 If
                      , \ (If c t e) -> (c, t, e)
                      ) $
               ( xpElem "if" $
                 xpTriple xpickle
                          xpickle
                          xpickle
               )
             , xpWrap ( uncurry While
                      , \ (While c b) -> (c, b)
                      ) $
               ( xpElem "while" $
                 xpPair xpickle
                        xpickle
               )
             ]

-- ------------------------------------------------------------
--
-- example programs

progs   :: [Program]
progs   = [p0, p1, p2]

p0, p1, p2 :: Program

p0 = Stmts []           -- the empty program

p1 = Stmts
     [ Assign i ( UnExpr UMinus ( IntConst (-22) ) )
     , Assign j ( IntConst 20 )
     , While
       ( BinExpr Neq ( Var i ) ( IntConst 0 ) )
       ( Stmts
         [ Assign i ( BinExpr Sub ( Var i ) ( IntConst 1 ) )
         , Assign j ( BinExpr Add ( Var j ) ( IntConst 1 ) )
         , If ( IntConst 0 ) (Stmts []) Nothing
         ]
       )
     ]
    where
    i = "i"
    j = "j"

p2 = Stmts
     [ Assign x (IntConst 6)
     , Assign y (IntConst 7)
     , Assign p (IntConst 0)
     , While
       ( BinExpr Neq (Var x) (IntConst 0) )
       ( If ( BinExpr Neq ( BinExpr Mod (Var x) (IntConst 2) ) (IntConst 0) )
            ( Stmts
              [ Assign x ( BinExpr Sub (Var x) (IntConst 1) )
              , Assign p ( BinExpr Add (Var p) (Var y) )
              ]
            )
            ( Just ( Stmts
                     [ Assign x ( BinExpr Div (Var x) (IntConst 2) )
                     , Assign y ( BinExpr Mul (Var y) (IntConst 2) )
                     ]
                   )
            )
       )
     ]
    where
    x = "x"
    y = "y"
    p = "p"

-- ------------------------------------------------------------

test0 = putStrLn . head . runLA
        ( xshow (arr (pickleDoc xpProgram)
                 >>> getChildren
                )
        )

test0' f = runLA
        ( xshow (arr (pickleDoc xpProgram)
                 >>> getChildren
                )
          >>>
          root [] [xread]
          >>>
          f
        )

test1' f = runLA
        ( xshow (arr (pickleDoc xpProgram)
                 >>> getChildren
                )
          >>>
          root [] [xread]
          >>>
          f
          >>>
          arr (unpickleDoc' xpProgram)
        )

test1 = test0' (processTopDown (setQName (mkName "real") `X.when` hasName "int"))
test2 = test1' this
test3 = test1' (processTopDown (setQName (mkName "real") `X.when` hasName "int"))
test4 = test1' (processTopDown (setQName (mkName "xxx")  `X.when` hasName "program"))
test5 = test1' (processTopDown (setQName (mkName "xxx")  `X.when` hasName "assign"))
test6 = test1' (processTopDownWithAttrl  (txt "xxx"      `X.when` hasText (== "UMinus")))
test7 = test1' (processTopDown (insertComment            `X.when` hasName "assign"))
    where insertComment = replaceChildren (getChildren <+> eelem "comment" <+> txt "zzz")

-- ------------------------------------------------------------

-- end embeded test cases -}
