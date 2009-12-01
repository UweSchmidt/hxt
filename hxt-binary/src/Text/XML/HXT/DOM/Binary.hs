{-# OPTIONS -fno-warn-orphans -fno-warn-unused-imports #-}

-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.DOM.Binary
   Copyright  : Copyright (C) 2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   De-/Serialisation for XmlTrees
-}

-- ------------------------------------------------------------

module Text.XML.HXT.DOM.Binary
where

import Data.Binary
import Data.List
import Data.Maybe
import Data.Tree.NTree.Binary

import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.QualifiedName

-- -----------------------------------------------------------------------------

instance Binary XNode where
    put	(XText s)		= put (0::Word8) >> put s
    put (XCharRef i)		= put (1::Word8) >> put i
    put (XEntityRef n)		= put (2::Word8) >> put n
    put (XCmt c)		= put (3::Word8) >> put c
    put (XCdata s)		= put (4::Word8) >> put s
    put (XPi qn ts)		= put (5::Word8) >> put qn >> put ts
    put (XTag qn cs)		= put (6::Word8) >> put qn >> put cs
    put (XDTD de al)		= put (7::Word8) >> put de >> put al
    put (XAttr qn)		= put (8::Word8) >> put qn
    put (XError n e)		= put (9::Word8) >> put n  >> put e

    get				= do
                                  tag <- getWord8
                                  case tag of
                                    0 -> get >>= return . XText
                                    1 -> get >>= return . XCharRef
                                    2 -> get >>= return . XEntityRef
                                    3 -> get >>= return . XCmt
                                    4 -> get >>= return . XCdata
                                    5 -> do
                                         qn <- get
                                         get >>= return . XPi qn
                                    6 -> do
                                         qn <- get
                                         get >>= return . XTag qn
                                    7 -> do
                                         de <- get
                                         get >>= return . XDTD de
                                    8 -> get >>= return . XAttr
                                    9 -> do
                                         n <- get
                                         get >>= return . XError n
                                    _ -> error "XNode.get: error while decoding XNode"

-- -----------------------------------------------------------------------------

dtdElems			:: [DTDElem]
dtdElems			= [ DOCTYPE
                                  , ELEMENT
                                  , CONTENT
                                  , ATTLIST
                                  , ENTITY
                                  , PENTITY
                                  , NOTATION
                                  , CONDSECT
                                  , NAME
                                  , PEREF
                                  ]

instance Binary DTDElem where
--  put de			= put ((toEnum . fromEnum $ de)::Word8)		-- DTDElem is not yet instance of Enum
    put	de			= let
                                  i = fromJust . elemIndex de $ dtdElems
                                  in
                                  put ((toEnum i)::Word8)

    get				= do
                                  tag <- getWord8
                                  return $ dtdElems !! (fromEnum tag)
--				  return . toEnum . fromEnum $ tag		-- see above

-- -----------------------------------------------------------------------------

instance Binary QName where
    put qn		= let
                          px = namePrefix   qn
                          lp = localPart    qn
                          ns = namespaceUri qn
                          in
                          put px >> put lp >> put ns
    get			= do
                          px <- get
                          lp <- get
                          ns <- get
                          return $ mkQName px lp ns

-- -----------------------------------------------------------------------------
