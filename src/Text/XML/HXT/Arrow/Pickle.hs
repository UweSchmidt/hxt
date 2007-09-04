-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.Pickle
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

module Text.XML.HXT.Arrow.Pickle
    ( xpickleDocument		 -- from this module Text.XML.HXT.Arrow.Pickle
    , xunpickleDocument
    , xpickleWriteDTD
    , xpickleDTD
    , checkPickler
    , xpickleVal
    , xunpickleVal
    , thePicklerDTD
    , a_addDTD

      -- from Text.XML.HXT.Arrow.Pickle.Xml
    , pickleDoc
    , unpickleDoc

    , PU(..)
    , XmlPickler

    , xp4Tuple
    , xp5Tuple
    , xpAddFixedAttr
    , xpAlt
    , xpAttr
    , xpAttrFixed
    , xpAttrImplied
    , xpChoice
    , xpCondSeq
    , xpElem
    , xpickle
    , xpLift
    , xpLiftMaybe
    , xpList
    , xpList1
    , xpOption
    , xpPair
    , xpPrim
    , xpRestrict
    , xpSeq
    , xpText
    , xpText0
    , xpTree
    , xpTriple
    , xpUnit
    , xpWrap
    , xpWrapMaybe
    , xpZero

      -- from Text.XML.HXT.Arrow.Pickle.Schema
    , Schema
    , Schemas
    , SchemaRestriction

    , emptyText
    , noneEmptyText
    , restrictRegEx
    , restrictDTDAttrType
    , restrictEnum
    , restrictOption
    )
where

import           Data.Maybe

import           Control.Arrow.ListArrows

import           Text.XML.HXT.Arrow.DOMInterface
import           Text.XML.HXT.Arrow.XmlArrow
import           Text.XML.HXT.Arrow.XmlIOStateArrow
import           Text.XML.HXT.Arrow.ReadDocument
import           Text.XML.HXT.Arrow.WriteDocument

import           Text.XML.HXT.Arrow.Pickle.Xml
import           Text.XML.HXT.Arrow.Pickle.Schema
import           Text.XML.HXT.Arrow.Pickle.DTD

-- ------------------------------------------------------------

-- the arrow interface for pickling and unpickling

-- | store an arbitray value in a persistent XML document
--
-- The pickler converts a value into an XML tree, this is written out with
-- 'Text.XML.HXT.Arrow.writeDocument'. The option list is passed to 'Text.XML.HXT.Arrow.writeDocument'
--
-- An options evaluated by this arrow is 'a_addDTD'.
-- If 'a_addDTD' is set ('v_1'), the pickler DTD is added as an inline DTD into the document.

xpickleDocument		:: PU a -> Attributes -> String -> IOStateArrow s a XmlTree
xpickleDocument xp al dest
    = xpickleVal xp
      >>>
      ( if lookup1 a_addDTD al == v_1
	then replaceChildren ( (constA undefined >>> xpickleDTD xp >>> getChildren)
			       <+>
			       getChildren
			     )
	else this
      )
      >>>
      writeDocument al dest

-- | Option for generating and adding DTD when document is pickled

a_addDTD	:: String
a_addDTD	= "addDTD"

-- | read an arbitray value from an XML document
--
-- The document is read with 'Text.XML.HXT.Arrow.readDocument'. Options are passed
-- to 'Text.XML.HXT.Arrow.readDocument'. The conversion from XmlTree is done with the
-- pickler.
--
-- @ xpickleDocument xp al dest >>> xunpickleDocument xp al' dest @ is the identity arrow
-- when applied with the appropriate options. When during pickling indentation is switched on,
-- the whitespace must be removed during unpickling.

xunpickleDocument	:: PU a -> Attributes -> String -> IOStateArrow s b a
xunpickleDocument xp al src
			= readDocument al src
			  >>>
			  xunpickleVal xp

-- | Write out the DTD generated out of a pickler. Calls 'xpicklerDTD'

xpickleWriteDTD		:: PU b -> Attributes -> String -> IOStateArrow s b XmlTree
xpickleWriteDTD	xp al dest
			= xpickleDTD xp
			  >>>
			  writeDocument al dest

-- | The arrow for generating the DTD out of a pickler
--
-- A DTD is generated from a pickler and check for consistency.
-- Errors concerning the DTD are issued.

xpickleDTD		:: PU b -> IOStateArrow s b XmlTree
xpickleDTD xp		= root [] [ constL (thePicklerDTD xp)
				    >>>
				    filterErrorMsg
				  ]

-- | An arrow for checking picklers
--
-- A value is transformed into an XML document by a given pickler,
-- the associated DTD is extracted from the pickler and checked,
-- the document including the DTD is tranlated into a string,
-- this string is read and validated against the included DTD,
-- and unpickled.
-- The last step is the equality with the input.
--
-- If the check succeeds, the arrow works like this, else it fails.

checkPickler		:: Eq a => PU a -> IOStateArrow s a a
checkPickler xp		= ( ( ( ( xpickleVal xp
				  >>>
				  replaceChildren ( (constA undefined >>> xpickleDTD xp >>> getChildren)
						    <+>
						    getChildren
						  )
				  >>>
				  writeDocumentToString []
				  >>>
				  readFromString [(a_validate, v_1)]
				  >>>
				  ( xunpickleVal xp
				    `orElse`
				    ( issueErr "unpickling the document failed"
				      >>>
				      none
				    )
				  )
				)
				&&& 
				this
			      )
			      >>> isA (uncurry (==))
			    )
			    `guards` this
			  )
			  `orElse` issueErr "pickle/unpickle combinators failed"

-- | The arrow version of the pickler function

xpickleVal		:: ArrowXml a => PU b -> a b XmlTree
xpickleVal xp		= arr (pickleDoc xp)

-- | The arrow version of the unpickler function

xunpickleVal		:: ArrowXml a => PU b -> a XmlTree b
xunpickleVal xp		= arrL (maybeToList . unpickleDoc xp)


-- | Compute the associated DTD of a pickler

thePicklerDTD		:: PU b -> XmlTrees
thePicklerDTD		= dtdDescrToXml . dtdDescr . theSchema

-- ------------------------------------------------------------


