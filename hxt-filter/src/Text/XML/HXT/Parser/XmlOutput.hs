-- |
-- output functions
-- implemented as filer

module Text.XML.HXT.Parser.XmlOutput
    ( putXmlDoc
    , putXmlDocToFile
    , putXmlTree	-- for trace output
    , putXmlSource	--  "    "     "

    , hPutXmlDoc
    , hPutXmlTree
    , hPutXmlSource

    , traceF
    , traceMsg
    , traceTree
    , traceSource
    )

where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.DOM.XmlState

import Text.XML.HXT.DOM.EditFilters	( indentDoc
					, numberLinesInXmlDoc
					, treeRepOfXmlDoc
					, addHeadlineToXmlDoc
					)

import System.IO
import System.IO.Error

-- ------------------------------------------------------------

-- |
-- document output for standard output
--
-- see also : 'hPutXmlDoc'

putXmlDoc	:: XmlStateFilter a
putXmlDoc	= hPutXmlDoc stdout

-- |
-- document output for arbitrary files.
--
-- Result is the input document

hPutXmlDoc	:: Handle -> XmlStateFilter a
hPutXmlDoc handle t
    = do
      res <- io $ try (hPutStr handle content)
      case res of
        Left ioerr
	    -> ( issueFatal (show ioerr)
		 +++>>
		 thisM
	       ) t
	Right _
	    -> thisM t

    where
    content = xshow . getChildren $ t

-- |
-- document output on a given file name
--
-- Result is the input document
--
-- see also : 'hPutXmlDoc', 'putXmlDoc'

putXmlDocToFile	:: String -> XmlStateFilter a
putXmlDocToFile fn t
    = do
      res <- io $ try (openBinaryFile fn WriteMode)
      case res of
        Left ioerr
	    -> ( issueFatal (show ioerr)
		 +++>>
		 thisM
	       ) t
	Right h
	    -> do
	       t' <- hPutXmlDoc h t
	       _  <- io $ try (hClose h)
	       trace 2 ("document written to file: " ++ fn)
	       return t'

-- ------------------------------------------------------------

-- |
-- output of tree representation for trace

hPutXmlTree	:: Handle -> XmlStateFilter a
hPutXmlTree handle
    = performAction
      (\ n -> liftMf (treeRepOfXmlDoc
		     .>
		     addHeadlineToXmlDoc
		    )
              .>>
              hPutXmlDoc handle
              $ n
      )

putXmlTree	:: XmlStateFilter a
putXmlTree	= hPutXmlTree stdout

-- |
-- output of text representation for trace

hPutXmlSource	:: Handle -> XmlStateFilter a
hPutXmlSource handle
    = performAction
      (\ n -> liftMf ( ( rootTag
			[ sattr a_source "internal tree" ]
			[ this ]
			`whenNot` isRoot
		      )
		      .>
		      indentDoc
		      .>
		      numberLinesInXmlDoc
		      .>
		      addHeadlineToXmlDoc
		    )
              .>>
              hPutXmlDoc handle
              $ n
      )

putXmlSource	:: XmlStateFilter a
putXmlSource	= hPutXmlSource stdout

-- ------------------------------------------------------------

-- trace filter for inserting trace operations
-- into a filter sequence
--
--    * 1.parameter level : like in 'traceCmd'
--
--    - 2.parameter cmd : the output filter, e.g. putXmlTree or putXmlSource
--
--    - 3.parameter : the tree
--
--    - returns: the tree

traceF		:: Int -> XmlStateFilter a -> XmlStateFilter a
traceF level cmd
    = performAction (\ t -> traceCmd level (cmd t))

traceMsg	:: Int -> String -> XmlStateFilter a
traceMsg level msg
    = performAction (\ _ -> trace level msg)

traceTree	:: XmlStateFilter a
traceTree
    = traceF 4 (hPutXmlTree stderr)

traceSource	:: XmlStateFilter a
traceSource
    = traceF 3 (hPutXmlSource stderr)

-- ------------------------------------------------------------
