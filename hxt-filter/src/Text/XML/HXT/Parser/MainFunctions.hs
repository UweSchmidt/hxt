-- |
-- Simple parse functions.
--
-- the main building blocks for an application.
-- this module exports complex filters and functions for
-- common tasks for input and parsing, output and option handling.
--

module Text.XML.HXT.Parser.MainFunctions
    ( getXmlDocument
    , putXmlDocument

    , parseDocument
    , writeDocument
    )
where

import Text.XML.HXT.DOM.XmlTree
import Text.XML.HXT.DOM.XmlState
import Text.XML.HXT.Parser.HtmlParser
import Text.XML.HXT.Parser.XmlInput
import Text.XML.HXT.Parser.XmlOutput
import Text.XML.HXT.Parser.DTDProcessing
import Text.XML.HXT.Validator.Validation
import Text.XML.HXT.DOM.EditFilters
import Text.XML.HXT.DOM.Namespace

import System.IO

-- ------------------------------------------------------------

-- |
-- convenient function for reading a XML document without
-- dealing with state monads, error messages collection and other details
--
-- getXmlDocument calls 'parseDocument' with the list of parsing options
-- and an url or filename as document source.
--
-- result is a triple
--
-- * the resulting document tree with a root node containing all
--   meta info about the document (options, status info, http header, ...)
--
-- - the list of errors and warnings
--
-- - the error level: one of 'c_ok', 'c_warn', 'c_err', 'c_fatal'
--
-- example for input (see also example in 'putXmlDocument' and example in 'writeDocument')
--
-- > main :: IO ()
-- > main
-- >   = do
-- >     (res, errs, rc) <- getXmlDocument [] "test.xml"
-- >     if rc >= c_err
-- >       then issueErrors errs
-- >       else processTree res
-- >
-- > issueErrors :: XmlTrees -> IO ()
-- >
-- > processTree :: XmlTree  -> IO ()
--
-- for options see 'parseDocument', 'a_collect_errors' is set implicitly

getXmlDocument	:: Attributes -> String -> IO (XmlTree, XmlTrees, Int)
getXmlDocument options url
    = do
      let options' = [ (a_collect_errors, v_1)		-- collect errors
		     , (a_issue_errors,   v_0)		-- but don't issue
		     ]					-- can be overwritten by supporting other values in options
	             ++
		     options
		     ++
		     [ (a_source, url)			-- set the source url
		     ]
      res <- run' $ parseDocument options' emptyRoot
      let root = head res
      let errs = tail res
      let elvl = intValueOf a_status root
      return (root, errs, elvl)


-- |
-- the inverse operation to 'getXmlDocument'
--
-- writes a complete document tree to a file, writing can be
-- controlled by options, the real work is done with filter 'writeDocument'.
-- useful options are the options of 'writeDocument'.
--
-- result is a pair: 1.part is a list of error messages, 2. part is the return code,
-- the status info of the write filter
--
-- this filter is useful, when processing XML in an arbitray context in the IO monad
--
-- an example main program for such an application is:
--
-- > main :: IO ()
-- > main
-- >   = do
-- >     (input, readErrs, rc) <- getXmlDocument [...] "test.xml"
-- >     if rc >= c_err
-- >       then issueErrors readErrs
-- >       else processTree input
-- >
-- > processTree :: XmlTree -> IO ()
-- > processTree t
-- >   = let res = computeNewTree input
-- >     in do
-- >        (writeErrs, rc2) <- putXmlDocument [...] "out.xml" res
-- >        if rc2 >= c_err
-- >          then issueErrors writeErrs
-- >          else return ()
-- >
-- > issueErrors :: XmlTrees -> IO ()
-- >
-- > computeNewTree :: XmlTree -> XmlTree


putXmlDocument	:: Attributes -> String -> XmlTree -> IO (XmlTrees, Int)
putXmlDocument options fileName t
    = do
      let options' = [ (a_collect_errors, v_1)		-- collect errors
		     , (a_issue_errors,   v_0)		-- but don't issue
		     ]					-- can be overwritten by supporting other values in options
	             ++
		     options
		     ++
		     [ (a_output_file, fileName)	-- set the source url
		     ]
      res <- run' $ writeDocument options' t
      let root = head res
      let errs = tail res
      let elvl = intValueOf a_status root
      return (errs, elvl)

-- ------------------------------------------------------------

-- |
-- the main parsing filter
--
-- this filter can be configured by an option list, a list of
-- option name, option value pairs.
-- the input tree must be a possibly empty document root tree.
-- all the options are stored as attributes in this root node to control processing.
--
-- available options:
--
-- * 'a_parse_html': use HTML parser, else use XML parser (default)
--
-- - 'a_validate' : validate document (default), else skip validation
--
-- - 'a_check_namespaces' : check namespaces, else skip namespace processing (default)
--
-- - 'a_canonicalize' : canonicalize document (default), else skip canonicalization
--
-- - 'a_preserve_comment' : preserve comments during canonicalization, else remove comments (default)
--
-- - 'a_remove_whitespace' : remove all whitespace, used for document indentation, else skip this step (default)
--
-- - 'a_indent' : indent document by inserting whitespace, else skip this step (default)
--
-- - 'a_issue_warnings' : issue warnings, when parsing HTML (default), else ignore HTML parser warnings
--
-- - 'a_issue_errors' : issue all error messages on stderr (default), or ignore all error messages
--
-- - 'a_collect_errors' : all error messages are collected during processing and appended to the result document
-- 			  for further processing within the calling modules
--
-- - 'a_trace' : trace level: values: 0 -4
--
-- - 'a_proxy' : proxy for http access, e.g. www-cache:3128
--
-- - 'a_use_curl' : for http access via external programm curl, default is native HTTP access
--
-- - 'a_options_curl' : more options for external program curl
--
-- - 'a_source' : the document source url
--
-- - 'a_encoding' : default document encoding ('utf8', 'isoLatin1', 'usAscii', ...)
--
-- examples:
--
-- > parseDocument [ (a_source,   "test.xml")
-- >               , (a_validate, "0")
-- >               , (a_encoding, isoLatin1)
-- >               ] emptyRoot
--
-- reads document \"test.xml\" without validation and default encoding 'isoLatin1'
--
-- > parseDocument [ (a_source,         "http://www.haskell.org/")
-- >               , (a_parse_html,     "1")
-- >               , (a_proxy,          "www-cache:3128")
-- >               , (a_curl,           "1")
-- >               , (a_issue_warnings, "0")
-- >               ] emptyRoot
--
-- reads Haskell homepage with HTML parser ignoring any warnings and with http access via external program curl and proxy \"www-cache\" at port 3128
--
-- > parseDocument [ (a_source,            "http://www.w3c.org/")
-- >               , (a_parse_html,        "0")                       -- default
-- >               , (a_validate,          "1")                       -- default
-- >               , (a_check_namespace,   "1")
-- >               , (a_remove_whitespace, "1")
-- >               , (a_trace,             "2")
-- >               ] emptyRoot
--
-- read w3c home page, validate and chech namespaces, remove whitespace between tags, trace activities with level 2
--
-- > parseDocument [ (a_source,   "test.xml")
-- >               , (a_validate,        "1")
-- >               , (a_check_namespace, "1")
-- >               , (a_collect_errors,  "1")
-- >               , (a_issue_errors,    "0")
-- >               ] emptyRoot
--
-- reads file \"test.xml\", validates it, checks namespaces, does not issue any erros
-- but collects errors and appends the list of errors to the single element list for the document.
-- this enables the calling application to define own error handlers.

parseDocument	:: Attributes -> XmlStateFilter state
parseDocument userOptions
    = processDocument userOptions defaultOptions
      ( traceMsg 1 "parseDocument: options added, start processing"
	.>>
	traceTree
	.>>
	getXmlContents					-- get the content as text
	.>>
	choiceM						-- select parser
	[ hasOption a_parse_html
		:-> parseHtmlDoc			-- parse everything as HTML
	, this
		:-> checkWellformedDoc			-- parse XML and process entities
	            .>>
                    ( getValidatedDoc			-- validate
		      `whenM`
		      hasOption a_validate
		    )
	]
	.>>
	( propagateAndValidateNamespaces			-- namespace processing
	  `whenM`
	  hasOption a_check_namespaces
	)
	.>>
	liftMf						-- canonicalization
	( choice
	  [ hasOption a_preserve_comment		-- don't remove comments (in XPath required)
 	  	:-> canonicalizeForXPath
	  , this
		:-> canonicalizeAllNodes		-- do normal canonicaliazion
	  ]
	  `when`
	  hasOption a_canonicalize			-- caconicalization can be switched off
	)
	.>>
	liftMf
	( removeDocWhiteSpace				-- remove all whitespace between tags
	  `when`
	  hasOption a_remove_whitespace
	)
	.>>
	traceMsg 1 "parseDocument: document processed"	-- trace output
	.>>
	traceSource
	.>>
	traceTree
      )
    where
    defaultOptions
	= [ ( a_parse_html,		v_0 )
	  , ( a_validate,		v_1 )
	  , ( a_issue_errors,		v_1 )
	  , ( a_issue_warnings,		v_1 )
	  , ( a_check_namespaces,	v_0 )
	  , ( a_canonicalize,		v_1 )
	  , ( a_preserve_comment,	v_0 )
	  , ( a_remove_whitespace,	v_0 )
	  ]

-- ------------------------------------------------------------

addOptions		:: Attributes -> XmlFilter
addOptions
    = seqF . map (\ (n,v) -> addAttr n v )

addDefaultOptions	:: Attributes -> XmlFilter
addDefaultOptions
    = seqF . map (\ (n,v) -> addAttr n v `whenNot` hasAttr n)

-- ------------------------------------------------------------

-- |
-- the main filter for writing documents
--
-- this filter can be configured by an option list like 'getXmlDocument'
--
-- available options are
--
-- * 'a_indent' : indent document for readability, (default: no indentation)
--
-- - 'a_remove_whitespace' : remove all redundant whitespace for shorten text (default: no removal)
--
-- - 'a_output_file' : destination file for document, default is \"-\" for stdout
--
-- - 'a_output_encoding' : encoding of document, default is 'a_encoding' or 'utf8'
--
-- - 'a_output_xml' : (default) issue XML: quote special XML chars \>,\<,\",\',&
--                    add XML processing instruction
--                    and encode document with respect to 'a_output_encoding',
--                    if explizitly switched of, the plain text is issued, this is useful
--                    for non XML output, e.g. generated Haskell code, LaTex, Java, ...
--
-- - 'a_show_tree' : show tree representation of document (for debugging)
--
-- - 'a_show_haskell' : show Haskell representaion of document (for debugging)
--
-- - 'a_issue_errors', 'a_collect_errors' : see 'parseDocument'
--
--  a typical main program running in the XmlState monad
--  has the following structure:
--
-- >
-- > main :: IO ()
-- > main
-- >     = do
-- >       argv <- getArgs                                              -- get the commandline arguments
-- >       (inp, outp, options) <- cmdlineOpts argv                     -- and evaluate them, return a key-value list
-- >                                                                    -- and input and output
-- >       res  <- run' $ application inp outp options $ emptyRoot      -- run the application
-- > 
-- >       exitWith (if null res
-- >                 then ExitFailure (-1)
-- >                 else exitSuccess
-- >                )
-- >
-- > application :: String -> String -> Attributes -> XmlStateFilter ()
-- > application inp outp al
-- >   = parseDocument (al ++ [(a_source, inp)])                        -- set options and source
-- >     .>>                                                            -- and parse document
-- >     processDocument                                                -- the hard work
-- >     .>>
-- >     writeDocument [(a_output_file, outp)]                          -- issue results
-- >     .>>
-- >     checkStatus                                                    -- check errors
-- >


writeDocument	:: Attributes -> XmlStateFilter state
writeDocument userOptions
    = processDocument userOptions defaultOptions
      ( traceMsg 1 "writeDocument: options added, start processing"
	.>>
	liftMf
	( choice
	  [ hasOption a_indent			:-> indentDoc			-- document indentation
	  , hasOption a_remove_whitespace	:-> removeDocWhiteSpace		-- remove all whitespace between tags
	  , this				:-> this
	  ]
	)
	.>>
	liftMf
	( choice
	  [ hasOption a_show_tree	:-> treeRepOfXmlDoc
	  , hasOption a_show_haskell	:-> haskellRepOfXmlDoc
	  , hasOption a_output_xml	:-> ( escapeXmlDoc		-- escape lt, gt, amp, quot, 
					      .>
					      addXmlPiToDoc		-- add <?xml ... > pi
					      .>
					      unparseXmlDoc		-- convert doc into text with respect to output encoding
					    )
	  , this			:-> this
	  ]
	)
	.>>
	writeXmlDoc
	.>>
	traceMsg 1 "writeDocument: finished"
      )
    where
    defaultOptions
	= [ ( a_output_file,		"-" )
	  , ( a_indent,			v_0 )
	  , ( a_remove_whitespace,	v_0 )
	  , ( a_output_xml,		v_1 )
	  , ( a_output_encoding,       utf8 )
	  , ( a_show_tree,		v_0 )
	  , ( a_show_haskell,		v_0 )
	  ]

writeXmlDoc	:: XmlStateFilter state
writeXmlDoc t'
    = put t'
    where
    put
	| null fn || fn == "-"	= putXmlDoc
	| otherwise		= putXmlDocToFile fn

    fn = xshow . getValue a_output_file $ t'


-- ------------------------------------------------------------
--

-- |
-- wrapper filter for running a monadic filter
-- controlled by common options
--
-- input tree must be a complete document
-- parameters and default parameters can be used to contol
-- the filer and common tasks like error message handling

processDocument	:: Attributes -> Attributes -> XmlStateFilter state -> XmlStateFilter state
processDocument userOptions defaultOptions processFilter
    = liftMf isRoot
      .>>
      liftMf (addOptions userOptions
	      .>
	      addOptions [(a_status, show c_ok)]
	     )
      .>>
      liftMf (addDefaultOptions defaultOptions)
      .>>
      setSystemParams							-- store options in system state
      .>>
      choiceM
      [ hasOption a_propagate_errors					-- error handling is set by calling environment
		:-> thisM

      , hasOption a_collect_errors
	.>
	hasOption a_issue_errors
		:-> performAction (\ _ -> setSysErrorHandler (errorMsgLogging.>> errorMsgToStderr)	)

      , hasOption a_collect_errors
		:-> performAction (\ _ -> setSysErrorHandler errorMsgLogging	)

      , hasOption a_issue_errors
		:-> performAction (\ _ -> setSysErrorHandler errorMsgToStderr	)

      , this
		:-> performAction (\ _ -> setSysErrorHandler noneM		)
      ]
      .>>
      processFilter
      .>>
      ( thisM
        +++>>
	( hasOption a_collect_errors
          `guardsM`
	  getErrorMsg
	)
      )

-- ------------------------------------------------------------

