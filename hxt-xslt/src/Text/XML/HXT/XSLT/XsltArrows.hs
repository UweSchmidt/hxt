-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.XSLT.XsltArrows
   Copyright  : Copyright (C) 2006 Tim Walkenhorst, Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: XsltArrows.hs,v 1.1 2006/11/11 15:36:05 hxml Exp $

   The HXT arrow interface for the XSLT module

   The application programming interface to the arrow modules of the Haskell XML Toolbox.
   This module exports all important arrows for input, output, parsing, validating and transforming XML.
   It also exports all basic datatypes and functions of the toolbox.

-}

-- ------------------------------------------------------------


module Text.XML.HXT.XSLT.XsltArrows
    ( xsltCompileStylesheet
    , xsltCompileStylesheetFromURI
    , xsltApplyStylesheet
    , xsltApplyStylesheetFromURI
    , CompiledStylesheet
    )
where

import Prelude hiding ( catch )

import Control.Exception
    ( SomeException
    , catch
    , evaluate )

import Control.Arrow.ListArrows

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlIOStateArrow
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.ReadDocument

import Text.XML.HXT.XSLT.Names

import Text.XML.HXT.XSLT.CompiledStylesheet
    ( CompiledStylesheet )

import Text.XML.HXT.XSLT.Compilation
    ( prepareXSLTDocument
    , assembleStylesheet
    )

import Text.XML.HXT.XSLT.Application
    ( applyStylesheet )

-- ------------------------------------------------------------

-- | arrow for applying a pure partial function, catch the error case and issue the error

arrWithCatch	:: (a -> b) -> IOSArrow a b
arrWithCatch f
    = arrIO (applyf f)
      >>>
      ( (applyA (arr issueErr) >>> none)
	|||
	this
      )

applyf	:: (a -> b) -> a -> IO (Either String b)
applyf f x
	= catch'  ( do
		    res <- evaluate ( f x )
		    return . Right $ res
		  )
	  (\ e -> return . Left . ("XSLT: " ++) . show $ e)
        where
        catch' :: IO a -> (SomeException -> IO a) -> IO a
	catch' = catch
	  
-- ------------------------------------------------------------

-- | lift prepareXSLTDocument

prepareXSLTDoc	:: IOSArrow XmlTree XmlTree
prepareXSLTDoc
    = ( arrWithCatch prepareXSLTDocument
	>>>
	traceDoc "prepareXSLTDocument"
      )
      `when`
      documentStatusOk

-- | read an XSLT stylesheet

readXSLTDoc	:: Attributes -> IOSArrow String XmlTree
readXSLTDoc options
    = readFromDocument (options ++ defaultOptions)
    where
    defaultOptions
	= [ (a_check_namespaces, v_1)
	  , (a_validate, v_0)
	  , (a_preserve_comment, v_0)
	  ]

-- | Normalize stylesheet, expand includes, select imports and assemble the rules

compileSSTWithIncludeStack :: [String] -> IOSArrow XmlTree CompiledStylesheet
compileSSTWithIncludeStack incStack
    = traceMsg 2 "compile stylesheet"
      >>>
      getChildren					-- remove document root
      >>>
      isElem						-- select XSLT root element
      >>>
      choiceA
      [ isXsltLREstylesheet				-- simplified syntax
	:-> ( xsltlre2stylesheet
	      >>>
	      assStylesheet []
	    )
      , isXsltStylesheetElem				-- xsl:stylesheet or xsl:transform
	:-> ( expStylesheet
	      $< ( listA ( getChildren			-- take contents and expand includes
			   >>>
			   expandIncludes incStack
			 )
		   >>>
		   partitionA isXsltImport		-- separate imports from normal rules
		 )
	    )
      , this
	:-> ( issueErr "XSLT: Either xsl:stylesheet/xsl:transform or simplified syntax expected"
	      >>>
	      none
	    )
      ]
      >>>
      traceValue 3 (("compiled stylesheet:\n" ++) . show)
    where
    assStylesheet imports				-- do the assembly, the compilation
	= arrWithCatch (flip assembleStylesheet $ imports)

    expStylesheet (imports, rest)
	= traceMsg 2 "expand stylesheet"
	  >>>
	  setChildren rest				-- remove import rules from stylesheet
	  >>>
	  assStylesheet $< listA ( constL imports	-- read the imports and assemble the stylesheet
				   >>>
				   getXsltAttrValue xsltHRef
				   >>>
				   compileSSTFromUriWithIncludeStack incStack
				 )

-- | read an include and check for recursive includes

readSSTWithIncludeStack	:: [String] -> IOSArrow String XmlTree
readSSTWithIncludeStack incStack
    = ifP (`elem` incStack)
      ( (issueErr $< arr recursiveInclude) >>> none )
      ( readXSLTDoc []
	>>>
	prepareXSLTDoc
      ) 
    where
    recursiveInclude uri
	= "XSLT error: "
	  ++ show uri ++ " is recursively imported/included." 
          ++ concatMap (("\n  imported/included from: " ++) . show) incStack

compileSSTFromUriWithIncludeStack	:: [String] -> IOSArrow String CompiledStylesheet
compileSSTFromUriWithIncludeStack incStack
    = comp $< this
    where
    comp uri
	= readSSTWithIncludeStack incStack
	  >>>
	  compileSSTWithIncludeStack (uri:incStack)

expandIncludes	:: [String] -> IOSArrow XmlTree XmlTree
expandIncludes incStack
    = isElem
      >>>
      ( ( expandInclude $< getXsltAttrValue xsltHRef )
        `when`
	isXsltInclude
      )
    where
    expandInclude href
	= ( constA href
	    >>>
	    readSSTWithIncludeStack incStack
	    >>>
	    getChildren
	    >>>
	    isElem
	    >>>
	    choiceA
	    [ isXsltLREstylesheet
	      :-> xsltlre2template

	    , isXsltStylesheetElem
	      :-> ( getChildren
		    >>>
		    expandIncludes (href:incStack)
		  )

	    , this
              :-> issueFatal ("XSLT error: Included file " ++ show href ++ " is not a stylesheet")
	    ]
	  )

isXsltElem		:: ArrowXml a => QName -> a XmlTree XmlTree
isXsltElem qn		= isElem >>> hasNameWith (equivQName qn)

isXsltAttr		:: ArrowXml a => QName -> a XmlTree XmlTree
isXsltAttr qn		= isAttr >>> hasNameWith (equivQName qn)

hasXsltAttr		:: ArrowXml a => QName -> a XmlTree XmlTree
hasXsltAttr qn		= ( getAttrl >>> isXsltAttr qn )
			  `guards`
			  this

isXsltInclude		:: ArrowXml a => a XmlTree XmlTree
isXsltInclude		= isXsltElem xsltInclude

isXsltImport		:: ArrowXml a => a XmlTree XmlTree
isXsltImport		= isXsltElem xsltImport

isXsltLREstylesheet	:: ArrowXml a => a XmlTree XmlTree
isXsltLREstylesheet	= hasXsltAttr xsltVersionLRE

isXsltStylesheetElem	:: ArrowXml a => a XmlTree XmlTree
isXsltStylesheetElem	=  ( isXsltElem xsltTransform
			     <+>
			     isXsltElem xsltStylesheet
			   )
                           >>>
			   hasXsltAttr xsltVersion

getXsltAttrValue	:: ArrowXml a => QName -> a XmlTree String
getXsltAttrValue qn	= getAttrl >>> isXsltAttr qn >>> xshow getChildren

xsltlre2template	:: ArrowXml a => a XmlTree XmlTree
xsltlre2template	= mkqelem xsltTemplate [sqattr xsltMatch "/"] [this]

xsltlre2stylesheet	:: ArrowXml a => a XmlTree XmlTree
xsltlre2stylesheet	= mkqelem xsltTransform [] [ this >>> xsltlre2template ]

--

checkApplySST	::  IOSArrow XmlTree XmlTree -> IOSArrow XmlTree XmlTree
checkApplySST appl
    = ( isRoot
	>>>
	replaceChildren appl
	>>>
	traceDoc "XSLT stylesheet applied"
	>>>
	setDocumentStatusFromSystemState "applying XSLT stylesheet"
      )
      `orElse`
      issueErr "XSLT: complete document with root node required for stylesheet application"

-- | Compile a document representing an XSLT stylesheet into an internal representation
--
-- The internal representation is an abstract syntax tree for the XSLT rules.
-- XSLT imports and includes are evaluated and the rules are normalized and prepared
-- for easy application.

xsltCompileStylesheet		:: IOSArrow XmlTree CompiledStylesheet
xsltCompileStylesheet
    = prepareXSLTDoc
      >>>
      compileSSTWithIncludeStack []

-- | A convinient function for combining reading a stylesheet and compilation.
--
-- Reading an XSLT stylesheet is always done without validation but with
-- namespace propagation. Comments are removed from the stylesheet.

xsltCompileStylesheetFromURI	:: IOSArrow String CompiledStylesheet
xsltCompileStylesheetFromURI	= compileSSTFromUriWithIncludeStack []

-- | apply a compiled XSLT stylesheet to a whole document tree
--
-- The compiled stylesheet must have been created with 'xsltCompileStylesheet'
-- or 'xsltCompileStylesheetFromURI'

xsltApplyStylesheet		:: CompiledStylesheet -> IOSArrow XmlTree XmlTree
xsltApplyStylesheet css
    = checkApplySST (arrWithCatch (applyStylesheet css) >>. concat)

-- | apply an XSLT stylesheet given by an URI to a whole document tree
--
-- The string parameter is the URI of the XSLT stylesheet.
-- In case of an error during stylesheet compilation or stylesheet application
-- all children of the root node are removed and
-- the error status is set in the attribute list of the root node of the input document.

xsltApplyStylesheetFromURI	:: String -> IOSArrow XmlTree XmlTree
xsltApplyStylesheetFromURI uri
    = xsltApplyStylesheet $< (constA uri >>> xsltCompileStylesheetFromURI)

{-
xsltApplyStylesheetWParams	:: Map ExName Expr -> CompiledStylesheet -> IOSArrow XmlTree XmlTree
xsltApplyStylesheetWParams wp css	= arrL (applyStylesheetWParams wp css)
-}
