-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.Namespace
   Copyright  : Copyright (C) 2005-2008 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   namespace specific arrows

-}

-- ------------------------------------------------------------

module Text.XML.HXT.Arrow.Namespace
    ( attachNsEnv
    , cleanupNamespaces
    , collectNamespaceDecl
    , collectPrefixUriPairs
    , isNamespaceDeclAttr
    , getNamespaceDecl
    , processWithNsEnv
    , processWithNsEnvWithoutAttrl
    , propagateNamespaces
    , uniqueNamespaces
    , uniqueNamespacesFromDeclAndQNames
    , validateNamespaces
    )
where

import Control.Arrow				-- arrow classes
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Control.Arrow.ListArrow

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow

import Data.Maybe		    ( isNothing
				    , fromJust
				    )
import Data.List		    ( nub )

-- ------------------------------------------------------------

-- | test whether an attribute node contains an XML Namespace declaration

isNamespaceDeclAttr	:: ArrowXml a => a XmlTree XmlTree
isNamespaceDeclAttr
    = fromLA $
      (getAttrName >>> isA isNameSpaceName) `guards` this

-- | get the namespace prefix and the namespace URI out of
-- an attribute tree with a namespace declaration (see 'isNamespaceDeclAttr')
-- for all other nodes this arrow fails

getNamespaceDecl	:: ArrowXml a => a XmlTree (String, String)
getNamespaceDecl
    = fromLA $
      isNamespaceDeclAttr
      >>>
      ( ( getAttrName
	  >>>
	  arr getNsPrefix
	)
	&&& xshow getChildren
      )
      where
      getNsPrefix = drop 6 . qualifiedName	-- drop "xmlns:"

-- ------------------------------------------------------------

-- | collect all namespace declarations contained in a document
--
-- apply 'getNamespaceDecl' to a whole XmlTree

collectNamespaceDecl	:: LA XmlTree (String, String)
collectNamespaceDecl    = multi getAttrl >>> getNamespaceDecl

-- | collect all (namePrefix, namespaceUri) pairs from a tree
--
-- all qualified names are inspected, whether a namespace uri is defined,
-- for these uris the prefix and uri is returned. This arrow is useful for
-- namespace cleanup, e.g. for documents generated with XSLT. It can be used
-- together with 'collectNamespaceDecl' to 'cleanupNamespaces'

collectPrefixUriPairs	:: LA XmlTree (String, String)
collectPrefixUriPairs
    = multi (isElem <+> getAttrl <+> isPi)
      >>>
      getQName
      >>>
      arrL getPrefixUri
    where
    getPrefixUri	:: QName -> [(String, String)]
    getPrefixUri n
	| null uri	= []
	| px == a_xmlns
	  ||
	  px == a_xml	= []				-- these ones are reserved an predefined
	| otherwise	= [(namePrefix n, uri)]
	where
	uri = namespaceUri n
	px  = namePrefix   n

-- ------------------------------------------------------------

-- | generate unique namespaces and add all namespace declarations to the root element
--
-- Calls 'cleanupNamespaces' with 'collectNamespaceDecl'

uniqueNamespaces		:: ArrowXml a => a XmlTree XmlTree
uniqueNamespaces
    = fromLA $
      cleanupNamespaces collectNamespaceDecl

-- | generate unique namespaces and add all namespace declarations for all prefix-uri pairs in all qualified names
--
-- useful for cleanup of namespaces in generated documents.
-- Calls 'cleanupNamespaces' with @ collectNamespaceDecl \<+> collectPrefixUriPairs @

uniqueNamespacesFromDeclAndQNames	:: ArrowXml a => a XmlTree XmlTree
uniqueNamespacesFromDeclAndQNames
    = fromLA $
      cleanupNamespaces (collectNamespaceDecl <+> collectPrefixUriPairs)

-- | does the real work for namespace cleanup.
--
-- The parameter is used for collecting namespace uris and prefixes from the input tree

cleanupNamespaces	:: LA XmlTree (String, String) -> LA XmlTree XmlTree
cleanupNamespaces collectNamespaces
    = renameNamespaces $< (listA collectNamespaces >>^ (toNsEnv >>> nub))
    where
    renameNamespaces :: NsEnv -> LA XmlTree XmlTree
    renameNamespaces env
	= processBottomUp
	  ( processAttrl
	    ( ( none `when` isNamespaceDeclAttr )	-- remove all namespace declarations
	      >>>
	      changeQName renamePrefix			-- update namespace prefix of attribute names, if namespace uri is set
	    )
	    >>>
	    changeQName renamePrefix			-- update namespace prefix of element names
	  )
	  >>>
	  attachEnv env1				-- all all namespaces as attributes to the root node attribute list
	where
	renamePrefix	:: QName -> QName
	renamePrefix n
	    | isNullXName uri	= n
	    | isNothing newPx	= n
	    | otherwise		= setNamePrefix' (fromJust newPx) n
	    where
	    uri   = namespaceUri' n
	    newPx = lookup uri revEnv1
	    
        revEnv1 = map (\ (x, y) -> (y, x)) env1

	env1 :: NsEnv
	env1 = newEnv [] uris

	uris :: [XName]
	uris = nub . map snd $ env

	genPrefixes :: [XName]
        genPrefixes = map (newXName . ("ns" ++) . show) [(0::Int)..]

        newEnv	:: NsEnv -> [XName] -> NsEnv
	newEnv env' []
	    = env'

	newEnv env' (uri:rest)
	    = newEnv env'' rest
	    where
	    env''    = (prefix, uri) : env'
	    prefix
		= head (filter notAlreadyUsed $ preferedPrefixes ++ genPrefixes)
	    preferedPrefixes
		= map fst . filter ((==uri).snd) $ env
	    notAlreadyUsed s
		= isNothing . lookup s $ env'

-- ------------------------------------------------------------

-- | auxiliary arrow for processing with a namespace environment
--
-- process a document tree with an arrow, containing always the
-- valid namespace environment as extra parameter.
-- The namespace environment is implemented as a 'Data.AssocList.AssocList'.
-- Processing of attributes can be controlled by a boolean parameter

processWithNsEnv1	:: ArrowXml a => Bool -> (NsEnv -> a XmlTree XmlTree) -> NsEnv -> a XmlTree XmlTree
processWithNsEnv1 withAttr f env
    = ifA isElem						-- the test is just an optimization
      ( processWithExtendedEnv $< arr (extendEnv env) )		-- only element nodes contain namespace declarations
      ( processWithExtendedEnv env )
    where
    processWithExtendedEnv env'
	= f env'						-- apply the env filter
	  >>>
	  ( ( if withAttr
	      then processAttrl (f env')			-- apply the env to all attributes
	      else this
	    )
	    >>>
	    processChildren (processWithNsEnv f env')		-- apply the env recursively to all children
	  )
          `when` isElem						-- attrl and children only need processing for elem nodes

    extendEnv	:: NsEnv -> XmlTree -> NsEnv
    extendEnv env' t'
	= addEntries (toNsEnv newDecls) env'
	where
	newDecls = runLA ( getAttrl >>> getNamespaceDecl ) t'

-- ------------------------------------------------------------

-- | process a document tree with an arrow, containing always the
-- valid namespace environment as extra parameter.
--
-- The namespace environment is implemented as a 'Data.AssocList.AssocList'

processWithNsEnv		:: ArrowXml a => (NsEnv -> a XmlTree XmlTree) -> NsEnv -> a XmlTree XmlTree
processWithNsEnv		= processWithNsEnv1 True

-- | process all element nodes of a document tree with an arrow, containing always the
-- valid namespace environment as extra parameter. Attribute lists are not processed.
--
-- See also: 'processWithNsEnv'

processWithNsEnvWithoutAttrl	:: ArrowXml a => (NsEnv -> a XmlTree XmlTree) -> NsEnv -> a XmlTree XmlTree
processWithNsEnvWithoutAttrl	= processWithNsEnv1 False

-- -----------------------------------------------------------------------------

-- | attach all valid namespace declarations to the attribute list of element nodes.
--
-- This arrow is useful for document processing, that requires access to all namespace
-- declarations at any element node, but which cannot be done with a simple 'processWithNsEnv'.

attachNsEnv	:: ArrowXml a => NsEnv -> a XmlTree XmlTree
attachNsEnv initialEnv
    = fromLA $ processWithNsEnvWithoutAttrl attachEnv initialEnv
    where

attachEnv	:: NsEnv -> LA XmlTree XmlTree
attachEnv env
    = ( processAttrl (none `when` isNamespaceDeclAttr)
	>>>
	addAttrl (catA nsAttrl)
      )
      `when` isElem
    where
    nsAttrl		:: [LA XmlTree XmlTree]
    nsAttrl		= map nsDeclToAttr env

    nsDeclToAttr	:: (XName, XName) -> LA XmlTree XmlTree
    nsDeclToAttr (n, uri)
	= mkAttr qn (txt (show uri))
	where
	qn :: QName
	qn | isNullXName n	= mkQName' nullXName  xmlnsXName xmlnsNamespaceXName
	   | otherwise		= mkQName' xmlnsXName n          xmlnsNamespaceXName

-- -----------------------------------------------------------------------------

-- |
-- propagate all namespace declarations \"xmlns:ns=...\" to all element and attribute nodes of a document.
--
-- This arrow does not check for illegal use of namespaces.
-- The real work is done by 'propagateNamespaceEnv'.
--
-- The arrow may be applied repeatedly if neccessary.

propagateNamespaces	:: ArrowXml a => a XmlTree XmlTree
propagateNamespaces	= fromLA $
			  propagateNamespaceEnv [ (xmlXName,   xmlNamespaceXName)
						, (xmlnsXName, xmlnsNamespaceXName)
						]

-- |
-- attaches the namespace info given by the namespace table
-- to a tag node and its attributes and children.

propagateNamespaceEnv	:: NsEnv -> LA XmlTree XmlTree
propagateNamespaceEnv
    = processWithNsEnv addNamespaceUri
    where
    addNamespaceUri	:: NsEnv -> LA XmlTree XmlTree
    addNamespaceUri env'
	= choiceA [ isElem :-> changeElemName (setNamespace env')
		  , isAttr :-> attachNamespaceUriToAttr env'
		  , isPi   :-> changePiName   (setNamespace env')
		  , this   :-> this
		  ]

    attachNamespaceUriToAttr	:: NsEnv -> LA XmlTree XmlTree
    attachNamespaceUriToAttr attrEnv
	= ( ( getQName >>> isA (not . null . namePrefix) )
	    `guards`
	    changeAttrName (setNamespace attrEnv)
	  )
          `orElse`
	  ( changeAttrName (const xmlnsQN)
	    `when`
	    hasName a_xmlns
	  )

-- -----------------------------------------------------------------------------

-- |
-- validate the namespace constraints in a whole tree.
--
-- Result is the list of errors concerning namespaces.
-- Predicates 'isWellformedQName', 'isWellformedQualifiedName', 'isDeclaredNamespace'
-- and 'isWellformedNSDecl' are applied to the appropriate elements and attributes.

validateNamespaces	:: ArrowXml a => a XmlTree XmlTree
validateNamespaces	= fromLA validateNamespaces1

validateNamespaces1	:: LA XmlTree XmlTree
validateNamespaces1
    = choiceA [ isRoot	:-> ( getChildren >>> validateNamespaces1 )		-- root is correct by definition
	      , this	:-> multi validate1Namespaces
	      ]

-- |
-- a single node for namespace constrains.

validate1Namespaces	:: LA XmlTree XmlTree
validate1Namespaces
    = choiceA
      [ isElem	:-> catA [ ( getQName >>> isA ( not . isWellformedQName )
			   )
			   `guards` nsError (\ n -> "element name " ++ show n ++ " is not a wellformed qualified name" )

			 , ( getQName >>> isA ( not . isDeclaredNamespace )
			   )
			   `guards` nsError (\ n -> "namespace for prefix in element name " ++ show n ++ " is undefined" )

		         , doubleOcc $< ( (getAttrl >>> getUniversalName) >>. doubles )

			 , getAttrl >>> validate1Namespaces
		         ]

      , isAttr	:-> catA [ ( getQName >>> isA ( not . isWellformedQName )
			   )
			   `guards` nsError (\ n -> "attribute name " ++ show n ++ " is not a wellformed qualified name" )

			 , ( getQName >>> isA ( not . isDeclaredNamespace )
			   )
			   `guards` nsError (\ n -> "namespace for prefix in attribute name " ++ show n ++ " is undefined" )

			 , ( hasNamePrefix a_xmlns >>> xshow getChildren >>> isA null
			   )
			   `guards` nsError (\ n -> "namespace value of namespace declaration for " ++ show n ++ " has no value" )

                         , ( getQName >>> isA (not . isWellformedNSDecl )
			   )
			   `guards`  nsError (\ n -> "illegal namespace declaration for name " ++ n ++ " starting with reserved prefix " ++ show "xml" )
			 ]

      , isDTD	:-> catA [ isDTDDoctype <+> isDTDAttlist <+> isDTDElement <+> isDTDName
		           >>>
			   getDTDAttrValue a_name
			   >>>
			   ( isA (not . isWellformedQualifiedName)
			     `guards`
			     nsErr (\ n -> "a DTD part contains a not wellformed qualified Name: " ++ show n)
			   )

			 , isDTDAttlist
			   >>>
			   getDTDAttrValue a_value
			   >>>
			   ( isA (not . isWellformedQualifiedName)
			     `guards`
			     nsErr (\ n -> "an ATTLIST declaration contains as attribute name a not wellformed qualified Name: " ++ show n)
			   )

                         , isDTDEntity <+> isDTDPEntity <+> isDTDNotation
			   >>>
			   getDTDAttrValue a_name
			   >>>
			   ( isA (not . isNCName)
                             `guards`
			     nsErr (\ n -> "an entity or notation declaration contains a not wellformed NCName: " ++ show n)
			   )
			 ]
      , isPi	:-> catA [ getName
		           >>>
			   ( isA (not . isNCName)
			     `guards`
			     nsErr (\ n -> "a PI contains a not wellformed NCName: " ++ show n)
			   )
			 ]
      ]
    where
    nsError	:: (String -> String) -> LA XmlTree XmlTree
    nsError msg
	= (getQName >>> arr show) >>> nsErr msg

    nsErr	:: (String -> String) -> LA String XmlTree
    nsErr msg	= arr msg >>> mkError c_err

    doubleOcc	:: String -> LA XmlTree XmlTree
    doubleOcc an
	= nsError (\ n -> "multiple occurences of universal name for attributes of tag " ++ show n ++ " : " ++ show an )

-- ------------------------------------------------------------
