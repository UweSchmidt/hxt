-- |
-- Namespace filter
--
-- Namespaces are processed with two main filter, 'propagateNamespaces'
-- and 'validateNamespaces'.
--
-- 'propagateNamespaces' takes a XML tree and
-- attaches extra namespace info at every tag name and attribute name.
-- So after processing a tree with 'propagateNamespaces', the access functions
-- "namespaceOf" and "universalNameOf" deliver the associated namespace (or \"\")
-- for tag names and attribute names.
--
-- 'validateNamespaces' checks whether names are wellformed related to the XML namespace definition.
-- whether a legal namespace is declared for all prefixes, and whether attribute names are unique
-- related to universal names.

module Text.XML.HXT.DOM.NamespaceFilter
    ( module Text.XML.HXT.DOM.NamespaceFilter
    , module Text.XML.HXT.DOM.NamespacePredicates
    )
where

import Text.XML.HXT.DOM.XmlTree

import Text.XML.HXT.DOM.NamespacePredicates

import Text.XML.HXT.DOM.Util
    ( doubles )

-- -----------------------------------------------------------------------------
--

-- |
-- Type for the namespace association list, used when propagating namespaces by
-- modifying the 'QName' values in a tree

type NamespaceTable = NsEnv

-- -----------------------------------------------------------------------------

-- |
-- propagate all namespace declarations \"xmlns:ns=...\" to all
-- tag and attribute nodes of a document.
--
-- This filter does not check for illegal use of namespaces.
-- The real work is done by 'propagateNamespaceEnv'.
--
-- The filter may be applied repeatedly if neccessary.

propagateNamespaces	:: XmlFilter
propagateNamespaces	= propagateNamespaceEnv [ (a_xml, xmlNamespace), (a_xmlns, xmlnsNamespace) ]

-- |
-- attaches the namespace info given by the namespace table
-- to a tag node and its attributes and children.

propagateNamespaceEnv	:: NamespaceTable -> XmlFilter
propagateNamespaceEnv env n
    = ( ( processAttr (attachNamespaceUrisToAttr newEnv)
	  .>
	  processChildren (propagateNamespaceEnv newEnv)
	  .>
	  modifyQName (setNamespace newEnv)
	)
	`when`
	isXTag
      )
      $ n
    where
    nsAttrs	= getAttrl				-- scan all attributes
		  .>
		  isOfAttr ( (\ (px, lp)
			     -> (px == a_xmlns		-- check for prefix or whole name is "xmlns"
				 &&
				 lp /= ":"		-- check for none empty local part, "xmlns:" is not a legal name
				)
			     )
			     . span (/= ':')		-- break the name into a pair ("prefix", ":localPart")
			     . qualifiedName		-- select attribute name
			   )
		  $ n
    nsDecl	= zip (map (drop 1			-- drop the ":", empty local part represents default name space
			    . snd			-- take the local part with leading ":"
			    . span (/= ':')		-- break it like above
			    . nameOf			-- select attribute name
			   ) nsAttrs)
		      (map (xshow
			    .
			    getChildren
			   ) nsAttrs)
    newEnv	= addEntries nsDecl env

    attachNamespaceUrisToAttr	:: NamespaceTable -> XmlFilter
    attachNamespaceUrisToAttr attrEnv
	= ( isOfAttr ( (\ (px, lp)
			-> ( (not . null . drop 1) lp	-- prefix and local part must not be empty
			     &&
			     (not . null) px
			   )
		       )
		       . span (/= ':')			-- break the name into a pair ("prefix", ":localPart")
		       . qualifiedName			-- select attribute name
		     )
	    `guards`
	    modifyQName (setNamespace attrEnv)
	  )
          `orElse`
	  ( modifyQName (const xmlnsQN)			-- "xmlns" ist the only attr name without prefix but with an associated namespace URI
	    `when`
	    isAttr a_xmlns
	  )

-- -----------------------------------------------------------------------------

-- |
-- validate the namespace constraints in a whole tree.
-- result is the list of errors concerning namespaces.
-- Work is done by applying 'validate1Namespaces' to all nodes.
-- Predicates 'isWellformedQName', 'isWellformedQualifiedName', 'isDeclaredNamespace'
-- and 'isWellformedNSDecl' are applied to the appropriate tags and attributes.

validateNamespaces	:: XmlFilter
validateNamespaces
    = choice [ isRoot
               :-> getChildren .> validateNamespaces		-- root is correct by definition
	     , this
	       :-> multi validate1Namespaces
	     ]

-- |
-- a single node for namespace constrains.

validate1Namespaces	:: XmlFilter
validate1Namespaces
    = choice [ isXTag
	       :->
	       cat [ isOfTag ( not . isWellformedQName )
		     `guards`
		     (\ n -> err ("tag name " ++ show (nameOf n) ++ " is not a wellformed qualified name") n )

		   , isOfTag ( not . isDeclaredNamespace )
		     `guards`
		     (\ n -> err ("namespace for prefix in tag name " ++ show (nameOf n) ++ " is undefined") n )

                   , (\ n -> ( let
		               dbls = doubles ((map universalNameOf . getAttrl) n)
		               in
		               if null dbls
		               then none
                               else err ( "multiple occurences of universal names for attributes of tag "
					  ++ show (nameOf n)
					  ++ " : " ++ foldr1 (\ x y -> x ++ ", " ++ y) (map show dbls)
					)
			     ) $ n
		     )

		   , getAttrl .> validate1Namespaces
		   ]

	     , isXAttr
	       :->
	       cat [ isOfAttr ( not . isWellformedQName )
		     `guards`
		     (\ n -> err ("attribute name " ++ show (nameOf n) ++ " is not a wellformed qualified name") n )

		   , isOfAttr ( not . isDeclaredNamespace )
	             `guards`
		     (\ n -> err ("namespace for prefix in attribute name " ++ show (nameOf n) ++ " is undefined") n )

                   , ( hasPrefix a_xmlns .> neg (xmlTreesToText . getChildren) )
		     `guards`
		     (\ n -> err ("namespace value of namespace declaration for " ++ show (nameOf n) ++ " has no value") n )

                   , isOfAttr ( not . isWellformedNSDecl )
		     `guards`
		     (\ n -> err ("illegal namespace declaration with reserved prefix " ++ show (localPartOf n) ++ " starting with \"xml\"") n )
	           ]

             , isXDTD
	       :->
	       cat [ ( ( isDoctype +++ isAttlist +++ isElement +++ isDTDName )
		       .>
		       isOf ( not . isWellformedQualifiedName . valueOfDTD a_name )
		     )
		     `guards`
                     (\ n -> err ("a DTD part contains a not wellformed qualified Name: " ++ show (valueOfDTD a_name n)) n )

		   , ( isAttlist
		       .>
		       isOf ( not . isWellformedQualifiedName . valueOfDTD a_value )
		     )
		     `guards`
                     (\ n -> err ("an ATTLIST declaration contains as attribute name a not wellformed qualified Name: " ++ show (valueOfDTD a_value n)) n )

                   , ( ( isEntity +++ isParameterEntity +++ isNotation )
		       .>
		       isOf ( not . isNCName . valueOfDTD a_name )
		     )
                     `guards`
                     (\ n -> err ("an entity or notation declaration contains a not wellformed NCName: " ++ show (valueOfDTD a_name n)) n )
		   ]
             , isXPi
	       :->
	       ( isOf ( not . isNCName . nameOf )
	         `guards`
                 (\ n -> err ("a PI contains a not wellformed NCName: " ++ show (nameOf n)) n )
	       )
	     ]

-- -----------------------------------------------------------------------------

isNamespaceDecl	:: XmlFilter
isNamespaceDecl
    = isOfAttr xmlnsName
      where
      xmlnsName	:: AttrName -> Bool
      xmlnsName a
	  = px == a_xmlns
	    &&
	    ( null ln || head ln == ':')
	  where
	  (px, ln) = splitAt 5 . qualifiedName $ a

-- -----------------------------------------------------------------------------
