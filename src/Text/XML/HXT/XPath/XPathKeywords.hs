-- |
-- $Id: XPathKeywords.hs,v 1.1 2004/09/02 19:12:05 hxml Exp $
--
-- the XPath keywords
--


module Text.XML.HXT.XPath.XPathKeywords
where

-- ------------------------------------------------------------
--
-- string constants for representing XPath keywords and axis

a_ancestor,			-- axisNames
 a_ancestor_or_self,
 a_attribute,
 a_child,
 a_descendant,
 a_descendant_or_self,
 a_following,
 a_following_sibling,
 a_namespace,
 a_parent,
 a_preceding,
 a_preceding_sibling,
 a_self				:: String


n_comment,			-- nodeTypes
 n_text,
 n_processing_instruction,
 n_node				:: String

-- ------------------------------------------------------------

a_ancestor			= "ancestor"  
a_ancestor_or_self		= "ancestor-or-self"  
a_attribute 			= "attribute"  
a_child				= "child"  
a_descendant			= "descendant"  
a_descendant_or_self		= "descendant-or-self"  
a_following			= "following"  
a_following_sibling		= "following-sibling"  
a_namespace			= "namespace"  
a_parent			= "parent"  
a_preceding			= "preceding"  
a_preceding_sibling		= "preceding-sibling"  
a_self				= "self" 


n_comment 			= "comment"
n_text 				= "text"
n_processing_instruction 	= "processing-instruction"
n_node 				= "node"

-- ------------------------------------------------------------
