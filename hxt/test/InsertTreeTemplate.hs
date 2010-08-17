module InsertTreeTemplate
where

import Text.XML.HXT.Arrow

insertTreeTemplateTest3	:: ArrowXml a => a b XmlTree
insertTreeTemplateTest3
    = doc
      >>>
      fillTemplate template [("title", txt "The real title"), ("body", none)]
    where
    doc								-- the input data
 	= constA "<x><y>The Title</y><z>The content</z></x>"
 	  >>> xread
    template								-- the output template with 2 holes: xxx and yyy
 	= constA "<html><head><title id='title'></title></head><body><h1 id='body'></h1></body></html>"
 	  >>> xread

id2ifThen :: (ArrowXml a) => (String, a XmlTree XmlTree) -> IfThen (a XmlTree XmlTree) (a XmlTree XmlTree)
id2ifThen (value, subst) = hasAttrValue "id" (== value) :-> (subst `orElse` txt "") 

fillTemplate :: (ArrowXml a) => a XmlTree XmlTree -> [(String, a XmlTree XmlTree)] -> a XmlTree XmlTree
fillTemplate template ids = insertTreeTemplate template (map id2ifThen ids)

t3 = runX (xshow insertTreeTemplateTest3)

-- ----------------------------------------

insertTreeTemplateTest1	:: ArrowXml a => a b XmlTree
insertTreeTemplateTest1
    = doc
      >>>
      insertTreeTemplate template pattern
    where
    doc								-- the input data
 	= constA "<x><y>The Title</y><z>The content</z></x>"
 	  >>> xread
    template								-- the output template with 2 holes: xxx and yyy
 	= constA "<html><head><title>xxx</title></head><body><h1>yyy</h1></body></html>"
 	  >>> xread
    pattern
 	= [ hasText (== "xxx")						-- fill the xxx hole with the input contents from element "x/y"
 	    :-> ( getChildren >>> hasName "y" >>> deep isText )
 
 	  , hasText (== "yyy")						-- fill the yyy hole with the input contents from element "x/z"
 	    :-> ( getChildren >>> hasName "z" >>> getChildren )
 	  ]

t1 = runX (xshow insertTreeTemplateTest1)

insertTreeTemplateTest2	:: ArrowXml a => a b XmlTree
insertTreeTemplateTest2
    = doc
      >>>
      insertTreeTemplate template pattern
    where
    doc								-- the input data
 	= constA "<x><y>The Title</y><z>The content</z></x>"
 	  >>> xread
    template								-- the output template with 2 holes: xxx and yyy
 	= constA "<html><head><title>xxx</title></head><body><h1>yyy</h1></body></html>"
 	  >>> xread
    pattern
 	= [ hasText (== "xxx")						-- fill the xxx hole with the input contents from element "x/y"
 	    :-> ( getChildren >>> hasName "y" >>> deep isText )
 
 	  , hasText (== "yyy")						-- fill the yyy hole with the input contents from element "x/zz"
	                                                                -- which does not exist, and so an empty text node is inserted
                                                                        -- as a dummy result
 	    :-> ( (getChildren >>> hasName "zz" >>> getChildren) `orElse` (txt "") )
 	  ]
t2 = runX (xshow insertTreeTemplateTest2)

