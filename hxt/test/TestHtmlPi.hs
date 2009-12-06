module Main
where

import Data.Maybe 
import Text.XML.HXT.Arrow
import System.Environment
 
main :: IO ()
main
    = do
      [src, dst] <- getArgs
      runX ( readDocument [(a_parse_html, v_1)
			  ] src
             >>>
             processChildren processDocumentRootElement  -- (1)
	     >>>
	     writeDocument [(a_output_html, v_1)
			   ,(a_no_xml_pi, v_1)
			   ] dst
	   )
      return ()

processDocumentRootElement	:: IOSArrow XmlTree XmlTree
processDocumentRootElement
      = find_name

-- you currently don't use namespaces
-- so you don't have to work with qualified names

seam_id = (QN "seam"  "id" "" )

find_name
    = processTopDown
      ( substSpanElements
	`when`
	( isElem
	  >>> hasName "span"
	  >>> hasAttr "seam:id" )
      )
    where
    substSpanElements
	= getAttrValue "seam:id"
	  >>>
	  arr lookupContents
	  >>>
	  mkText

lookupContents	:: String -> String
lookupContents n
    = fromMaybe "" . lookup n $ content_handler_data
    where
    content_handler_data
	= [("name", "Uwe Schmidt"),
           ("date", "January 11, 2008")
	  ]
