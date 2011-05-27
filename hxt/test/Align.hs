module Align where

import Text.XML.HXT.Core

doc = concat $
      [ "<collection>"
      , "<photo url='somewhere' align='left' alt=''/>"
      , "<photo url='somewhere' align='' alt=''/>"
      , "</collection>"
      ]

main =
 runX ( constA doc
	>>>
	readFromString []
	>>>
	writeDocument [withIndent yes] ""
	>>>
	modifyAlt
	>>>
	writeDocument [withIndent yes] ""
      )

modifyAlt
    = processTopDownUntil
      (isPhotoWithNonEmptyAlign `guards` addClassAttr)

isPhotoWithNonEmptyAlign
    = hasName "photo"
      >>>
      hasAttrValue "align" (not .null)

addClassAttr
    = addAttr "class" "someclass"
      >>>
      removeAttr "align"

