module TextExtract
where

import Text.XML.HXT.Core

main = do
  rs <- runX (readDocument [] "table.xml" >>> extractCell "x")
  print rs


extractCell :: String -> IOSArrow XmlTree String
extractCell s
    = this
      />
      hasElemName "table"
      />
      hasElemName "tr"
      >>>
      listA (getChildren >>> hasElemName "td")
      >>>
      ( ((take 1 ^>> unlistA) /> hasText (== s))
	`guards`
        (((drop 1 >>> take 1) ^>> unlistA) /> getText)
      )
    where
    hasElemName n
	= isElem >>> hasName n
{-
    has1st s
	= ( getChildren >>> hasElemName "td" ) >>. take 1
	  />
	  hasText (== s)
    get2nd 
	= ( getChildren >>> hasElemName "td" ) >>. (take 1 . drop 1)
	  />
	  getText
-}
