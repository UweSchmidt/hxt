module Books
where

-- used hxt-xpath version: 9.1.2
 
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
 
parseHtml :: FilePath -> IOStateArrow s b XmlTree
parseHtml path = readDocument [withParseHTML yes, withWarnings no] path
 
getFirst :: FilePath -> IO [String]
getFirst src
    = do runX (parseHtml src
               >>>
               ( getXPathTrees "/descendant-or-self::node()/bookstore/book/name/text()"
                 `when`
                 isElem
               ) >>>
               getText
              )


-- expected result: ["Learn You a Haskell for a Great Good!","JavaScript: The Good Parts"]
-- actual result:
-- *Main> getFirst "test.xml"
-- ["Learn You a Haskell for a Great Good!"]
 
getLast :: FilePath -> IO [String]
getLast src
    = do runX (parseHtml src
               >>>
               ( getXPathTrees "//bookstore/book[last()]/name/text()"
                 `when`
                 isElem
               )
               >>>
               getText
              )

-- expected result: ["Programming in Haskell","JavaScript: The Definitive Guide"]
-- actual result:
-- *Main> getLast "test2.xml"
-- ["JavaScript: The Definitive Guide"]

getFirst' :: FilePath -> IO [String]
getFirst' src
    = do runX (parseHtml src
               >>>
               ( getXPathTrees "(//bookstore/book)[1]/name/text()"
                 `when`
                 isElem
               ) >>>
               getText
              )

