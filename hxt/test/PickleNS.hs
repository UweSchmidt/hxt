import qualified Data.ByteString.Lazy.Char8 as BSLChar

import Control.Arrow.ArrowIf
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Edit
import Text.XML.HXT.Arrow.ParserInterface
import Text.XML.HXT.Arrow.Pickle.Xml

import Data.Tree.NTree.TypeDefs (NTree(..))

serializeXml :: Bool -> String -> XmlTree -> BSLChar.ByteString
serializeXml indent enc xml =
    let [str] = runLA (constA xml >>> encodeA) (error "serializeXml': undefined")
    in (BSLChar.pack (strip str))
    where rmpi = enc == unicodeString
          encodeA =
              (if indent then processChildren indentDoc else this)
              >>>
              escapeXmlDoc
              >>>
              encodeDocument' rmpi enc
              >>>
              -- this doesn't work?
              uniqueNamespacesFromDeclAndQNames
              >>>
              replaceChildren (xshow getChildren >>> arr encode >>> mkText)
              >>>
              xshow getChildren
          encode = id
          strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
          isSpace = (`elem` " \n\t")

parseXml :: Monad m => Bool -> Bool -> BSLChar.ByteString -> m XmlTree
parseXml rmpi rmspace bstr =
    let trees = runLA pipe (error "parseXml: undefined")
    in case trees of
         [] -> fail "empty result"
         (NTree _ ((NTree (XError _ msg) _) : _) : _) -> fail msg
         trees' -> return (last trees')
    where str = BSLChar.unpack bstr
          pipe = root [] []
                 >>> replaceChildren parse
                 >>> (if rmpi then removeXmlPi else this)
                 >>> (if rmspace then removeDocWhiteSpace else this)
          parse = constA ("urn:Data.ByteString", str)
                  >>> parseXmlDoc
                  >>> substXmlEntityRefs
                  >>> canonicalizeContents

removeXmlPi :: ArrowXml a => a (NTree XNode) (NTree XNode)
removeXmlPi = processTopDown (none `when` (isPi >>> hasName t_xml))

xpNs :: PU String
xpNs =
    xpElemQN (mkNsName "foo" "bar") $
    xpElemQN (mkNsName "foo" "baz") $
    xpText

xpPlain :: PU String
xpPlain =
    xpElem "foo" $
    xpElem "foo" $
    xpText

main =
    do putStrLn "WITHOUT NAMESPACES"
       run xpPlain src
       putStrLn "\nWITH NAMESPACES"
       run xpNs src
    where
      src = "hello"
      run xp src =
          do let srcTree = pickleDoc xp src
                 bs = serializeXml False iso8859_1 srcTree
             putStrLn "pickled document:"
             BSLChar.putStrLn bs
             let tgtTree = parseXml True True bs
             tgt <-
                 case tgtTree >>= unpickleDoc xp of
                  Nothing -> fail "unpickling failed"
                  Just x -> return x
             putStrLn "unpickled data:"
             print tgt
