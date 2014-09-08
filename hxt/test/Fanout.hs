import Text.XML.HXT.Core

fanout f g = (\a -> (a,a)) ^>> f *** g

main = runX $
    readString [] "<a><b attr=\"foo\"/><c attr=\"bar\"/></a>" />
    deep (
        hasName "b" >>> getAttrValue "attr"
    ) `fanout` deep (
        hasName "c" >>> getAttrValue "attr"
    ) >>>
    arrIO print
    
