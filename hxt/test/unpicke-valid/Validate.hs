import           Text.XML.HXT.Core

pickle_int :: PU (Maybe Int)
pickle_int = xpElem "int" $
               xpAttr "val" (xpOption xpInt)

main :: IO ()
main = do
  a <- runX ( xunpickleDocument pickle_int [withValidate no, withTrace 2] "int.xml")
  print a
  b <- runX ( xunpickleDocument pickle_int [withValidate yes, withTrace 2] "int.xml")
  print b
