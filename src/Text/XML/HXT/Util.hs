module Text.XML.HXT.Util (
   swap,
   partitionEither,
   toMaybe,
  ) where


swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

partitionEither :: [Either a b] -> ([a], [b])
partitionEither =
   foldr (\x ~(ls,rs) -> either (\l -> (l:ls,rs)) (\r -> (ls,r:rs)) x) ([],[])

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x
