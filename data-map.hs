import qualified Data.Map as Map

{- 
  NOTES on Data.Map
https://hackage.haskell.org/package/containers-0.5.7.1/docs/src/Data.Map.Base.html#lookupIndex
-}
--data thKey :: (k -> a -> b) -> Map k a -> Map k b Source
--O(n). Map a function over all values in the map.
f key x = (show key) ++ ":" ++ x
Map.mapWithKey f (Map.fromList [(5,"a"), (3,"b")]) == Map.fromList [(3, "3:b"), (5, "5:a")]
--Maybe a = Just a | Nothing deriving (Show)


--mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a Source
--O(n*log n). mapKeys f s is the map obtained by applying f to each key of s.
--The size of the result may be smaller if f maps two or more distinct keys to the same new key. In this case the value at the greatest of the original keys is retained.
Map.mapKeys (+ 1) (Map.fromList [(5,"a"), (3,"b")])                        == Map.fromList [(4, "b"), (6, "a")]
Map.mapKeys (\ _ -> 1) (Map.fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == Map.singleton 1 "c"
Map.mapKeys (\ _ -> 3) (Map.fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == Map.singleton 3 "c"

--elems :: Map k a -> [a] Source
--O(n). Return all elements of the map in the ascending order of their keys. Subject to list fusion.
elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
elems empty == []

--keys :: Map k a -> [k] Source
--O(n). Return all keys of the map in ascending order. Subject to list fusion.
keys (fromList [(5,"a"), (3,"b")]) == [3,5]
keys empty == []


