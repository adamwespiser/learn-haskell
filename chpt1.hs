import Data.Array

-- Find the smallest positive integer not in an array

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) y x = filter (\xx -> not $ elem xx x) y

-- solution 1
minFree ::(Num a, Eq a, Enum a) =>  [a] -> a
minFree x = head ([0..] \\ x)

-- solution 2
-- accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
-- 
-- elems :: Ix i => Array i e -> [e]
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checkList :: [Int] -> Array Int Bool
checkList xs = accumArray (||) False (0,n)
  (zip (filter (<= n) xs) (repeat True))
  where n = length xs
-- let x = [3,4,5,2,10,0]
-- search $ checkList x
-- returns 1
