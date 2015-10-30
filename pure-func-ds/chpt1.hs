-- p11
-- write a function that return suffiixes in decreasing order of size
-- f [1,2,3,4] -> [[1,2,3,4], [2,3,4], [3,4], [4], []]
--

import Data.List as DL
import Data.List.Split as DLS


suffs :: (Num a) =>  (Show a) => [a] -> [[a]]
suffs [] = [[]]
suffs (x:[]) = [x]:(suffs [])
suffs xx@(x:xs) = xx:(suffs xs)

-- maxEntry :: Tree a  -> a
-- maxEntry tree = max (map (\(x,y,z) -> y )(displayTree  tree))


data Tree a = EmptyNode | FilledNode a (Tree a) (Tree a) deriving (Show,Eq)


buildTree' ::  (Ord a) =>  [a] -> Tree a
buildTree' [] = EmptyNode
buildTree' (x:[]) = FilledNode x (EmptyNode) (EmptyNode)
buildTree' xx@(x:xs) = 
  let mid = xx !! (quot (length xx) 2)
      low = filter (< mid) xx
      high = filter (> mid) xx
  in  FilledNode mid (buildTree low) (buildTree high)

buildTree :: (Ord a) => [a] -> Tree a
buildTree x = buildTree' $ DL.sort x

-- 83 mergePlaces :: [a] -> [a] -> [a]
--   84 mergePlaces (x:xs) (y:ys)  = x:y:(mergePlaces xs ys)
-- 	 85 mergePlaces []     y      = y
	--   86 mergePlaces x      []      = x

joinTo :: [a] -> [a] -> [a]
joinTo []        []        = []
joinTo []        yy        = yy
joinTo xx        []        = xx
joinTo (x:xs) (y:ys)       = x:y:(joinTo xs ys)


expNum :: (Num a, Eq a) => a -> a -> a
expNum base 0 = 1
expNum base 1 = base
expNum base pow = base * (expNum base (pow - 1) )

updateOffsetDepth :: (Num a, Eq a) => (a -> a -> a) -> a -> a -> a
updateOffsetDepth fn pow offset = fn offset (expNum 2 pow) 


--
displayTree' ::  (Num b, Eq b) => Tree a -> b -> String -> [(Maybe a, b,String)]
displayTree' EmptyNode depth offset = [(Nothing, depth, offset)]
displayTree' (FilledNode val tLeft tRight) depth offset = 
   (Just  val, depth, offset):( joinTo 
	                                (displayTree' tLeft (depth + 1) loffset) 
                                  (displayTree' tRight(depth + 1) roffset)
														  )
  where loffset = offset ++ "l:"
        roffset = offset ++ "r:"

displayTree ::  (Num b, Eq b) => Tree a -> [(Maybe a, b,String)]
displayTree (FilledNode val tLeft tRight)  = displayTree' (FilledNode val tLeft tRight) 0 "0:"
displayTree EmptyNode  = displayTree' EmptyNode 1 "R:"


-- 1 item -> center
-- 2 items -> 1/3, 1/3
-- n items -> 1 / (n + 1)
-- getWidthOnRange :: ( Integral a) => (Show b, Num b) => a -> [b] -> String

{-
valFromTuplet :: (Ord a, Enum a, Num a)=> (Num b, Eq b) => [(Maybe a, b, String)] -> Maybe a
valFromTuplet tup = x
valFromTuplet (Nothing _  _)  = Nothing
-}

convValues :: Show b => Maybe b -> String
convValues maybeValue = case maybeValue of
                              Nothing -> "*"
                              Just x -> (show x)



getWidthOnRange :: [[Char]] -> [Char]
getWidthOnRange items  = 
  let 
    numitems = fromIntegral $ length items 
    openlen = (-) 68 numitems
    ngaps = (+) numitems  1
    gapsize = fromIntegral $ quot openlen ngaps
    trgapsize = fromIntegral $ truncate gapsize
    extras   = openlen - (trgapsize * ngaps) 
    unicov = ngaps * trgapsize + numitems
    extraspace =   80 - (trgapsize * ngaps + numitems)
    nonextra = ngaps - extraspace
    gaps_extra = replicate (extraspace :: Int) (replicate (trgapsize + 1 ) " ")
    gaps_reg = (replicate ((ngaps - extraspace) :: Int )(replicate (trgapsize) " "))
    items_str =  fmap (:[]) items ++ [[""]]
    -- items_str =  (fmap (:[]) items) ++ [[""]]
    -- items_str =  (map (\z -> [(show z)]) items)  ++ [[""]]
    zipp = zipWith (++) (gaps_reg ++ gaps_extra)  items_str
  in concat (concat zipp)
{-
-}

getLevelFromTree :: (Ord a1, Num a1, Num a, Eq a, Enum a1, Enum a) =>Tree a1 -> [a] ->  [[(Maybe a1, a, String)]]
getLevelFromTree tree levels = [filter (\(x,y,z) -> y == level) (displayTree $ tree ) | level <- levels ]



getDeepestLevelIndex :: (Num b,Ord b,Eq b) => Tree a -> b
getDeepestLevelIndex tree = maximum $ map (\(x,y,z)->y) (displayTree $ tree )

getLevelIdx :: (Num b,Ord b,Eq b, Enum b) => Tree a -> Bool -> [b]
getLevelIdx tree True = 
  let 
    deepLevel = maximum $ map (\(x,y,z)->y) (displayTree $ tree )
  in [0..deepLevel]


t1 :: (Ord a, Enum a, Num a) => Tree a
t1 = buildTree [0..22]
t1disp :: (Ord a, Enum a, Num a)=> (Num b, Eq b) => [(Maybe a, b, String)]
t1disp =  displayTree t1
-- t1_1 :: [(Maybe a1, a String)]
t1_1 = getLevelFromTree t1 [0..(getDeepestLevelIndex t1)] !! 1
t1_a = getLevelFromTree t1 [0..(getDeepestLevelIndex t1)] 
t1_0 = getLevelIdx t1 True
t1_vals = [map (\(maybeValue,y,z)-> (convValues maybeValue)) (t1_a !! x) | x <- [0..(getDeepestLevelIndex t1)] ]
-- t1_tree = map (getWidthOnRange .t1_vals
-- do { putStrLn $ concat $ intersperse "\n" (map getWidthOnRange  t1_vals)}
