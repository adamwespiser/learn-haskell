--  x[j] is a surpasser of x[i] if i < j and x[i] < x[j]
-- write a function that find that maximum surpasser

-- Runs in order of O(n*n)
msc :: Ord a => [a] -> Int
msc xs = maximum [ scount z zs | z:zs <- tails xs ]

scount :: Ord a => a -> [a] -> Int
scount x xs = length $ filter (\y -> y < x) xs

tails :: Ord a => [a] -> [[a]]
tails [] = []
tails (x:xs) = (x:xs): (tails xs)

-- Divide and Conquer Approach
-- goal: get a O( n * log n ) function
-- need: msc (xs ++ ys) = join (msc xs) (msc ys)

-- solution 1dv
table :: Ord a => [a] -> [(a,Int)]
table xs = [(z, scount z zs) | z:zs <- tails xs]

msc_dv1 :: Ord a => [a] -> Int
msc_dv1 = maximum . map snd . table

-- find join to satisfy:
-- table ( xs ++ ys ) = join (table xs) (table ys)
-- tails (xs ++ ys) = map (\x -> x ++ ys) (tails xs) ++ tails ys 
--  table ( xs ++ ys ) = 
--    = [(z, scount z zs) | z:zs <- tails ( xs ++ ys) ] 
--    = [(z, scount z zs) | z:zs <- map (\x -> x ++ ys) (tails xs) ++ tails ys]
--    = [(z, scount z (zs ++ ys)) | z:zs <- tails xs] ++ [(z, scount z zs) | z:zs <- tails ys]
--    = [(z, scount z zs + scount x ys) | z:zs <- tails xs] ++ [(z, scount z zs) | z:zs <- tails ys]
--   { def of table && ys = map fst $ table ys }
--   = [(z, (+) c $ map fst $ table ys) <- (z,c) <- table xs] ++ table ys


-- data table = [(Ord, Int)]
join :: ( Ord a ) =>  [(a,Int)]  -> [(a , Int)] -> [(a , Int)]
join txs tys = [(z, c + tcount z tys) | (z,c) <- txs ] ++ tys

tcount :: Ord a => Int -> [(a, Int)] -> [(a, Int)] 
tcount z tys = scount z (map fst tys)



