-- the countdown problem is as follows: given an intege and a set of 
-- ints, find an expression of the ints that is closest to the integer


-- set up datastructures
data Expr = Num Int
  | App Op Expr Expr deriving (Show)
data Op = Add 
  | Sub
  | Mul
  | Div deriving (Show)
type Value = Int 


-- get all the subsequences for a list of integers
subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x:) xss
  where xss = subseqs xs

-- apply = take a function and apply it to argument
apply :: Op -> (Value -> Value -> Value)
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = (div)



-- get the value for a given expression
-- (this is our eval fun)
value :: Expr -> Value
value (Num x) = x
value (App op e1 e2) = apply op (value e1) (value e2)


-- make sure an operation is legal
legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = True
legal Sub v1 v2 = (v2 < v1) -- can't be less than 0
legal Mul v1 v2 = True
legal Div v1 v2 = (mod v1 v2 == 0) -- can't be a fractional number


-- create a list of all legal expression that can be built using the 
-- given subsequence
mkExprs :: [Int] -> [(Expr, Value)]
mkExprs [x] = [(Num x, x)]
mkExprs xs = [ev | (ys, zs) <- unmerges xs, 
                   ev1      <- mkExprs ys, 
                   ev2      <- mkExprs zs, 
                   ev       <- combine ev1 ev2 ]

-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
-- > concatMap (\x -> [(x,x+1)]) [10,20,30] == [(10,11),(20,21),(30,31)]

-- unmerges xs is the list of all pairs (ys,zs) s.t. merge ys zs = xs
-- where merge merges two ordered lists into one
unmerges :: [a] -> [([a], [a])]
unmerges [x,y]  = [([x], [y]), ([y], [x])] 
unmerges (x:xs) = [([x], xs), (xs, [x])] ++ concatMap (add x) (unmerges xs)
  where add x (ys, zs) = [(x : ys, zs), (ys, x : zs)]

--combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1,v1) (e2,v2) = 
  [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]
  where 
    ops = [Add, Sub, Mul, Div]

-- nearest n takes a nonempty list of expression and
-- returns some expressoin in the list who value is the nearest to 
-- the tagert n.
-- also, we want to stop seraching when we find a direct match
--nearest :: Int -> [(Expr, Value)] -> (Expr, Value)
nearest n ((e, v) : evs) = if d == 0 then (e, v)
  else search n d (e, v) evs
  where d = abs ( n - v)
search :: Int -> Value -> (Expr, Value) -> [(Expr,Value)] -> (Expr, Value)
search n d ev []           = ev
search n d ev ((e, v) : evs) 
  | d' == 0 = (e, v)
  | d' < d  = search n d' (e, v) evs
  | d' >= d = search n d ev evs
  where d' = abs $ (-) n v                             

-- set up algorithms using defined functions:
countdown1 :: Int -> [Int] -> (Expr, Value)
countdown1 n = nearest n . concatMap mkExprs . subseqs











