data PairT a b= Pair a b 

one :: (PairT a b) -> a
one (Pair a b) = a

two :: (PairT a b) -> b
two (Pair a b) = b

instance Functor (PairT a) where
  fmap f (Pair a b) = Pair  (a)  (f b)

instance (Show a, Show b) => Show (PairT a b) where
  show (Pair a b) = "<" ++ show a ++ "|" ++ show b ++ ">"

instance (Eq a, Eq b) => Eq (PairT a b) where
  (==) (Pair a1 b1) (Pair a2 b2) = (a1 == a2) && (b2 == b1)

instance (Ord a, Num a, Ord b, Num b) => Ord (PairT a b) where
  compare (Pair a1 b1) (Pair a2 b2) = compare b1 b2

--type ReadS a =  String -> [(a,String)]
--reads :: (Read a) => ReadS a
readsPair :: (Read a,Read b) => ReadS (PairT a b)
readsPair ('<':s) =  [(Pair r l,x) | (l, '|':t) <-   reads s,
                                     (r, '>':x)   <- reads t ]
--readsPair s =  [x | (x,t)  <- reads s]

instance (Read a, Read b) => Read (PairT a b) where
  readsPrec _ s = readsPair s 

testRead :: Bool
testRead =
  let x = one (read "<3|4>" :: (PairT Int Int)) == 3
      y = one (read "<3|4>" :: (PairT Int Int)) == 4
      z = ((read . show) (Pair 2 3) :: (PairT Int Int)) == Pair 3 2
  in x && y && z



