data PairT a b = Pair a b 

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

--------------------------------------------
{- Sum Types via Constructor -}
data SumT a b = Inl a | Inr b | Summ a b deriving (Show, Eq, Ord, Read)

fill :: SumT a b -> SumT a b -> SumT a b
fill (Inl a) (Inr b) = Summ a b
-----------------------------------------------
{- Sum types, with monoid combination 
   addRL (toR x) (toL y) = Sm $ x + y

-}
newtype Sum a = Sum { getSum :: a } deriving (Show)
instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0
  (Sum x) `mappend` (Sum y) = Sum $ x + y

newtype Product a = Product { getProduct :: a } deriving (Show)
instance (Num a) =>  Monoid (Product a) where
  mempty = Product 1
  (Product x) `mappend` (Product y) = Product $ x * y

data RrT a = Rr a deriving (Show)
data LtT a = Lt a deriving (Show)
data SmT a = Sm a deriving (Show)

type TagR a = RrT (Sum a) 
type TagL a = LtT (Sum a)


toR :: a -> TagR a
toR x = Rr (Sum x)

toL :: a -> TagL a
toL x = Lt (Sum x)

addRL :: (Monoid a) => RrT a -> LtT a -> SmT a
addRL (Rr a)  (Lt b) = Sm $ a `mappend`  b
{- 
  Try TAPL p. 133
-}
data PhysAddr = Phys { flPhys :: String
               , addr      :: String 
               } deriving (Eq, Read, Show)


data VirAddr = Vir { flVir :: String
               , email      :: String 
               } deriving (Eq, Read, Show)

--toPhys :: String -> String -> PhysAddr
--toPhys x y = PhysAddr x (Just y)                  

data SumType a tag = Addr {getVal :: a } deriving (Show)
data PhysTag
data VirTag


inL :: PhysAddr -> SumType PhysAddr PhysTag
inL (Phys a  b) =  Addr (Phys a b)

inR :: VirAddr -> SumType VirAddr VirTag
inR (Vir a b) =  Addr (Vir a b)


getName :: SumType a VirTag -> String
getName (Addr t) = case t of
  _ -> "test"
  --(Phys x y)  -> x

  --(Addr v@(Phys x y)) -> addr v


{- 
   this block doesn't make sense
   i tried to add phantom types to 
   sum types...
-}
data P a b c = Phl a | Phr b | Phb a b  deriving (Show)
data Lt 
data Rt
data Both

inl :: a -> P a b Lt
inl a = Phl a 

inr :: b -> P a b Rt
inr b = Phr b 

fillP :: P a b Lt -> P a b Rt -> P a b Both
fillP (Phl aa) (Phr bb) = Phb aa bb 
--fillP (Phr bb) (Phl aa) = Phb aa bb
--fillP (Phb aa bb) (Phb aa1 bb1) = Phb aa bb1 












