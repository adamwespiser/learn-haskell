{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
class (Show pokemon, Show (Move pokemon)) => Pokemon pokemon where
  data Move pokemon :: *
  pickMove :: pokemon -> Move pokemon

data Fire = Charmander | Charmeleon | Charizard deriving Show
instance Pokemon Fire where
  data Move Fire = Ember | FlameThrower | FireBlast deriving Show
  pickMove Charmander = Ember
  pickMove Charmeleon = FlameThrower
  pickMove Charizard = FireBlast

data Water = Squirtle | Wartortle | Blastoise deriving Show
instance Pokemon Water where
  data Move Water = Bubble | WaterGun deriving Show
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun

data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
instance Pokemon Grass where
  data Move Grass = VineWhip deriving Show
  pickMove _ = VineWhip

data Psych = Alba | Mu | Mutwo deriving Show
instance Pokemon Psych where
  data Move Psych = Hypno | Mindbend | Focus deriving Show
  pickMove Alba = Hypno
  pickMove Mu =  Mindbend
  pickMove Mutwo = Focus

class T a where
  data D a :: * -- type function
  fn :: a -> D a

data Da = A | B | C -- the 'a', in T a
instance T Da where 
  data D Da = E | F | G  -- the type function on 'a' in T a
  fn A = E
  fn B = F
  fn C = G

class (T a, T b) =>  TT a b where
  type W a b :: *
  type W a b = a

  compareDa :: a -> b -> W a b
    
  

{- 
  Pokemon pokemon, 
    type function:  Move pokemon of kind *
    define pickMove :: pokemon -> Move pokemon
  make data for psych
  instance Pokemon Psych
    data Move Psych = Conctructors of kind *
    define pickMove for Psych -> Move Psych
-}

printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
  putStrLn $ pokemonOne ++ " used " ++ moveOne
  putStrLn $ pokemonTwo ++ " used " ++ moveTwo
  putStrLn $ "Winner is: " ++ winner ++ "\n"

class (Show (Winner pokemon foe), Pokemon pokemon, Pokemon foe) => Battle pokemon foe where
  type Winner pokemon foe :: *
  type Winner pokemon foe = pokemon

  battle :: pokemon -> foe -> IO ()
  battle pokemon foe = do
    printBattle (show pokemon) (show move) (show foe) (show foeMove) (show winner)
   where
    foeMove = pickMove foe
    move = pickMove pokemon
    winner = pickWinner pokemon foe
  
  pickWinner :: pokemon -> foe -> (Winner pokemon foe)

instance Battle Water Fire where
  pickWinner pokemon foe = pokemon

instance Battle Fire Water where
  type Winner Fire Water = Water
  pickWinner = flip pickWinner

instance Battle Grass Water where
  pickWinner pokemon foe = pokemon

instance Battle Water Grass where
  type Winner Water Grass = Grass
  pickWinner = flip pickWinner

instance Battle Fire Grass where
  pickWinner pokemon foe = pokemon

instance Battle Grass Fire where
  type Winner Grass Fire = Fire
  pickWinner = flip pickWinner
    
main :: IO ()
main = do
  battle Squirtle Charmander
  battle Charmeleon Wartortle
  battle Bulbasaur Blastoise
  battle Wartortle Ivysaur
  battle Charmeleon Ivysaur
  battle Venusaur Charizard
