{-# LANGUAGE ViewPatterns, PatternSynonyms #-}  
{- https://www.reddit.com/r/haskell/comments/3hfdmn/understanding_the_state_monad/ -}

import qualified Data.Map as Map
import Control.Monad.State

x = Map.fromList [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]
has5 = Map.lookup 5 x -- Nothing
has2 = Map.lookup 2 x -- Just 'b'

x' = Map.insert 10 'z' x -- add (10, 'z') to x
hasX10 = Map.member 10 x' -- True


data Val  = VInt Int
  | VBool Bool
  | VChar Char
  | VString String 
  | StateNone deriving (Eq, Show, Read)

type Store = Map.Map String Val 
--data State b a = State { runState :: a -> (b, a) }
type EnvState  = State Store Val

--instance Show EnvState where
  --show (State s a ) = show s ++ show a 

demoStore :: Store
demoStore = Map.fromList [("a", VInt 1), ("b", VBool True), ("c", VChar 'c'), ("d", VString "adam")]

demoEnvir :: EnvState
demoEnvir = state $ \s -> (StateNone ,demoStore)

emptyEnvir :: EnvState
emptyEnvir = state $ \s -> (StateNone ,Map.empty)

assignStore :: String  -> Val -> Store -> Store 
assignStore name val env = Map.insert name val env

assign ::  String -> Val ->  EnvState
assign nm val = state $ \s -> (StateNone, assignStore nm val s)

lookupSafe :: String -> Store -> Val
lookupSafe nm ctx = case (Map.lookup nm ctx) of
  Just x -> x
  Nothing -> StateNone

lookupName :: String -> EnvState
lookupName nm = state $ \s -> (lookupSafe nm s, s)

{-
assignVar :: String -> Val -> Eval ()
assignVar name val = do
 st <- get
 put $ Map.insert name val st

lookupVar name = do
 st <- get
 return $ Map.lookup name st
-}
-- runState  (assign  "aa" (VInt 1) >>= (\s -> lookupName "aa"))  demoStore
-- runState  (assign  "aa" (VInt 1))  demoStore

{-
test :: EnvState
test = do 
  ( assignStore "x" (VInt 1 ))
  --put x
-}

--import Control.Monad.State  
type Stack = [Int]
pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)  

modS :: (Int -> Int)  -> State Stack ()
modS f = do 
  a <- pop
  push $  f a
  
 
stackManip :: State Stack Int  
stackManip = do  
  push 3  
  modS (+1)
  a <- pop  
  pop  

stackStuff :: State Stack ()  
stackStuff = do  
  a <- pop  
  modS (+100)
  if a == 5  
    then push 5  
    else do  
      push 3  
      push 8 
--modify :: (Val -> Val) 
--modify f = state $ \s -> (f s, f)



{- 
(>>=) :: State b a -> b -> State b a -> State b a
pA >>=  f = State $ \s1 -> 
  let (v1, s2) = runState pA s
    pB = f v1
  in (v2,s3) = runState pB s2

get :: State s a
get = State $ \s -> (s, s)




pA >>=  f = State $ \s1 -> 
   let (v1, s2) = runState pA s
       pB      = f v1
   in (v2,s3) = runState pB s2

emptyState >>= assign "x" (VInt 1) = 
  State $ \s1 -> 
    let (v1, s2)  = runState emptyState s1


-}






--insertVar 



