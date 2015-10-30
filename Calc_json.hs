{-# LANGUAGE OverloadedStrings #-}
--
-- import Prelude hiding( Monad, Maybe (..) )
import Control.Applicative
import Data.HashMap.Strict as Map
import Data.Aeson
import Data.Text as T
import Data.Vector
import Data.Scientific
import Data.ByteString.Lazy.Char8 as BS
-- maybe monad stuff

-- Expression stuff
data Exp
  = Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Val Double
  deriving (Eq, Show)

data ExpJson = ExpJson { tag     :: String,
                         payload :: [ExpJson] } deriving Show

instance FromJSON Exp where
  parseJSON (Object o) = 
    case (Map.lookup "tag" o) of

      Nothing -> fail "no tag present"

      Just (String "Add") -> do 
        [xa, xb] <- o .: "payload"
        return $ Add xa xb
      Just (String "Sub") -> do 
        [xa, xb] <- o .: "payload";
        return $ Sub xa xb
      Just (String "Mul") -> do 
        [xa, xb] <- o .: "payload"
        return $ Mul xa xb
      Just (String "Div") -> do 
        [xa, xb] <- o .: "payload"
        return $ Div xa xb
      Just (String "Val") -> do 
        xa <- o .: "payload"
        return $ Val xa
      _ -> fail "couldn't parse json vals"


toValue :: Exp -> Value
toValue (Val x) =  Object $ Map.fromList [( "tag", String "Val"), ("payload", Number $ fromFloatDigits x )]
toValue (Add x y) =  Object $ Map.fromList [("tag", String "Add"), ( "payload", Array $ Data.Vector.fromList [ toValue x, toValue y]   )]
toValue (Div x y) =  Object $ Map.fromList [("tag", String "Div"), ( "payload", Array $ Data.Vector.fromList [ toValue x, toValue y]   )]
toValue (Mul x y) =  Object $ Map.fromList [("tag", String "Mul"), ( "payload", Array $ Data.Vector.fromList [ toValue x, toValue y]   )]
toValue (Sub x y) =  Object $ Map.fromList [("tag", String "Sub"), ( "payload", Array $ Data.Vector.fromList [ toValue x, toValue y]   )]
--toValue (_ _ _)   = Null 


-- add in bind
eval :: Exp -> Either String Exp
eval ( Val x ) = Right $ Val x 
eval x = fmap Val $ eval_aux  x 



expToString :: Exp -> String
expToString (Val x) = "Val"

expToString (Add x y) = "Add"
expToString (Sub x y) = "Sub"
expToString (Mul x y) = "Mul"
expToString (Div x y) = "Div"

applyFn :: (Show  a) => (Num a) => String -> a -> [String]
-- applyFn cmd state =  Prelude.foldr1 (app) [  "var_" , (show state) , " = fn_", cmd,  "(var_" , show (state + 10 ) , ", " , "var_" , show (state + 2), ");\n"  ]
applyFn cmd state =  [Prelude.concat [  "var_" , (show state) , " = fn_", cmd,  "(var_" , show (state + 10 ) , ", " , "var_" , show (state + 2), ");\n"  ] ]



revArgs  :: ( [a] -> [a] -> [a]) -> [a] -> [a] -> [a]
revArgs fn x y = Prelude.reverse $ fn  (Prelude.reverse x )  (Prelude.reverse y )


mergePlaces :: [a] -> [a] -> [a]
mergePlaces (x:xs) (y:ys)  = x:y:(mergePlaces xs ys)
mergePlaces []     y      = y
mergePlaces x      []      = x

applyExp :: (Show a) => (Num a) => Exp -> Exp -> a -> [String]
-- applyExp x y state =  Prelude.foldr1 (Prelude.++) $ mergePlaces (treeToCommands y (state + 2))    (treeToCommands x (state + 10) ) 
applyExp x y state =  revArgs mergePlaces (treeToCommands y (state + 2))    (treeToCommands x (state + 10) ) 


app :: [a] -> [a] -> [a]
app x y = x Prelude.++ y

treeToCommands :: (Num a) => (Show a) => Exp -> a -> [String]
treeToCommands (Val x) state = [ Prelude.concat  ["var_",  (show state), " = ", (show x), ";\n"] ]
-- treeToCommands (Add x y) state = Prelude.foldr1 (app) [ (treeToCommands x (state + 1)) ,   (treeToCommands y (state + 2) ) , applyFn "add" state ]
treeToCommands (Add x y) state = app (applyExp x y state )   (applyFn "add" state )
treeToCommands (Sub x y) state = app (applyExp x y state )   (applyFn "sub" state )
treeToCommands (Mul x y) state = app (applyExp x y state )   (applyFn "mul" state )
treeToCommands (Div x y) state = app (applyExp x y state )   (applyFn "div" state )



traverseEvalTree :: Num a => Show a=> Exp -> a-> String
traverseEvalTree (Val x) d = ( "l=" ) Prelude.++ ( ( show d)) Prelude.++ (" [Val = " ) Prelude.++(  (show x) ) Prelude.++(  "] ")
traverseEvalTree (Sub x y) d =( "l=" ) Prelude.++(  (show d) ) Prelude.++(  "Subtract(" ) Prelude.++(  (traverseEvalTree x (d + 1)) ) Prelude.++(  (traverseEvalTree y (d + 1)) ) Prelude.++(  ")")
traverseEvalTree (Add x y) d =( "l=" ) Prelude.++(  (show d) ) Prelude.++(  "Add(" ) Prelude.++(  (traverseEvalTree x (d + 1)) ) Prelude.++(  (traverseEvalTree y (d + 1)) ) Prelude.++(  ")")
traverseEvalTree (Div x y) d =( "l=" ) Prelude.++(  (show d) ) Prelude.++(  "Divide(" ) Prelude.++(  (traverseEvalTree x (d + 1)) ) Prelude.++(  (traverseEvalTree y (d + 1)) ) Prelude.++(  ")")
traverseEvalTree (Mul x y) d =( "l=" ) Prelude.++(  (show d) ) Prelude.++(  "Multiply(" ) Prelude.++(  (traverseEvalTree x (d + 1)) ) Prelude.++(  (traverseEvalTree y (d + 1)) ) Prelude.++(  ")")


travT :: Num a => Show a=> Exp -> a-> [String]
travT (Val x) d = "Val= " :[(show x)]
travT (Add x y) d = "add": (xx Prelude.++ yy)
  where xx = travT y (d+1)
        yy = travT y (d+1)



eval_aux :: Exp -> Either String Double
eval_aux (Val x  )   =  Right x
eval_aux (Add x y)   =  do 
  ex <-  (eval_aux x)  
  ey <- (eval_aux y)  
  return $ ex + ey

eval_aux (Mul x y)   =  do 
  ex <-  (eval_aux x)  
  ey <- (eval_aux y)  
  return $ ex * ey

eval_aux (Sub x y)   =  do 
  ex <-  (eval_aux x)  
  ey <- (eval_aux y)  
  return $ ex - ey

eval_aux (Div x y) =  case y_eval_aux of
  Right 0.0 -> Left "Cannot defined divide by zero for Div"
  otherwise -> do 
    ex <- (eval_aux x)
    ey <- y_eval_aux
    return $ ex / ey
  where y_eval_aux = eval_aux y


ex1 :: Exp
ex1 = Div (Add (Val 1.0) (Val 2.0)) (Val 3.0)

ex2 :: Exp
ex2 = Div (Add (Val 1.0) (Val 2.0)) (Val 0.0)

ex3 :: Exp
ex3 = Add (Val 1.0) (Val 2.0)

main :: IO ()
main = do
    print(eval ex1)
    print(eval ex2)
    print(treeToCommands ex2 0)
    json <- BS.readFile "exp.json"
    let jsonlines = BS.lines json
        output = Prelude.mapM decode jsonlines 
        decoded = (fmap (fmap eval) output)
    case decoded of
      Nothing -> print "Parsing error"
      Just x -> Prelude.mapM_ print x


