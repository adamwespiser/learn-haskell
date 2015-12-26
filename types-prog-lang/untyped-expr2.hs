import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

data Term = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmSucc Term
  | TmPred Term
  | TmZero
  | TmError
  | TmIsZero Term deriving (Show,Eq)


data Value a = TmReal a | TmNumError deriving (Show)

-- new for 7.10, must declare Monad instance as Functor and Applicative
-- as well...
instance Functor Value where
  fmap f (TmReal x) = TmReal $ f x
  fmap f (TmNumError) = TmNumError

instance Applicative Value where
  pure  = TmReal
  (<*>) = ap 

instance Monad Value where
  return x = TmReal x 
  TmNumError >>= f = TmNumError
  TmReal x >>= f =  f x
  fail _ = TmNumError


isNumericalValue :: Term -> Bool
isNumericalValue  t1 = case (t1) of 
  (TmZero )   -> True
  (TmSucc t1) -> isNumericalValue t1
  (TmPred t1) -> isNumericalValue t1
  _           -> False

isVal :: Term -> Bool
isVal t 
  | t == TmTrue        = True
  | t == TmFalse       = True
  | isNumericalValue t = True
  | otherwise          = False
  
sub :: Term -> Term
-- TmIf
sub (TmIf TmTrue t2 t3 ) = sub t2
sub (TmIf TmFalse t2 t3 ) = sub t3
sub (TmIf t1 t2 t3 ) = sub $ TmIf t1' t2 t2
  where t1' = sub t1
-- TmSucc
sub (TmSucc t1) = TmSucc t1'
  where t1' = sub t1
-- TmPred
sub (TmPred TmZero) = TmZero
sub (TmPred t1) = sub $ TmPred t1'
  where t1' = sub t1
sub (TmIsZero TmZero) = TmTrue
sub (TmIsZero (TmSucc t1)) = case (isNumericalValue t1) of 
  True  -> TmFalse
  False -> error "runtime type error"
  --False -> TmIsZero (TmSucc t1)
sub (TmIsZero t1) = sub $ TmIsZero t1' 
  where t1' = sub t1
--sub (_ ) ->  Nothing
sub t1 = t1

evalNumeric :: Term -> Value Int
evalNumeric t1 = case (isNumericalValue t1) of
  True -> case (t1) of
    (TmZero)    -> (TmReal 0)
    (TmSucc t1) -> (evalNumeric t1) >>= (\x -> return $ x + 1)
    (TmPred t1) -> (evalNumeric t1) >>=  (\x -> return $ if (x > 0) then  (x - 1) else 0 )
  False -> TmNumError
-- eval $ sub $ TmSucc $ TmSucc $ TmSucc $ TmPred $ TmZero
-- sub (TmPred (TmPred TmZero))
-- TmPred (sub (TmPred TmZero))
-- TmPred TmZero


