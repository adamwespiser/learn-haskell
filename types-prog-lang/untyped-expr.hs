
data Term = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term deriving (Show,Eq)


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
  
eval1 :: Term -> Term
-- TmIf
eval1 (TmIf TmTrue t2 t3 ) = eval1 t2
eval1 (TmIf TmFalse t2 t3 ) = eval1 t3
eval1 (TmIf t1 t2 t3 ) = eval1 $ TmIf t1' t2 t2
  where t1' = eval1 t1
-- TmSucc
eval1 (TmSucc t1) = TmSucc t1'
  where t1' = eval1 t1
-- TmPred
eval1 (TmPred TmZero) = TmZero
eval1 (TmPred (TmSucc t1)) = case (isNumericalValue t1) of
  True  -> eval1 t1 
  False -> TmPred (TmSucc $ eval1 t1)
eval1 (TmPred t1) = eval1 $ TmPred t1'
  where t1' = eval1 t1
-- TmIsZero
eval1 (TmIsZero TmZero) = TmTrue
eval1 (TmIsZero (TmSucc t1)) = case (isNumericalValue t1) of 
  True  -> TmFalse
  False -> TmIsZero (TmSucc t1)
eval1 (TmIsZero t1) = TmIsZero t1' 
  where t1' = eval1 t1
--eval1 (_ ) ->  Nothing
eval1 t1 = t1

termToInt :: Term -> Int
termToInt t1 = case (isNumericalValue t1) of
  True -> case (t1) of
    (TmZero)    -> 0
    (TmSucc t1) -> (termToInt t1) + 1
    (TmPred t1) -> (termToInt t1) - 1
  False -> -1

-- eval1 (TmPred (TmPred TmZero))
-- TmPred (eval1 (TmPred TmZero))
-- TmPred TmZero


