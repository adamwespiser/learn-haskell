import qualified Data.Map as Map
{- 
  A Simple Typed Lambda Calculus
    conversion from ML: https://www.cis.upenn.edu/~bcpierce/tapl/
-}

-- carries (names and binding(type constructor)
type Context = [(String, Binding)]

-- Implement a context carrying type information
data Binding = NameBind
  | VarBind Ty 
  | BindError String deriving (Eq, Show)

-- The types of variables
data Ty  = TyArr Ty Ty
  | TyBool 
  | TyError Info deriving (Eq, Show)

-- define term, use info as a tag for debugging
type Info = String
data Term = 
    TmVar Info Int Int
  | TmAbs Info String Ty Term
  | TmApp Info Term Term 
  | TmTrue Info
  | TmFalse Info
  | TmIf Info Term Term Term deriving (Eq, Show)
  
-- Add a variable & binding to the store
-- x is a string (variable name), bind is a binding
--addBinding ctx x bind = Map.insert x bind ctx
addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x,bind):ctx

-- get the binding for a given context
getBinding :: String -> Int -> Context -> Binding
getBinding fi i ctx = case (i <= length ctx ) of
  True  -> snd $ ctx !! i 
  False -> BindError $ "failed to get binding \n\t" ++ fi ++ "\nvariable not found"

-- get the type of a given binding
getTypeFromBinding ::  String -> Int  -> Context -> Ty
getTypeFromBinding fi i ctx = 
  let db = getBinding fi i ctx
  in case db of
    NameBind   -> TyError $ "binding has no type" ++ fi
    BindError msg -> TyError $ "failed to get type for binding \n\t" ++ fi ++ "\nvariable not found in context"
    VarBind ty -> ty 

-- get the type from a binding (from a context)
getTypeFromContext :: String -> Context ->  Int -> Ty
getTypeFromContext fi ctx i = 
  case (getBinding fi i ctx ) of
    VarBind ty -> ty
    BindError msg -> TyError $ "could not get type from context" ++ msg

-- do we have a bind error?
isBindError :: Binding -> Bool
isBindError bd = case bd of
  BindError _ -> True
  _         -> False

-- quick show error on binding
showError :: Binding -> String
showError bd = case bd of
  (BindError msg) -> show msg
  (VarBind  ty)  -> show ty


showTypes :: [Ty] -> String
showTypes (x:xs) = (show x) ++ " " ++ showTypes xs
showTypes (x:_) = show x
showTypes _ = ""

-- recursive function: get the type of a given argument...
typeof :: Context -> Term -> Ty
typeof ctx t = case t of
  TmVar fi i _ -> getTypeFromContext fi ctx i 
  TmAbs fi x tyT1 t2 -> -- TmAbs fi (variable string) (type binding) (term) 
    let ctx' = addBinding ctx x $ VarBind tyT1 -- add binding to context
        tyT2 = typeof ctx' t2 --get binding from context
    in TyArr tyT1 tyT2 -- return array of types
  TmApp fi t1 t2 -> -- TmApp fi Term1 Term2
    let tyT1 = typeof ctx t1
        tyT2 = typeof ctx t2
    in case tyT1 of
      TyArr tyT11 tyT12 -> 
        if ((==) tyT2 tyT11) then tyT12 else TyError $ "parameter mismatch" ++ fi
      _                 -> TyError $ "arrow type expected " ++ fi ++ showTypes [tyT1,tyT2]
  TmTrue msg  -> TyBool 
  TmFalse msg -> TyBool 
  TmIf fi t1 t2 t3 -> 
    if (==) (typeof ctx t1) TyBool then 
      let tyT2 = typeof ctx t2
      in if (==) tyT2 (typeof ctx t2) then tyT2
        else TyError $ "arms of conditions have different types" ++ fi
    else TyError $ "gaurd of conditional not a boolean"



{- 
-- From: http://dev.stephendiehl.com/fun/002_parsers.html
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval t = case nf t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"
-}


emptyContext :: Context
emptyContext = []

idBool :: Term
idBool = (TmAbs "" "x" TyBool  (TmVar "" 0 1))


{-  TESTS
typeof emptyContext (TmIf "if" (TmTrue "t") (TmFalse "t") (TmTrue "t"))
-}

{-  TESTS
typeof emptyContext (TmIf "if" (TmTrue "t") (TmFalse "t") (TmTrue "t"))

  -- TmVar Info Int Int  -- lambda
  -- TmAbs Info String Ty Term  -- inside
  -- TmApp Info Term Term  -- appyly lambda inside..

typeof emptyContext $ TmAbs "" "x" TyBool  (TmAbs "" "y" TyBool  (TmApp ""  (TmVar ""  1 2) (TmVar "" 1 2)))

typeof [("y",VarBind TyBool)] (TmApp "" idBool (TmVar " " 0 1))

data Maybe a = Just a | Nothing deriving (Show)
-}














