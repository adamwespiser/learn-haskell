{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

import qualified Data.Map as Map
import Control.Monad.State

data Lit 
  =  LitInt Int
  | LitDouble Double
  | LitStr String
  | LitBool Bool

class toValue a where
  toValue :: a -> Value

instance toValue Lit where
  LitInit x = VInt x
  LitDoulbe d = VDouble d
  LitStr s = VStr s
  LitBool = VBool


type Name = String

data Exp  = EInt Int
  | EAtom Name -- identifiers like variables
  | ELit Lit  -- ints, floats, etc
  | EDef Name Exp -- define x ...
  | EFnCall Exp [Exp] -- function call
  | ELambda Exp [Name] -- lambda declaration
  | EList [Exp]
  | EArray [Exp]

data Val 
  = VInt Int
  | VBool Bool
  | VDouble Double 
  | VStr String
  | VAtom Name
  | VList ConstT Val
  | VArray [Val]
  | VLambda { args :: [Exp],  scope :: Store, body :: Exp}
  | VClosure { args :: Signature, scope :: Store, body :: ([Val] -> EnvStack Val)}
data Signature = Signature [Name]

data ConsT a = Cons a (ConsT a) | ConsEmpty 

type Store = Map.Map String Val 
data EnvCtx = EnvCtx { stack :: [Store],
                       global :: Store }

-- runState
type EnvStack a = State EnvCtx a

ctxStack :: EnvCtx -> [Store]
ctxStack ctx = stack ctx ++ [global ctx]

emptyStore :: Map.Map String Val
emptyStore = Map.empty


pushFrame :: EnvStack ()
pushFrame = do
 st <- get
 put $ st { stack = emptyStore : (stack st) }

popFrame :: EnvStack ()
popFrame = do
 ctx <- get
 put $ ctx { stack = case stack ctx of
                       (top:rest) -> rest
                       []     -> [] }

searchStack :: String -> [Store] -> Val
searchStack name (x:xs) =
  case (Map.lookup name x) of
      Just x -> x
      Nothing -> searchStack name xs
searchStack name [] = undefined 


lookUpVar :: String -> EnvStack Val
lookUpVar name = do
  ctx <- get
  return $ searchStack name $ ctxStack ctx

assignVarToFrame :: String  -> Val -> EnvStack ()
assignVarToFrame name val = do
  ctx <- get
  put $ ctx { stack =  (Map.insert name val (head $ stack ctx)):(tail $ stack ctx) }

assignVarToGlobal :: String  -> Val -> EnvStack ()
assignVarToGlobal name val = do
  ctx <- get
  put $ ctx { global =  (Map.insert name val $ global ctx) }

lexicalScope :: EnvStack Store
lexicalScope = do
  ctx <- get
  return $ (Map.unionsWith const $ ctxStack ctx) 


-- ECall fn [args]
--       eval fn , eval <$> [args]
--       (value) , [value]
--      look up atom, call
--      (vlam)  , [args] -> call 
--      (vlam)  , []     -> closure

-- functions: [Val] -> Eval Val

-- eval lambda
-- args names as exp, body as exp, hold scope

-- funs -> closures
-- Expr parsed -> grab context then? or on return?
-- (lambda x y : (+ x y)  
-- expr -> vclosure

evalLambda :: Exp -> [Exp] -> EnvStack Val
evalLambda body args = do 
  scope <- lexicalScope
  return $ VClosure argList scope body

evalExp :: Val -> EnvStack Val
evalExp val = case val of
    (EAtom s) -> return $ VAtom s
    (ELit i)  -> return $ toValue i
    (EDef nameExp valExp) -> do 
                         name <- evalExp nameExp
                         val <- evalExp valExp
                         assignVarToFrame name val
    (ELambda exBody exArgs) -> evalLambda exBody exArgs
    (EList x)        -> VList x
    (EArray x)       -> VArray x
    (ECall f args)   -> fval <- evalExp f   


main :: IO ()
main = 
  putStrLn "hello"
