module Eval where

import           Syntax
import           Control.Monad.Except           ( throwError )

-- evaluation 

isVal :: Context -> Term -> Bool
isVal ctx (TmAbs _ _) = True
isVal ctx _           = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TmApp (TmAbs s t12) v2) | isVal ctx v2 = return $ termSubstTop v2 t12
eval1 ctx (TmApp v1 t2) | isVal ctx v1            = do
  t2' <- eval1 ctx t2
  return $ TmApp v1 t2'
eval1 ctx (TmApp t1 t2) = do
  t1' <- eval1 ctx t1
  return $ TmApp t1' t2
eval1 ctx _ = Nothing

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 ctx t)


