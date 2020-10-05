module Eval where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Debug.Trace                    ( trace )

import           Syntax

-- call-by-value evaluation 

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
  TmIf TmTrue  t2 _  -> return t2
  TmIf TmFalse _  t3 -> return t3
  TmIf t1      t2 t3 -> do
    t1' <- eval1 ctx t1
    return $ TmIf t1' t2 t3
  TmApp (TmAbs s ty t12) v2 | isVal ctx v2 -> return $ termSubstTop v2 t12
  TmApp v1 t2 | isVal ctx v1               -> do
    t2' <- eval1 ctx t2
    return $ TmApp v1 t2'
  TmApp t1 t2 -> do
    t1' <- eval1 ctx t1
    return $ TmApp t1' t2
  _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 ctx t)
