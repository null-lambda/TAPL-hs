module Eval where

import           Syntax
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Debug.Trace                    ( trace )

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

-- typing 

typeof :: Context -> Term -> Either String Ty
typeof ctx t = case t of
  TmTrue        -> return TyBool
  TmFalse       -> return TyBool
  TmIf t1 t2 t3 -> do
    ty1 <- typeof ctx t1
    ty2 <- typeof ctx t2
    ty3 <- typeof ctx t3
    case ty1 of
      TyBool | ty2 == ty3 -> return ty2
      TyBool -> throwError "arms of conditional has different types"
      _ -> throwError "guard of conditional not a boolean"
  TmVar i n      -> getType ctx i
  TmAbs s ty1 t2 -> do
    let ctx' = (s, VarBind ty1) : ctx
    ty2 <- typeof ctx' t2
    return $ TyArrow ty1 ty2
  TmApp t1 t2 -> do
    ty1 <- typeof ctx t1
    ty2 <- typeof ctx t2
    case ty1 of
      TyArrow ty11 ty12 | ty11 == ty2 -> return ty12
      TyArrow ty11 ty12               -> throwError "parameter type mismatch"
      _                               -> throwError "arrow type expected"

