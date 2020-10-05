
module Typechecker where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Debug.Trace                    ( trace )

import           Syntax

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
