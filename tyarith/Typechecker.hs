module Typechecker where

import           Control.Monad.Except           ( throwError )
import           Syntax

-- typing

typeof :: Term -> Either String Ty
typeof TmTrue          = return TyBool
typeof TmFalse         = return TyBool
typeof TmZero          = return TyNat
typeof (TmIf t1 t2 t3) = do
  ty1 <- typeof t1
  case ty1 of
    TyBool -> do
      ty2 <- typeof t2
      ty3 <- typeof t3
      if ty2 == ty3
        then return ty3
        else throwError "arms of conditional have different types"
    _ -> throwError "guard of conditional not a boolean"
typeof (TmSucc t1) = if typeof t1 == return TyNat
  then return TyNat
  else throwError "argument of succ is not a number"
typeof (TmPred t1) = if typeof t1 == return TyNat
  then return TyNat
  else throwError "argument of pred is not a number"
typeof (TmIsZero t1) = if typeof t1 == return TyNat
  then return TyBool
  else throwError "argument of iszero is not a number"
