module Eval where

import           Syntax
import           Control.Monad.Except           ( throwError )

-- evaluation 

isnumerical :: Term -> Bool
isnumerical t = case t of
  TmZero    -> True
  TmSucc t1 -> isnumerical t1
  _         -> False

isval :: Term -> Bool
isval TmTrue  = True
isval TmFalse = True
isval t | isnumerical t = True
        | otherwise     = False

eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue  t2 _ ) = return t2
eval1 (TmIf TmFalse _  t3) = return t3
eval1 (TmIf t1      t2 t3) = do
  t1' <- eval1 t1
  return $ TmIf t1' t2 t3
eval1 (TmSucc   t1          ) = TmSucc <$> eval1 t1
eval1 (TmPred   TmZero      ) = return TmZero
eval1 (TmPred   (TmSucc nv1)) = return nv1
eval1 (TmPred   t1          ) = TmPred <$> eval1 t1
eval1 (TmIsZero TmZero      ) = return TmTrue
eval1 (TmIsZero (TmSucc nv1)) = return TmFalse
eval1 (TmIsZero t1          ) = TmIsZero <$> eval1 t1
eval1 _                       = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)

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

-- printing

instance Show Term where
  show TmTrue  = "true"
  show TmFalse = "false"
  show (TmIf t1 t2 t3) =
    "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show TmZero = "0"
  show (TmSucc t) | isnumerical t = showAsNum t 1
                  | otherwise     = "(succ " ++ show t ++ ")"
   where
    showAsNum TmZero     num = show num
    showAsNum (TmSucc t) num = showAsNum t (num + 1)
  show (TmPred   t) = "(pred " ++ show t ++ ")"
  show (TmIsZero t) = "iszero " ++ show t

instance Show Ty where
  show TyBool = "bool"
  show TyNat  = "nat"
