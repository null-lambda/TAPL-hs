module Eval where

import           Syntax
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Either
import           Data.List
import           Debug.Trace                    ( trace )

-- value 

isNatural :: Term -> Bool
isNatural t = case t of
  TmZero    -> True
  TmSucc t1 -> isNatural t1
  _         -> False

isVal :: Context -> Term -> Bool
isVal ctx t = case t of
  TmAbs{}         -> True
  TmUnit          -> True
  TmTrue          -> True
  TmFalse         -> True
  TmString _      -> True
  t | isNatural t -> True
  _               -> False

-- call-by-value evaluation 

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
  TmVar i n -> case indexToBinding ctx i of
    TmAbbBind t1 _ -> return t1
    _              -> Nothing
  TmApp (TmAbs s ty t12) v2 | isVal ctx v2 -> return $ termSubstTop v2 t12
  TmApp v1 t2 | isVal ctx v1               -> do
    t2' <- eval1 ctx t2
    return $ TmApp v1 t2'
  TmApp t1 t2 -> do
    t1' <- eval1 ctx t1
    return $ TmApp t1' t2
  TmLet s v1 t2 | isVal ctx v1 -> return $ termSubstTop v1 t2
  TmLet s t1 t2                -> do
    t1' <- eval1 ctx t1
    return $ TmLet s t1' t2
  TmIf TmTrue  t2 _  -> return t2
  TmIf TmFalse _  t3 -> return t3
  TmIf t1      t2 t3 -> do
    t1' <- eval1 ctx t1
    return $ TmIf t1' t2 t3
  TmSucc   t1                            -> TmSucc <$> eval1 ctx t1
  TmPred   TmZero                        -> return TmZero
  TmPred   (TmSucc nv1)                  -> return nv1
  TmPred   t1                            -> TmPred <$> eval1 ctx t1
  TmIsZero TmZero                        -> return TmTrue
  TmIsZero (TmSucc nv1)                  -> return TmFalse
  TmIsZero t1                            -> TmIsZero <$> eval1 ctx t1
  TmTimesFloat (TmFloat f1) (TmFloat f2) -> return $ TmFloat (f1 * f2)
  TmTimesFloat (TmFloat f1) t2           -> do
    t2' <- eval1 ctx t2
    return $ TmTimesFloat (TmFloat f1) t2'
  TmTimesFloat t1 t2 -> do
    t1' <- eval1 ctx t1
    return $ TmTimesFloat t1' t2
  _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 ctx t)

-- typing 

typeof :: Context -> Term -> Either String Ty
typeof ctx t = case t of
  TmVar i n      -> indexToBindingType ctx i
  TmAbs s ty1 t2 -> do
    let ctx' = (s, VarBind ty1) : ctx
    ty2 <- typeShift (-1) <$> typeof ctx' t2
    return $ TyArrow ty1 ty2
  TmApp t1 t2 -> do
    ty1 <- typeof ctx t1
    ty2 <- typeof ctx t2
    case evalType ctx ty1 of
      TyArrow ty11 ty12 | typeEquals ctx ty11 ty2 -> return ty12
      TyArrow ty11 ty12 -> throwError "parameter type mismatch"
      _ -> throwError "arrow type expected"
  TmLet s t1 t2 -> do
    ty1 <- typeof ctx t1
    let ctx' = (s, VarBind ty1) : ctx
    typeShift (-1) <$> typeof ctx' t2
  TmUnit        -> return TyUnit
  TmTrue        -> return TyBool
  TmFalse       -> return TyBool
  TmIf t1 t2 t3 -> do
    ty1 <- typeof ctx t1
    ty2 <- typeof ctx t2
    ty3 <- typeof ctx t3
    case ty1 of
      TyBool | typeEquals ctx ty2 ty3 -> return ty2
      TyBool -> throwError "arms of conditional has different types"
      _ -> throwError "guard of conditional not a boolean"
  TmZero    -> return TyNat
  TmSucc t1 -> do
    ty1 <- typeof ctx t1
    if typeEquals ctx ty1 TyNat
      then return TyNat
      else throwError "argument of succ is not a number"
  TmPred t1 -> do
    ty1 <- typeof ctx t1
    if typeEquals ctx ty1 TyNat
      then return TyNat
      else throwError "argument of pred is not a number"
  TmIsZero t1 -> do
    ty1 <- typeof ctx t1
    if typeEquals ctx ty1 TyNat
      then return TyBool
      else throwError "argument of iszero is not a number"
  TmString _         -> return TyString
  TmFloat  _         -> return TyFloat
  TmTimesFloat t1 t2 -> do
    ty1 <- typeof ctx t1
    ty2 <- typeof ctx t2
    if typeEquals ctx ty1 TyFloat && typeEquals ctx ty2 TyFloat
      then return TyFloat
      else throwError "argument of timesfloat is not a float"

-- type equivalence

isTypeAbb :: Context -> Int -> Bool
isTypeAbb ctx i = case indexToBinding ctx i of
  TyAbbBind _ -> True
  _           -> False

evalType1 :: Context -> Ty -> Maybe Ty
evalType1 ctx ty = case ty of
  TyVar i n -> case indexToBinding ctx i of
    TyAbbBind ty' -> return ty'
    _             -> empty
  _ -> empty

evalType :: Context -> Ty -> Ty
evalType ctx t = maybe t (evalType ctx) (evalType1 ctx t)

typeEquals :: Context -> Ty -> Ty -> Bool
typeEquals ctx ty1 ty2 =
  let
    ty1' = evalType ctx ty1
    ty2' = evalType ctx ty2
  in
    case (ty1', ty2') of
      (TyVar i n, TyVar i' n') -> i == i'
      (TyVar i n, _) | isTypeAbb ctx i ->
        typeEquals ctx (evalType ctx ty1') ty2'
      (_, TyVar i n) | isTypeAbb ctx i ->
        typeEquals ctx (evalType ctx ty1') ty2'
      (TyArrow ty1 ty2, TyArrow ty1' ty2') ->
        typeEquals ctx ty1 ty2 && typeEquals ctx ty1 ty2
      (TyUnit , TyUnit ) -> True
      (TyBool , TyBool ) -> True
      (TyNat  , TyNat  ) -> True
      (TyFloat, TyFloat) -> True
      _                  -> False

-- fix bindings 

checkBinding :: Context -> Binding -> Either String Binding
checkBinding ctx = assign where
  assign (TmAbbBind t mty) = do
    ty  <- typeof ctx t
    ty' <- case mty of
      Just ty' -> do
        unless (typeEquals ctx ty ty')
               (Left "Type of binding does not match declared type")
        return ty'
      Nothing -> return ty
    return $ TmAbbBind t (Just ty')
  assign b = return b

evalBinding :: Context -> Binding -> Binding
evalBinding ctx b = case b of
  TmAbbBind t mty -> TmAbbBind (eval ctx t) mty
  _               -> b
