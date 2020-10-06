module Typechecker where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Either
import           Data.Maybe
import           Data.List
import           Debug.Trace

import           Syntax

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
    fieldEquals ctx (label1, ty1) (label2, ty2) =
      label1 == label2 && typeEquals ctx ty1 ty2
  in
    case (ty1', ty2') of
      (TyVar i n, TyVar i' n') -> i == i'
      (TyVar i n, _) | isTypeAbb ctx i ->
        typeEquals ctx ty1' (evalType ctx ty2')
      (_, TyVar i n) | isTypeAbb ctx i ->
        typeEquals ctx (evalType ctx ty1') ty2'
      (TyArrow ty1 ty2, TyArrow ty1' ty2') ->
        typeEquals ctx ty1 ty1' && typeEquals ctx ty2 ty2'
      (TyRecord fields, TyRecord fields') ->
        length fields == length fields' && and
          (zipWith (fieldEquals ctx) fields fields')
      (TyVariant fields, TyVariant fields') ->
        length fields == length fields' && and
          (zipWith (fieldEquals ctx) fields fields')
      (TyUnit  , TyUnit  ) -> True
      (TyBool  , TyBool  ) -> True
      (TyNat   , TyNat   ) -> True
      (TyString, TyString) -> True
      (TyFloat , TyFloat ) -> True
      _                    -> False

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
      TyArrow ty11 ty12 ->
        throwError $ "parameter type mismatch on \"" ++ showTerm ctx t ++ "\""
      _ -> throwError $ "expected arrow type on \"" ++ showTerm ctx t1 ++ "\""
  TmLet s t1 t2 -> do
    ty1 <- typeof ctx t1
    let ctx' = (s, VarBind ty1) : ctx
    typeShift (-1) <$> typeof ctx' t2
  TmRecord fields -> do
    fields' <- forM fields $ \(l, t) -> do
      ty <- typeof ctx t
      return (l, ty)
    return $ TyRecord fields'
  TmProj t1 l -> do
    ty1 <- typeof ctx t1
    case evalType ctx ty1 of
      TyRecord fields ->
        let mty = snd <$> find ((l ==) . fst) fields
        in  maybe (throwError $ "field " ++ l ++ " not found") Right mty
      _ -> throwError "expected record type"
  TmTag l t1 ty1 -> case evalType ctx ty1 of
    TyVariant fields -> do
      ty1Field <- typeof ctx t1
      let mty2 = snd <$> find ((l ==) . fst) fields
      ty2 <- maybe (throwError $ "field " ++ l ++ " not found") Right mty2
      if typeEquals ctx ty1Field ty2
        then return ty1
        else throwError "field does not have expected type"
    _ -> throwError "tag annotation is not a variant type"
  TmCase t0 cases -> do
    ty0 <- typeof ctx t0
    case evalType ctx ty0 of
      TyVariant fields -> do
        caseTypes <- forM cases $ \(ln, (xn, tn)) -> do
          tyN <- case lookup ln fields of
            Just ty -> return ty
            _       -> throwError $ "label " ++ ln ++ " not in type"
          let ctx' = (xn, VarBind tyN) : ctx
          typeShift (-1) <$> typeof ctx' tn
        let tyTag1 = head caseTypes
        forM_ (tail caseTypes) $ \tyTagN -> unless
          (typeEquals ctx tyTag1 tyTagN)
          (throwError "fields of case statement do not have the same type")
        return tyTag1
      _ ->
        throwError $ "expected variant type on \"" ++ showType ctx ty0 ++ "\""
  TmUnit            -> return TyUnit
  TmAscrib t1 tyAsc -> do
    ty1 <- typeof ctx t1
    unless (typeEquals ctx ty1 tyAsc)
           (throwError "body of as-term does not have the expected type")
    return tyAsc
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
  TmZero             -> return TyNat
  TmSucc   t1        -> unary "succ" t1 TyNat TyNat
  TmPred   t1        -> unary "pred" t1 TyNat TyNat
  TmIsZero t1        -> unary "iszero" t1 TyNat TyBool
  TmString _         -> return TyString
  TmFloat  _         -> return TyFloat
  TmTimesFloat t1 t2 -> do
    ty1 <- typeof ctx t1
    ty2 <- typeof ctx t2
    if typeEquals ctx ty1 TyFloat && typeEquals ctx ty2 TyFloat
      then return TyFloat
      else throwError "argument of timesfloat is not a float"
  TmFix t1 -> do
    ty1 <- typeof ctx t1
    case ty1 of
      TyArrow ty11 ty12 -> if typeEquals ctx ty11 ty12
        then return ty11
        else throwError "result of body not compatible with domain"
      _ -> throwError "arrow type expected"
 where
  unary name t1 ty1 ty2 = do
    ty1' <- typeof ctx t1
    if typeEquals ctx ty1 ty1'
      then return ty2
      else throwError $ "argument of " ++ name ++ " is not a number"
