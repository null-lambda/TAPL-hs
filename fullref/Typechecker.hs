{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Typechecker where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List
import Data.Maybe

import           Syntax

-- type equivalence

isTypeAbb :: Context -> Int -> Bool
isTypeAbb ctx i = case indexToBinding ctx i of
  TyAbbBind _ -> True
  _           -> False

unwrapTypeAbb1 :: Context -> Ty -> Maybe Ty
unwrapTypeAbb1 ctx ty = case ty of
  TyVar i _ -> case indexToBinding ctx i of
    TyAbbBind ty' -> return ty'
    _             -> empty
  _ -> empty

unwrapTypeAbb :: Context -> Ty -> Ty
unwrapTypeAbb ctx t = maybe t (unwrapTypeAbb ctx) (unwrapTypeAbb1 ctx t)

typeEquals :: Context -> Ty -> Ty -> Bool
typeEquals ctx = walk where
  fe (label1, ty1) (label2, ty2) = label1 == label2 && walk ty1 ty2
  fieldsEquals fs fs' = length fs == length fs' && and (zipWith fe fs fs')
  walk :: Ty -> Ty -> Bool
  walk ty1 ty2 = case (ty1, ty2) of
    (TyVar i _, TyVar i' _) -> i == i'
    (TyVar i _, _) | isTypeAbb ctx i -> walk (unwrapTypeAbb ctx ty1) ty2
    (_, TyVar i _) | isTypeAbb ctx i -> walk ty1 (unwrapTypeAbb ctx ty2)
    (TyArrow ty1 ty2, TyArrow ty1' ty2') -> walk ty1 ty1' && walk ty2 ty2'
    (TyRecord fields, TyRecord fields') -> fieldsEquals fields fields'
    (TyVariant fields, TyVariant fields') -> fieldsEquals fields fields'
    (TyUnit, TyUnit)        -> True
    (TyBool, TyBool)        -> True
    (TyNat, TyNat)          -> True
    (TyString, TyString)    -> True
    (TyFloat, TyFloat)      -> True
    (TyRef ty1, TyRef ty2)  -> walk ty1 ty2
    (TyTop, TyTop) -> True 
    (TyBot, TyBot) -> True
    _                       -> False

-- subtyping 

subtype :: Context -> Ty -> Ty -> Bool
subtype ctx tyS tyT =
  typeEquals ctx tyS tyT
    || let
         tyS' = unwrapTypeAbb ctx tyS
         tyT' = unwrapTypeAbb ctx tyT
       in
         case (tyS', tyT') of
           (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) ->
             subtype ctx tyT1 tyS1 && subtype ctx tyS2 tyT2
           (TyRecord fS, TyRecord fT) -> flip all fT $ \(ln, tyTn) ->
             case lookup ln fS of
               Just tySn -> subtype ctx tySn tyTn
               Nothing   -> False
           (TyVariant fS, TyVariant fT) -> flip all fS $ \(ln, tySn) ->
             case lookup ln fT of
               Just tyTn -> subtype ctx tySn tyTn
               Nothing   -> False
           (TyRef tyS1, TyRef tyT1) ->
             subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1
           (_, TyTop) -> True
           (TyBot, _) -> True
           _          -> False

joinType :: Context -> Ty -> Ty -> Ty
joinType ctx tyS tyT = case () of 
  _ | subtype ctx tyS tyT -> tyT
  _ | subtype ctx tyT tyS -> tyS 
  _ -> let tyS' = unwrapTypeAbb ctx tyS
           tyT' = unwrapTypeAbb ctx tyT
    in  case (tyS', tyT') of 
           (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) ->
             TyArrow (meetType ctx tyT1 tyS1) (joinType ctx tyT2 tyS2)
           (TyRecord fS, TyRecord fT) -> let 
              lS = map fst fS 
              lT = map fst fT
              lJ = intersect lS lT 
              fJ = flip map lJ $ \ln ->fromJust $ do 
                tySn <- lookup ln fS 
                tyTn <- lookup ln fT 
                let tyJn =  joinType ctx tySn tyTn
                return $ (ln, tyJn)
             in TyRecord fJ
           (TyRef _, TyRef _) -> TyTop
           _          -> TyTop

meetType :: Context -> Ty -> Ty -> Ty
meetType ctx tyS tyT = case () of
  _ | subtype ctx tyS tyT -> tyS
  _ | subtype ctx tyT tyS -> tyT 
  _ -> let tyS' = unwrapTypeAbb ctx tyS
           tyT' = unwrapTypeAbb ctx tyT
    in  case (tyS', tyT') of 
           (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) ->
             TyArrow (joinType ctx tyT1 tyS1) (meetType ctx tyT2 tyS2)
           (TyRecord fS, TyRecord fT) -> let 
              lS = map fst fS 
              lT = map fst fT
              lM = union lS lT 
              fM = flip map lM $ \ln ->fromJust $ do 
                tySn <- lookup ln fS 
                tyTn <- lookup ln fT 
                let tyJn =  meetType ctx tySn tyTn
                return $ (ln, tyJn)
             in TyRecord fM
           (TyRef _, TyRef _) -> TyBot
           _          -> TyBot

-- typing

typeof :: Context -> Store -> Term -> Either IError Ty
typeof ctx store t = runReaderT (walk t) ctx where
  walk :: (MonadError IError m, MonadReader Context m) => Term -> m Ty
  walk t = do
    ctx <- ask
    let unary name t1 ty1 ty2 = do
          ty1' <- walk t1
          if typeEquals ctx ty1 ty1'
            then return ty2
            else throwError $ ETypeMismatch $ concat
              ["argument of ", name, " is not a number"]
    case t of
      TmVar i _      -> liftEither $ indexToBindingType ctx i
      TmAbs s ty1 t2 -> do
        let modifyCtx = addBinding s (VarBind ty1)
        ty2 <- typeShift (-1) <$> (local modifyCtx $ walk t2)
        return $ TyArrow ty1 ty2
      TmApp t1 t2 -> do
        ty1 <- walk t1
        ty2 <- walk t2
        case unwrapTypeAbb ctx ty1 of
          TyArrow ty11 ty12 | subtype ctx ty2 ty11 -> return ty12
          TyArrow _ _ -> throwError $ ETypeMismatch $ concat
            ["parameter type mismatch on \"", showTerm ctx t, "\""]
          TyBot -> return TyBot
          _ -> throwError $ ETypeMismatch $ concat
            ["expected arrow type on \"" ++ showTerm ctx t1 ++ "\""]
      TmLet s t1 t2 -> do
        ty1 <- walk t1
        let modifyCtx = addBinding s (VarBind ty1)
        typeShift (-1) <$> (local modifyCtx $ walk t2)
      TmRecord fields -> do
        fields' <- forM fields $ \(l, t) -> do
          ty <- walk t
          return (l, ty)
        return $ TyRecord fields'
      TmProj t1 l -> do
        ty1 <- walk t1
        case unwrapTypeAbb ctx ty1 of
          TyRecord fields ->
            let mty = snd <$> find ((l ==) . fst) fields
            in  maybe
                  (throwError $ EMisc $ concat ["field ", l, " not found"])
                  (liftEither . return)
                  mty
          TyBot -> return TyBot
          _ -> throwError $ ETypeMismatch $ "expected record type"
      TmTag l t1 ty1 -> case unwrapTypeAbb ctx ty1 of
        TyVariant fields -> do
          ty1Field <- walk t1
          let mty2 = snd <$> find ((l ==) . fst) fields
          ty2 <- maybe
            (throwError $ EUndefinedField $ concat ["field ", l, " not found"])
            (liftEither . return)
            mty2
          if subtype ctx ty1Field ty2
            then return ty1
            else throwError $ ETypeMismatch "field does not have expected type"
        _ -> throwError $ ETypeMismatch "tag annotation is not a variant type"
      TmCase t0 cases -> do
        ty0 <- walk t0
        case unwrapTypeAbb ctx ty0 of
          TyVariant fields -> do
            caseTypes <- forM cases $ \(ln, (xn, tn)) -> do
              tyN <- case lookup ln fields of
                Just ty -> return ty
                _       -> throwError $ EUndefinedField $ concat
                  ["label ", ln, " not in type"]
              let modifyCtx = addBinding xn (VarBind tyN)
              typeShift (-1) <$> (local modifyCtx $ walk tn)
            return $ foldl' (joinType ctx) TyBot caseTypes
          TyBot -> return TyBot
          _ -> throwError $ ETypeMismatch $ concat
            ["expected variant type on \"", showType ctx ty0, "\""]
      TmUnit            -> return TyUnit
      TmAscrib t1 tyAsc -> do
        ty1 <- walk t1
        unless (subtype ctx ty1 tyAsc) $ throwError $ ETypeMismatch
          "body of as-term does not have the expected type"
        return tyAsc
      TmTrue        -> return TyBool
      TmFalse       -> return TyBool
      TmIf t1 t2 t3 -> do
        ty1 <- walk t1
        ty2 <- walk t2
        ty3 <- walk t3
        if subtype ctx ty1 TyBool  
          then return $ joinType ctx ty2 ty3
        else throwError $ ETypeMismatch "guard of conditional not a boolean"
      TmZero             -> return TyNat
      TmSucc   t1        -> unary "succ" t1 TyNat TyNat
      TmPred   t1        -> unary "pred" t1 TyNat TyNat
      TmIsZero t1        -> unary "iszero" t1 TyNat TyBool
      TmString _         -> return TyString
      TmFloat  _         -> return TyFloat
      TmTimesFloat t1 t2 -> do
        ty1 <- walk t1
        ty2 <- walk t2
        if subtype ctx ty1 TyFloat && subtype ctx ty2 TyFloat
          then return TyFloat
          else throwError
            $ ETypeMismatch "argument of timesfloat is not a float"
      TmFix t1 -> do
        ty1 <- walk t1
        case ty1 of
          TyArrow ty11 ty12 -> if subtype ctx ty12 ty11
            then return ty12
            else throwError
              $ ETypeMismatch "result of body not compatible with domain"
          TyBot -> return TyBot
          _ -> throwError $ ETypeMismatch "arrow type expected"
      TmLoc l -> do
        (_, ty) <- liftEither $ locationToCell store l
        return $ TyRef ty
      TmRef t1 -> do
        ty1 <- walk t1
        return $ TyRef ty1
      TmDeref t1 -> unwrapTypeAbb ctx <$> walk t1 >>= \case
        TyRef ty11 -> return ty11
        TyBot -> return TyBot
        _          -> throwError $ ETypeMismatch "argument of ! is not a Ref"
      TmAssign t1 t2 -> do
        ty1 <- unwrapTypeAbb ctx <$> walk t1
        case ty1 of
          TyRef ty11 -> do
            ty2 <- walk t2
            if subtype ctx ty2 ty11
              then return TyUnit
              else throwError $ ETypeMismatch "arguments of := are incompatible"
          TyBot -> walk t2 >> return TyBot
          _ -> throwError $ ETypeMismatch "left-hand side of := is not a ref"
