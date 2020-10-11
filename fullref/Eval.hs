{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Eval where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import           Syntax
import           Typechecker

-- value 

isNatural :: Term -> Bool
isNatural t = case t of
  TmZero    -> True
  TmSucc t1 -> isNatural t1
  _         -> False

isVal :: Term -> Bool
isVal t = case t of
  TmAbs{}         -> True
  TmRecord fields -> all (isVal . snd) fields
  TmTag _ t1 _    -> isVal t1
  TmUnit          -> True
  TmTrue          -> True
  TmFalse         -> True
  TmString _      -> True
  TmFloat  _      -> True
  TmLoc    _      -> True
  t | isNatural t -> True
  _               -> False

-- call-by-value evaluation 

eval1 :: Context -> Store -> Term -> Either IError (Term, Store)
eval1 ctx store t = runStateT (walk t) store where
  walk :: (MonadError IError m, MonadState Store m) => Term -> m Term
  walk t = do
    case t of
      TmVar i _ -> case indexToBinding ctx i of
        TmAbbBind t1 _ -> return t1
        _              -> throwError ENoRuleApplies
      TmApp (TmAbs _ _ t12) v2 | isVal v2 -> return $ termSubstTop v2 t12
      TmApp v1 t2 | isVal v1              -> do
        t2' <- walk t2
        return $ TmApp v1 t2'
      TmApp t1 t2 -> do
        t1' <- walk t1
        return $ TmApp t1' t2
      TmLet _ v1 t2 | isVal v1 -> return $ termSubstTop v1 t2
      TmLet s t1 t2            -> do
        t1' <- walk t1
        return $ TmLet s t1' t2
      TmRecord fields -> do
        let evalFields1 fields = case fields of
              []                         -> throwError ENoRuleApplies
              ((ln, vn) : fs) | isVal vn -> do
                fs' <- evalFields1 fs
                return ((ln, vn) : fs')
              ((ln, tn) : fs) -> do
                tn' <- walk tn
                return ((ln, tn') : fs)
        fields' <- evalFields1 fields
        return (TmRecord fields')
      TmProj v1@(TmRecord fields) l | isVal v1 -> case lookup l fields of
        Just ty1 -> return ty1
        Nothing ->
          throwError $ EUndefinedField $ "projection of undefined field " ++ l
      TmProj t1 l -> do
        t1' <- walk t1
        return $ TmProj t1' l
      TmTag l t1 ty -> do
        t1' <- walk t1
        return $ TmTag l t1' ty
      TmCase (TmTag ln v1 ty0) cases | isVal v1 -> do
        (_, tn) <- case lookup ln cases of
          Just ty -> return ty
          Nothing -> throwError $ EUndefinedField $ concat
            ["label ", ln, " is not in in type ", showType ctx ty0]
        return $ termSubstTop v1 tn
      TmCase t1 cases -> do
        t1' <- walk t1
        return $ TmCase t1' cases
      TmAscrib v1 _ | isVal v1 -> return v1 
      TmAscrib t1 ty1 -> do 
        t1' <- walk t1 
        return $ TmAscrib t1' ty1
      TmIf TmTrue  t2 _  -> return t2
      TmIf TmFalse _  t3 -> return t3
      TmIf t1      t2 t3 -> do
        t1' <- walk t1
        return $ TmIf t1' t2 t3
      TmSucc   t1                            -> TmSucc <$> walk t1
      TmPred   TmZero                        -> return TmZero
      TmPred   (TmSucc nv1)                  -> return nv1
      TmPred   t1                            -> TmPred <$> walk t1
      TmIsZero TmZero                        -> return TmTrue
      TmIsZero (TmSucc _)                    -> return TmFalse
      TmIsZero t1                            -> TmIsZero <$> walk t1
      TmTimesFloat (TmFloat f1) (TmFloat f2) -> return $ TmFloat (f1 * f2)
      TmTimesFloat (TmFloat f1) t2           -> do
        t2' <- walk t2
        return $ TmTimesFloat (TmFloat f1) t2'
      TmTimesFloat t1 t2 -> do
        t1' <- walk t1
        return $ TmTimesFloat t1' t2
      TmFix (TmAbs _ _ t12) -> return $ termSubstTop t t12
      TmFix t1              -> TmFix <$> walk t1
      TmRef v1 | isVal v1   -> do
        ty1 <- liftEither $ typeof ctx store v1
        let (loc, store') = addRef v1 ty1 store
        put store'
        return loc
      TmRef t1              -> TmRef <$> walk t1
      TmDeref v1 | isVal v1 -> case v1 of
        TmLoc l -> do
          (tDeref, _) <- liftEither $ locationToCell store l
          return tDeref
        _ -> throwError ENoRuleApplies
      TmDeref t1                       -> TmDeref <$> walk t1
      TmAssign (TmLoc l) v2 | isVal v2 -> do
        store' <- liftEither $ updateCell l v2 store
        put store'
        return TmUnit
      TmAssign v1 t2 | isVal v1 -> do
        t2' <- walk t2
        return $ TmAssign v1 t2'
      TmAssign t1 t2 -> do
        t1' <- walk t1
        return $ TmAssign t1' t2
      _ -> throwError ENoRuleApplies

eval :: Context -> Store -> Term -> Either IError (Term, Store)
eval ctx store t = case eval1 ctx store t of
  Right (t, store')    -> eval ctx store' t
  Left  ENoRuleApplies -> return (t, store)
  Left  err            -> Left err

-- fix bindings 

checkBinding :: Context -> Store -> Binding -> Either IError Binding
checkBinding ctx store = assign where
  assign (TmAbbBind t mty) = do
    ty  <- typeof ctx store t
    ty' <- case mty of
      Just ty' -> do
        unless (typeEquals ctx ty ty') $ throwError $ ETypeMismatch
          "Type of binding does not match declared type"
        return ty'
      Nothing -> return ty
    return $ TmAbbBind t (Just ty')
  assign b = return b

evalBinding :: Context -> Store -> Binding -> Either IError (Store, Binding)
evalBinding ctx store b = case b of
  TmAbbBind t mty -> do
    (t', store') <- eval ctx store t
    return (store', TmAbbBind t' mty)
  _ -> return (store, b)
