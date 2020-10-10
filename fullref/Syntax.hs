{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Syntax where

import           Control.Monad.Except
import           Data.Bifunctor
import           Data.List
import           Data.Maybe
import qualified Data.IntMap                   as IntMap
import           Text.Read

-- datatypes

data Ty
  = TyVar Int Int
  | TyArrow Ty Ty -- ty1 -> ty2
  | TyRecord [(String, Ty)] -- {l1:T1, ..., ln:Tn} // default value of ln = n
  | TyVariant [(String, Ty)] -- <l1:T1, ..., ln:Tn>
  | TyUnit -- ()
  | TyBool
  | TyNat
  | TyString
  | TyFloat
  | TyRef Ty
  deriving Show

data Term
  = TmVar Int Int
  | TmAbs String Ty Term -- lambda x:T.t1 // \x:T.t1
  | TmApp Term Term -- t1 t2
  | TmLet String Term Term -- let x=t1 in t2
  | TmRecord [(String, Term)] -- {l1=t1, ..., ln=tn} // default value of ln = n
  | TmProj Term String   -- t1.ln
  | TmTag String Term Ty -- <l=t> as T
  | TmCase Term [(String, (String, Term))] {- case t1 of <l1=x1> ...  t t -}
  | TmAscrib Term Ty
  | TmUnit -- unit // () 
  | TmTrue
  | TmFalse
  | TmIf Term Term Term -- if t1 then t2 else t3
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  | TmString String
  | TmFloat Double
  | TmTimesFloat Term Term
  | TmFix Term
  | TmLoc Int
  | TmRef Term
  | TmDeref Term
  | TmAssign Term Term

data Binding
  = NameBind
  | VarBind Ty
  | TyVarBind
  | TmAbbBind Term (Maybe Ty)
  | TyAbbBind Ty

type Context = [(String, Binding)]

type Store = IntMap.IntMap (Term, Ty)

data Command
  = CmdEval Term
  | CmdBind String Binding

data IError
  = EParse String
  | ETypeMismatch String
  | EUnboundVar String
  | EUndefinedField String
  | EMemoryViolation String
  | ENoRuleApplies
  | ETimeout
  | EMisc String


-- context 

emptyContext :: Context
emptyContext = []

addBinding :: String -> Binding -> Context -> Context
addBinding s b = ((s, b) :)

indexToName :: Context -> Int -> String
indexToName ctx i = fst (ctx !! i)

nameToIndex :: Context -> String -> Maybe Int
nameToIndex ctx s = s `elemIndex` map fst ctx

isNameBound :: Context -> String -> Bool
isNameBound ctx s = s `elem` map fst ctx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx s = if isNameBound ctx s && s /= "_"
  then pickFreshName ctx (s ++ "'")
  else ((s, NameBind) : ctx, s)

indexToBinding :: Context -> Int -> Binding
indexToBinding ctx i = bindingShift (i + 1) $ snd (ctx !! i)

indexToBindingType :: Context -> Int -> Either IError Ty
indexToBindingType ctx i = case indexToBinding ctx i of
  VarBind ty            -> return ty
  TmAbbBind _ (Just ty) -> return ty
  TmAbbBind _ Nothing ->
    throwError $ EMisc $ "No type recorded for variable " ++ x
  _ -> throwError $ EMisc $ "wrong kind of binding for variable " ++ x
  where x = indexToName ctx i

-- store 

emptyStore :: Store
emptyStore = IntMap.empty

addRef :: Term -> Ty -> Store -> (Term, Store)
addRef t ty store =
  let n      = IntMap.size store
      store' = IntMap.insert n (t, ty) store
  in  (TmLoc n, store')

updateCell :: Int -> Term -> Store -> Either IError Store
updateCell l v store = if l `IntMap.member` store
  then return $ IntMap.adjust (\(_, ty) -> (v, ty)) l store
  else throwError $ EMemoryViolation $ ""

locationToCell :: Store -> Int -> Either IError (Term, Ty)
locationToCell store i = case IntMap.lookup i store of
  Just cell -> return cell
  Nothing   -> throwError $ EMemoryViolation $ ""

-- shifting & substitution on de brujin index

typeMap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
typeMap onVar = walk where
  walk c ty = case ty of
    TyVar   i   n    -> onVar c i n
    TyArrow ty1 ty2  -> TyArrow (walk c ty1) (walk c ty2)
    TyRecord  fields -> TyRecord (map (second $ walk c) fields)
    TyVariant fields -> TyVariant (map (second $ walk c) fields)
    TyUnit           -> TyUnit
    TyBool           -> TyBool
    TyNat            -> TyNat
    TyString         -> TyString
    TyFloat          -> TyFloat
    TyRef ty1        -> TyRef (walk c ty1)

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
  typeMap (\c i n -> if i >= c then TyVar (i + d) (n + d) else TyVar i (n + d))

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

termMap
  :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
termMap onVar onType = walk where
  walk c t = case t of
    TmVar i n       -> onVar c i n
    TmAbs x ty1 t2  -> TmAbs x (onType c ty1) (walk (c + 1) t2)
    TmApp t1 t2     -> TmApp (walk c t1) (walk c t2)
    TmLet s t1 t2   -> TmLet s (walk c t1) (walk (c + 1) t2)
    TmRecord fields -> TmRecord (map (second $ walk c) fields)
    TmProj t1 l     -> TmProj (walk c t1) l
    TmTag l t1 ty1  -> TmTag l (walk c t1) (onType c ty1)
    TmCase t1 cases -> TmCase
      (walk c t1)
      (map (\(ln, (xn, tn)) -> (ln, (xn, walk (c + 1) tn))) cases)
    TmAscrib t1 ty1    -> TmAscrib (walk c t1) (onType c ty1)
    TmIf t1 t2 t3      -> TmIf (walk c t1) (walk c t2) (walk c t3)
    TmSucc   t1        -> TmSucc (walk c t1)
    TmPred   t1        -> TmPred (walk c t1)
    TmIsZero t1        -> TmIsZero (walk c t1)
    TmTimesFloat t1 t2 -> TmTimesFloat (walk c t1) (walk c t2)
    TmFix   t1         -> TmFix (walk c t1)
    TmRef   t1         -> TmRef (walk c t1)
    TmDeref t1         -> TmDeref (walk c t1)
    TmAssign t1 t2     -> TmAssign (walk c t1) (walk c t2)
    _                  -> t -- literal values 

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d = termMap
  (\c i n -> if i >= c then TmVar (i + d) (n + d) else TmVar i (n + d))
  (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j ts = termMap
  (\c i n -> if i == j + c then termShift c ts else TmVar i n)
  (const id)
  j

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

bindingShift :: Int -> Binding -> Binding
bindingShift d b = case b of
  VarBind ty            -> VarBind (typeShift d ty)
  TmAbbBind t (Just ty) -> TmAbbBind (termShift d t) (Just (typeShift d ty))
  TyAbbBind ty          -> TyAbbBind (typeShift d ty)
  _                     -> b

storeShift :: Int -> Store -> Store
storeShift d = fmap (\(t, ty) -> (termShift d t, typeShift d ty))

-- printing

showType :: Context -> Ty -> String
{- 
(precedence):
  Inf Atom Parens Brackets Tag
  15 Arrow 
  11 Application 
-}
showType = sp 0 where
  parenIf b s = if b then "(" ++ s ++ ")" else s
  sp d ctx t =
    let sp0 d t = sp d ctx t
    in
      case t of
        TyVar i n ->
          if length ctx == n then indexToName ctx i else "[bad index]"
        TyArrow ty1 ty2 ->
          concat [parenIf (d > 15) $ concat [sp0 16 ty1, "->", sp0 15 ty2]]
        TyRecord fields -> concat
          ["{" ++ intercalate ", " (map showRec fields) ++ "}"]
         where
          showRec (l, ty) = case readMaybe l :: Maybe Int of
            Just i | i >= 1 -> sp0 0 ty
            _               -> concat [l, ":", sp0 0 ty]
        TyVariant fields -> concat
          ["<" ++ intercalate ", " (map showRec fields) ++ ">"]
          where showRec (l, ty) = l ++ ":" ++ sp0 0 ty
        TyUnit   -> "()"
        TyBool   -> "Bool"
        TyNat    -> "Nat"
        TyString -> "String"
        TyFloat  -> "Float"
        TyRef ty -> "ref " ++ sp0 11 ty

showTerm :: Context -> Term -> String
{-   
(check grammar section in Parser.hs)
precedence : 
  Inf Atom Parens Brackets Tag 
  21  Proj
  20  Ascription
  11  Application Ref Deref
  10  Abs If Let Case Assignment
  0~9 Binary operators
-}
showTerm = sp 0 where
  sp d ctx t =
    let
      sp0 d t = sp d ctx t
      parenIf b s = if b then "(" ++ s ++ ")" else s
      showAsNum t acc = case t of
        TmZero    -> Just $ show acc
        TmSucc t1 -> showAsNum t1 (acc + 1)
        _         -> Nothing
      unary name d t1 = parenIf (d > 11) $ name ++ sp0 12 t1
    in
      case t of
        TmVar i n ->
          if length ctx == n then indexToName ctx i else "[bad index]"
        TmAbs x ty t ->
          let (ctx', x') = pickFreshName ctx x
              sty        = showType ctx ty
              st         = sp 10 ctx' t
          in  parenIf (d > 10) $ concat ["\\", x', ":", sty, ". ", st]
        TmApp t1 t2 ->
          concat [parenIf (d > 11) $ concat [sp0 11 t1, " ", sp0 12 t2]]
        TmLet x t1 t2 ->
          let (ctx', x') = pickFreshName ctx x
              s1         = sp0 10 t1
              s2'        = sp 10 ctx' t2
          in  parenIf (d > 10) $ concat ["let ", x', " = ", s1, " in ", s2']
        TmRecord fields -> concat
          ["{", intercalate ", " (map showRec fields), "}"]
         where
          showRec (l, t) = case readMaybe l :: Maybe Int of
            Just i | i > 0 -> sp0 0 t
            _              -> concat [l, "=", sp0 0 t]
        TmProj t1 l ->
          let st1 = sp0 21 t1 in parenIf (d > 21) $ concat [st1, ".", l]
        TmTag l t1 tyT ->
          let st1  = sp0 0 t1
              styT = showType ctx tyT
          in  concat ["<", l, "=", st1, "> as ", styT]
        TmCase t0 cases ->
          let st = sp0 10 t0
              showCase (ln, (xn, tn)) =
                  let (ctx', xn') = pickFreshName ctx xn
                      stn         = sp 10 ctx' tn
                  in  concat ["<", ln, "=", xn', "> => ", stn]
              scases = intercalate " | " $ map showCase cases
          in  parenIf (d > 10) $ concat ["case ", st, " of ", scases]
        TmAscrib t1 ty1 ->
          let st1  = sp0 20 t1
              sty1 = showType ctx ty1
          in  parenIf (d > 20) $ concat [st1, " as ", sty1]
        TmUnit  -> "()"
        TmTrue  -> "true"
        TmFalse -> "false"
        TmIf t1 t2 t3 ->
          let s1 = sp0 10 t1
              s2 = sp0 10 t2
              s3 = sp0 10 t3
          in  parenIf (d > 10) $ concat ["if ", s1, " then ", s2, " else ", s3]
        TmZero      -> "0"
        TmSucc   t1 -> fromMaybe (unary "succ " d t1) (showAsNum t1 1)
        TmPred   t1 -> unary "pred " d t1
        TmIsZero t1 -> unary "iszero " d t1
        TmString s  -> concat ["\"", s, "\""]
        TmFloat  f  -> show f
        TmTimesFloat t1 t2 ->
          parenIf (d > 11) $ concat ["timesfloat ", sp0 12 t1, " ", sp0 12 t2]
        TmFix   t1 -> unary "fix" d t1
        TmLoc   i  -> concat ["<loc #", show i, ">"]
        TmRef   t  -> parenIf (d > 11) $ concat ["ref ", sp0 12 t]
        TmDeref t  -> parenIf (d > 11) $ concat ["!", sp0 12 t]
        TmAssign t1 t2 ->
          parenIf (d > 10) $ concat [sp0 10 t1, " := ", sp0 10 t2]

showBinding :: Context -> Binding -> String
showBinding ctx b = case b of
  NameBind   -> ""
  VarBind ty -> " : " ++ showType ctx ty
  TyVarBind  -> ""
  TmAbbBind t mty ->
    let sty = case mty of
          Just ty -> " : " ++ showType ctx ty
          Nothing -> ""
    in  concat [" = ", showTerm ctx t, sty]
  TyAbbBind ty -> " = " ++ showType ctx ty

showContext :: Context -> String
showContext ctx =
  let walk ((s, b) : ctx') = (s ++ showBinding ctx' b) : walk ctx'
      walk []              = []
  in  "{" ++ intercalate ", " (reverse (walk ctx)) ++ "}"


instance Show IError where
  show = \case
    EParse           s -> concat ["Parse failed\n", s]
    ETypeMismatch    s -> concat ["Type mismatch \n", s]
    EUnboundVar      s -> concat ["Unbound variable \n", s]
    EUndefinedField  s -> concat ["Undefined label\n", s]
    EMemoryViolation s -> concat ["Memory Violation\n", s]
    ENoRuleApplies     -> undefined
    ETimeout           -> concat ["Timeout"]
    EMisc s            -> s
