module Syntax where

import           Control.Monad.Except
import           Data.List
import           Data.Maybe
import           Debug.Trace

-- datatypes

data Ty
  = TyVar Int Int
  | TyArrow Ty Ty -- ty1 -> ty2
  | TyUnit -- ()
  | TyBool
  | TyNat
  | TyString
  | TyFloat

data Term
  = TmVar Int Int
  | TmAbs String Ty Term -- lambda x:T.t1 // \x:T.t1
  | TmApp Term Term -- t1 t2
  | TmLet String Term Term -- let x=t1 in t2
  | TmUnit -- unit // () 
  | TmTrue
  | TmFalse
  | TmIf Term Term Term -- if true then true else false
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  | TmString String
  | TmFloat Double
  | TmTimesFloat Term Term

data Binding
  = NameBind
  | VarBind Ty
  | TyVarBind
  | TmAbbBind Term (Maybe Ty)
  | TyAbbBind Ty

type Context = [(String, Binding)]

data Command
  = CmdEval Term
  | CmdBind String Binding

-- context 

indexToName :: Context -> Int -> String
indexToName ctx i = fst (ctx !! i)

nameToIndex :: Context -> String -> Maybe Int
nameToIndex ctx s = s `elemIndex` map fst ctx

isNameBound :: Context -> String -> Bool
isNameBound ctx s = s `elem` map fst ctx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx s = if isNameBound ctx s
  then pickFreshName ctx (s ++ "'")
  else ((s, NameBind) : ctx, s)

indexToBinding :: Context -> Int -> Binding
indexToBinding ctx i = bindingShift (i + 1) $ snd (ctx !! i)

indexToBindingType :: Context -> Int -> Either String Ty
indexToBindingType ctx i = case indexToBinding ctx i of
  VarBind ty            -> return ty
  TmAbbBind t (Just ty) -> return ty
  TmAbbBind t Nothing   -> Left $ "No type recorded for variable " ++ x
  _                     -> Left $ "wrong kind of binding for variable " ++ x
  where x = indexToName ctx i

-- shifting & substitution on de brujin index

typeMap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
typeMap onVar = walk where
  walk c ty = case ty of
    TyVar   i   n   -> onVar c i n
    TyArrow ty1 ty2 -> TyArrow (walk c ty1) (walk c ty2)
    TyUnit          -> TyUnit
    TyBool          -> TyBool
    TyNat           -> TyNat
    TyString        -> TyString
    TyFloat         -> TyFloat

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
  typeMap (\c i n -> if i >= c then TyVar (i + d) (n + d) else TyVar i (n + d))

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

termMap
  :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
termMap onVar onType = walk where
  walk c t = case t of
    TmVar i n          -> onVar c i n
    TmAbs x ty1 t2     -> TmAbs x (onType c ty1) (walk (c + 1) t2)
    TmApp t1 t2        -> TmApp (walk c t1) (walk c t2)
    TmLet s  t1 t2     -> TmLet s (walk c t1) (walk (c + 1) t2)
    TmIf  t1 t2 t3     -> TmIf (walk c t1) (walk c t2) (walk c t3)
    TmSucc   t1        -> TmSucc (walk c t1)
    TmPred   t1        -> TmPred (walk c t1)
    TmIsZero t1        -> TmIsZero (walk c t1)
    TmTimesFloat t1 t2 -> TmTimesFloat (walk c t1) (walk c t2)
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

-- printing

showType :: Context -> Ty -> String
showType = sp 0 where
  sp d ctx t = case t of
    TyVar i n -> if length ctx == n then indexToName ctx i else "[bad index]"
    TyArrow ty1 ty2 -> paren (d > 5) $ sp 6 ctx ty1 ++ "->" ++ sp 5 ctx ty2
    TyUnit          -> "()"
    TyBool          -> "Bool"
    TyNat           -> "Nat"
    TyString        -> "String"
    TyFloat         -> "Float"
  paren b s = if b then "(" ++ s ++ ")" else s

showTerm :: Context -> Term -> String
showTerm = sp 0 where
  -- precedence : Atomic 12 > App 11 > Abs If Let 10 
  sp d ctx t = case t of
    TmVar i n -> if length ctx == n then indexToName ctx i else "[bad index]"
    -- TmAbs{} ->
    --   let (varList, body) = walk ctx t
    --       walk ctx (TmAbs s ty t) =
    --           let (ctx', s'  ) = pickFreshName ctx s
    --               (vs  , body) = walk ctx' t
    --           in  ((s' ++ ":" ++ showType ctx ty) : vs, body)
    --       walk ctx t = ([], ". " ++ sp 10 ctx t)
    --   in  "\\" ++ intercalate "," varList ++ body
    TmAbs s ty t ->
      let (ctx', s') = pickFreshName ctx s
          sty        = showType ctx ty
          st         = sp 10 ctx' t
      in  paren (d > 10) $ "\\" ++ s ++ ":" ++ sty ++ ". " ++ st
    TmApp t1 t2 -> paren (d > 11) $ sp 11 ctx t1 ++ " " ++ sp 12 ctx t2
    TmLet s t1 t2 ->
      let (ctx', s') = pickFreshName ctx s
          s1         = sp 10 ctx t1
          s2'        = sp 10 ctx' t2
      in  paren (d > 10) $ "let " ++ s' ++ " = " ++ s1 ++ " in " ++ s2'
    TmUnit  -> "()"
    TmTrue  -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 ->
      let s1 = sp 10 ctx t1
          s2 = sp 10 ctx t2
          s3 = sp 10 ctx t3
      in  paren (d > 10) ("if " ++ s1 ++ " then " ++ s2 ++ " else " ++ s3)
    TmZero -> "0"
    TmSucc t1 ->
      fromMaybe (paren (d > 11) $ "succ " ++ sp 12 ctx t1) (showAsNum t1 1)
    TmPred   t1 -> paren (d > 11) $ "pred " ++ sp 12 ctx t1
    TmIsZero t1 -> paren (d > 11) $ "iszero " ++ sp 12 ctx t1
    TmString s  -> "\"" ++ s ++ "\""
    TmFloat  f  -> show f
    TmTimesFloat t1 t2 ->
      paren (d > 11) $ "timesfloat " ++ sp 12 ctx t1 ++ " " ++ sp 12 ctx t2

  paren b s = if b then "(" ++ s ++ ")" else s
  showAsNum t acc = case t of
    TmZero    -> Just $ show acc
    TmSucc t1 -> showAsNum t1 (acc + 1)
    _         -> Nothing

showBinding :: Context -> Binding -> String
showBinding ctx b = case b of
  NameBind        -> ""
  VarBind ty      -> " : " ++ showType ctx ty
  TyVarBind       -> ""
  TmAbbBind t mty -> " = " ++ showTerm ctx t ++ case mty of
    Just ty -> " : " ++ showType ctx ty
    Nothing -> ""
  TyAbbBind ty -> " = " ++ showType ctx ty

showContext :: Context -> String
showContext ctx =
  let walk ((s, b) : ctx') = (s ++ showBinding ctx' b) : walk ctx'
      walk []              = []
  in  "{" ++ intercalate ", " (reverse (walk ctx)) ++ "}"


