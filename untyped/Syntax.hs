module Syntax where

import           Data.List

-- datatypes

data Term
    = TmVar Int Int
    | TmAbs String Term
    | TmApp Term Term

isVal :: Context -> Term -> Bool
isVal ctx (TmAbs _ _) = True
isVal ctx _           = False

-- context

type Context = [String]

indexToName :: Context -> Int -> String
indexToName ctx i = ctx !! i

nameToIndex :: Context -> String -> Maybe Int
nameToIndex ctx s = elemIndex s ctx

isNameBound :: Context -> String -> Bool
isNameBound ctx s = s `elem` ctx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx s =
  if isNameBound ctx s then pickFreshName ctx (s ++ "'") else (s : ctx, s)

-- command

data Command
  = Eval Term
  | Bind String

-- term shifting & substitution

termShift :: Int -> Term -> Term
termShift d = walk 0 where
  walk c (TmVar i n) =
    if i >= c then TmVar (i + d) (n + d) else TmVar i (n + d)
  walk c (TmAbs s  t1) = TmAbs s (walk (c + 1) t1)
  walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst x s = walk 0 where
  walk c (TmVar i  n ) = if i == x + c then termShift c s else TmVar i n
  walk c (TmAbs s  t1) = TmAbs s (walk (c + 1) t1)
  walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

-- printing

{-
showTerm :: Context -> Term -> String
showTerm ctx (TmVar i n) =
  if length ctx == n then indexToName ctx i else "[bad index]"
showTerm ctx (TmAbs s t1) =
  let (ctx', s') = pickFreshName ctx s
  in  "(\\" ++ s' ++ "." ++ showTerm ctx' t1 ++ ")"
showTerm ctx (TmApp t1 t2) =
  "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"
-}

showTerm :: Context -> Term -> String
showTerm ctx t = case t of
  TmVar i n -> if length ctx == n then indexToName ctx i else "[bad index]"
  TmAbs s t1 ->
    let varList ctx (TmAbs s t1) =
            let (ctx', s') = pickFreshName ctx s in (ctx', s') : varList ctx' t1
        varList ctx _ = []
        body (TmAbs s t1) = body t1
        body t            = t
        vs   = map snd $ varList ctx t
        ctx' = fst . last $ varList ctx t
    in  "\\" ++ unwords vs ++ "." ++ showTerm ctx' (body t)
  TmApp t1 t2 ->
    let appList (TmApp t1 t2) = t2 : appList t1
        appList t            = [t]
    in  unwords $ map (checkedShowTerm ctx) $ reverse $ appList t
 where
  isVar (TmVar _ _) = True
  isVar _           = False
  parens s = "(" ++ s ++ ")"
  checkedShowTerm ctx t = (if isVar t then id else parens) (showTerm ctx t)


