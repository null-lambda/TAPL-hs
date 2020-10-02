module Syntax where

import           Control.Monad.Except
import           Data.List

-- datatypes

data Term
    = TmVar Int Int
    | TmAbs String Ty Term
    | TmApp Term Term
    | TmTrue
    | TmFalse
    | TmIf Term Term Term
    deriving Eq

data Ty
  = TyBool
  | TyArrow Ty Ty
  deriving Eq

-- context

type Context = [(String, Binding)]
data Binding = NameBind | VarBind Ty deriving Eq

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

getBinding :: Context -> Int -> Binding
getBinding ctx i = snd (ctx !! i)

getType :: Context -> Int -> Either String Ty
getType ctx i = case getBinding ctx i of
  VarBind ty -> return ty
  NameBind   -> throwError "getType: wrong kind of binding for variable"

-- value 

isVal :: Context -> Term -> Bool
isVal ctx TmAbs{} = True
isVal ctx _       = False

-- command

data Command
  = CmdEval Term
  | CmdBind String Ty

-- term shifting & substitution

termMapOnVar :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
termMapOnVar onVar = walk where
  walk c t = case t of
    TmVar i n      -> onVar c i n
    TmAbs x ty1 t2 -> TmAbs x ty1 (walk (c + 1) t2)
    TmApp t1 t2    -> TmApp (walk c t1) (walk c t2)
    TmTrue         -> TmTrue
    TmFalse        -> TmFalse
    TmIf t1 t2 t3  -> TmIf (walk c t1) (walk c t2) (walk c t3)

termShift :: Int -> Term -> Term
termShift d = termMapOnVar
  (\c i n -> if i >= c then TmVar (i + d) (n + d) else TmVar i (n + d))
  0

termSubst :: Int -> Term -> Term -> Term
termSubst x s =
  termMapOnVar (\x i n -> if i == x then termShift x s else TmVar i n) x

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

-- printing

showTerm :: Context -> Term -> String
showTerm = sp 0 where
  -- precedence : App > Abs >>> If
  sp d ctx t = case t of
    TmTrue  -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 ->
      let [s1, s2, s3] = map (sp 2 ctx) [t1, t2, t3]
      in  paren (d > 1) $ "if " ++ s1 ++ " then " ++ s2 ++ " else " ++ s3
    TmVar i n -> if length ctx == n then indexToName ctx i else "[bad index]"
    TmAbs s ty t1 ->
      let varList ctx (TmAbs s ty t1) =
              let (ctx', s') = pickFreshName ctx s
              in  (ctx', (s', ty)) : varList ctx' t1
          varList ctx _ = []
          body (TmAbs s ty t1) = body t1
          body t               = t
          vs = map snd $ varList ctx t
          showTypedVar (s, ty) = s ++ ":" ++ show ty
          ctx' = fst . last $ varList ctx t
      in  paren (d > 4)
            $  ("\\" ++ unwords (map showTypedVar vs))
            ++ (". " ++ sp 5 ctx' (body t))
    TmApp t1 t2 -> paren (d > 7) $ sp 7 ctx t1 ++ " " ++ sp 8 ctx t2
  paren b s = if b then "(" ++ s ++ ")" else s

instance Show Ty where
  showsPrec d TyBool = (++) "Bool"
  showsPrec d (TyArrow ty1 ty2) =
    showParen (d > 5) $ showsPrec 6 ty1 . (++) "->" . showsPrec 5 ty2

instance Show Binding where
  show (VarBind ty) = ":" ++ show ty
  show NameBind     = ""
