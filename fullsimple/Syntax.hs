module Syntax where

import           Data.Bifunctor
import           Data.List
import           Data.Maybe
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

indexToBindingType :: Context -> Int -> Either String Ty
indexToBindingType ctx i = case indexToBinding ctx i of
  VarBind ty            -> return ty
  TmAbbBind _ (Just ty) -> return ty
  TmAbbBind _ Nothing   -> Left $ "No type recorded for variable " ++ x
  _                     -> Left $ "wrong kind of binding for variable " ++ x
  where x = indexToName ctx i

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
    TmFix t1           -> TmFix (walk c t1)
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
    TyArrow ty1 ty2 ->
      concat [parenIf (d > 5) $ concat [sp 6 ctx ty1, "->", sp 5 ctx ty2]]
    TyRecord fields -> concat
      ["{" ++ intercalate ", " (map showRec fields) ++ "}"]
     where
      showRec (l, ty) = case readMaybe l :: Maybe Int of
        Just i | i >= 1 -> showType ctx ty
        _               -> concat [l, ":", showType ctx ty]
    TyVariant fields -> concat
      ["<" ++ intercalate ", " (map showRec fields) ++ ">"]
      where showRec (l, ty) = l ++ ":" ++ showType ctx ty
    TyUnit   -> "()"
    TyBool   -> "Bool"
    TyNat    -> "Nat"
    TyString -> "String"
    TyFloat  -> "Float"
  parenIf b s = if b then "(" ++ s ++ ")" else s

{-   
(check grammar section in Parser.hs)
precedence : 
  Inf Atom Parens Brackets Tag 
  21  Proj
  20  Ascription
  11  Application
  10  Abs If Let Case
  0~9 Binary operators
-}
showTerm :: Context -> Term -> String
showTerm = sp 0 where
  parenIf b s = if b then "(" ++ s ++ ")" else s
  dInf = 10000
  showAsNum t acc = case t of
    TmZero    -> Just $ show acc
    TmSucc t1 -> showAsNum t1 (acc + 1)
    _         -> Nothing
  unary name d ctx t1 = parenIf (d > 11) $ name ++ sp 12 ctx t1
  sp d ctx t = case t of
    TmVar i n -> if length ctx == n then indexToName ctx i else "[bad index]"
    TmAbs x ty t ->
      let (ctx', x') = pickFreshName ctx x
          sty        = showType ctx ty
          st         = sp 10 ctx' t
      in  parenIf (d > 10) $ concat ["\\", x', ":", sty, ". ", st]
    TmApp t1 t2 ->
      concat [parenIf (d > 11) $ concat [sp 11 ctx t1, " ", sp 12 ctx t2]]
    TmLet x t1 t2 ->
      let (ctx', x') = pickFreshName ctx x
          s1         = sp 10 ctx t1
          s2'        = sp 10 ctx' t2
      in  parenIf (d > 10) $ concat ["let ", x', " = ", s1, " in ", s2']
    TmRecord fields -> concat ["{", intercalate "," (map showRec fields), "}"]
     where
      showRec (l, t) = case readMaybe l :: Maybe Int of
        Just i | i > 0 -> sp dInf ctx t
        _              -> concat [l, "=", sp dInf ctx t]
    TmProj t1 l ->
      let st1 = sp 21 ctx t1 in parenIf (d > 21) $ concat [st1, ".", l]
    TmTag l t1 tyT ->
      let st1  = sp dInf ctx t1
          styT = showType ctx tyT
      in  concat ["<", l, "=", st1, "> as ", styT]
    TmCase t0 cases ->
      let st = sp 10 ctx t0
          showCase (ln, (xn, tn)) =
              let (ctx', xn') = pickFreshName ctx xn
                  stn         = sp 10 ctx' tn
              in  concat ["<", ln, "=", xn', "> => ", stn]
          scases = intercalate " | " $ map showCase cases
      in  parenIf (d > 10) $ concat ["case ", st, " of ", scases]
    TmAscrib t1 ty1 ->
      let st1  = sp 20 ctx t1
          sty1 = showType ctx ty1
      in  parenIf (d > 20) $ concat [st1, " as ", sty1]
    TmUnit  -> "()"
    TmTrue  -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 ->
      let s1 = sp 10 ctx t1
          s2 = sp 10 ctx t2
          s3 = sp 10 ctx t3
      in  parenIf (d > 10) $ concat ["if ", s1, " then ", s2, " else ", s3]
    TmZero             -> "0"
    TmSucc   t1        -> fromMaybe (unary "succ " d ctx t1) (showAsNum t1 1)
    TmPred   t1        -> unary "pred " d ctx t1
    TmIsZero t1        -> unary "iszero " d ctx t1
    TmString s         -> concat ["\"", s, "\""]
    TmFloat  f         -> show f
    TmTimesFloat t1 t2 -> parenIf (d > 11)
      $ concat ["timesfloat ", sp 12 ctx t1, " ", sp 12 ctx t2]
    TmFix t1 -> unary "fix" d ctx t1

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


