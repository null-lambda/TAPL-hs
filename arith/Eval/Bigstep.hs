  module Eval.Bigstep
  ( Eval.isVal
  , Eval.isNumerical
  , eval
  )
where

import           Syntax
import           Eval                    hiding ( eval )


eval :: Term -> Term
eval t = case t of
  v | isVal v   -> v
  TmIf t1 t2 t3 -> case t1' of
    TmTrue | isVal v2  -> v2
    TmFalse | isVal v3 -> v3
    _                  -> t
   where
    t1' = eval t1
    v2  = eval t2
    v3  = eval t3
  TmSucc t1 -> case eval t1 of
    nv1 | isNumerical nv1 -> TmSucc nv1
    _                     -> t
  TmPred t1 -> case eval t1 of
    TmZero                       -> TmZero
    TmSucc nv1 | isNumerical nv1 -> nv1
    _                            -> t
  TmIsZero t1 -> case eval t1 of
    TmZero                       -> TmTrue
    TmSucc nv1 | isNumerical nv1 -> TmFalse
  _ -> t


