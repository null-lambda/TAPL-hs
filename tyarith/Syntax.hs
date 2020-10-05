module Syntax where

import Data.Maybe

-- datatypes 

data Term
    = TmTrue
    | TmFalse
    | TmZero
    | TmIf Term Term Term
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    deriving Eq

data Ty
    = TyBool
    | TyNat
    deriving Eq

-- printing

instance Show Term where
  show TmTrue  = "true"
  show TmFalse = "false"
  show (TmIf t1 t2 t3) =
    "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show TmZero     = "0"
  show (TmSucc t) = fromMaybe ("(succ " ++ show t ++ ")") (showAsNum t 1)
   where
    showAsNum t acc = case t of
      TmZero    -> Just $ show acc
      TmSucc t1 -> showAsNum t1 (acc + 1)
      _         -> Nothing
  show (TmPred   t) = "(pred " ++ show t ++ ")"
  show (TmIsZero t) = "iszero " ++ show t

instance Show Ty where
  show TyBool = "bool"
  show TyNat  = "nat"
