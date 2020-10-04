module Syntax where

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