module Syntax where

data Term
    = TmTrue
    | TmFalse
    | TmZero
    | TmIf Term Term Term
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term