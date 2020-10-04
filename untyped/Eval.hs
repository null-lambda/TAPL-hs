module Eval where

import           Syntax
import           Control.Applicative
import           Control.Monad
import           Debug.Trace                    ( trace )

-- call-by-value evaluation 

eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TmApp (TmAbs s t12) v2) | isVal ctx v2 = return $ termSubstTop v2 t12
eval1 ctx (TmApp t1 t2)                           = eApp1 <|> eApp2 where
  eApp1 = do
    t1' <- eval1 ctx t1
    return $ TmApp t1' t2
  eApp2 = do
    guard $ isVal ctx t1
    t2' <- eval1 ctx t2
    return $ TmApp t1 t2'
eval1 ctx _ = Nothing

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 ctx t)
-- eval ctx t = maybe t (\t -> trace ("step: " ++ showTerm ctx t) $ eval ctx t) (eval1 ctx t)


