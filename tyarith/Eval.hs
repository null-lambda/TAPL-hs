module Eval where

import           Control.Monad.Except           ( throwError )
import           Syntax
import           Typechecker

-- evaluation 

isnumerical :: Term -> Bool
isnumerical t = case t of
  TmZero    -> True
  TmSucc t1 -> isnumerical t1
  _         -> False

isval :: Term -> Bool
isval TmTrue  = True
isval TmFalse = True
isval t | isnumerical t = True
        | otherwise     = False

eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue  t2 _ ) = return t2
eval1 (TmIf TmFalse _  t3) = return t3
eval1 (TmIf t1      t2 t3) = do
  t1' <- eval1 t1
  return $ TmIf t1' t2 t3
eval1 (TmSucc   t1          ) = TmSucc <$> eval1 t1
eval1 (TmPred   TmZero      ) = return TmZero
eval1 (TmPred   (TmSucc nv1)) = return nv1
eval1 (TmPred   t1          ) = TmPred <$> eval1 t1
eval1 (TmIsZero TmZero      ) = return TmTrue
eval1 (TmIsZero (TmSucc nv1)) = return TmFalse
eval1 (TmIsZero t1          ) = TmIsZero <$> eval1 t1
eval1 _                       = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
