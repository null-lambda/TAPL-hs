module Eval where
import           Syntax

isNumerical :: Term -> Bool
isNumerical TmZero      = True
isNumerical (TmSucc t1) = isNumerical t1
isNumerical _           = False

isVal :: Term -> Bool
isVal TmTrue  = True
isVal TmFalse = True
isVal t | isNumerical t = True
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

instance Show Term where
  show TmTrue  = "true"
  show TmFalse = "false"
  show (TmIf t1 t2 t3) =
    "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show TmZero = "0"
  show (TmSucc t) | isNumerical t = showAsNum t 1
                  | otherwise     = "(succ " ++ show t ++ ")"
   where
    showAsNum TmZero     num = show num
    showAsNum (TmSucc t) num = showAsNum t (num + 1)
  show (TmPred   t) = "(pred " ++ show t ++ ")"
  show (TmIsZero t) = "iszero " ++ show t
