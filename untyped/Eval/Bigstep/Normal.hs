module Eval.Bigstep.Normal where

import           Syntax
import           Control.Applicative
import           Debug.Trace                    ( trace )

-- normal order evaluation 

eval :: Context -> Term -> Term
eval ctx t@(TmApp t1 t2) =
  let v1 = eval ctx t1
  in  case v1 of
        TmAbs s t12 -> eval ctx $ termSubstTop t2 t12
        _           -> let v2 = eval ctx t2 in TmApp v1 v2
eval ctx (TmAbs s t1) = TmAbs s (eval ctx t1)
eval ctx t            = t


