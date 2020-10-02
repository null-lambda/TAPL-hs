module Eval.Bigstep.CallByName where

import           Syntax
import           Control.Applicative
import           Debug.Trace                    ( trace )

-- call-by-name evaluation 

eval :: Context -> Term -> Term
eval ctx t@(TmApp t1 t2) = 
  let v1 = eval ctx t1 
  in case v1 of 
    TmAbs s t12 -> eval ctx $ termSubstTop t2 t12
    _ -> t
eval ctx t = t


