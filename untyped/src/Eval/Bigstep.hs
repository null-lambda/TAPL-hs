module Eval.Bigstep where

import           Syntax
import           Control.Applicative
import           Debug.Trace                    ( trace )

-- call-by-value evaluation 

eval :: Context -> Term -> Term
eval ctx t@(TmApp t1 t2) = 
  let v1 = eval ctx t1 
      v2 = eval ctx t2 
  in case v1 of 
    TmAbs s t12 -> eval ctx $ termSubstTop v2 t12
    _ -> t
eval ctx t = t
