module Helper where

-- monadic and with short-circuit

andM :: Monad m => [m Bool] -> m Bool
andM = foldr (&&&) (return True)
  where ma &&& mb = ma >>= \p -> if p then mb else return p

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM mb mt mf = mb >>= \b -> if b then mt else mf

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb ma = mb >>= \b -> if b then return () else ma
