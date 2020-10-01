module Main where

import           Eval
import qualified Eval.Bigstep                  as BS
import           Syntax
import           SyntaxParser
import           Control.Monad
import           Data.Either
import           System.Environment
import           Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile] -> do
      result <- parse program "main" <$> readFile sourceFile
      case result of
        Right ts -> forM_ ts $ \term -> do
          let reducedTerm = eval term
          putStrLn
            $  "Eval:  " ++ show term
            ++ "\n   =>  "
            ++ show (eval term)
            ++ "  (Small-step)"
            ++ "\n   =>  "
            ++ show (BS.eval term)
            ++ "  (Big-step) "
        Left err -> print err
    _ -> putStrLn "stack run examples/test.f"


