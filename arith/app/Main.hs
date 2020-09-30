module Main where

import           Eval
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
      result <- parse program "arith" <$> readFile sourceFile
      case result of
        Right ts -> forM_ ts $ \term -> do
          let reducedTerm = eval term
          putStrLn
            $  show term
            ++ "\n  => "
            ++ show reducedTerm
            ++ (if isval reducedTerm then "" else " (not a value)")
        Left err -> print err
    _ -> putStrLn "stack run examples/test.f"


