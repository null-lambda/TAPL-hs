module Main where

import           Eval
import           Syntax
import           SyntaxParser
import           Control.Monad
import           Data.Either
import           System.Environment
import           Text.Parsec

processTerm :: Term -> IO ()
processTerm term = do
  let reducedTerm = eval term
  let typeInfo    = either id show (typeof term)
  putStrLn $ show term ++ "\n  => " ++ show reducedTerm ++ " : " ++ typeInfo

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile] -> do
      result <- parse program "tyarith" <$> readFile sourceFile
      print 1234.2
      case result of
        Right ts  -> forM_ ts processTerm
        Left  err -> print err
    _ -> putStrLn "Usage: arith examples\\test.f"


