module Main where

import           Eval
import           Syntax
import           SyntaxParser

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State
import           Data.List
import           System.Environment             ( getArgs )
import           System.IO.Error                ( tryIOError )
import           Text.Parsec                    ( runParser )

processCommand :: Command -> StateT Context IO ()
processCommand cmd = do
  ctx <- get
  case cmd of
    Eval term -> do
      let reducedTerm = eval ctx term
      liftIO
        .  putStrLn
        $  "Eval : "
        ++ showTerm ctx term
        ++ "\n    => "
        ++ showTerm ctx reducedTerm
    Bind s -> do
      let ctx' = s : ctx
      put ctx'
      liftIO . putStrLn $ "Context : " ++ intercalate ", " ctx'

processProgram :: String -> IO ()
processProgram s = case runParser program [] "main" s of
  Right cmds -> loop cmds []   where
    loop :: [Command] -> Context -> IO ()
    loop (c : cs) ctx = do
      ctx' <- execStateT (processCommand c) ctx
      loop cs ctx'
    loop [] _ = return ()
  Left err -> print err

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile] -> do
      result <- tryIOError (readFile sourceFile)
      case result of
        Left  err -> print err
        Right s   -> processProgram s
    _ -> putStrLn "examples: untyped-exe examples\\test.f"


