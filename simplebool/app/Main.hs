module Main where

import           Eval
import           Syntax
import           SyntaxParser

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs )
import           System.IO.Error                ( tryIOError )
import           System.Timeout                 ( timeout )
import           Text.Parsec                    ( runParser )

processCommand :: Command -> StateT Context IO ()
processCommand cmd = do
  ctx <- get
  case cmd of
    CmdEval term -> liftIO $ do
      putStrLn $ "Eval: " ++ showTerm ctx term
      let resultOp = return $! showTerm ctx $ eval ctx term
      result <- fromMaybe "[infinite loop]" <$> timeout 5000 resultOp
      putStrLn $ "   => " ++ result
      putStrLn $ "    : " ++ either id show (typeof ctx term)
    CmdBind s ty -> do
      let ctx' = (s, VarBind ty) : ctx
      put ctx'
      let showBind (s, b) = s ++ show b
      liftIO . putStrLn $ "Context : " ++ intercalate ", " (map showBind ctx')

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


