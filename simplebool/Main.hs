module Main where

import           Syntax
import           Parser
import           Typechecker
import           Eval

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs )
import           System.IO.Error                ( tryIOError )
import           System.Timeout                 ( timeout )
import           Text.Megaparsec

processCommand :: Command -> StateT Context IO ()
processCommand cmd = do
  ctx <- get
  case cmd of
    CmdEval term -> liftIO $ do
      putStrLn $ "Eval: " ++ showTerm ctx term
      let resultOp = return $! showTerm ctx $ eval ctx term
      result <- fromMaybe "[infinite loop]" <$> timeout 5000 resultOp
      putStrLn $ "   => " ++ result
      putStrLn $ "    : " ++ either angular show (typeof ctx term)
      where angular s = "[" ++ s ++ "]"
    CmdBind s ty -> do
      let ctx' = (s, VarBind ty) : ctx
      put ctx'
      let showBind (s, b) = s ++ show b
      liftIO . putStrLn $ "Context : " ++ intercalate ", " (map showBind ctx')

processProgram :: String -> IO ()
processProgram s =
  let baseCtx = []
      p       = runStateT program baseCtx
  in  case runParser p "main" s of
        Right (cmds, st) -> loop cmds baseCtx         where
          loop :: [Command] -> Context -> IO ()
          loop (c : cs) ctx = do
            ctx' <- execStateT (processCommand c) ctx
            loop cs ctx'
          loop [] _ = return ()
        Left err -> putStrLn $ errorBundlePretty err

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


