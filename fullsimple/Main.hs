module Main where

import           Eval
import           Syntax
import           SyntaxParser

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State
import           Data.List
import           Data.Maybe                     ( fromMaybe )
import           System.Environment
import           System.IO.Error                ( tryIOError )
import           System.Timeout                 ( timeout )
import           Text.Megaparsec
import           Text.Printf

processCommand :: Command -> StateT Context IO ()
processCommand cmd = do
  ctx <- get
  case cmd of
    CmdEval term -> liftIO $ do
      putStrLn $ "Eval: " ++ showTerm ctx term
      let resultOp = return $! showTerm ctx $ eval ctx term
      result <- fromMaybe "[infinite loop]" <$> timeout 5000 resultOp
      putStrLn $ "   => " ++ result
      putStrLn $ "    : " ++ either angular (showType ctx) (typeof ctx term)
      where angular s = "[" ++ s ++ "]"
    CmdBind s b -> case checkBinding ctx b of
      Right b' -> do
        modify ((s, b') :)
        ctx <- get
        -- liftIO . putStrLn $ "Bind : " ++ (s ++ showBinding (ctx b')
        liftIO . putStrLn $ "Context : " ++ showContext ctx
      Left err -> liftIO $ putStrLn err

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
        Left err -> do
          putStrLn "parse error"
          putStrLn $ errorBundlePretty err

main :: IO ()
main = do
  args     <- getArgs
  progName <- getProgName
  case args of
    [sourceFile] -> do
      result <- tryIOError (readFile sourceFile)
      case result of
        Left  err -> print err
        Right s   -> processProgram s
    _ -> printf "Usage: %s examples\\test.f\n" progName


