module Main where

import           Eval
import           Syntax
import           Parser
import           Typechecker

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State
import           Data.List
import           Data.Maybe                     ( fromMaybe )
import           System.Environment
import System.IO
import           System.IO.Error                ( tryIOError )
import           System.Timeout                 ( timeout )
import           Text.Printf

processCommand :: Command -> StateT Context IO ()
processCommand cmd = do
  ctx <- get
  case cmd of
    CmdEval term -> liftIO $ do
      putStrLn $ "> Eval : " ++ showTerm ctx term
      let resultOp = return $! showTerm ctx $ eval ctx term
      result <- fromMaybe "[timeout]" <$> timeout 3000000 resultOp
      putStrLn $ "   => " ++ result
      putStrLn $ "    : " ++ either angular (showType ctx) (typeof ctx term)
      where angular s = "[" ++ s ++ "]"
    CmdBind s b -> case checkBinding ctx b of
      Right b' -> do
        ctx <- get
        let ctx' = (s, b') : ctx
        put ctx'
        -- liftIO . putStrLn $ "Context : " ++ showContext ctx'

        liftIO . putStrLn $ "> Bind : " ++ (s ++ showBinding ctx b')
      Left err -> liftIO . putStrLn $ "> *Err : " ++ err

processProgram :: String -> IO ()
processProgram input =
  case runParser program [] "main" input of
        Right (cmds, _) -> loop cmds []         where
          loop :: [Command] -> Context -> IO ()
          loop (c : cs) ctx = do
            ctx' <- execStateT (processCommand c) ctx
            loop cs ctx'
          loop [] _ = return ()
        Left err -> do
          putStrLn "parse error"
          putStrLn err

main :: IO ()
main = do
  args     <- getArgs
  progName <- getProgName
  case args of
    [sourceFile] -> do
      result <- tryIOError $ do
        inputHandle <- openFile sourceFile ReadMode
        hSetEncoding inputHandle utf8
        hGetContents inputHandle
      case result of
        Right s   -> processProgram s
        Left  err -> print err
    _ -> printf "Usage: %s examples\\test.f\n" progName


