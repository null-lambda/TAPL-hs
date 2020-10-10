{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Main where

import           Eval
import           Syntax
import           Parser
import           Typechecker

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Maybe
import           System.Environment
import           System.IO
import           System.IO.Error                ( tryIOError )
import           System.Timeout                 ( timeout )
import           Text.Printf

processCommand :: Command -> Context -> IO (Context)
processCommand cmd ctx = execStateT (work cmd) ctx where
  work :: (MonadState Context m, MonadIO m) => Command -> m ()
  work cmd = do
    ctx <- get
    case cmd of
      CmdEval term -> liftIO $ do
        putStrLn $ "Eval : " ++ showTerm ctx term
        let op = showTerm ctx $ eval ctx term
        result <- fromMaybe "[timeout]" <$> timeout 3000000 (return $! op)
        putStrLn $ "   => " ++ result
        putStrLn $ "    : " ++ either angular (showType ctx) (typeof ctx term)
        where angular s = "[" ++ s ++ "]"
      CmdBind s b -> case checkBinding ctx b of
        Right b' -> do
          let ctx' = addBinding s b' ctx
          put ctx'
          liftIO . putStrLn $ " Bind : " ++ (s ++ showBinding ctx b')
        Left err -> liftIO . putStrLn $ " *Err : " ++ err

processProgram :: String -> Context -> IO Context
processProgram input ctx = case runParser program ctx "main" input of
  Right (cmds, _) -> loop cmds ctx   where
    loop :: [Command] -> Context -> IO Context
    loop (cmd : cs) ctx = do
      ctx' <- processCommand cmd ctx
      loop cs ctx'
    loop [] ctx = return ctx
  Left err -> do
    putStrLn "parse error"
    putStrLn err
    return ctx

runREPL :: String -> IO ()
runREPL progName = printHelp >> loop emptyContext where
  printHelp :: IO ()
  printHelp = do
    printf "Usage: %s examples\\test.f\n" progName
    putStrLn "press :q to exit"
  loop :: Context -> IO ()
  loop ctx = do
    input <- putStr (progName ++ "> ") >> getLine
    unless (take 2 input == ":q") $ do
      ctx' <- processProgram input ctx
      loop ctx'

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
        Right s   -> processProgram s emptyContext >> return ()
        Left  err -> print err
    _ -> do
      runREPL progName


