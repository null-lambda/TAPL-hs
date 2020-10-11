{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Main where

import           Eval
import           Syntax
import           Parser
import           Typechecker

import           Control.Monad.Except
import           System.Environment
import           System.IO
import           System.IO.Error                ( tryIOError )
import           System.Timeout                 ( timeout )
import           Text.Printf

processCommand :: Command -> Context -> IO Context
processCommand cmd ctx = do
  ecs <- runExceptT work
  case ecs of
    Right ctx' -> return ctx'
    Left  err  -> do
      putStrLn $ concat ["Error: ", show err]
      return ctx
 where
  timeoutErr :: (MonadError IError m, MonadIO m) => Int -> a -> m a
  timeoutErr timeLimit op = do
    x <- liftIO $ timeout timeLimit (return $! op)
    case x of
      Just success -> return success
      Nothing      -> throwError ETimeout
  work :: (MonadError IError m, MonadIO m) => m Context
  work = do
    case cmd of
      CmdEval term -> do
        liftIO $ putStrLn $ "Eval> " ++ showTerm ctx term
        ty <- liftEither $ typeof ctx term
        liftIO $ putStrLn $ "    : " ++ showType ctx ty
        let op = eval ctx term
        term' <- timeoutErr 3000000 op >>= liftEither
        liftIO $ putStrLn $ "   => " ++ showTerm ctx term'
        return ctx
      CmdBind s b -> do
        b'  <- liftEither $ checkBinding ctx b
        b'' <- liftEither $ evalBinding ctx b'
        liftIO . putStrLn $ "Bind> " ++ (s ++ showBinding ctx b')
        let ctx' = addBinding s b'' ctx
        return ctx'

processProgram :: String -> Context -> IO Context
processProgram input ctx = case runParser program ctx "main" input of
  Right (cmds, _) -> loop cmds ctx   where
    loop :: [Command] -> Context -> IO Context
    loop (cmd : cs) ctx = do
      ctx' <- processCommand cmd ctx
      loop cs ctx'
    loop [] ctx = return ctx
  Left err -> do
    putStrLn $ concat ["Error : ", show err]
    return ctx

runREPL :: String -> IO ()
runREPL progName = printHelp >> loop emptyContext where
  printHelp :: IO ()
  printHelp = do
    printf "Usage: %s examples\\test.f\n" progName
    putStrLn "\n-- Commmands --"
    putStrLn "\t:q    exit"
    putStrLn "\t:c    display context"
    putStrLn "\t<expression | binding> evaluate"
  loop :: Context -> IO ()
  loop ctx = do
    putStr (progName ++ ">>> ")
    input <- getLine
    case take 2 input of
      ":q" -> return ()
      ":c" -> do
        putStrLn $ showContext ctx
        loop ctx
      _ -> do
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


