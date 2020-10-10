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

processCommand :: Command -> Context -> Store -> IO (Context, Store)
processCommand cmd ctx store = do
  ecs <- runExceptT work
  case ecs of
    Right (ctx', store') -> return (ctx', store')
    Left  err            -> do
      putStrLn $ concat ["Error: ", show err]
      return (ctx, store)
 where
  timeoutErr :: (MonadError IError m, MonadIO m) => Int -> a -> m a
  timeoutErr timeLimit op = do
    x <- liftIO $ timeout timeLimit (return $! op)
    case x of
      Just success -> return success
      Nothing      -> throwError ETimeout
  work :: (MonadError IError m, MonadIO m) => m (Context, Store)
  work = do
    case cmd of
      CmdEval term -> do
        liftIO $ putStrLn $ "Eval> " ++ showTerm ctx term
        ty <- liftEither $ typeof ctx store term
        liftIO $ putStrLn $ "    : " ++ showType ctx ty
        let op = eval ctx store term
        (term', store') <- timeoutErr 3000000 op >>= liftEither
        liftIO $ putStrLn $ "   => " ++ showTerm ctx term'
        return (ctx, store')
      CmdBind s b -> do
        (store', b') <-
          liftEither $ checkBinding ctx store b >>= evalBinding ctx store
        let ctx' = addBinding s b' ctx
        liftIO . putStrLn $ "Bind> " ++ (s ++ showBinding ctx b')
        return (ctx', store')

processProgram :: String -> Context -> Store -> IO (Context, Store)
processProgram input ctx store = case runParser program ctx "main" input of
  Right (cmds, _) -> loop cmds ctx store   where
    loop :: [Command] -> Context -> Store -> IO (Context, Store)
    loop (cmd : cs) ctx store = do
      (ctx', store') <- processCommand cmd ctx store
      loop cs ctx' store'
    loop [] ctx store = return (ctx, store)
  Left err -> do
    putStrLn $ concat ["Error : ", show err]
    return (ctx, store)

runREPL :: String -> IO ()
runREPL progName = printHelp >> loop emptyContext emptyStore where
  printHelp :: IO ()
  printHelp = do
    printf "Usage: %s examples\\test.f\n" progName
    putStrLn "\n-- Commmands --"
    putStrLn "\t:q    exit"
    putStrLn "\t:c    display context"
    putStrLn "\t:s    display store"
    putStrLn "\t<expression | binding> evaluate"
  loop :: Context -> Store -> IO ()
  loop ctx store = do
    input <- putStr (progName ++ ">>> ") >> getLine
    case take 2 input of
      ":q" -> return ()
      ":c" -> do
        putStrLn $ showContext ctx
        loop ctx store
      ":s" -> do
        putStrLn
          $   show
          $   (\(t, ty) -> concat [showTerm ctx t, ":", showType ctx ty])
          <$> store
        loop ctx store
      _ -> do
        (ctx', store') <- processProgram input ctx store
        loop ctx' store'

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
        Right s   -> processProgram s emptyContext emptyStore >> return ()
        Left  err -> print err
    _ -> do
      runREPL progName


