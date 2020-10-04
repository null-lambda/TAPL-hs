module Main where

import qualified Eval
import qualified Eval.Bigstep
import qualified Eval.CallByName
import qualified Eval.Bigstep.CallByName
import qualified Eval.Normal
import qualified Eval.Bigstep.Normal
import           Syntax
import           SyntaxParser

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State
import           Data.List
import           Data.Maybe
import           System.Environment             ( getArgs )
import           System.IO.Error                ( tryIOError )
import           System.Timeout
import           Text.Parsec                    ( runParser )

processCommand :: Command -> StateT Context IO ()
processCommand cmd = do
  ctx <- get
  case cmd of
    Eval term -> liftIO $ do
      let evalStrategies =
            [ (Eval.eval                   , "Call-By-Value")
            , (Eval.Bigstep.eval           , "Call-By-Value, Big-Step")
            , (Eval.Normal.eval            , "Normal Order")
            , (Eval.Bigstep.Normal.eval    , "Normal Order, Big-Step")
            , (Eval.CallByName.eval        , "Call-By-Name")
            , (Eval.Bigstep.CallByName.eval, "Call-By-Name, Big-Step")
            ]
      putStrLn $ "Eval: " ++ showTerm ctx term
      forM_ evalStrategies $ \(eval, name) -> do
        let resultOp = return $! showTerm ctx $ eval ctx term
        result <- fromMaybe "[infinite loop]" <$> timeout 5000 resultOp
        putStrLn $ ("   => " ++ result) ++ ("  (" ++ name ++ ")")
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


