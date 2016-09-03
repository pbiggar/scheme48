module Runner (run, evalString, evalAndPrint) where

import           Control.Monad (liftM)
import qualified Control.Monad.Except as Except

import qualified Parser
import qualified Interpreter
import qualified PrettyPrinter as PP
import qualified Errors
import qualified Env


run :: String -> IO ()
run source = Env.nullEnv >>= flip evalAndPrint source

evalString :: Env.Env -> String -> IO String
evalString env expr =
  Errors.runIOThrows
  $ liftM PP.showVal
  $ (Errors.liftThrows $ Parser.readExpr expr)
  >>= Interpreter.eval env

evalAndPrint :: Env.Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
