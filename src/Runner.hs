module Runner (run, evalString, evalAndPrint) where

import           Control.Monad (liftM)
import qualified Control.Monad.Except as Except

import qualified Parser
import qualified Interpreter
import qualified PrettyPrinter as PP
import qualified Errors
import qualified Env
import Types


run :: String -> IO ()
run source = Env.primitiveBindings >>= flip evalAndPrint source

evalString :: Env -> String -> IO String
evalString env expr =
  Errors.runIOThrows
  $ liftM PP.showVal
  $ (Errors.liftThrows $ Parser.readExpr expr)
  >>= Interpreter.eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
  str <- Errors.runIOThrows $ liftM show $ Errors.liftThrows $ Parser.readExpr expr
--  putStrLn str
  evalString env expr >>= putStrLn
