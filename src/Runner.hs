module Runner (run, evalString) where

import           Control.Monad (liftM)

import qualified Parser
import qualified Interpreter
import qualified PrettyPrinter
import qualified Errors


run :: String -> IO ()
run source = evalString source >>= putStr

evalString :: String -> IO String
evalString expr =
  return $ Errors.extractValue
         $ Errors.trapError
            (liftM PrettyPrinter.showVal $ Parser.readExpr expr >>= Interpreter.eval)
