module Runner (runFile, runSource, evalString, evalAndPrint) where

import           Control.Monad (liftM)
import qualified Control.Monad.Except as Except
import qualified System.IO as IO

import qualified Parser
import qualified Interpreter
import qualified PrettyPrinter as PP
import qualified Errors
import qualified Env
import Types


runSource :: String -> IO ()
runSource source = Env.primitiveBindings >>= flip evalAndPrint source

runFile :: [String] -> IO ()
runFile args = do
    env <- Env.primitiveBindings >>= flip Env.bindVars [("args", List $ map String $ drop 1 args)]
    (Errors.runIOThrows $ liftM show $ Interpreter.eval env (List [Atom "load", String (args !! 0)]))
        >>= IO.hPutStrLn IO.stderr

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
