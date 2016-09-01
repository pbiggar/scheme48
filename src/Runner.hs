module Runner (run, runRepl) where

import           Control.Monad (liftM)
import qualified System.IO as IO

import qualified Parser
import qualified Interpreter
import qualified PrettyPrinter
import qualified Errors


run :: String -> IO ()
run source = do
  let debug = False
  if debug
    then putStrLn $ "Args are:  " ++ source
    else return ()

  parsed <- return $ Parser.readExpr source
  if debug
    then do
--       putStrLn $ "Parsed:    " ++ (extractValue $ trapError parsed)
       putStrLn $ "Structure: " ++ (show parsed)
    else return ()


  evaled <- return $ liftM PrettyPrinter.showVal $ parsed >>= Interpreter.eval
  putStr $ Errors.extractValue $ Errors.trapError evaled


flushStr :: String -> IO ()
flushStr str = putStr str >> IO.hFlush IO.stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr =
  return $ Errors.extractValue
         $ Errors.trapError
            (liftM PrettyPrinter.showVal $ Parser.readExpr expr >>= Interpreter.eval)


evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
