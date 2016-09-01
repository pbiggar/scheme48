module Repl (runRepl) where

import           Control.Monad (liftM)
import qualified System.IO as IO
import qualified Runner as Runner



flushStr :: String -> IO ()
flushStr str = putStr str >> IO.hFlush IO.stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint expr = Runner.evalString expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit")
                 (readPrompt "Lisp>>> ")
                 evalAndPrint
