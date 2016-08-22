module Runner (run) where

import Control.Monad

import Parser (readExpr)
import Interpreter (eval)
import PrettyPrinter (showVal)
import Errors (extractValue, trapError)


run :: String -> IO ()
run source = do
  let debug = False
  if debug
    then putStrLn $ "Args are:  " ++ source
    else return ()

  parsed <- return $ readExpr source
  if debug
    then do
--       putStrLn $ "Parsed:    " ++ (extractValue $ trapError parsed)
       putStrLn $ "Structure: " ++ (show parsed)
    else return ()


  evaled <- return $ liftM showVal $ parsed >>= eval
  putStr $ extractValue $ trapError evaled
