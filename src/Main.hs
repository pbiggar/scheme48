module Main where

import System.Environment as Env
import System.Exit as Exit
import Control.Monad

import Parser (readExpr)
import Interpreter (eval)
import PrettyPrinter (showVal)
import Errors (extractValue, trapError)


main :: IO ()
main = do
  (expr:rest) <- Env.getArgs
  let debug = rest /= []
  if debug
    then putStrLn $ "Args are:  " ++ expr
    else return ()

  parsed <- return $ readExpr expr
  if debug
    then do
--       putStrLn $ "Parsed:    " ++ (extractValue $ trapError parsed)
       putStrLn $ "Structure: " ++ (show parsed)
    else return ()


  evaled <- return $ liftM showVal $ parsed >>= eval
  putStrLn $ extractValue $ trapError evaled
  Exit.exitSuccess
