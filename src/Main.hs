module Main where

import System.Environment as Env
import System.Exit as Exit

import Parser (readExpr)
import Interpreter (eval)
import PrettyPrinter (showVal)


main :: IO ()
main = do
  (expr:rest) <- Env.getArgs
  let debug = rest /= []
  if debug
    then putStrLn $ "Args are:  " ++ expr
    else return ()

  let parsed = readExpr expr
  if debug
    then do
       putStrLn $ "Parsed:    " ++ (showVal parsed)
       putStrLn $ "Structure: " ++ (show parsed)
    else return ()

  let evaled = eval parsed
  putStrLn $ showVal evaled
  Exit.exitSuccess
