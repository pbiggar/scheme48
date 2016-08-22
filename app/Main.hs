module Main where

import System.Environment as Env
import System.Exit as Exit

import qualified Runner

main :: IO ()
main = do
  (expr:_) <- Env.getArgs
  Runner.run expr
  Exit.exitSuccess
