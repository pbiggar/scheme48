module Main where

import qualified System.Environment as Env
import qualified System.Exit as Exit

import qualified Runner as Runner
import qualified Repl as Repl

main :: IO ()
main = do
        args <- Env.getArgs
        case length args of
          0 -> Repl.runRepl
          1 -> Runner.run $ args !! 0
          otherwise -> putStrLn "Program takes only 0 or 1 argument"
        Exit.exitSuccess
