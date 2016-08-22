import Control.Monad (unless)
import System.Exit   (exitFailure)

import Test.Hspec
import System.IO.Silently
import System.Exit

import qualified Runner

et :: String -> String -> SpecWith ()
et l r = do
  it ("[" ++ l ++ " == " ++ r ++ "]") $ do
    (result, _) <- capture $ Runner.run l
    result `shouldBe` r

main :: IO ()
main = do
  hspec $ do
    describe "simple operations" $ do
      et "(+ 2 2)" "4"
      et "(symbol? 'a)" "#t"
      et "(string? asd)" "#f"
      et "(- (+ 4 6 3) 3 5 2)" "3"
      et "(+ 2 \"two\")" "Invalid type: expected Int, found String \"two\""
      et "(+ 2)" "Expected 2 args; found values 2"
      et "(what? 2)" "Unrecognized primitive function: \"what?\""
      et "(+ 2 2)" "4"
      et "(+ 2 (- 4 1))" "5"
      et "(- (+ 4 6 3) 3 5 2)" "3"
      et "'atom" "atom"
      et "2" "2"
      et "\"a string\"" "\"a string\""
      et "(a '(imbalanced parens)"
       "Parse error at \"lisp\" (line 1, column 24):\nunexpected end of input\nexpecting space or \")\""
