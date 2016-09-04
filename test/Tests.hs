import Control.Monad (unless)
import System.Exit   (exitFailure)

import Test.Hspec
import System.IO.Silently
import System.Exit
import Control.Monad (liftM)

import qualified Runner
import qualified Env
import Types


et :: String -> String -> SpecWith ()
et l r = do
  it ("[" ++ l ++ " == " ++ r ++ "]") $ do
    (result, _) <- capture $ Runner.runSource l
    result `shouldBe` (r ++ "\n")

rt :: Env -> String -> String -> SpecWith ()
rt ioenv l r = do
  it ("[" ++ l ++ " == " ++ r ++ "]") $ do
    (result, _) <- capture $ Runner.evalAndPrint ioenv l
    result `shouldBe` (r ++ "\n")

main :: IO ()
main = do
  env <- Env.primitiveBindings
  env2 <- Env.primitiveBindings

  hspec $ do
    describe "simple operations" $ do
      et "(+ 2 2)" "4"
      et "(symbol? 'a)" "#t"
      et "(string? 5)" "#f"
      et "(- (+ 4 6 3) 3 5 2)" "3"
      et "(+ 2 \"two\")" "Invalid type: expected Int, found String \"two\""
      et "(+ 2)" "Expected 2 args; found values 2"
      et "(what? 2)" "Getting an unbound variable: what?"
      et "(+ 2 2)" "4"
      et "(+ 2 (- 4 1))" "5"
      et "(- (+ 4 6 3) 3 5 2)" "3"
      et "'atom" "atom"
      et "2" "2"
      et "\"a string\"" "\"a string\""
      et "(a '(imbalanced parens)"
       "Parse error at \"lisp\" (line 1, column 24):\nunexpected end of input\nexpecting space or \")\""
      et "(< 2 3)" "#t"
      et "(> 2 3)" "#f"
      et "(>= 3 3)" "#t"
      et "(string=? \"test\"  \"test\")" "#t"
      et "(string<? \"abc\" \"bba\")" "#t"
      et "(if (> 2 3) \"no\" \"yes\")" "\"yes\""
      et "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")" "9"
      et "(cdr '(a simple test))" "(simple test)"
      et "(car (cdr '(a simple test)))" "simple"
      et "(car '((this is) a test))" "(this is)"
      et "(cons '(this is) 'test)" "((this is) . test)"
      et "(cons '(this is) '())" "((this is))"
      et "(eqv? 1 3)" "#f"
      et "(eqv? 3 3)" "#t"
      et "(eqv? 'atom 'atom)" "#t"

    describe "repl operations" $ do
      rt env "(define x 3)" "3"
      rt env "(+ x 2)" "5"
      rt env "(+ y 2)" "Getting an unbound variable: y"
      rt env "(define y 5)" "5"
      rt env "(+ x (- y 2))" "6"
      rt env "(define str \"A string\")" "\"A string\""
      rt env "(< str \"The string\")" "Invalid type: expected Int, found String \"A string\""
      rt env "(string<? str \"The string\")" "#t"

    describe "functions" $ do
      rt env2 "(define (f x y) (+ x y))" "(lambda (\"x\" \"y\") ...)"
      rt env2 "(f 1 2)" "3"
      rt env2 "(f 1 2 3)" "Expected 2 args; found values 1 2 3"
      rt env2 "(f 1)" "Expected 2 args; found values 1"
      rt env2 "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))" "(lambda (\"x\") ...)"
      rt env2 "(factorial 10)" "3628800"
      rt env2 "(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))" "(lambda (\"inc\") ...)"
      rt env2 "(define my-count (counter 5))" "(lambda (\"x\") ...)"
      rt env2 "(my-count 3)" "8"
      rt env2 "(my-count 6)" "14"
      rt env2 "(my-count 5)" "19"
