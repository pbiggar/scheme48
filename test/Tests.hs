import Control.Monad (unless)
import System.Exit   (exitFailure)

import Test.Hspec
import System.IO.Silently
import System.Exit

import qualified Runner

et l r = do
  it ("should evaluate " ++ l ++ " to " ++ r) $ do
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
