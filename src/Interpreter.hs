module Interpreter (eval) where

import AST
import qualified Builtins


eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval val@(Bool _) = val
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Character _) = val
eval (List (Atom func : args)) = apply func $ map eval args
--eval (Vector as) = "'#(" ++ (unwordsList as) ++ ")"
--eval (DottedList head tail) = "(" ++ (unwordsList head) ++ " . " ++ (eval tail) ++ ")"

eval other = String $ "Not supported yet: " ++ show other

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) (lookup func Builtins.builtins)
