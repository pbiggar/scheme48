module Interpreter (eval) where

import Control.Monad.Except (throwError)

import AST
import Errors
import qualified Builtins

eval :: LispVal -> ThrowsError LispVal
eval val@(Bool _) = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
--eval (Vector as) = "'#(" ++ (unwordsList as) ++ ")"
--eval (DottedList head tail) = "(" ++ (unwordsList head) ++ " . " ++ (eval tail) ++ ")"
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func Builtins.builtins)
