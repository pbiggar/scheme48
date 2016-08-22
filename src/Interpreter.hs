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
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = evalIf pred conseq alt
eval (List (Atom func : args)) = callFunction func args

--eval (Vector as) = "'#(" ++ (unwordsList as) ++ ")"
--eval (DottedList head tail) = "(" ++ (unwordsList head) ++ " . " ++ (eval tail) ++ ")"
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm



evalIf :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
evalIf pred conseq alt = do
  result <- eval pred
  asBool <- Builtins.unpackBool result
  case result of
    Bool True -> eval conseq
    otherwise -> eval alt

callFunction func args = do
  mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func Builtins.builtins)
