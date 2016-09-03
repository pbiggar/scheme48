module Interpreter (eval) where

import Control.Monad.Except (throwError)

import AST
import Errors
import qualified Builtins
import Env

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(Bool _) = return val
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Character _) = return val
eval _ (List [Atom "quote", val]) = return val


eval env (List [Atom "if", pred, conseq, alt]) =
  evalIf env pred conseq alt
eval env val@(Atom id) =
  getVar env id

eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= Env.setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= Env.defineVar env var
eval env (List (Atom func : args)) =
  callFunction env func args

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalIf :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalIf env pred conseq alt = do
  result <- eval env pred
  case result of
    Bool True -> eval env conseq
    otherwise -> eval env alt

callFunction env func args = do
  mapM (eval env) args >>= liftThrows . apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func Builtins.builtins)
