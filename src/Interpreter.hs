module Interpreter (eval) where

import Control.Monad.Except (throwError)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace

import Types
import Errors
import qualified Builtins
import Env
import qualified PrettyPrinter as PP

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
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (func : args)) =
  apply env func args

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalIf :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalIf env pred conseq alt = do
  result <- eval env pred
  case result of
    Bool True -> eval env conseq
    otherwise -> eval env alt

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body =
  return $ Func (map PP.showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . PP.showVal

apply :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ (PrimitiveFunc func) args = liftThrows $ func args
apply env (Func params varargs body closure) args = do
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args)
         >>= bindVarArgs varargs
         >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env

apply env function args = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply env func argVals
