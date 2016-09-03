module Errors where

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Control.Monad.Except as Except

import AST
import qualified PrettyPrinter as PP

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser Parsec.ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ PP.unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


type ThrowsError = Either LispError
type IOThrowsError = Except.ExceptT LispError IO

trapError action = Except.catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = Except.throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = Except.runExceptT (trapError action) >>= return . extractValue
