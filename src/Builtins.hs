{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Builtins (builtins) where

import Control.Monad.Error (throwError)

import AST
import Errors


builtins :: [(String, [LispVal] -> ThrowsError LispVal)]
builtins = numbers ++ symbols ++ strings

wrongNumArgs :: Integer -> [LispVal] -> ThrowsError LispVal
wrongNumArgs i args = throwError $ NumArgs i args

numbers = [
    ("number?", isNumber)
  , ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)]

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number n] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber ps = wrongNumArgs 1 ps

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = wrongNumArgs 2 []
numericBinop _ [a] = wrongNumArgs 2 [a]
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum lv@(String n) = let parsed = reads n in
                        if null parsed
                          then throwError $ TypeMismatch "Int" lv
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = return 0



strings = [("string?", isString)]

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString as = wrongNumArgs 1 as



symbols = [ ("symbol?", isSymbol)
          , ("string->symbol", string2symbol)
          , ("symbol->string", symbol2string)]

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol as = wrongNumArgs 1 as

symbol2string :: [LispVal] -> ThrowsError LispVal
symbol2string [Atom s] = return $ String s
symbol2string as = wrongNumArgs 1 as

string2symbol :: [LispVal] -> ThrowsError LispVal
string2symbol [String s] = return $ Atom s
string2symbol as = wrongNumArgs 1 as
